# The superpopulation: covariates drawn from a DAG-structured Gaussian copula
# (latent standard normals, exogenous ones correlated, endogenous ones linear in
# their parents; then each latent is mapped to its marginal by the quantile
# transform, NORTA). Generated in fixed-size chunks with seeds derived from
# (seed, scenario, chunk), so person k is the same person whatever the design or
# sample size asks for: the "same population across designs" promise.

CHUNK <- 5000L

# A 28-bit integer seed from any parts (a student key, a scenario id, a chunk).
derive_seed <- function(...) {
    h <- digest::digest(paste(..., sep = "|"), algo = "xxhash32", serialize = FALSE)
    strtoi(substr(h, 1, 7), 16L)
}

# Evaluate expr under a fixed seed and RNG kind, leaving the caller's stream as it was.
with_seed <- function(seed, expr) {
    genv <- globalenv()
    had <- exists(".Random.seed", envir = genv, inherits = FALSE)
    old <- if (had) get(".Random.seed", envir = genv) else NULL
    oldk <- RNGkind()
    on.exit({
        RNGkind(oldk[1], oldk[2], oldk[3])
        if (had) assign(".Random.seed", old, envir = genv)
        else if (exists(".Random.seed", envir = genv, inherits = FALSE)) rm(".Random.seed", envir = genv)
    })
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
    expr
}

# shift: means of the exogenous latents (a region's, in the ecological design)
latent_draw <- function(sc, n, shift = NULL) {
    p <- length(sc$order)
    Z <- matrix(0, n, p, dimnames = list(NULL, sc$order))
    exo <- sc$exogenous
    if (length(exo)) {
        L <- chol(sc$R_exo[exo, exo, drop = FALSE])
        Z[, exo] <- matrix(stats::rnorm(n * length(exo)), n) %*% L
        if (!is.null(shift)) Z[, exo] <- Z[, exo] + rep(shift[exo], each = n)
    }
    for (v in setdiff(sc$order, exo)) {
        pe <- sc$edges[sc$edges$to == v, , drop = FALSE]
        Z[, v] <- Z[, pe$from, drop = FALSE] %*% pe$b + sc$resid[[v]] * stats::rnorm(n)
    }
    Z
}

# latent z -> the variable's internal code: numbers for continuous/count,
# 0/1 for binary, 1..K for nominal/ordinal/genotype
latent_to_code <- function(z, v) {
    u <- stats::pnorm(z)
    switch(v$type,
        continuous = {
            x <- qdist(u, v$dist, v$range)
            if (!is.null(v$round)) x <- round(x, v$round)
            x
        },
        count = as.integer(qdist(u, v$dist, v$range)),
        binary = as.integer(u > 1 - v$p),
        findInterval(u, cumsum(v$probs)[-length(v$probs)]) + 1L)
}

gen_covariates <- function(sc, n, shift = NULL) {
    Z <- latent_draw(sc, n, shift)
    cov <- as.data.frame(lapply(stats::setNames(sc$order, sc$order), function(v) {
        x <- sc$variables[[v]]
        if (!is.null(x$formula)) return(rep(NA_real_, n))   # filled by its rule
        latent_to_code(Z[, v], x)
    }), stringsAsFactors = FALSE)
    if (length(sc$rule_order)) cov <- apply_rules(cov, Z, sc)
    cov
}

# Chunk k of the superpopulation for one outcome: covariates (seeded by the
# scenario only) plus the outcome's own noise (seeded by the outcome as well).
pop_chunk <- function(sc, outcome, seed, k) {
    cov <- with_seed(derive_seed(seed, sc$id, "cov", k), gen_covariates(sc, CHUNK))
    noise <- with_seed(derive_seed(seed, sc$id, outcome, "noise", k),
                       data.frame(u = stats::runif(CHUNK), z = stats::rnorm(CHUNK),
                                  u2 = stats::runif(CHUNK), u3 = stats::runif(CHUNK)))
    cov$.pid <- (k - 1L) * CHUNK + seq_len(CHUNK)
    list(cov = cov, noise = noise)
}

# the first n people of the superpopulation
population <- function(sc, outcome, seed, n) {
    ks <- seq_len(ceiling(n / CHUNK))
    ch <- lapply(ks, function(k) pop_chunk(sc, outcome, seed, k))
    cov <- do.call(rbind, lapply(ch, `[[`, "cov"))[seq_len(n), , drop = FALSE]
    noise <- do.call(rbind, lapply(ch, `[[`, "noise"))[seq_len(n), , drop = FALSE]
    rownames(cov) <- rownames(noise) <- NULL
    list(cov = cov, noise = noise)
}
