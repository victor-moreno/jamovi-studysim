# Marginal distributions of the library: the shorthand "normal(12.6, 1.4)" and
# its quantile/CDF on the natural scale, optionally truncated to a range.
# Pure R, no jmvcore: the engine is tested and usable without jamovi.

# Numbers in the library may be written as small expressions ("log(1.04)",
# "2.5/1000"). Only arithmetic and a few functions are allowed, so a library
# file can never run arbitrary code.
num_expr <- function(x) {
    if (is.numeric(x)) return(as.numeric(x))
    s <- trimws(as.character(x))
    bare <- gsub("\\b(log|exp|sqrt|log10)\\b", "", s)
    if (!grepl("^[0-9eE.+*/() -]*$", bare) || !nzchar(s))
        stop(sprintf("'%s' is not a number", s), call. = FALSE)
    env <- list2env(list(log = log, exp = exp, sqrt = sqrt, log10 = log10,
                         `+` = `+`, `-` = `-`, `*` = `*`, `/` = `/`, `(` = `(`),
                    parent = emptyenv())
    v <- tryCatch(eval(parse(text = s), env), error = function(e) NA_real_)
    if (!is.numeric(v) || length(v) != 1 || is.na(v))
        stop(sprintf("'%s' is not a number", s), call. = FALSE)
    v
}

# family -> number of parameters, as written in the library (natural scale)
DIST_ARGS <- c(normal = 2, lognormal = 2, gamma = 2, beta = 2, uniform = 2,
               poisson = 1, negbin = 2, exponential = 1)

parse_dist <- function(x) {
    if (is.list(x) && !is.null(x$family)) return(x)
    m <- regmatches(x, regexec("^\\s*([a-z_]+)\\s*\\((.*)\\)\\s*$", x))[[1]]
    if (length(m) == 0 && grepl("^\\s*[a-z_]+\\s*\\([^)]*$", x))
        stop(sprintf("cannot read the distribution '%s': inside {...} the comma splits the line; quote it, e.g. dist: \"normal(25, 4)\"", x), call. = FALSE)
    if (length(m) == 0)
        stop(sprintf("cannot read the distribution '%s' (expected e.g. normal(12.6, 1.4))", x), call. = FALSE)
    fam <- m[2]
    if (!fam %in% names(DIST_ARGS))
        stop(sprintf("unknown distribution '%s' (known: %s)", fam,
                     paste(names(DIST_ARGS), collapse = ", ")), call. = FALSE)
    args <- trimws(strsplit(m[3], "[,;]")[[1]])
    if (length(args) != DIST_ARGS[[fam]])
        stop(sprintf("%s() takes %d values, got %d", fam, DIST_ARGS[[fam]], length(args)), call. = FALSE)
    vals <- vapply(args, num_expr, numeric(1), USE.NAMES = FALSE)
    d <- list(family = fam, args = vals)
    dist_funs(d)   # validates the parameters
    d
}

# p/q functions with the library's parameters translated to R's
dist_funs <- function(d) {
    a <- d$args
    bad <- function(msg) stop(sprintf("%s(%s): %s", d$family, paste(a, collapse = ", "), msg), call. = FALSE)
    switch(d$family,
        normal = {
            if (a[2] <= 0) bad("sd must be > 0")
            list(p = function(x) stats::pnorm(x, a[1], a[2]), q = function(u) stats::qnorm(u, a[1], a[2]))
        },
        lognormal = {
            # mean and sd on the natural scale, as papers report them
            if (a[1] <= 0 || a[2] <= 0) bad("mean and sd must be > 0")
            sl <- sqrt(log(1 + (a[2] / a[1])^2)); ml <- log(a[1]) - sl^2 / 2
            list(p = function(x) stats::plnorm(x, ml, sl), q = function(u) stats::qlnorm(u, ml, sl))
        },
        gamma = {
            if (a[1] <= 0 || a[2] <= 0) bad("mean and sd must be > 0")
            sh <- (a[1] / a[2])^2; rt <- a[1] / a[2]^2
            list(p = function(x) stats::pgamma(x, sh, rt), q = function(u) stats::qgamma(u, sh, rt))
        },
        beta = {
            m <- a[1]; v <- a[2]^2
            if (m <= 0 || m >= 1 || v <= 0 || v >= m * (1 - m)) bad("needs 0 < mean < 1 and sd^2 < mean(1-mean)")
            k <- m * (1 - m) / v - 1
            list(p = function(x) stats::pbeta(x, m * k, (1 - m) * k), q = function(u) stats::qbeta(u, m * k, (1 - m) * k))
        },
        uniform = {
            if (a[2] <= a[1]) bad("max must be > min")
            list(p = function(x) stats::punif(x, a[1], a[2]), q = function(u) stats::qunif(u, a[1], a[2]))
        },
        poisson = {
            if (a[1] <= 0) bad("mean must be > 0")
            list(p = function(x) stats::ppois(x, a[1]), q = function(u) stats::qpois(u, a[1]))
        },
        negbin = {
            if (a[1] <= 0 || a[2] <= 0) bad("mean and size must be > 0")
            list(p = function(x) stats::pnbinom(x, size = a[2], mu = a[1]),
                 q = function(u) stats::qnbinom(u, size = a[2], mu = a[1]))
        },
        exponential = {
            if (a[1] <= 0) bad("mean must be > 0")
            list(p = function(x) stats::pexp(x, 1 / a[1]), q = function(u) stats::qexp(u, 1 / a[1]))
        })
}

# Quantile transform u in (0,1) -> x, truncated to range = c(lo, hi) by
# rescaling u into [F(lo), F(hi)] (exact truncation, no clipping pile-ups).
qdist <- function(u, d, range = NULL) {
    f <- dist_funs(d)
    if (!is.null(range)) {
        lo <- f$p(range[1]); hi <- f$p(range[2])
        if (d$family %in% c("poisson", "negbin")) lo <- f$p(range[1] - 1)
        if (hi - lo < 1e-6)
            stop(sprintf("range [%s, %s] holds almost none of %s(%s)", range[1], range[2],
                         d$family, paste(d$args, collapse = ", ")), call. = FALSE)
        u <- lo + u * (hi - lo)
    }
    u <- pmin(pmax(u, 1e-12), 1 - 1e-12)
    f$q(u)
}
