# Realism layer, applied to a formatted study (engine names, library levels):
# measurement error (also applied to the answer key's large study, so the key
# shows the dilution it causes), extra noise variables and data-entry errors
# (student data only; the key comes from clean data).

# Non-differential measurement error of the measured variables:
# - numeric: observed = true + error, with reliability = var(true) / var(observed)
# - categorical: with probability `misclass` a value is recorded as another
#   level (an adjacent one for ordinal variables)
measurement_error <- function(fd, sc, factors, reliability = 1, misclass = 0, seed = 1) {
    if (reliability >= 1 && misclass <= 0) return(fd)
    with_seed(seed, {
        for (v in intersect(factors, names(fd))) {
            x <- sc$variables[[v]]; col <- fd[[v]]
            desc <- attr(col, "jmv-desc")
            if (x$type %in% c("continuous", "count") && is.numeric(col) && reliability < 1) {
                s <- stats::sd(col, na.rm = TRUE)
                y <- col + stats::rnorm(length(col), 0, s * sqrt((1 - reliability) / reliability))
                if (!is.null(x$range)) y <- pmin(pmax(y, x$range[1]), x$range[2])
                y <- if (x$type == "count") as.integer(round(pmax(y, 0))) else if (!is.null(x$round)) round(y, x$round) else y
                col <- y
            } else if (is.factor(col) && misclass > 0) {
                lv <- levels(col); K <- length(lv)
                k <- as.integer(col)
                flip <- !is.na(k) & stats::runif(length(k)) < misclass
                if (any(flip)) {
                    new <- if (is.ordered(col) || K == 2) {
                        # one step up or down, staying within the levels
                        step <- ifelse(stats::runif(sum(flip)) < 0.5, -1L, 1L)
                        kk <- k[flip] + step
                        ifelse(kk < 1L | kk > K, k[flip] - step, kk)
                    } else vapply(k[flip], function(j) sample(setdiff(seq_len(K), j), 1), 1L)
                    k[flip] <- new
                }
                col <- factor(lv[k], levels = lv, ordered = is.ordered(col))
            }
            attr(col, "jmv-desc") <- desc
            fd[[v]] <- col
        }
    })
    fd
}

# k extra variables that have no effect at all (half numeric, half yes/no),
# for teaching multiple testing and variable selection
noise_names <- function(k) if (k > 0) sprintf("extra_%02d", seq_len(k)) else character()

add_noise_vars <- function(fd, k, seed = 1, lang = "en") {
    if (k <= 0) return(fd)
    n <- nrow(fd)
    unit <- fd$id %||% seq_len(n)
    ids <- unique(unit)
    with_seed(seed, {
        for (j in seq_len(k)) {
            nm <- noise_names(k)[j]
            v <- if (j %% 2 == 1) round(stats::rnorm(length(ids), 50, 10), 1)[match(unit, ids)]
                 else factor(c("no", "yes")[1 + (stats::runif(length(ids)) < 0.3)][match(unit, ids)], levels = c("no", "yes"))
            fd[[nm]] <- with_desc(v, sprintf("%s %d", gtext("extra", lang), j))
        }
    })
    fd
}

# Data-entry errors in the numeric measured variables: a share `pct` of their
# cells gets a decimal slip (x10 or /10), two digits swapped, or the code 999.
# Returns the data with an attribute "entry_errors" (how many cells).
entry_errors <- function(fd, cols, pct, seed = 1) {
    attr(fd, "entry_errors") <- 0L
    cols <- cols[vapply(cols, function(v) is.numeric(fd[[v]]), TRUE)]
    if (pct <= 0 || length(cols) == 0) return(fd)
    total <- 0L
    with_seed(seed, {
        for (v in cols) {
            x <- fd[[v]]; desc <- attr(x, "jmv-desc")
            hit <- which(!is.na(x) & stats::runif(length(x)) < pct)
            for (i in hit) {
                kind <- sample.int(4, 1)
                x[i] <- switch(kind, x[i] * 10, x[i] / 10, {
                    # the last two digits swapped (12.3 -> 13.2)
                    s <- strsplit(base::format(x[i], scientific = FALSE), "")[[1]]
                    d <- which(grepl("[0-9]", s))
                    if (length(d) >= 2) s[d[length(d) - c(1, 0)]] <- s[d[length(d) - c(0, 1)]]
                    y <- suppressWarnings(as.numeric(paste(s, collapse = "")))
                    if (is.na(y)) x[i] else y
                }, 999)
            }
            if (is.integer(fd[[v]])) x <- as.integer(round(x))
            attr(x, "jmv-desc") <- desc
            fd[[v]] <- x
            total <- total + length(hit)
        }
    })
    attr(fd, "entry_errors") <- total
    fd
}

# Laboratory realism for assays: markers (diagnostic design) and variables
# with a limit of detection (`lod:` in the library).
# - batches: people are assayed in `batches` consecutive groups (or, when
#   `confounded`, diseased and non-diseased in separate batches); each batch
#   multiplies the markers by exp(N(0, cv)) (adds N(0, cv) to a standardised
#   block), and a `batch` column records it;
# - below the limit of detection a value is reported missing, as LOD/2 or as
#   LOD (`lod_handling`), or kept ("none").
assay_realism <- function(fd, sc, oc, design, factors, opt, seed = 1, status = NULL, lang = "en") {
    nb <- as.integer(opt$batches %||% 0)
    mk <- if (design == "diagnostic") oc$markers else list()
    with_seed(seed, {
        if (nb >= 2 && length(mk)) {
            n <- nrow(fd)
            ord <- if (isTRUE(opt$batch_confounded) && !is.null(status)) order(status, seq_len(n)) else seq_len(n)
            b <- integer(n); b[ord] <- ceiling(seq_len(n) * nb / n)
            shift <- stats::rnorm(nb, 0, opt$batch_cv %||% 0.15)
            for (m in mk) for (nm in marker_cols(m)) {
                x <- fd[[nm]]; desc <- attr(x, "jmv-desc")
                x <- if (is.null(m$block) && all(x > 0, na.rm = TRUE)) x * exp(shift[b]) else x + shift[b]
                if (!is.null(m$round)) x <- round(x, m$round)
                attr(x, "jmv-desc") <- desc; fd[[nm]] <- x
            }
            fd$batch <- with_desc(b, gtext("batch", lang))
            # right after the markers, where planned_columns() puts it
            last <- max(match(unlist(lapply(mk, marker_cols)), names(fd)))
            cols <- names(fd)[names(fd) != "batch"]
            fd <- fd[, append(cols, "batch", after = last), drop = FALSE]
        }
    })
    how <- opt$lod_handling %||% "missing"
    if (how == "none") return(fd)
    lods <- c(lapply(stats::setNames(mk, vapply(mk, `[[`, "", "id")), `[[`, "lod"),
              lapply(stats::setNames(intersect(factors, names(fd)), intersect(factors, names(fd))), function(v) sc$variables[[v]]$lod))
    for (nm in names(lods)) {
        L <- lods[[nm]]
        if (is.null(L) || is.na(L) || !is.numeric(fd[[nm]])) next
        x <- fd[[nm]]; desc <- attr(x, "jmv-desc")
        low <- !is.na(x) & x < L
        x[low] <- switch(how, missing = NA, half = L / 2, lod = L)
        attr(x, "jmv-desc") <- sprintf("%s; < %s: %s", desc, base::format(L), switch(how, missing = gtext("lod_missing", lang),
                                       half = "LOD/2", lod = "LOD"))
        fd[[nm]] <- x
    }
    fd
}
