# Missing covariates in Derive scenario. The outcome model can use:
# - complete cases (default);
# - mean / mode imputation: fast, keeps every row, but standard errors are too
#   small and associations with the imputed variables are diluted;
# - single stochastic imputation: one set of chained equations (each variable
#   drawn from a model of the others and of the outcome, with its residual
#   noise), unbiased under missing at random but standard errors too small;
# - multiple imputation: m such sets, each model fitted to a bootstrap sample
#   of the observed rows (approximately proper), pooled by Rubin's rules.
# The outcome enters every imputation model (White & Royston 2009): the
# outcome itself, or for an event the status and the Nelson-Aalen cumulative
# hazard at the person's time. No mice: jamovi does not bundle it.

MISSING_METHODS <- c("complete", "mean", "single", "multiple")

# md: the model frame (covariates by id, plus .y or .s/.t); covs: ids
impute_mean <- function(md, covs) {
    for (v in covs) {
        x <- md[[v]]
        if (!anyNA(x)) next
        if (is.factor(x)) { tb <- table(x); x[is.na(x)] <- names(tb)[which.max(tb)] }
        else x[is.na(x)] <- mean(x, na.rm = TRUE)
        md[[v]] <- x
    }
    md
}

# outcome terms for the imputation models
outcome_terms <- function(md, type) {
    if (type == "event") {
        km <- survival::survfit(survival::Surv(md$.t, md$.s) ~ 1)
        h <- stats::stepfun(km$time, c(0, cumsum(km$n.event / km$n.risk)))
        return(data.frame(.os = md$.s, .oh = h(md$.t)))
    }
    data.frame(.oy = as.numeric(md$.y))
}

# m completed copies of md (chained equations, `iters` cycles each)
impute_chained <- function(md, covs, type, m = 1, iters = 10, seed = 1) {
    targets <- covs[vapply(covs, function(v) anyNA(md[[v]]), TRUE)]
    if (length(targets) == 0) return(rep(list(md), m))
    ot <- outcome_terms(md, type)
    with_seed(seed, lapply(seq_len(m), function(k) {
        d <- md
        # start from random observed values
        for (v in targets) { obs <- d[[v]][!is.na(d[[v]])]; mi <- is.na(d[[v]]); d[[v]][mi] <- obs[sample.int(length(obs), sum(mi), replace = TRUE)] }
        for (it in seq_len(iters)) for (v in targets) {
            mi <- which(is.na(md[[v]])); obs <- which(!is.na(md[[v]]))
            # a bootstrap sample of the observed rows makes the imputations proper
            boot <- if (m > 1) obs[sample.int(length(obs), length(obs), replace = TRUE)] else obs
            X <- cbind(d[setdiff(covs, v)], ot)
            fit_d <- cbind(.v = d[[v]], X)[boot, , drop = FALSE]
            new_d <- X[mi, , drop = FALSE]
            x <- d[[v]]
            if (is.factor(x) && nlevels(x) == 2) {
                f <- suppressWarnings(stats::glm(.v ~ ., stats::binomial(), fit_d))
                p <- suppressWarnings(stats::predict(f, new_d, type = "response"))
                x[mi] <- levels(x)[1 + (stats::runif(length(mi)) < p)]
            } else if (is.factor(x)) {
                f <- nnet::multinom(.v ~ ., fit_d, trace = FALSE)
                pr <- matrix(stats::predict(f, new_d, type = "probs"), nrow = length(mi))
                if (ncol(pr) == 1) pr <- cbind(1 - pr, pr)
                x[mi] <- levels(x)[apply(pr, 1, function(q) sample.int(length(q), 1, prob = q))]
            } else {
                f <- stats::lm(.v ~ ., fit_d)
                y <- suppressWarnings(stats::predict(f, new_d)) + stats::rnorm(length(mi), 0, summary(f)$sigma)
                rg <- range(md[[v]], na.rm = TRUE)
                y <- pmin(pmax(y, rg[1]), rg[2])
                if (all(abs(md[[v]][obs] - round(md[[v]][obs])) < 1e-8)) y <- round(y)
                x[mi] <- y
            }
            d[[v]] <- x
        }
        d
    }))
}

# Rubin's rules over a list of fits list(b, se)
pool_rubin <- function(fits) {
    m <- length(fits)
    B <- do.call(rbind, lapply(fits, `[[`, "b"))
    U <- do.call(rbind, lapply(fits, function(f) f$se^2))
    qbar <- colMeans(B); ubar <- colMeans(U)
    bvar <- if (m > 1) apply(B, 2, stats::var) else 0
    list(b = qbar, se = sqrt(ubar + (1 + 1 / m) * bvar))
}

# Messages for the person deriving: problems the data may hide
derive_warnings <- function(df, md, outcome_cols, type, cc, fit, est, ids, time = NULL) {
    w <- character()
    n <- nrow(md)
    if (sum(cc) < 0.5 * n)
        w <- c(w, sprintf("only %d of %d rows (%.0f%%) are complete for the model: consider an imputation method", sum(cc), n, 100 * sum(cc) / n))
    # missingness that differs by outcome: possibly recorded after it
    st <- if (type == "event") md$.s else if (type == "binary") md$.y else NULL
    if (!is.null(st)) for (v in setdiff(names(ids), outcome_cols)) {
        id <- ids[[v]]
        if (!id %in% names(md)) next
        r1 <- mean(is.na(md[[id]][st == 1])); r0 <- mean(is.na(md[[id]][st == 0]))
        if (is.finite(r1) && is.finite(r0) && ((r0 > 0.8 && r1 < 0.2) || (r1 > 0.8 && r0 < 0.2)))
            w <- c(w, sprintf("'%s' is missing in %.0f%% of rows without the outcome and %.0f%% with it: it may be recorded after the outcome; leave it out",
                              v, 100 * r0, 100 * r1))
    }
    # events per coefficient and a heuristic shrinkage factor
    ev <- if (type == "event") sum(md$.s[cc]) else if (type == "binary") min(sum(md$.y[cc]), sum(1 - md$.y[cc])) else NA
    k <- length(fit$b)
    if (is.finite(ev) && k > 0 && ev / k < 10) {
        sh <- if (isTRUE(fit$lr > k)) sprintf("; heuristic shrinkage (LR - df) / LR = %.2f", (fit$lr - k) / fit$lr) else ""
        w <- c(w, sprintf("%d events for %d coefficients (%.1f per coefficient, < 10): the adjusted estimates are overfitted%s", ev, k, ev / k, sh))
    }
    bad <- est$variable[!is.finite(est$estimate) | est$estimate <= 0.01 | est$estimate >= 100 |
                        (is.finite(est$upper / est$lower) & est$upper / est$lower > 100)]
    if (length(bad) && est$measure[1] != "beta")
        w <- c(w, sprintf("not estimable (infinite, extreme or very imprecise): %s; give it no effect", paste(unique(bad), collapse = ", ")))
    if (type == "event" && stats::median(md$.t, na.rm = TRUE) > 50)
        w <- c(w, sprintf("the median follow-up is %.0f time units: if the times are days or months, convert them to years (or set time_units)",
                          stats::median(md$.t, na.rm = TRUE)))
    w
}
