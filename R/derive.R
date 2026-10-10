# Derive a scenario from a dataset (the plasmode idea): marginals, latent
# correlations and conditional effects estimated from real data, written as a
# library scenario that simulates studies like the one analysed. The data
# cannot tell causes from correlates: the result has undirected correlations
# only, and a `derivation:` block with the estimates and their context, for a
# person (or an AI, see documents/derive-skill.md) to turn into a causal
# structure. Pure R, no jmvcore.

# a valid, unique id for each column name
make_ids <- function(nm) {
    x <- tolower(gsub("[^A-Za-z0-9]+", "_", nm))
    x <- gsub("^_+|_+$", "", x)
    x[!grepl("^[a-z]", x)] <- paste0("v_", x[!grepl("^[a-z]", x)])
    make.unique(x, sep = "_")
}

decimals <- function(x) {
    x <- x[is.finite(x)]
    for (k in 0:3) if (all(abs(x * 10^k - round(x * 10^k)) < 1e-6)) return(k)
    3L
}

sig <- function(x, d = 3) signif(x, d)

# one column -> a library variable (marginal only)
derive_variable <- function(x, name) {
    v <- list(label = name)
    if (is.factor(x) || is.character(x) || is.logical(x)) {
        x <- factor(x)
        lv <- levels(x)
        tab <- as.numeric(table(x)) / sum(!is.na(x))
        if (length(lv) == 2) return(c(v, list(type = "binary", levels = lv, p = sig(tab[2]))))
        return(c(v, list(type = if (is.ordered(x)) "ordinal" else "nominal", levels = lv,
                         probs = sig(tab / sum(tab), 4))))
    }
    x <- as.numeric(x)
    y <- x[is.finite(x)]
    if (length(unique(y)) == 2) {
        lv <- as.character(sort(unique(y)))
        return(c(v, list(type = "binary", levels = lv, p = sig(mean(y == max(y))))))
    }
    m <- mean(y); s <- stats::sd(y)
    integer_valued <- all(abs(y - round(y)) < 1e-8)
    if (integer_valued && min(y) >= 0 && m <= 20 && any(y == 0)) {
        dist <- if (stats::var(y) > m * 1.1) sprintf("negbin(%s, %s)", sig(m), sig(m^2 / (stats::var(y) - m)))
                else sprintf("poisson(%s)", sig(m))
        return(c(v, list(type = "count", dist = dist, range = c(min(y), max(y)))))
    }
    cand <- list(normal = sum(stats::dnorm(y, m, s, log = TRUE)))
    if (min(y) > 0) {
        sl <- sqrt(log(1 + (s / m)^2)); ml <- log(m) - sl^2 / 2
        cand$lognormal <- sum(stats::dlnorm(y, ml, sl, log = TRUE))
        cand$gamma <- sum(stats::dgamma(y, (m / s)^2, m / s^2, log = TRUE))
    }
    # a uniform spread (e.g. age by design) beats the others when it is flat
    cand$uniform <- -length(y) * log(diff(range(y)) * (1 + 1 / length(y)))
    fam <- names(which.max(unlist(cand)))
    if (fam == "uniform")
        return(c(v, list(type = "continuous", dist = sprintf("uniform(%s, %s)", min(y), max(y)), round = decimals(y))))
    c(v, list(type = "continuous", dist = sprintf("%s(%s, %s)", fam, sig(m, 4), sig(s, 4)),
              range = c(min(y), max(y)), round = decimals(y)))
}

# numeric codes of a column, as the engine codes it (for correlations)
code_of <- function(x, v) {
    switch(v$type,
        binary = if (is.numeric(x)) as.numeric(x == max(x, na.rm = TRUE)) else as.numeric(factor(x, levels = v$levels)) - 1,
        nominal = , ordinal = as.numeric(factor(x, levels = v$levels)),
        as.numeric(x))
}

# NORTA inversion: the latent correlation whose transformed pair has the
# observed Pearson correlation, with the same marginal maps the engine uses
latent_rho <- function(target, va, vb, z1, z2) {
    na <- normalize_variable("a", va, list(), new_problems())
    nb <- normalize_variable("b", vb, list(), new_problems())
    if (is.null(na) || is.null(nb) || !is.finite(target)) return(0)
    xa <- latent_to_code(z1, na)
    f <- function(r) stats::cor(xa, latent_to_code(r * z1 + sqrt(1 - r^2) * z2, nb)) - target
    lo <- f(-0.98); hi <- f(0.98)
    if (!is.finite(lo) || !is.finite(hi)) return(0)
    if (lo >= 0) return(-0.98)          # beyond what these marginals can reach
    if (hi <= 0) return(0.98)
    stats::uniroot(f, c(-0.98, 0.98), tol = 1e-3)$root
}

# data frame of named columns -> list(scenario, estimates, correlations, summary)
derive_scenario <- function(df, outcome, type = c("auto", "binary", "continuous", "count", "event"),
                            covs, time = NULL, id = "derived_scenario", label = NULL,
                            log_outcome = FALSE, min_rho = 0.05, source = "the data",
                            missing = "complete", m = 20) {
    type <- match.arg(type)
    if (!missing %in% MISSING_METHODS) stop(sprintf("missing must be one of %s", paste(MISSING_METHODS, collapse = ", ")), call. = FALSE)
    cols <- c(outcome, covs, time)
    ids <- stats::setNames(make_ids(cols), cols)
    y <- df[[outcome]]
    if (type == "auto") {
        type <- if (!is.null(time)) "event" else if (is.factor(y) || length(unique(stats::na.omit(y))) == 2) "binary"
                else if (all(abs(stats::na.omit(y) - round(stats::na.omit(y))) < 1e-8) && min(y, na.rm = TRUE) >= 0 &&
                         mean(y, na.rm = TRUE) <= 20) "count" else "continuous"
    }
    if (type == "event" && is.null(time)) stop("an event outcome needs a time variable", call. = FALSE)

    # marginals
    vars <- lapply(covs, function(v) derive_variable(df[[v]], v))
    names(vars) <- ids[covs]

    # latent correlations, pairwise complete
    codes <- as.data.frame(lapply(covs, function(v) code_of(df[[v]], vars[[ids[v]]])))
    names(codes) <- ids[covs]
    z <- with_seed(derive_seed("derive", id), list(z1 = stats::rnorm(10000), z2 = stats::rnorm(10000)))
    cors <- list()
    if (length(covs) > 1) for (i in seq_along(covs)[-length(covs)]) for (j in (i + 1):length(covs)) {
        a <- names(codes)[i]; b <- names(codes)[j]
        ok <- stats::complete.cases(codes[, c(a, b)])
        r_obs <- if (sum(ok) > 10) stats::cor(codes[ok, a], codes[ok, b]) else NA
        r_lat <- if (is.na(r_obs) || abs(r_obs) < 0.01) 0 else latent_rho(r_obs, vars[[a]], vars[[b]], z$z1, z$z2)
        cors[[length(cors) + 1]] <- data.frame(a = a, b = b, observed = r_obs, latent = r_lat, stringsAsFactors = FALSE)
    }
    cors <- if (length(cors)) do.call(rbind, cors) else data.frame(a = character(), b = character(), observed = numeric(), latent = numeric())
    keep <- cors[abs(cors$latent) >= min_rho, ]
    # keep the set positive definite
    if (nrow(keep)) {
        R <- diag(length(covs)); dimnames(R) <- list(names(codes), names(codes))
        for (k in seq_len(nrow(keep))) R[keep$a[k], keep$b[k]] <- R[keep$b[k], keep$a[k]] <- keep$latent[k]
        R <- near_pd(R)
        keep$latent <- vapply(seq_len(nrow(keep)), function(k) R[keep$a[k], keep$b[k]], 0)
    }

    # outcome model on complete cases
    md <- data.frame(lapply(covs, function(v) {
        x <- df[[v]]; vv <- vars[[ids[v]]]
        if (vv$type %in% c("binary", "nominal", "ordinal")) factor(as.character(x), levels = vv$levels) else as.numeric(x)
    }))
    names(md) <- ids[covs]
    oid <- ids[outcome]
    if (type == "event") {
        st <- df[[outcome]]
        md$.s <- if (is.factor(st)) as.integer(st == levels(st)[nlevels(st)]) else as.integer(as.numeric(st) == max(as.numeric(st), na.rm = TRUE))
        md$.t <- as.numeric(df[[time]])
    } else if (type == "binary") {
        md$.y <- if (is.factor(y)) as.integer(y == levels(y)[nlevels(y)]) else as.integer(as.numeric(y) == max(as.numeric(y), na.rm = TRUE))
    } else md$.y <- as.numeric(y)
    # rows with the outcome observed; covariates may be missing (handled below)
    out_cols <- if (type == "event") c(".s", ".t") else ".y"
    md <- md[stats::complete.cases(md[out_cols]), , drop = FALSE]
    cc <- stats::complete.cases(md)
    mdc <- md[cc, , drop = FALSE]
    rhs <- paste(ids[covs], collapse = " + ")
    fit_one <- function(terms, data = mdc) {
        f <- switch(type,
            binary = stats::glm(stats::as.formula(paste(".y ~", terms)), stats::binomial(), data),
            count = stats::glm(stats::as.formula(paste(".y ~", terms)), stats::poisson(), data),
            continuous = stats::lm(stats::as.formula(paste(if (log_outcome) "log(.y)" else ".y", "~", terms)), data),
            event = survival::coxph(stats::as.formula(paste("survival::Surv(.t, .s) ~", terms)), data = data))
        b <- stats::coef(f); se <- sqrt(diag(stats::vcov(f)))[names(b)]
        # likelihood-ratio statistic of the model (for the shrinkage heuristic)
        lr <- if (inherits(f, "coxph")) 2 * diff(f$loglik) else if (inherits(f, "glm")) f$null.deviance - f$deviance else NA
        list(b = b, se = se, lr = lr)
    }
    cid <- unname(ids[covs])
    adj_cc <- fit_one(rhs)
    adj <- switch(missing,
        complete = adj_cc,
        mean = fit_one(rhs, impute_mean(md, cid)),
        single = fit_one(rhs, impute_chained(md, cid, type, m = 1, seed = derive_seed("impute", id))[[1]]),
        multiple = pool_rubin(lapply(impute_chained(md, cid, type, m = m, seed = derive_seed("impute", id)), function(d) fit_one(rhs, d))))
    measure <- switch(type, binary = "OR", count = "RR", event = "HR", continuous = if (log_outcome) "ratio" else "beta")
    tf <- function(x) if (measure == "beta") x else exp(x)
    est <- list(); effects <- list()
    for (v in covs) {
        vid <- ids[v]; vv <- vars[[vid]]
        # crude: the rows where this variable and the outcome are observed
        crude <- fit_one(vid, md[stats::complete.cases(md[c(vid, out_cols)]), , drop = FALSE])
        nm <- if (vv$type %in% c("binary", "nominal", "ordinal")) paste0(vid, vv$levels[-1]) else vid
        pick <- function(fit, k) { i <- match(k, names(fit$b)); if (is.na(i)) c(NA, NA) else c(fit$b[i], fit$se[i]) }
        b <- t(vapply(nm, function(k) pick(adj, k), numeric(2)))
        bc <- t(vapply(nm, function(k) pick(crude, k), numeric(2)))
        contrast <- if (vv$type %in% c("binary", "nominal", "ordinal")) sprintf("%s vs %s", vv$levels[-1], vv$levels[1]) else "per 1"
        bcc <- t(vapply(nm, function(k) pick(adj_cc, k), numeric(2)))
        est[[length(est) + 1]] <- data.frame(variable = vid, contrast = contrast, measure = measure,
                                             estimate = tf(b[, 1]), lower = tf(b[, 1] - 1.96 * b[, 2]),
                                             upper = tf(b[, 1] + 1.96 * b[, 2]), crude = tf(bc[, 1]),
                                             complete_case = tf(bcc[, 1]), stringsAsFactors = FALSE)
        vals <- sig(tf(b[, 1]), 3)
        if (anyNA(vals)) next
        effects[[vid]] <- stats::setNames(list(if (length(vals) > 1) c(if (measure == "beta") 0 else 1, vals) else vals), measure)
        if (!vv$type %in% c("binary", "nominal", "ordinal")) effects[[vid]]$per <- 1
    }
    est <- do.call(rbind, est); rownames(est) <- NULL

    # outcome targets
    oc <- list(type = type, label = outcome, effects = effects)
    yc <- md$.y                        # every row with the outcome observed
    if (type == "binary") { oc$prevalence <- sig(mean(yc)); oc$levels <- if (is.factor(y)) levels(y) else c("no", "yes") }
    if (type == "count") oc$mean <- sig(mean(yc))
    if (type == "continuous") {
        f <- stats::lm(stats::as.formula(paste(if (log_outcome) "log(.y)" else ".y", "~", rhs)), mdc)
        oc$mean <- sig(if (log_outcome) exp(mean(log(yc))) else mean(yc), 4)
        oc$sd <- sig(summary(f)$sigma, 3)
        oc$round <- decimals(yc)
        if (log_outcome) oc$scale <- "log"
    }
    if (type == "event") {
        ev <- sum(md$.s); pt <- sum(md$.t)
        w <- tryCatch(survival::survreg(stats::as.formula(paste("survival::Surv(.t, .s) ~", rhs)), data = mdc, dist = "weibull"),
                      error = function(e) NULL)
        shape <- if (is.null(w)) 1 else sig(1 / w$scale, 2)
        fu <- sig(stats::quantile(md$.t, 0.9, names = FALSE), 2)
        km <- survival::survfit(survival::Surv(.t, .s) ~ 1, data = md)
        s_fu <- summary(km, times = fu, extend = TRUE)$surv
        oc$risk <- list(value = sig(1 - s_fu), at = fu)
        oc$shape <- shape
        oc$followup <- fu; oc$window <- fu
        oc$designs <- c("prognosis", "cohort", "case_control", "matched_cc")
        oc$levels <- c("no", "yes")
    }

    sc <- list(id = id, label = label %||% sprintf("Derived from %s", source), domain = "derived",
               population = sprintf("Like the %d participants of %s", nrow(df), source),
               variables = vars,
               correlations = if (nrow(keep)) lapply(seq_len(nrow(keep)), function(k)
                   c(keep$a[k], keep$b[k], sprintf("%.2f", keep$latent[k]))) else NULL,
               outcomes = stats::setNames(list(oc), oid),
               references = sprintf("Parameters estimated from %s (%d rows, %d complete) with studysim Derive, %s.",
                                    source, nrow(df), sum(cc), format(Sys.Date())),
               teaching = paste("Marginals, correlations and conditional effects estimated from data.",
                                "The correlations are undirected: decide which variables cause which (structure),",
                                "which are confounders, mediators or colliders, and add rules for impossible combinations",
                                "before using the scenario for teaching."))
    sc$correlations <- Filter(Negate(is.null), sc$correlations)
    sc$derivation <- list(
        source = source, rows = nrow(df), complete_rows = sum(cc), outcome = outcome, outcome_type = type,
        time = time, model = switch(type, binary = "logistic", count = "Poisson", event = "Cox", continuous = "linear"),
        original_names = as.list(stats::setNames(cols, ids[cols])),
        missing = as.list(stats::setNames(vapply(cols, function(v) round(mean(is.na(df[[v]])), 3), 0), ids[cols])),
        missing_method = missing, imputations = if (missing == "multiple") m else NULL,
        estimates = lapply(seq_len(nrow(est)), function(i) c(list(variable = est$variable[i], contrast = est$contrast[i],
            adjusted = sig(est$estimate[i]), ci = sig(c(est$lower[i], est$upper[i])), crude = sig(est$crude[i])),
            if (missing != "complete") list(complete_case = sig(est$complete_case[i])))),
        correlations = lapply(seq_len(nrow(cors)), function(i) list(pair = c(cors$a[i], cors$b[i]),
            observed = round(cors$observed[i], 3), latent = round(cors$latent[i], 3))))
    warns <- derive_warnings(df, md, out_cols, type, cc, adj_cc, est, ids[covs], time)
    if (length(warns)) sc$derivation$warnings <- warns
    list(scenario = sc, estimates = est, correlations = cors, type = type, outcome_id = unname(oid),
         ids = ids, complete = sum(cc), rows_model = nrow(md), missing = missing, warnings = warns)
}

# Round trip: simulate a large study from the derived scenario and re-estimate
derive_check <- function(d, n = 20000) {
    sc <- normalize_scenario(d$scenario, strict = TRUE)
    use_i18n(sc)
    oid <- d$outcome_id
    design <- if (d$type == "event") "cohort" else "cross_sectional"
    opt <- design_defaults(design, sc$outcomes[[oid]], sc)
    opt$loss <- 0
    ex <- expected_estimates(sc, sc$outcomes[[oid]], design, opt, sc$order)
    ex
}
