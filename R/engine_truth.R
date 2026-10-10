# The answer key: the true conditional effects the data were generated from,
# and what a study of this design is expected to estimate (crude and adjusted,
# from one large simulated study) -- so the key shows why crude differs from
# adjusted (confounding), why an OR from a case-control study is not the HR that
# generated the events, and what is lost by an unmeasured variable.

# One row per contrast a variable contributes: numeric variables per their
# effect's `per` unit, categorical ones each level against the first.
contrast_rows <- function(sc, oc, v, lang = "en") {
    eff <- oc$effects[[v]]
    if (v %in% names(sc$interventions)) {
        lv <- sc$interventions[[v]]$levels
        tl <- dlevel(lv, lang)   # shown; the coefficient names keep the library's levels
        return(data.frame(variable = v, contrast = sprintf("%s %s %s", tl[-1], gtext("vs", lang), tl[1]),
                          coefname = paste0(v, lv[-1]), per = 1, level = seq_along(lv)[-1], stringsAsFactors = FALSE))
    }
    x <- sc$variables[[v]]
    if (x$type %in% c("continuous", "count")) {
        per <- eff$per %||% 1
        u <- if (is.null(x$units)) "" else paste0(" ", tr_text(x$units, lang))
        return(data.frame(variable = v, contrast = sprintf("%s %s%s", gtext("per", lang), base::format(per), u), coefname = v,
                          per = per, level = NA_integer_, stringsAsFactors = FALSE))
    }
    lv <- x$levels; tl <- dlevel(lv, lang)
    data.frame(variable = v, contrast = sprintf("%s %s %s", tl[-1], gtext("vs", lang), tl[1]), coefname = paste0(v, lv[-1]),
               per = 1, level = seq_along(lv)[-1], stringsAsFactors = FALSE)
}

# the true conditional value of one contrast row, on the library's measure scale
true_value <- function(eff, row) {
    if (is.null(eff)) return(NA_real_)
    back <- function(b) if (eff$log) exp(b) else b
    if (!is.na(row$level) && eff$type %in% c("nominal", "ordinal", "intervention")) return(back(eff$coef[row$level]))
    if (eff$type == "genotype") {
        g <- row$level - 1L
        k <- switch(eff$model, additive = g, dominant = as.numeric(g >= 1), recessive = as.numeric(g == 2))
        return(back(eff$coef * k))
    }
    back(eff$coef)
}

# the measure a null variable is reported on: the outcome's natural one
null_measure <- function(oc) switch(oc$type, binary = "OR", event = "HR", count = "RR",
                                    continuous = if (isTRUE(oc$log)) "ratio" else "beta")
null_value <- function(oc) if (null_measure(oc) == "beta") 0 else 1

true_effects <- function(sc, oc, lang = "en") {
    vars <- unique(c(names(oc$effects), intersect(sc$null_variables, names(sc$variables))))
    rows <- lapply(vars, function(v) {
        cr <- contrast_rows(sc, oc, v, lang)
        eff <- oc$effects[[v]]
        note <- ""
        if (!is.null(eff$by)) {
            mv <- sc$variables[[eff$by$var]]
            back <- function(b) if (eff$log) exp(b) else b
            note <- paste(sprintf("%s=%s: %.3g", dname(eff$by$var, lang), dlevel(mv$levels, lang), back(eff$by$coef)), collapse = "; ")
        }
        data.frame(variable = v, contrast = cr$contrast,
                   measure = if (is.null(eff)) null_measure(oc) else eff$measure,
                   value = if (is.null(eff)) null_value(oc) else vapply(seq_len(nrow(cr)), function(i) true_value(eff, cr[i, ]), 0),
                   note = if (is.null(eff)) "no effect (null variable)" else note, stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
}

# Fit the model a student would fit for this design on formatted data `fd`.
# Returns named estimates on the measure scale, per the contrast rows.
# full = FALSE gives the crossover's naive (unpaired) comparison.
fit_design_model <- function(design, sc, oc, fd, terms, full = TRUE) {
    for (v in names(fd)) if (is.ordered(fd[[v]])) fd[[v]] <- factor(fd[[v]], ordered = FALSE)
    rhs <- paste(terms, collapse = " + ")
    model <- if (design %in% c("matched_cc", "nested_cc")) "clogit" else if (design %in% c("case_control")) "logistic"
             else if (design == "case_cohort") "prentice"
             else if (!is.null(fd$time)) "cox" else if (oc$type %in% c("binary", "event")) "logistic"
             else if (oc$type == "count") "poisson" else "linear"
    if (design %in% CASE_DESIGNS) fd$.s <- as.integer(fd$case) - 1L
    else if (model == "logistic") fd$.s <- as.integer(fd[[oc$id]]) - 1L
    # case-cohort, Prentice: the subcohort is at risk from the start, a case
    # outside it only just before its event; robust (sandwich) variance
    if (model == "prentice") {
        fd$.start <- ifelse(fd$subcohort == "yes", 0, fd$time * (1 - 1e-6))
        fd <- fd[fd$time > fd$.start, , drop = FALSE]   # censored at time 0: no contribution
    }
    if (design == "longitudinal") return(fit_longitudinal(sc, oc, fd, terms, full))
    if (design == "ecological") return(fit_ecological(sc, oc, fd, terms))
    if (design == "crossover" && full) {
        # subject fixed effects by the within transformation: same estimate as
        # lm(y ~ trt + period + factor(id)), without a column per subject
        X <- stats::model.matrix(stats::as.formula(paste("~", rhs, "+ factor(period)")), fd)[, -1, drop = FALSE]
        y <- fd[[oc$id]]
        if (isTRUE(oc$log)) y <- log(y)
        w <- function(x) x - stats::ave(x, fd$id)
        Xw <- apply(X, 2, w); yw <- w(y)
        q <- qr(Xw)
        b <- qr.coef(q, yw)
        # residual df lose one per subject as well (the absorbed fixed effects)
        df <- length(yw) - length(unique(fd$id)) - q$rank
        s2 <- sum(base::qr.resid(q, yw)^2) / max(1, df)
        se <- sqrt(diag(chol2inv(qr.R(q))) * s2)[order(q$pivot)]
        names(se) <- names(b)
        return(list(coef = b, se = se, measure = if (isTRUE(oc$log)) "ratio" else "beta", log = isTRUE(oc$log)))
    }
    f <- switch(model,
        logistic = stats::glm(stats::as.formula(paste(".s ~", rhs)), stats::binomial(), fd),
        clogit = survival::clogit(stats::as.formula(paste(".s ~", rhs, "+ strata(set)")), data = fd),
        # status 2 (a competing event) counts as censored: cause-specific hazards
        cox = if (design == "cluster_rct")
                  survival::coxph(stats::as.formula(paste("Surv(time,", oc$id, "== 1) ~", rhs)), data = fd, cluster = cluster)
              else survival::coxph(stats::as.formula(paste("Surv(time,", oc$id, "== 1) ~", rhs)), data = fd),
        prentice = survival::coxph(stats::as.formula(paste("Surv(.start, time,", oc$id, ") ~", rhs)), data = fd,
                                   robust = TRUE, id = id, control = survival::coxph.control(timefix = FALSE)),
        poisson = stats::glm(stats::as.formula(paste(oc$id, "~", rhs)), stats::poisson(), fd),
        linear = stats::lm(stats::as.formula(paste(if (isTRUE(oc$log)) sprintf("log(%s)", oc$id) else oc$id, "~", rhs)), fd))
    se <- sqrt(diag(as.matrix(stats::vcov(f))))
    if (design == "cluster_rct" && model != "cox") se <- cluster_se(f, fd$cluster)
    if (model == "linear" && isTRUE(oc$log))
        return(list(coef = stats::coef(f), se = se, measure = "ratio", log = TRUE))
    list(coef = stats::coef(f), se = se, measure = c(logistic = "OR", clogit = "OR", cox = "HR", prentice = "HR", poisson = "RR", linear = "beta")[[model]],
         log = model != "linear")
}

# Longitudinal trial. full = FALSE: completers only, difference at the last
# visit (what a naive analysis gives, biased when dropout depends on the
# outcome); full = TRUE: a linear mixed model with all visits (random
# intercept, a treatment effect per follow-up visit, baseline without one),
# unbiased under missing at random. Terms other than the intervention adjust.
fit_longitudinal <- function(sc, oc, fd, terms, full) {
    iv <- intersect(names(sc$interventions), terms)
    if (length(iv) == 0) iv <- intersect(names(sc$interventions), names(fd))
    lv <- levels(fd[[iv]])
    K <- max(fd$visit)
    y <- if (isTRUE(oc$log)) log(fd[[oc$id]]) else fd[[oc$id]]
    meas <- if (isTRUE(oc$log)) "ratio" else "beta"
    if (!full) {
        last <- fd$visit == K
        f <- stats::lm(y[last] ~ fd[[iv]][last])
        b <- stats::coef(f)[-1]; se <- sqrt(diag(stats::vcov(f)))[-1]
        names(b) <- names(se) <- paste0(iv, lv[-1])
        return(list(coef = b, se = se, measure = meas, log = isTRUE(oc$log)))
    }
    d <- data.frame(.y = y, .visit = factor(fd$visit), .id = fd$id)
    cols <- character()
    for (L in lv[-1]) for (j in seq_len(K)) {
        nm <- if (j == K) paste0(iv, L) else sprintf(".t%d_%s", j, L)
        d[[nm]] <- as.numeric(fd[[iv]] == L & fd$visit == j)
        cols <- c(cols, nm)
    }
    adj <- setdiff(terms, iv)
    for (v in adj) d[[v]] <- fd[[v]]
    rhs <- paste(c(".visit", sprintf("`%s`", cols), adj), collapse = " + ")
    f <- nlme::lme(stats::as.formula(paste(".y ~", rhs)), random = ~ 1 | .id, data = d, na.action = stats::na.omit,
                   control = nlme::lmeControl(returnObject = TRUE, opt = "optim"))
    tt <- summary(f)$tTable
    rownames(tt) <- gsub("`", "", rownames(tt))
    b <- tt[, "Value"]; se <- tt[, "Std.Error"]
    # factor terms of the adjustment are named as lm names them already
    list(coef = b, se = se, measure = meas, log = isTRUE(oc$log))
}

# Ecological regression on region totals: cases out of the population
# (logistic, OR), the mean outcome weighted by population (linear, beta; quasi-
# Poisson for counts, RR). A variable stands for its region mean or
# percentage(s); a percentage's coefficient is per percentage point.
fit_ecological <- function(sc, oc, fd, terms) {
    cols <- unlist(lapply(terms, function(v) if (v %in% names(fd)) v else eco_cols(sc, v)))
    rhs <- paste(sprintf("`%s`", cols), collapse = " + ")
    f <- switch(if (oc$type %in% c("binary", "event")) "binary" else oc$type,
        binary = stats::glm(stats::as.formula(sprintf("cbind(`%s`, population - `%s`) ~ %s", oc$id, oc$id, rhs)), stats::binomial(), fd),
        count = stats::glm(stats::as.formula(sprintf("`%s` ~ %s", oc$id, rhs)), stats::quasipoisson(), fd, weights = population),
        continuous = stats::lm(stats::as.formula(sprintf("%s ~ %s", if (isTRUE(oc$log)) sprintf("log(`%s`)", oc$id) else sprintf("`%s`", oc$id), rhs)),
                               fd, weights = population))
    b <- stats::coef(f); names(b) <- gsub("`", "", names(b))
    se <- sqrt(diag(as.matrix(stats::vcov(f)))); names(se) <- names(b)
    meas <- if (oc$type %in% c("binary", "event")) "OR" else if (oc$type == "count") "RR" else if (isTRUE(oc$log)) "ratio" else "beta"
    list(coef = b, se = se, measure = meas, log = meas != "beta")
}

# Cluster-robust (sandwich) standard errors of a linear or canonical-link GLM:
# the scores x (y - mu) summed within clusters, with the small-sample factor
# G / (G - 1)
cluster_se <- function(f, cl) {
    if (!is.null(f$na.action)) cl <- cl[-f$na.action]
    X <- stats::model.matrix(f)
    U <- rowsum(X * stats::residuals(f, type = "response"), cl)
    B <- summary(f)$cov.unscaled
    G <- nrow(U)
    V <- B %*% crossprod(U) %*% B * G / (G - 1)
    stats::setNames(sqrt(diag(V)), colnames(X))
}

expected_estimates <- function(sc, oc, design, opt, factors, lang = "en", categorize = list()) {
    cal <- calibrate(sc, oc)
    n_big <- switch(design, case_control = 3000 * (1 + opt$ratio), matched_cc = 1500 * (1 + opt$ratio),
                    nested_cc = 2000 * (1 + opt$ratio), case_cohort = 8000,
                    cluster_rct = max(20000, 50 * (opt$clusters %||% 20)), longitudinal = 3000, ecological = 200,
                    crossover = 4000, 40000)
    # a rare outcome needs a larger study for a stable expectation: about 5000
    # events (cases in a case-cohort), at most 300,000 people
    p_ev <- if (oc$type == "event") cal$achieved$risk_followup else if (oc$type == "binary") min(oc$prevalence, 1 - oc$prevalence) else NA
    if (!is.na(p_ev) && design %in% c("cohort", "prognosis", "rct", "cluster_rct", "factorial", "cross_sectional"))
        n_big <- min(300000, max(n_big, ceiling(5000 / p_ev)))
    if (!is.na(p_ev) && design == "case_cohort")
        n_big <- min(300000, max(n_big, ceiling(opt$subcohort * 5000 / p_ev + 5000)))
    # the expectation of a cluster trial does not depend on the number of
    # clusters; many clusters keep the cluster effects from blurring it
    if (design == "cluster_rct") opt$clusters <- max(opt$clusters, 400)
    if (design == "ecological") opt$regions <- 200
    big <- run_design(design, sc, oc, cal, derive_seed("truth", sc$id, oc$id), n_big, opt)
    fd <- format_study(sc, oc, design, big, sc$order, "en")
    # the measured variables with the same measurement error as the student's data
    fd <- measurement_error(fd, sc, factors, opt$reliability %||% 1, opt$misclass %||% 0, derive_seed("truth", "measurement"))
    fd <- categorize_study(fd, sc, categorize[intersect(names(categorize), factors)], "en")
    estimate_table(sc, oc, design, fd, factors, lang, categorize)
}

# Crude and adjusted estimates of the main contrasts (the intervention, or
# every measured factor) in one formatted study `fd` (engine names). With
# ci = TRUE, also the 95% confidence interval of the adjusted estimate.
estimate_table <- function(sc, oc, design, fd, factors, lang = "en", categorize = list(), ci = FALSE) {
    iv <- intersect(names(sc$interventions), names(fd))
    main <- if (length(iv)) iv else factors
    if (length(main) == 0) return(NULL)
    # covariates are constant within a crossover subject: the subject terms absorb them
    adj_terms <- if (design == "crossover") iv else unique(c(iv, factors))
    adj <- fit_design_model(design, sc, oc, fd, adj_terms)
    rows <- lapply(main, function(v) {
        cr <- contrast_rows(sc, oc, v, lang)
        catd <- is.ordered(fd[[v]]) && is_grouping(categorize[[v]])
        logd <- !is.null(categorize[[v]]) && categorize[[v]]$form %in% LOG_FORMS && is.numeric(fd[[v]])
        scaled <- !is.null(categorize[[v]]) && categorize[[v]]$form == "scale" && is.numeric(fd[[v]])
        if (logd || scaled) cr <- data.frame(variable = v, contrast = cat_rows(v, categorize[[v]], lang, sc)$contrast, coefname = v,
                                             per = 1, level = NA_integer_, stringsAsFactors = FALSE)
        if (catd) {
            # categorised: groups against the first, named by position (the big
            # study's cut points differ slightly from the student's)
            cr0 <- cat_rows(v, categorize[[v]], lang)
            cr0 <- cr0[cr0$idx <= nlevels(fd[[v]]), ]
            cr <- data.frame(variable = v, contrast = cr0$contrast, coefname = paste0(v, levels(fd[[v]])[cr0$idx]),
                             per = 1, level = NA_integer_, stringsAsFactors = FALSE)
        }
        if (design == "ecological" && !catd && !logd && !scaled && !is.null(sc$variables[[v]]) && !sc$variables[[v]]$type %in% c("continuous", "count")) {
            # all versus none of the region in the level (100 percentage points)
            x <- sc$variables[[v]]
            cr$coefname <- if (x$type == "binary") v else eco_col(v, x$levels[cr$level])
            cr$per <- 100
        }
        crude <- fit_design_model(design, sc, oc, fd, v, full = FALSE)
        get <- function(fit, z = 0) {
            b <- (fit$coef[cr$coefname] + z * fit$se[cr$coefname]) * cr$per
            if (fit$log) exp(b) else b
        }
        eff <- oc$effects[[v]]
        tv <- vapply(seq_len(nrow(cr)), function(i) true_value(eff, cr[i, ]), 0)
        note <- ""
        if (catd) { tv[] <- NA_real_; note <- gtext("categorised", lang) }
        if (logd) { tv[] <- NA_real_; note <- gtext("transformed", lang) }
        # scaled: the true effect re-expressed per k units (the library gives it per `per`)
        if (scaled && !is.null(eff) && is.null(eff$by))
            tv[] <- (if (eff$log) exp else identity)(eff$coef * scale_of(categorize[[v]]) / eff$per)
        if (!is.null(eff$by)) {
            # a modified effect has no single true value: give it per level
            back <- function(b) if (eff$log) exp(b) else b
            note <- paste(sprintf("%s=%s: %.3g", dname(eff$by$var, lang), dlevel(sc$variables[[eff$by$var]]$levels, lang), back(eff$by$coef)), collapse = "; ")
            tv[] <- NA_real_
        }
        data.frame(variable = v, contrast = cr$contrast,
                   true = tv,
                   true_measure = if (is.null(eff)) null_measure(oc) else eff$measure,
                   crude = unname(get(crude)), adjusted = unname(get(adj)),
                   lower = if (ci) unname(get(adj, -stats::qnorm(0.975))) else NA_real_,
                   upper = if (ci) unname(get(adj, stats::qnorm(0.975))) else NA_real_,
                   measure = adj$measure, note = note, stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, rows)
    # variables without an effect: the null value on the estimate's own scale
    nul <- !out$variable %in% names(oc$effects)
    out$true[nul] <- if (adj$log) 1 else 0
    out$note[nul] <- ""
    attr(out, "n") <- nrow(fd)
    attr(out, "adjusted_for") <- adj_terms
    out
}

# markers' true AUC and the AUC of one large diagnostic study
expected_markers <- function(sc, oc, opt) {
    big <- run_design("diagnostic", sc, oc, calibrate(sc, oc), derive_seed("truth", sc$id, oc$id), 20000, opt)
    marker_table(oc, format_study(sc, oc, "diagnostic", big, character(), "en"))
}

answer_key <- function(sc, oc, design, opt, factors, lang = "en", categorize = list()) {
    cal <- calibrate(sc, oc)
    unmeasured <- setdiff(names(oc$effects), c(factors, names(sc$interventions)))
    list(effects = true_effects(sc, oc, lang),
         markers = if (design == "diagnostic") expected_markers(sc, oc, opt),
         expected = expected_estimates(sc, oc, design, opt, factors, lang, categorize),
         calibration = cal,
         structure = sc$edges,
         unmeasured = unmeasured,
         teaching = tr_text(sc$teaching, lang),
         references = sc$references)
}
