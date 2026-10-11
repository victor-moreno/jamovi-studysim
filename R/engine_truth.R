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

# a modifier's effect holds where the numeric effects it modifies are centred:
# "at bmi = 26.5" (engine_scenario.R, fill_centers)
center_note <- function(oc, v, lang = "en") {
    at <- Filter(function(e) !is.null(e$center) && identical(e$by$var, v), oc$effects)
    if (!length(at)) return("")
    paste(vapply(at, function(e) sprintf(gtext("at_center", lang), dname(e$var, lang), base::format(e$center)), ""), collapse = "; ")
}

# The true model: one row per contrast of each variable with a direct effect,
# each null variable and each variable that acts only through others. `value`
# is the direct (conditional) effect; `total` the total effect, equal to the
# direct one unless the variable acts through mediators, where it is NA unless
# total = TRUE (computed by simulation, total_effects()).
true_effects <- function(sc, oc, lang = "en", total = FALSE) {
    # illness-death: the effects of the two transitions (death without the
    # first event, and after it); there is no single hazard ratio
    if (!is.null(oc[["after"]])) {   # [[ ]]: $ would partially match after_event
        ox <- sc$outcomes[[oc[["after"]]]]
        base <- if (!is.null(oc$target)) oc else ox$competing
        base$after <- NULL   # its own target: the plain outcome, or this recurses
        a <- true_effects(sc, base, lang); a$note <- rep_len(sprintf(gtext("without_x", lang), tr_text(ox$label, lang)), nrow(a))
        b <- true_effects(sc, oc$after_event, lang); b$note <- rep_len(sprintf(gtext("after_x", lang), tr_text(ox$label, lang)), nrow(b))
        out <- rbind(a[a$variable %in% names(base$effects), ], b[b$variable %in% names(oc$after_event$effects), ])
        out$total <- out$value
        return(out)
    }
    medvars <- Filter(function(v) length(mediators(sc, oc, v)) > 0, setdiff(sc$order, names(oc$effects)))
    vars <- unique(c(names(oc$effects), intersect(sc$null_variables, names(sc$variables)), medvars))
    tot <- if (total) total_effects(sc, oc) else NULL
    rows <- lapply(vars, function(v) {
        cr <- contrast_rows(sc, oc, v, lang)
        eff <- oc$effects[[v]]
        med <- mediators(sc, oc, v)
        note <- ""
        if (!is.null(eff$by)) {
            mv <- sc$variables[[eff$by$var]]
            back <- function(b) if (eff$log) exp(b) else b
            note <- paste(sprintf("%s=%s: %.3g", dname(eff$by$var, lang), dlevel(mv$levels, lang), back(eff$by$coef)), collapse = "; ")
        }
        medtxt <- paste(dname(med, lang), collapse = ", ")
        if (is.null(eff)) note <- if (length(med)) sprintf(gtext("no_direct", lang), medtxt) else gtext("null_var", lang)
        else if (length(med)) note <- trimws(paste(note, sprintf(gtext("also_through", lang), medtxt)))
        note <- trimws(paste(note, center_note(oc, v, lang)))
        value <- if (is.null(eff)) rep(null_value(oc), nrow(cr)) else vapply(seq_len(nrow(cr)), function(i) true_value(eff, cr[i, ]), 0)
        total_v <- if (!length(med)) value else if (!is.null(tot) && any(tot$variable == v)) tot$total[tot$variable == v] else rep(NA_real_, nrow(cr))
        data.frame(variable = v, contrast = cr$contrast,
                   measure = if (is.null(eff)) null_measure(oc) else eff$measure,
                   value = value, total = total_v, note = note, stringsAsFactors = FALSE)
    })
    # no effects at all (e.g. a transition without covariates): an empty table
    if (!length(rows)) return(data.frame(variable = character(), contrast = character(), measure = character(),
                                         value = numeric(), total = numeric(), note = character(), stringsAsFactors = FALSE))
    out <- do.call(rbind, rows)
    attr(out, "n_total") <- attr(tot, "n")
    out
}

# Fit the model a student would fit for this design on formatted data `fd`.
# Returns named estimates on the measure scale, per the contrast rows.
# full = FALSE gives the crossover's naive (unpaired) comparison.
fit_design_model <- function(design, sc, oc, fd, terms, full = TRUE) {
    for (v in names(fd)) if (is.ordered(fd[[v]])) fd[[v]] <- factor(fd[[v]], ordered = FALSE)
    # a log-scale outcome recorded with zeros (rounding): log(y + c), as an analyst would
    if (isTRUE(oc$log) && is.numeric(fd[[oc$id]])) fd[[oc$id]] <- fd[[oc$id]] + log_shift(fd[[oc$id]], oc)
    rhs <- paste(terms, collapse = " + ")
    model <- if (design %in% c("matched_cc", "nested_cc")) "clogit" else if (design %in% c("case_control")) "logistic"
             else if (design == "case_cohort") "prentice"
             else if (!is.null(fd[["time"]])) "cox" else if (oc$type %in% c("binary", "event")) "logistic"
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

# c added before taking logs of a log-scale outcome with zeros or negatives: half
# its rounding unit (or half its smallest positive value); 0 when all are positive
log_shift <- function(y, oc) {
    y <- y[is.finite(y)]
    if (!length(y) || all(y > 0)) return(0)
    if (!is.null(oc$round)) 10^(-oc$round) / 2 else min(y[y > 0]) / 2
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
    p_ev <- (if (oc$type == "event") cal$achieved$risk_followup else if (oc$type == "binary") min(oc$prevalence, 1 - oc$prevalence)) %||% NA
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
    # ci: the large study's own interval is the Monte Carlo uncertainty of the expectation
    out <- estimate_table(sc, oc, design, fd, factors, lang, categorize, ci = TRUE, opt = opt)
    add_total(out, sc, oc, lang)
}

# the true total effect of each row of an expected table (NA where the row is
# not one of the true model's contrasts: categorised, transformed, special)
add_total <- function(ex, sc, oc, lang = "en") {
    if (is.null(ex) || !nrow(ex)) return(ex)
    te <- true_effects(sc, oc, lang, total = TRUE)
    k <- match(paste(ex$variable, ex$contrast), paste(te$variable, te$contrast))
    ex$total <- te$total[k]
    ex$total[is.na(ex$true)] <- NA_real_
    attr(ex, "n_total") <- attr(te, "n_total")
    ex
}

# Crude and adjusted estimates of the main contrasts (the intervention, or
# every measured factor) in one formatted study `fd` (engine names). With
# ci = TRUE, also the 95% confidence interval of the adjusted estimate.
# rows of the designs whose key is not a set of contrasts of the factors
special_rows <- function(design, sc, oc, lang = "en") {
    if (design == "pre_post")
        return(data.frame(variable = trial_iv(sc, oc)$id, contrast = gtext("change", lang), stringsAsFactors = FALSE))
    if (design == "method_comparison")
        return(data.frame(variable = "method", contrast = vapply(c("mc_diff", "mc_low", "mc_high", "mc_slope"), gtext, "", lang),
                          stringsAsFactors = FALSE))
    NULL
}

# before-after: the mean change, which is what such a study can estimate
# method comparison: Bland-Altman quantities and the slope of B on A
special_table <- function(design, sc, oc, fd, lang, opt) {
    rows <- special_rows(design, sc, oc, lang)
    z <- stats::qnorm(0.975)
    if (design == "pre_post") {
        ch <- fd[[oc$id]] - fd[[paste0(oc$id, "_before")]]
        ch <- ch[!is.na(ch)]
        m <- mean(ch); se <- stats::sd(ch) / sqrt(length(ch))
        eff <- oc$effects[[rows$variable]]
        out <- data.frame(rows, true = eff$coef[length(eff$coef)], true_measure = "beta", crude = m, adjusted = NA_real_, dag = NA_real_,
                          lower = m - z * se, upper = m + z * se, measure = "beta", note = gtext("prepost_note", lang),
                          stringsAsFactors = FALSE)
    } else {
        a <- fd[[paste0(oc$id, "_a")]]; b <- fd[[paste0(oc$id, "_b")]]
        ok <- !is.na(a) & !is.na(b); a <- a[ok]; b <- b[ok]
        d <- b - a; md <- mean(d); sdd <- stats::sd(d); n <- length(d)
        f <- stats::lm(b ~ a); sl <- stats::coef(f)[[2]]; sse <- sqrt(stats::vcov(f)[2, 2])
        out <- data.frame(rows,
            true = c(opt$bias + (opt$slope - 1) * mean(a), NA, NA, opt$slope), true_measure = "beta",
            crude = c(md, md - z * sdd, md + z * sdd, sl), adjusted = NA_real_, dag = NA_real_,
            lower = c(md - z * sdd / sqrt(n), NA, NA, sl - z * sse), upper = c(md + z * sdd / sqrt(n), NA, NA, sl + z * sse),
            measure = "", note = c("", "", "", gtext("mc_note", lang)), stringsAsFactors = FALSE)
    }
    attr(out, "n") <- nrow(fd); attr(out, "adjusted_for") <- character()
    out
}

estimate_table <- function(sc, oc, design, fd, factors, lang = "en", categorize = list(), ci = FALSE, opt = list()) {
    if (design %in% c("pre_post", "method_comparison")) return(special_table(design, sc, oc, fd, lang, opt))
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
            if (is.null(fit)) return(rep(NA_real_, nrow(cr)))
            b <- (fit$coef[cr$coefname] + z * fit$se[cr$coefname]) * cr$per
            if (fit$log) exp(b) else b
        }
        # adjusted as the diagram says: for the measured variables v does not
        # cause (confounders), not for its mediators or colliders (Table 2 fallacy)
        dag_terms <- if (v %in% iv || design == "crossover") adj_terms else unique(c(iv, v, adjustment_set(sc, v, factors)))
        dagfit <- if (setequal(dag_terms, adj_terms)) adj
                  else tryCatch(fit_design_model(design, sc, oc, fd, dag_terms), error = function(e) NULL)
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
                   crude = unname(get(crude)), adjusted = unname(get(adj)), dag = unname(get(dagfit)),
                   lower = if (ci) unname(get(adj, -stats::qnorm(0.975))) else NA_real_,
                   upper = if (ci) unname(get(adj, stats::qnorm(0.975))) else NA_real_,
                   measure = adj$measure, note = note, stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, rows)
    # variables without an effect: the null value on the estimate's own scale
    nul <- !out$variable %in% names(oc$effects)
    out$true[nul] <- if (adj$log) 1 else 0
    out$note[nul] <- ""
    # a modifier's expected effect is the one where the modified effects are centred
    for (v in unique(out$variable)) {
        cn <- center_note(oc, v, lang)
        if (nzchar(cn)) out$note[out$variable == v] <- trimws(paste(out$note[out$variable == v], cn))
    }
    if (!is.null(oc[["after"]])) { out$true <- NA_real_; out$note <- gtext("illness_death", lang) }
    # not estimable: matched on (constant within sets) or aliased with other measured variables
    na <- is.na(out$adjusted) & out$note == ""
    if (any(na)) {
        matched <- design == "matched_cc" & out$variable %in% unlist(opt$match)
        out$note[na] <- ifelse(matched[na], gtext("not_est_matched", lang), gtext("not_est_alias", lang))
    }
    sh <- if (isTRUE(oc$log) && is.numeric(fd[[oc$id]])) log_shift(fd[[oc$id]], oc) else 0
    if (sh > 0) out$note <- trimws(paste(out$note, sprintf(gtext("log_zero", lang), base::format(sh))))
    attr(out, "n") <- nrow(fd)
    attr(out, "adjusted_for") <- adj_terms
    out
}

# Largest 95% Monte Carlo half-width of expected estimates (rows of
# estimate_table with ci = TRUE): in percent for ratios, in units for
# differences; NA when there is none of that kind
mc_halfwidth <- function(ex) {
    ok <- is.finite(ex$lower) & is.finite(ex$upper) & ex$lower > -Inf
    rat <- ok & ex$measure %in% c("OR", "HR", "RR", "ratio") & ex$lower > 0
    dif <- ok & !ex$measure %in% c("OR", "HR", "RR", "ratio")
    list(pct = if (any(rat)) max(100 * (sqrt(ex$upper[rat] / ex$lower[rat]) - 1)) else NA_real_,
         diff = if (any(dif)) max((ex$upper[dif] - ex$lower[dif]) / 2) else NA_real_)
}

# markers' true AUC and the AUC of one large diagnostic study
expected_markers <- function(sc, oc, opt) {
    big <- run_design("diagnostic", sc, oc, calibrate(sc, oc), derive_seed("truth", sc$id, oc$id), 20000, opt)
    fd <- format_study(sc, oc, "diagnostic", big, character(), "en")
    # the truth: a clean population (no spectrum or verification bias) of 100,000
    pop <- run_design("diagnostic", sc, oc, calibrate(sc, oc), derive_seed("truth", "population", sc$id, oc$id), 100000,
                      opt[setdiff(names(opt), c("spectrum", "verify"))])
    pfd <- format_study(sc, oc, "diagnostic", pop, sc$order, "en")
    acc <- accuracy_table(oc, fd)
    if (!is.null(acc)) {
        tru <- accuracy_table(oc, pfd)
        acc$true <- tru$value; acc$expected <- acc$value; acc$value <- NULL
    }
    list(auc = marker_table(oc, fd, pfd), accuracy = acc)
}

# The expectations of every outcome from one large study of this design with
# the scenario's other outcomes (Simulated study option): list by outcome id
expected_all <- function(sc, oc, design, opt, factors, lang = "en", categorize = list()) {
    out <- list()
    out[[oc$id]] <- expected_estimates(sc, oc, design, opt, factors, lang, categorize)
    if (!isTRUE(opt$secondary) || !design %in% SECONDARY_DESIGNS) return(out)
    cal <- calibrate(sc, oc)
    big <- run_design(design, sc, oc, cal, derive_seed("truth", sc$id, oc$id), 60000, opt)
    big <- secondary_outcomes(sc, oc, design, big, derive_seed("truth", sc$id, oc$id), opt)
    fd <- format_study(sc, oc, design, big, sc$order, "en")
    fd <- measurement_error(fd, sc, factors, opt$reliability %||% 1, opt$misclass %||% 0, derive_seed("truth", "measurement"))
    fd <- categorize_study(fd, sc, categorize[intersect(names(categorize), factors)], "en")
    for (o in big$secondary) {
        fd2 <- fd
        # the secondary outcome's own follow-up, or none
        fd2$time <- fd[[paste0("time_", o)]]
        out[[o]] <- add_total(estimate_table(sc, sc$outcomes[[o]], design, fd2, factors, lang, categorize, ci = TRUE, opt = opt), sc, sc$outcomes[[o]], lang)
    }
    out
}

answer_key <- function(sc, oc, design, opt, factors, lang = "en", categorize = list()) {
    cal <- calibrate(sc, oc)
    unmeasured <- setdiff(names(oc$effects), c(factors, names(sc$interventions)))
    list(effects = true_effects(sc, oc, lang, total = TRUE),
         markers = if (design == "diagnostic") expected_markers(sc, oc, opt),
         expected = expected_estimates(sc, oc, design, opt, factors, lang, categorize),
         calibration = cal,
         structure = sc$edges,
         unmeasured = unmeasured,
         teaching = tr_text(sc$teaching, lang),
         references = sc$references)
}

# Trials with a binary or event outcome: what the randomised comparison
# estimates in absolute terms, from the potential outcomes of a large
# population (everybody under each arm, the other interventions at their
# reference): risk per arm, risk difference, NNT, risk ratio, and the marginal
# (population) odds or hazard ratio next to the conditional one the data were
# generated with -- they differ without any confounding (non-collapsibility).
trial_absolute <- function(sc, oc, design, opt, lang = "en", n = 100000L) {
    if (!design %in% c("rct", "cluster_rct", "factorial") || !oc$type %in% c("binary", "event") || !is.null(oc[["after"]])) return(NULL)
    ivs <- if (design == "factorial") factorial_ivs(sc, oc) else list(trial_iv(sc, oc))
    cal <- calibrate(sc, oc)
    pop <- population(sc, oc$id, derive_seed("potential outcomes", sc$id), n)
    fu <- opt$followup %||% oc$followup
    rows <- list()
    add <- function(key, quantity, value) rows[[length(rows) + 1]] <<- data.frame(key = key, quantity = quantity, value = value, stringsAsFactors = FALSE)
    for (iv in ivs) {
        lv <- iv$levels; tl <- dlevel(lv, lang)
        out <- lapply(seq_along(lv), function(k) {
            lp <- linear_predictor(oc, pop$cov, stats::setNames(list(rep(k, n)), iv$id))
            if (oc$type == "binary") return(list(risk = mean(stats::plogis(cal$a + lp))))
            t1 <- draw_outcome(oc, cal, lp, pop$noise)
            t2 <- competing_time(oc, cal, pop$noise, pop$cov, stats::setNames(list(rep(k, n)), iv$id))
            list(risk = mean(t1 <= fu & t1 < t2), time = pmin(t1, t2, fu), event = as.integer(t1 <= fu & t1 < t2))
        })
        when <- if (oc$type == "event") sprintf(" (%s %s %s)", gtext("by", lang), base::format(fu), unit_text(oc$time_units, lang)) else ""
        for (k in seq_along(lv)) add(paste(iv$id, "risk", k), sprintf("%s: %s%s", gtext("risk", lang), tl[k], when), out[[k]]$risk)
        eff <- oc$effects[[iv$id]]
        for (k in seq_along(lv)[-1]) {
            r0 <- out[[1]]$risk; r1 <- out[[k]]$risk
            vs <- sprintf("%s %s %s", tl[k], gtext("vs", lang), tl[1])
            add(paste(iv$id, "rd", k), sprintf("%s, %s", gtext("rd", lang), vs), r1 - r0)
            add(paste(iv$id, "nnt", k), sprintf("%s, %s", gtext(if (r1 < r0) "nnt" else "nnh", lang), vs), if (r1 != r0) 1 / abs(r1 - r0) else NA_real_)
            add(paste(iv$id, "rr", k), sprintf("%s, %s", gtext("rr", lang), vs), r1 / r0)
            if (oc$type == "binary") {
                add(paste(iv$id, "or_m", k), sprintf("%s, %s", gtext("or_marg", lang), vs), (r1 / (1 - r1)) / (r0 / (1 - r0)))
                add(paste(iv$id, "or_c", k), sprintf("%s, %s", gtext("or_cond", lang), vs), if (is.null(eff)) 1 else exp(eff$coef[k]))
            } else {
                # marginal hazard ratio: Cox on everybody's two potential outcomes, the arm the only term
                d <- data.frame(time = c(out[[1]]$time, out[[k]]$time), event = c(out[[1]]$event, out[[k]]$event), arm = rep(0:1, each = n))
                hr <- exp(stats::coef(survival::coxph(survival::Surv(time, event) ~ arm, data = d))[[1]])
                add(paste(iv$id, "hr_m", k), sprintf("%s, %s", gtext("hr_marg", lang), vs), hr)
                add(paste(iv$id, "hr_c", k), sprintf("%s, %s", gtext("hr_cond", lang), vs), if (is.null(eff)) 1 else exp(eff$coef[k]))
            }
        }
    }
    structure(do.call(rbind, rows), n = n)
}

# row keys and labels of trial_absolute(), known before any simulation
trial_absolute_rows <- function(sc, oc, design, lang = "en") {
    if (!design %in% c("rct", "cluster_rct", "factorial") || !oc$type %in% c("binary", "event") || !is.null(oc[["after"]])) return(character())
    ivs <- if (design == "factorial") factorial_ivs(sc, oc) else list(trial_iv(sc, oc))
    unname(unlist(lapply(ivs, function(iv) {
        k <- seq_along(iv$levels)
        c(paste(iv$id, "risk", k), unlist(lapply(k[-1], function(j) paste(iv$id, c("rd", "nnt", "rr", if (oc$type == "binary") c("or_m", "or_c") else c("hr_m", "hr_c")), j))))
    })))
}
