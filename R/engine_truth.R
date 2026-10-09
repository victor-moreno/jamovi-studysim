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
        return(data.frame(variable = v, contrast = sprintf("%s vs %s", lv[-1], lv[1]),
                          coefname = paste0(v, lv[-1]), per = 1, level = seq_along(lv)[-1], stringsAsFactors = FALSE))
    }
    x <- sc$variables[[v]]
    if (x$type %in% c("continuous", "count")) {
        per <- eff$per %||% 1
        u <- if (is.null(x$units)) "" else paste0(" ", tr_text(x$units, lang))
        return(data.frame(variable = v, contrast = sprintf("per %s%s", format(per), u), coefname = v,
                          per = per, level = NA_integer_, stringsAsFactors = FALSE))
    }
    lv <- x$levels
    data.frame(variable = v, contrast = sprintf("%s vs %s", lv[-1], lv[1]), coefname = paste0(v, lv[-1]),
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

true_effects <- function(sc, oc, lang = "en") {
    vars <- unique(c(names(oc$effects), intersect(sc$null_variables, names(sc$variables))))
    rows <- lapply(vars, function(v) {
        cr <- contrast_rows(sc, oc, v, lang)
        eff <- oc$effects[[v]]
        note <- ""
        if (!is.null(eff$by)) {
            mv <- sc$variables[[eff$by$var]]
            back <- function(b) if (eff$log) exp(b) else b
            note <- paste(sprintf("%s=%s: %.3g", eff$by$var, mv$levels, back(eff$by$coef)), collapse = "; ")
        }
        data.frame(variable = v, contrast = cr$contrast,
                   measure = if (is.null(eff)) "none" else eff$measure,
                   value = vapply(seq_len(nrow(cr)), function(i) true_value(eff, cr[i, ]), 0),
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
    model <- if (design == "matched_cc") "clogit" else if (design %in% c("case_control")) "logistic"
             else if (!is.null(fd$time)) "cox" else if (oc$type %in% c("binary", "event")) "logistic"
             else if (oc$type == "count") "poisson" else "linear"
    if (design %in% c("case_control", "matched_cc")) fd$.s <- as.integer(fd$case) - 1L
    else if (model == "logistic") fd$.s <- as.integer(fd[[oc$id]]) - 1L
    if (design == "crossover" && full) {
        # subject fixed effects by the within transformation: same estimate as
        # lm(y ~ trt + period + factor(id)), without a column per subject
        X <- stats::model.matrix(stats::as.formula(paste("~", rhs, "+ factor(period)")), fd)[, -1, drop = FALSE]
        y <- fd[[oc$id]]
        if (isTRUE(oc$log)) y <- log(y)
        w <- function(x) x - stats::ave(x, fd$id)
        b <- qr.coef(qr(apply(X, 2, w)), w(y))
        return(list(coef = b, measure = if (isTRUE(oc$log)) "ratio" else "beta", log = isTRUE(oc$log)))
    }
    f <- switch(model,
        logistic = stats::glm(stats::as.formula(paste(".s ~", rhs)), stats::binomial(), fd),
        clogit = survival::clogit(stats::as.formula(paste(".s ~", rhs, "+ strata(set)")), data = fd),
        cox = survival::coxph(stats::as.formula(paste("Surv(time,", oc$id, ") ~", rhs)), data = fd),
        poisson = stats::glm(stats::as.formula(paste(oc$id, "~", rhs)), stats::poisson(), fd),
        linear = stats::lm(stats::as.formula(paste(if (isTRUE(oc$log)) sprintf("log(%s)", oc$id) else oc$id, "~", rhs)), fd))
    if (model == "linear" && isTRUE(oc$log))
        return(list(coef = stats::coef(f), measure = "ratio", log = TRUE))
    list(coef = stats::coef(f), measure = c(logistic = "OR", clogit = "OR", cox = "HR", poisson = "RR", linear = "beta")[[model]],
         log = model != "linear")
}

expected_estimates <- function(sc, oc, design, opt, factors, lang = "en") {
    cal <- calibrate(sc, oc)
    n_big <- switch(design, case_control = 3000 * (1 + opt$ratio), matched_cc = 1500 * (1 + opt$ratio),
                    crossover = 4000, 20000)
    big <- run_design(design, sc, oc, cal, derive_seed("truth", sc$id, oc$id), n_big, opt)
    fd <- format_study(sc, oc, design, big, sc$order, lang)
    iv <- intersect(names(sc$interventions), names(fd))
    main <- if (length(iv)) iv else factors
    if (length(main) == 0) return(NULL)
    # covariates are constant within a crossover subject: the subject terms absorb them
    adj_terms <- if (design == "crossover") iv else unique(c(iv, factors))
    adj <- fit_design_model(design, sc, oc, fd, adj_terms)
    rows <- lapply(main, function(v) {
        cr <- contrast_rows(sc, oc, v, lang)
        crude <- fit_design_model(design, sc, oc, fd, v, full = FALSE)
        get <- function(fit) {
            b <- fit$coef[cr$coefname] * cr$per
            if (fit$log) exp(b) else b
        }
        eff <- oc$effects[[v]]
        tv <- vapply(seq_len(nrow(cr)), function(i) true_value(eff, cr[i, ]), 0)
        note <- ""
        if (!is.null(eff$by)) {
            # a modified effect has no single true value: give it per level
            back <- function(b) if (eff$log) exp(b) else b
            note <- paste(sprintf("%s=%s: %.3g", eff$by$var, sc$variables[[eff$by$var]]$levels, back(eff$by$coef)), collapse = "; ")
            tv[] <- NA_real_
        }
        data.frame(variable = v, contrast = cr$contrast,
                   true = tv,
                   true_measure = if (is.null(eff)) "none" else eff$measure,
                   crude = unname(get(crude)), adjusted = unname(get(adj)), measure = adj$measure,
                   note = note, stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, rows)
    out$true[out$true_measure == "none"] <- if (adj$log) 1 else 0
    attr(out, "n") <- nrow(fd)
    attr(out, "adjusted_for") <- adj_terms
    out
}

answer_key <- function(sc, oc, design, opt, factors, lang = "en") {
    cal <- calibrate(sc, oc)
    unmeasured <- setdiff(names(oc$effects), c(factors, names(sc$interventions)))
    list(effects = true_effects(sc, oc, lang),
         expected = expected_estimates(sc, oc, design, opt, factors, lang),
         calibration = cal,
         structure = sc$edges,
         unmeasured = unmeasured,
         teaching = tr_text(sc$teaching, lang),
         references = sc$references)
}
