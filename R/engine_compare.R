# Compare designs: one scenario, one outcome, the same superpopulation (same
# seed and student key), several designs. Each design draws its own study of
# n people; the student-size estimates (with 95% CI) sit next to the truth, and
# optionally next to what each design estimates in a large study. The studies
# are stacked into one dataset with a `design` column.

compare_designs <- function(sc, outcome, designs, n, factors = NULL, seed = 1, student_key = "",
                            options = list(), lang = "en", categorize = list(), expected = FALSE,
                            titles = NULL) {
    use_i18n(sc)
    oc <- sc$outcomes[[outcome]]
    if (is.null(factors)) factors <- sc$order
    factors <- intersect(sc$order, factors)
    designs <- intersect(designs, compatible_designs(sc, outcome))
    titles <- titles %||% stats::setNames(designs, designs)
    runs <- list(); est <- list(); exp_ <- list()
    for (d in designs) {
        r <- simulate_study(sc, outcome, d, n, factors, seed = seed, student_key = student_key, options = options,
                            lang = lang, key = FALSE, categorize = categorize)
        runs[[d]] <- r
        e <- estimate_table(sc, oc, d, r$data_raw, factors, lang, categorize, ci = TRUE, opt = r$options)
        if (!is.null(e)) { e$design <- d; est[[d]] <- e }
        if (expected) {
            x <- expected_estimates(sc, oc, d, r$options, factors, lang, categorize)
            if (!is.null(x)) { x$design <- d; exp_[[d]] <- x }
        }
    }
    list(runs = runs,
         estimates = if (length(est)) do.call(rbind, est) else NULL,
         expected = if (length(exp_)) do.call(rbind, exp_) else NULL,
         data = stack_studies(lapply(runs, `[[`, "data"), titles[designs]),
         data_raw = stack_studies(lapply(runs, `[[`, "data_raw"), stats::setNames(designs, designs)))
}

# Studies of different designs in one data frame: a first `design` column, the
# union of the columns (empty where a design has none), factor levels merged.
stack_studies <- function(dfs, titles) {
    if (length(dfs) == 0) return(NULL)
    cols <- unique(unlist(lapply(dfs, names)))
    parts <- lapply(names(dfs), function(d) {
        x <- dfs[[d]]
        for (nm in setdiff(cols, names(x))) x[[nm]] <- rep(NA, nrow(x))
        x <- x[cols]
        x
    })
    out <- list(design = factor(rep(unname(titles), vapply(dfs, nrow, 0L)), levels = unique(unname(titles))))
    for (nm in cols) {
        vals <- lapply(parts, `[[`, nm)
        fac <- vapply(vals, is.factor, TRUE)
        x <- if (any(fac)) {
            lv <- unique(unlist(lapply(vals[fac], levels)))
            factor(unlist(lapply(vals, as.character)), levels = lv, ordered = all(vapply(vals[fac], is.ordered, TRUE)))
        } else unlist(vals)
        attr(x, "jmv-desc") <- Find(Negate(is.null), lapply(vals, attr, "jmv-desc"))
        out[[nm]] <- x
    }
    df <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
    for (nm in cols) attr(df[[nm]], "jmv-desc") <- attr(out[[nm]], "jmv-desc")
    df
}

# Forest-style plot: one panel per contrast, one line per design with the
# student-size estimate and its 95% CI, the crude estimate as an open circle,
# the large-study expectation as a cross, the true value as a dashed line.
compare_plot <- function(est, expected = NULL, titles, lang = "en", ncol = 2) {
    est$variable_t <- dname(est$variable, lang)
    est$design_t <- factor(titles[est$design], levels = rev(unique(titles[est$design])))
    est$panel <- factor(paste(est$variable_t, est$contrast, sep = ": "), levels = unique(paste(est$variable_t, est$contrast, sep = ": ")))
    ratio <- all(est$measure != "beta")
    p <- ggplot2::ggplot(est, ggplot2::aes(y = design_t)) +
        ggplot2::geom_vline(data = est[!is.na(est$true), ], ggplot2::aes(xintercept = true), linetype = "dashed", colour = "grey40") +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper), height = 0.25, colour = "#2e5f8f") +
        ggplot2::geom_point(ggplot2::aes(x = adjusted), colour = "#2e5f8f", size = 2.4) +
        ggplot2::geom_point(ggplot2::aes(x = crude), shape = 1, colour = "#c0392b", size = 2.4)
    if (!is.null(expected) && nrow(expected)) {
        expected$variable_t <- dname(expected$variable, lang)
        expected$design_t <- factor(titles[expected$design], levels = levels(est$design_t))
        expected$panel <- factor(paste(expected$variable_t, expected$contrast, sep = ": "), levels = levels(est$panel))
        p <- p + ggplot2::geom_point(data = expected, ggplot2::aes(x = adjusted, y = design_t), shape = 4, size = 2.6, colour = "grey15")
    }
    if (ratio) p <- p + ggplot2::scale_x_log10()
    p + ggplot2::facet_wrap(~panel, ncol = ncol, scales = "free_x") +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_bw(base_size = 11) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), strip.background = ggplot2::element_rect(fill = "grey95"),
                       plot.caption = ggplot2::element_text(hjust = 0, size = 9, colour = "grey35"))
}

utils::globalVariables(c("design_t", "true", "lower", "upper", "adjusted", "crude", "panel"))

# Replicates: `R` studies of each design (seeds derived from the study's seed
# and the replicate number, the same student key), each analysed as the
# student would; returns every replicate's estimates. `tick` is called after
# each study (jamovi's checkpoint, so a changed option can stop the loop).
compare_replicates <- function(sc, outcome, designs, n, factors, R, seed = 1, student_key = "", options = list(),
                               lang = "en", categorize = list(), tick = function() NULL) {
    use_i18n(sc)
    oc <- sc$outcomes[[outcome]]
    factors <- intersect(sc$order, factors %||% sc$order)
    out <- list()
    for (d in intersect(designs, compatible_designs(sc, outcome))) for (r in seq_len(R)) {
        s <- simulate_study(sc, outcome, d, n, factors, seed = derive_seed(seed, "replicate", r), student_key = student_key,
                            options = options, lang = lang, key = FALSE, categorize = categorize)
        e <- tryCatch(estimate_table(sc, oc, d, s$data_raw, factors, lang, categorize, ci = TRUE, opt = s$options), error = function(e) NULL)
        if (!is.null(e)) { e$design <- d; e$rep <- r; out[[length(out) + 1]] <- e }
        tick()
    }
    if (!length(out)) return(NULL)
    do.call(rbind, out)
}

# Per design and contrast: mean estimate (geometric for ratios), bias against
# the true value (% for ratios), empirical and model standard errors (log
# scale for ratios), power (95% CI excluding no effect) and coverage of the
# true value by the 95% CI
summarise_replicates <- function(reps) {
    if (is.null(reps)) return(NULL)
    z <- stats::qnorm(0.975)
    key <- paste(reps$design, reps$variable, reps$contrast, sep = "\r")
    rows <- lapply(split(seq_len(nrow(reps)), factor(key, levels = unique(key))), function(i) {
        x <- reps[i, ]
        lg <- x$measure[1] %in% c("OR", "HR", "RR", "ratio")
        tf <- if (lg) log else identity
        ok <- is.finite(tf(x$adjusted))
        est <- tf(x$adjusted[ok]); lo <- tf(x$lower[ok]); hi <- tf(x$upper[ok])
        tv <- x$true[1]; null <- if (lg) 0 else 0
        m <- mean(est)
        data.frame(design = x$design[1], variable = x$variable[1], contrast = x$contrast[1], true = tv,
                   mean = if (lg) exp(m) else m,
                   bias = if (!is.finite(tv)) NA_real_ else if (lg) 100 * (exp(m) / tv - 1) else m - tv,
                   se_emp = stats::sd(est), se_model = mean((hi - lo) / (2 * z), na.rm = TRUE),
                   power = 100 * mean(lo > null | hi < null, na.rm = TRUE),
                   coverage = if (!is.finite(tv)) NA_real_ else 100 * mean(lo <= tf(tv) & tf(tv) <= hi, na.rm = TRUE),
                   n = sum(ok), measure = x$measure[1], stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, rows); rownames(out) <- NULL
    out
}

# Sampling distribution of the adjusted estimates: one box per design and
# contrast, the true value dashed
replicates_plot <- function(reps, titles, lang = "en", ncol = 2) {
    reps$design_t <- factor(titles[reps$design], levels = rev(unique(titles[reps$design])))
    reps$panel <- factor(paste(dname(reps$variable, lang), reps$contrast, sep = ": "), levels = unique(paste(dname(reps$variable, lang), reps$contrast, sep = ": ")))
    tv <- reps[!duplicated(reps$panel) & is.finite(reps$true), ]
    # ratios beyond 1/100 or 100 (separated models) would squash the panels: left out of the plot only
    show <- is.finite(reps$adjusted) & (reps$measure == "beta" | (reps$adjusted > 0.01 & reps$adjusted < 100))
    p <- ggplot2::ggplot(reps[show, ], ggplot2::aes(x = adjusted, y = design_t)) +
        ggplot2::geom_vline(data = tv, ggplot2::aes(xintercept = true), linetype = "dashed", colour = "grey40") +
        ggplot2::geom_boxplot(fill = "#dbe7f3", colour = "#2e5f8f", outlier.size = 0.8, width = 0.5)
    if (all(reps$measure != "beta")) p <- p + ggplot2::scale_x_log10()
    p + ggplot2::facet_wrap(~panel, ncol = ncol, scales = "free_x") + ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_bw(base_size = 11) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), strip.background = ggplot2::element_rect(fill = "grey95"),
                       plot.caption = ggplot2::element_text(hjust = 0, size = 9, colour = "grey35"))
}
