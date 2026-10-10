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
