# Examples in the tutorial are computed by the module's own engine.
pkgload::load_all("..", quiet = TRUE, export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)
lib <- builtin_library()
# the answer key's expected estimates; total and DAG-adjusted columns only
# where they differ from the direct effect and the fully adjusted estimate
key_table <- function(id, outcome, design, factors, options = list(), categorize = list()) {
    sc <- load_scenario(lib, id)
    use_i18n(sc)
    oc <- sc$outcomes[[outcome]]
    opt <- utils::modifyList(design_defaults(design, oc, sc), options)
    e <- expected_estimates(sc, oc, design, opt, factors, "en", categorize)
    r2 <- function(x) round(x, 2)
    cols <- c("variable", "contrast", "true_measure", "true")
    nm <- c("Variable", "Contrast", "True", "direct")
    if (any(!is.na(e$total) & abs(e$total - e$true) > 1e-9)) { cols <- c(cols, "total"); nm <- c(nm, "total") }
    cols <- c(cols, "crude", "adjusted"); nm <- c(nm, "Crude", "Adjusted (all)")
    if (any(!is.na(e$dag) & abs(e$dag - e$adjusted) > 1e-9)) { cols <- c(cols, "dag"); nm <- c(nm, "Adjusted (DAG)") }
    cols <- c(cols, "measure"); nm <- c(nm, "Estimate")
    for (k in intersect(c("true", "total", "crude", "adjusted", "dag"), cols)) e[[k]] <- r2(e[[k]])
    knitr::kable(e[, cols], col.names = nm, row.names = FALSE)
}
# absolute effects of a trial (risks, RD, NNT, marginal and conditional ratios)
trial_table <- function(id, outcome, design = "rct") {
    sc <- load_scenario(lib, id)
    use_i18n(sc)
    oc <- sc$outcomes[[outcome]]
    a <- trial_absolute(sc, oc, design, design_defaults(design, oc, sc))
    a$value <- signif(a$value, 3)
    knitr::kable(a[, c("quantity", "value")], col.names = c("", "Value"), row.names = FALSE)
}
