# Examples in the tutorial are computed by the module's own engine.
pkgload::load_all("..", quiet = TRUE, export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)
lib <- builtin_library()
key_table <- function(id, outcome, design, factors, options = list(), categorize = list()) {
    sc <- load_scenario(lib, id)
    use_i18n(sc)
    oc <- sc$outcomes[[outcome]]
    opt <- utils::modifyList(design_defaults(design, oc, sc), options)
    e <- expected_estimates(sc, oc, design, opt, factors, "en", categorize)
    e$true <- round(e$true, 2); e$crude <- round(e$crude, 2); e$adjusted <- round(e$adjusted, 2)
    knitr::kable(e[, c("variable", "contrast", "true_measure", "true", "crude", "adjusted", "measure")],
                 col.names = c("Variable", "Contrast", "True", "", "Crude", "Adjusted", "Estimate"), row.names = FALSE)
}
