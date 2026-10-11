# The engine's one entry point: scenario + outcome + design (+ n, seed, ...)
# -> a dataset, its dictionary, the design facts and the answer key.

simulate_study <- function(sc, outcome, design, n, factors = NULL, seed = 1, student_key = "",
                           options = list(), missing = 0, missing_mech = "mcar", lang = "en", key = TRUE,
                           categorize = list(), noise = 0, entry = 0) {
    use_i18n(sc)   # column names and levels in the scenario's translations
    oc <- sc$outcomes[[outcome]]
    if (is.null(oc)) stop(sprintf("scenario '%s' has no outcome '%s'", sc$id, outcome), call. = FALSE)
    ok <- compatible_designs(sc, outcome)
    if (!design %in% ok)
        stop(sprintf("a %s design does not fit outcome '%s' (possible: %s)", design, outcome,
                     paste(ok, collapse = ", ")), call. = FALSE)
    if (is.null(factors)) factors <- sc$order
    factors <- intersect(sc$order, factors)
    opt <- utils::modifyList(design_defaults(design, oc, sc), options)
    # a student key ignores case and surrounding spaces ("Anna " = "anna")
    seed_eff <- if (nzchar(trimws(student_key))) derive_seed(seed, tolower(trimws(student_key))) else seed
    cal <- calibrate(sc, oc)
    res <- run_design(design, sc, oc, cal, seed_eff, n, opt)
    if (isTRUE(opt$secondary)) res <- secondary_outcomes(sc, oc, design, res, seed_eff, opt)
    df <- format_study(sc, oc, design, res, factors, lang)
    # measurement error before grouping (misclassification follows), as in the key
    df <- measurement_error(df, sc, factors, opt$reliability %||% 1, opt$misclass %||% 0, derive_seed(seed_eff, "measurement"))
    df <- assay_realism(df, sc, oc, design, factors, opt, derive_seed(seed_eff, "assay"),
                        status = if (oc$id %in% names(df)) as.integer(df[[oc$id]]) else NULL, lang = lang)
    df <- apply_biases(df, sc, oc, design, opt, seed_eff, lang)   # recall, collider, immortal time
    df <- categorize_study(df, sc, categorize, lang)
    df <- add_noise_vars(df, noise, derive_seed(seed_eff, "noise"), lang)
    # missingness "depending on the outcome": case status, or method A in a method comparison (no outcome column)
    driver <- if (design %in% CASE_DESIGNS) "case" else if (design == "method_comparison") paste0(oc$id, "_a") else oc$id
    mcols <- c(if (design == "ecological") unlist(lapply(factors, function(v) eco_cols(sc, v))) else factors, noise_names(noise))
    df <- apply_missing(df, mcols, missing, missing_mech, derive_seed(seed_eff, "missing"), driver, df$id %||% df$region)
    df <- entry_errors(df, intersect(mcols, names(df)), entry, derive_seed(seed_eff, "entry"))
    res$facts$entry_errors <- attr(df, "entry_errors")
    if (isTRUE(opt$layout == "wide") && design %in% c("crossover", "longitudinal")) df <- to_wide(df, sc, oc, design, opt, lang)
    if (noise > 0) res$facts$noise <- noise
    ids <- names(df)
    raw <- df   # engine names and the library's levels, for the tables keyed by them
    df <- localise_data(df, lang)
    out <- list(data = df, data_raw = raw, dictionary = data_dictionary(sc, df, factors, ids), facts = res$facts,
                secondary = res$secondary,
                design = design, outcome = outcome, options = opt, seed = seed_eff, calibration = cal)
    if (key) out$key <- answer_key(sc, oc, design, opt, factors, lang, categorize)
    out
}
