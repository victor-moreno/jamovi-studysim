# The engine's one entry point: scenario + outcome + design (+ n, seed, ...)
# -> a dataset, its dictionary, the design facts and the answer key.

simulate_study <- function(sc, outcome, design, n, factors = NULL, seed = 1, student_key = "",
                           options = list(), missing = 0, missing_mech = "mcar", lang = "en", key = TRUE) {
    oc <- sc$outcomes[[outcome]]
    if (is.null(oc)) stop(sprintf("scenario '%s' has no outcome '%s'", sc$id, outcome), call. = FALSE)
    ok <- compatible_designs(sc, outcome)
    if (!design %in% ok)
        stop(sprintf("a %s design does not fit outcome '%s' (possible: %s)", design, outcome,
                     paste(ok, collapse = ", ")), call. = FALSE)
    if (is.null(factors)) factors <- sc$order
    factors <- intersect(sc$order, factors)
    opt <- utils::modifyList(design_defaults(design, oc, sc), options)
    seed_eff <- if (nzchar(student_key)) derive_seed(seed, trimws(student_key)) else seed
    cal <- calibrate(sc, oc)
    res <- run_design(design, sc, oc, cal, seed_eff, n, opt)
    df <- format_study(sc, oc, design, res, factors, lang)
    driver <- if (design %in% c("case_control", "matched_cc")) "case" else oc$id
    df <- apply_missing(df, factors, missing, missing_mech, derive_seed(seed_eff, "missing"), driver, df$id)
    ids <- names(df)
    raw <- df   # engine names and the library's levels, for the tables keyed by them
    df <- localise_data(df, lang)
    out <- list(data = df, data_raw = raw, dictionary = data_dictionary(sc, df, factors, ids), facts = res$facts,
                design = design, outcome = outcome, options = opt, seed = seed_eff, calibration = cal)
    if (key) out$key <- answer_key(sc, oc, design, opt, factors, lang)
    out
}
