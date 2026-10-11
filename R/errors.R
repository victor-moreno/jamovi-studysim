# One error handler for the four analyses: the engine's own messages (raised
# with call. = FALSE, in English, since the engine knows nothing of jamovi)
# are matched and given in jamovi's language; R's internal errors (they carry
# a call) are prefaced by a translated sentence and keep R's text as detail.
# Messages already built with .() in the analyses pass through unchanged.
# `self` is the calling analysis: .() translates through it.
friendly_error <- function(e, self) {
    msg <- if (inherits(e, "condition")) conditionMessage(e) else as.character(e)
    rules <- list(
        "^the outcome is too rare: ([0-9]+) cases in ([0-9,]+) people" = function(m)
            jmvcore::format(.("The outcome is too rare: {n} cases in {pop} people. Ask for fewer cases, or raise its frequency."), n = m[2], pop = m[3]),
        "^the outcome is too rare: too few events among ([0-9,]+) people" = function(m)
            jmvcore::format(.("The outcome is too rare: too few events among {pop} people. Ask for a smaller study, or raise its frequency."), pop = m[2]),
        "^a cluster trial needs at least 4 clusters" = function(m)
            .("A cluster trial needs at least 4 clusters and 2 people per cluster: raise the sample size or lower the number of clusters."),
        "^too few people above the entry threshold" = function(m)
            .("Too few people above the entry threshold: lower it."),
        "^the library has no scenario '(.*)'$" = function(m)
            jmvcore::format(.("The library has no scenario '{id}'. Choose another scenario, or load the library that has it."), id = m[2]),
        "^'(.*)': a library is a " = function(m)
            jmvcore::format(.("'{file}' is not a library: use a .yaml, .yml, .json, .xlsx or .zip file."), file = m[2]),
        "^the library payload is not valid base64" = function(m)
            .("The custom library could not be read: load it again with Browse…"),
        "^cut points are empty" = function(m)
            .("Cut points are empty: write e.g. 18.5, 25, 30 or 10%, 90%."),
        "^'(.*)': use either values or percentiles, not both" = function(m)
            jmvcore::format(.("'{cuts}': use either values or percentiles, not both."), cuts = m[2]),
        "^'(.*)' is not a list of numbers" = function(m)
            jmvcore::format(.("'{cuts}' is not a list of numbers."), cuts = m[2]),
        "^'(.*)': percentiles must be between 0 and 100" = function(m)
            jmvcore::format(.("'{cuts}': percentiles must be between 0 and 100."), cuts = m[2]),
        "^write the divisor in Cuts" = function(m)
            .("Write the divisor in Cuts, a positive number such as 10."),
        "^scenario '(.*)' has problems:\n(.*)$" = function(m)
            paste(jmvcore::format(.("The scenario '{id}' has problems in its library definition (check it with Library):"), id = m[2]), m[3]),
        "^an event outcome needs a time variable" = function(m)
            .("An event outcome needs a time variable: choose the follow-up time."))
    for (p in names(rules)) {
        m <- regmatches(msg, regexec(p, msg))[[1]]
        if (length(m)) return(rules[[p]](m))
    }
    if (inherits(e, "condition") && !is.null(conditionCall(e)))
        return(paste(.("The calculation failed for these options. R said:"), msg))
    msg
}
