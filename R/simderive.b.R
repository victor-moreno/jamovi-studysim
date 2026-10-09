# Derive scenario from data: orchestration only (derive.R does the work).
# The derivation takes a few seconds, so it is kept in the variables table's
# state and reused on clicks that do not change it; the tables are filled from
# it in .run() (their rows depend on the data, which .init() does not read).

simDeriveClass <- R6::R6Class(
    "simDeriveClass",
    inherit = simDeriveBase,
    private = list(

        .notice = function(html) {
            self$results$notice$setContent(html)
            self$results$notice$setVisible(TRUE)
        },

        .init = function() {
            if (self$results$exportState$rowCount == 0)
                self$results$exportState$addRow(rowKey = 1, values = list(fired = ""))
        },

        .run = function() {
            o <- self$options
            if (is.null(o$outcome) || length(o$covs) == 0) {
                self$results$summary$setContent(paste0("<p>", .("Choose the outcome and the variables of the scenario (and the follow-up time for an event outcome)."), "</p>"))
                return()
            }
            d <- self$results$variables$state
            if (is.null(d)) {
                df <- self$data
                cols <- unique(c(o$outcome, o$covs, o$time))
                # jamovi reads integer columns of a CSV as nominal: numbers with
                # more than 5 distinct values are taken as numbers again
                asnum <- character()
                for (v in cols) {
                    x <- df[[v]]
                    if (is.factor(x) && nlevels(x) > 5 && !anyNA(suppressWarnings(as.numeric(levels(x))))) {
                        df[[v]] <- jmvcore::toNumeric(x)
                        asnum <- c(asnum, v)
                    }
                }
                id <- make_ids(o$scenarioId)[1]
                d <- tryCatch(derive_scenario(df[, cols, drop = FALSE], o$outcome, o$outcomeType, covs = setdiff(o$covs, c(o$outcome, o$time)),
                                              time = o$time, id = id,
                                              label = if (nzchar(trimws(o$scenarioLabel))) trimws(o$scenarioLabel) else NULL,
                                              log_outcome = o$logOutcome,
                                              source = if (nzchar(trimws(o$sourceName))) trimws(o$sourceName) else "a jamovi dataset"),
                              error = function(e) e)
                if (inherits(d, "error")) {
                    private$.notice(sprintf("<p><b>%s</b></p>", htmlEscape(conditionMessage(d))))
                    return()
                }
                d$as_numeric <- asnum
                self$results$variables$setState(d)
            }
            private$.fill(d)
            if (o$validate) {
                ex <- self$results$effects$state
                if (is.null(ex)) {
                    ex <- tryCatch(derive_check(d), error = function(e) NULL)
                    if (!is.null(ex)) self$results$effects$setState(ex)
                }
                if (!is.null(ex)) {
                    tbl <- self$results$effects
                    for (i in seq_len(nrow(ex))) {
                        k <- paste(ex$variable[i], sub("^per .*$", "per 1", ex$contrast[i]))
                        if (k %in% tbl$rowKeys) tbl$setRow(rowKey = k, values = list(simulated = ex$adjusted[i]))
                    }
                    tbl$setNote("check", jmvcore::format(.("Re-estimated: the same model fitted to one study of {n} rows simulated from the derived scenario; it should be close to the adjusted estimate."),
                                                         n = base::format(attr(ex, "n"), big.mark = ",")))
                }
            }
            private$.export(d)
        },

        .fill = function(d) {
            sc <- d$scenario
            orig <- unlist(sc$derivation$original_names)
            miss <- unlist(sc$derivation$missing)
            vt <- self$results$variables
            for (v in names(sc$variables)) {
                x <- normalize_variable(v, sc$variables[[v]], list(), new_problems())
                vt$addRow(rowKey = v, values = list(name = v, original = orig[[v]], type = x$type,
                                                     distribution = describe_variable(x), missing = miss[[v]]))
            }
            et <- self$results$effects
            e <- d$estimates
            for (i in seq_len(nrow(e)))
                et$addRow(rowKey = paste(e$variable[i], e$contrast[i]), values = list(
                    variable = e$variable[i], contrast = e$contrast[i], measure = e$measure[i],
                    estimate = e$estimate[i], lower = e$lower[i], upper = e$upper[i], crude = e$crude[i]))
            ct <- self$results$correlations
            cr <- d$correlations
            for (i in seq_len(nrow(cr)))
                ct$addRow(rowKey = i, values = list(a = cr$a[i], b = cr$b[i], observed = cr$observed[i], latent = cr$latent[i],
                                                    kept = if (abs(cr$latent[i]) >= 0.05) .("yes") else .("no")))
            oc <- sc$outcomes[[1]]
            target <- switch(d$type,
                binary = jmvcore::format(.("prevalence {p}"), p = oc$prevalence),
                continuous = jmvcore::format(.("mean {m}, residual SD {s}"), m = oc$mean, s = oc$sd),
                count = jmvcore::format(.("mean {m}"), m = oc$mean),
                event = jmvcore::format(.("risk {r} at {t} years"), r = oc$risk$value, t = oc$risk$at))
            self$results$summary$setContent(paste0(
                "<p><b>", htmlEscape(sc$id), "</b>: ", htmlEscape(sc$label), ".</p><ul>",
                "<li>", jmvcore::format(.("Outcome {o} ({type}): {target}"), o = d$outcome_id, type = d$type, target = target), "</li>",
                "<li>", jmvcore::format(.("{n} rows, {c} complete for the model"), n = sc$derivation$rows, c = d$complete), "</li>",
                if (length(d$as_numeric)) paste0("<li>", jmvcore::format(.("read as numbers although their type is nominal: {vars}"),
                                                                       vars = paste(d$as_numeric, collapse = ", ")), "</li>") else "",
                "</ul>",
                "<p>", .("The correlations are undirected: before teaching with this scenario, decide which variables cause which (structure), add rules for impossible combinations, labels and a teaching note. Export it and edit the file, or give it to an AI with the prompt in the module's documentation."), "</p>"))
            self$results$yaml$setContent(yaml::as.yaml(sc))
        },

        .export = function(d) {
            st <- self$results$exportState
            if (!isTRUE(self$options$exportScenario)) { st$setRow(rowNo = 1, values = list(fired = "")); return() }
            if (isTRUE(st$getCell(rowNo = 1, col = "fired")$value == "yes")) return()
            sc <- d$scenario
            lib <- list(dictionary = list(), scenarios = stats::setNames(list(sc), sc$id), i18n = list())
            self$options$option("exportScenario")$perform(function(action) {
                switch(self$options$exportFormat,
                    yaml = { writeLines(yaml::as.yaml(sc), action$params$fullPath, useBytes = TRUE)
                             list(filename = paste0(sc$id, ".yaml")) },
                    xlsx = { write_library_xlsx(lib, action$params$fullPath); list(filename = paste0(sc$id, ".xlsx")) },
                    json = { writeLines(library_to_json(lib, pretty = TRUE), action$params$fullPath, useBytes = TRUE)
                             list(filename = paste0(sc$id, ".json")) })
            })
            st$setRow(rowNo = 1, values = list(fired = "yes"))
        }
    )
)
