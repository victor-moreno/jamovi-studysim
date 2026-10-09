# Library manager: orchestration only (reading, checking and writing live in
# library_io.R). Everything shown is cheap to compute, so the tables are
# filled in .init(); .run() only serves the two export buttons.

simLibraryClass <- R6::R6Class(
    "simLibraryClass",
    inherit = simLibraryBase,
    private = list(
        .state = NULL,

        .lang = function() {
            l <- tryCatch(self$options$translate("studysim-language-code"), error = function(e) "")
            if (is.character(l) && length(l) == 1 && grepl("^[a-z]{2}$", l)) l else "en"
        },

        # list(lib, custom_ids) or list(error)
        .library = function() {
            if (!is.null(private$.state)) return(private$.state)
            o <- self$options
            st <- tryCatch({
                custom <- if (nzchar(o$libContent)) read_library_payload(o$libContent, o$libName) else empty_library()
                lib <- switch(o$source,
                    builtin = builtin_library(),
                    custom = custom,
                    merge_libraries(builtin_library(), custom))
                if (length(lib$scenarios) == 0)
                    stop(.("The library has no scenarios. Press Browse… to read a custom library."), call. = FALSE)
                list(lib = lib, custom = names(custom$scenarios))
            }, error = function(e) list(error = conditionMessage(e)))
            private$.state <- st
            st
        },

        .init = function() {
            for (nm in c("exportState", "templateState"))
                if (self$results[[nm]]$rowCount == 0)
                    self$results[[nm]]$addRow(rowKey = 1, values = list(fired = ""))
            st <- private$.library()
            if (!is.null(st$error)) {
                self$results$notice$setContent(sprintf("<p><b>%s</b></p>", htmlEscape(st$error)))
                self$results$notice$setVisible(TRUE)
                for (nm in c("scenarios", "problems", "variables", "effects", "structure", "about"))
                    self$results[[nm]]$setVisible(FALSE)
                return()
            }
            lib <- st$lib; l <- private$.lang()
            if (self$options$showScenarios) private$.fillScenarios(lib, st$custom, l)
            if (self$options$showProblems) private$.fillProblems(lib)
            if (self$options$showDetails) private$.fillDetails(lib, l)
        },

        .fillScenarios = function(lib, custom, l) {
            tbl <- self$results$scenarios
            for (id in names(lib$scenarios)) {
                s <- lib$scenarios[[id]]
                sc <- tryCatch(load_scenario(lib, id, strict = FALSE), error = function(e) NULL)
                des <- if (is.null(sc)) "" else paste(unique(unlist(lapply(names(sc$outcomes), function(o)
                    tryCatch(compatible_designs(sc, o), error = function(e) character())))), collapse = ", ")
                tbl$addRow(rowKey = id, values = list(
                    id = id, label = tr_text(s$label, l),
                    outcomes = paste(names(s$outcomes), collapse = ", "), designs = des,
                    nvars = length(s$variables), origin = if (id %in% custom) .("custom") else .("built-in")))
            }
        },

        .fillProblems = function(lib) {
            tbl <- self$results$problems
            p <- check_library(lib)
            if (nrow(p) == 0) {
                tbl$setNote("ok", jmvcore::format(.("No problems found in {n} scenarios."), n = length(lib$scenarios)))
                return()
            }
            for (i in seq_len(nrow(p))) tbl$addRow(rowKey = i, values = as.list(p[i, ]))
        },

        .fillDetails = function(lib, l) {
            id <- self$options$scenario
            if (!id %in% names(lib$scenarios)) {
                self$results$about$setContent(sprintf("<p>%s</p>", htmlEscape(jmvcore::format(
                    .("There is no scenario '{id}' in this library; choose one listed under Scenarios."), id = id))))
                for (nm in c("variables", "effects", "structure")) self$results[[nm]]$setVisible(FALSE)
                return()
            }
            sc <- tryCatch(load_scenario(lib, id, strict = FALSE), error = function(e) NULL)
            if (is.null(sc)) return()
            use_i18n(sc)
            vt <- self$results$variables
            for (v in sc$order) {
                x <- sc$variables[[v]]
                vt$addRow(rowKey = v, values = list(name = v, label = var_desc(x, l), type = x$type,
                                                     distribution = describe_variable(x)))
            }
            for (iv in sc$interventions)
                vt$addRow(rowKey = iv$id, values = list(name = iv$id, label = tr_text(iv$label, l), type = .("intervention"),
                                                         distribution = paste(iv$levels, collapse = " / ")))
            et <- self$results$effects
            k <- 0
            for (o in sc$outcomes) {
                te <- true_effects(sc, o, l)
                for (i in seq_len(nrow(te))) {
                    k <- k + 1
                    et$addRow(rowKey = k, values = list(outcome = o$id, variable = te$variable[i], contrast = te$contrast[i],
                                                        measure = te$measure[i], value = te$value[i], note = te$note[i]))
                }
            }
            stt <- self$results$structure
            ed <- sc$edges
            for (i in seq_len(nrow(ed)))
                stt$addRow(rowKey = paste0("e", i), values = list(from = ed$from[i], to = ed$to[i], strength = ed$b[i], kind = .("path")))
            R <- sc$R_exo
            if (length(R) > 1) for (i in seq_len(nrow(R) - 1)) for (j in (i + 1):nrow(R)) if (R[i, j] != 0)
                stt$addRow(rowKey = paste0("c", i, "_", j), values = list(from = rownames(R)[i], to = rownames(R)[j],
                                                                          strength = R[i, j], kind = .("correlation")))
            target <- function(o) switch(o$type,
                binary = jmvcore::format(.("prevalence {p}"), p = o$prevalence),
                continuous = jmvcore::format(.("mean {m}, residual SD {s}"), m = o$mean, s = o$sd),
                count = jmvcore::format(.("mean {m}"), m = o$mean),
                event = switch(o$target$kind,
                    rate = jmvcore::format(.("rate {r} per year"), r = signif(o$target$value, 3)),
                    risk = jmvcore::format(.("risk {r} at {t} years"), r = o$target$value, t = o$target$at),
                    median = jmvcore::format(.("median {m} years"), m = o$target$value)))
            outs <- vapply(sc$outcomes, function(o) sprintf("<li><b>%s</b> (%s, %s): %s</li>", o$id, o$type,
                htmlEscape(tr_text(o$label, l)), htmlEscape(target(o))), "")
            self$results$about$setContent(paste0(
                "<p><b>", htmlEscape(tr_text(sc$label, l)), "</b>. ", htmlEscape(tr_text(sc$population, l)), "</p>",
                "<ul>", paste(outs, collapse = ""), "</ul>",
                if (!is.null(sc$teaching)) paste0("<p><b>", .("Teaching note"), "</b>: ", htmlEscape(tr_text(sc$teaching, l)), "</p>") else "",
                if (length(sc$references)) paste0("<p><b>", .("References"), "</b></p><ul>",
                    paste0("<li>", htmlEscape(sc$references), "</li>", collapse = ""), "</ul>") else ""))
        },

        .run = function() {
            st <- private$.library()
            o <- self$options
            once <- function(stateName, optName, fun) {
                s <- self$results[[stateName]]
                if (!isTRUE(o[[optName]])) { s$setRow(rowNo = 1, values = list(fired = "")); return() }
                if (isTRUE(s$getCell(rowNo = 1, col = "fired")$value == "yes")) return()
                self$options$option(optName)$perform(fun)
                s$setRow(rowNo = 1, values = list(fired = "yes"))
            }
            if (is.null(st$error)) once("exportState", "exportLib", function(action) {
                lib <- st$lib
                base <- "studysim-library"
                if (o$exportScope == "scenario" && o$scenario %in% names(lib$scenarios)) {
                    lib <- library_subset(lib, o$scenario)
                    base <- o$scenario
                }
                switch(o$exportFormat,
                    xlsx = { write_library_xlsx(lib, action$params$fullPath); list(filename = paste0(base, ".xlsx")) },
                    json = { writeLines(library_to_json(lib, pretty = TRUE), action$params$fullPath, useBytes = TRUE)
                             list(filename = paste0(base, ".json")) },
                    yaml = { write_library_yaml_zip(lib, action$params$fullPath); list(filename = paste0(base, ".zip")) })
            })
            once("templateState", "exportTemplate", function(action) {
                lib <- library_subset(builtin_library(), "breast_cancer_risk")
                help <- readLines(system.file("library", "schema.md", package = "studysim"), warn = FALSE, encoding = "UTF-8")
                write_library_xlsx(lib, action$params$fullPath, help = help)
                list(filename = "studysim-template.xlsx")
            })
        }
    )
)
