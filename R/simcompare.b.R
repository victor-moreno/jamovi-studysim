# Compare designs: orchestration only (engine_compare.R does the work). Rows
# of the tables are planned in .init() from the designs that fit the outcome;
# .run() draws one study per design from the same superpopulation.

simCompareClass <- R6::R6Class(
    "simCompareClass",
    inherit = simCompareBase,
    private = list(
        .ctx = NULL,

        .lang = function() {
            l <- tryCatch(self$options$translate("studysim-language-code"), error = function(e) "")
            if (is.character(l) && length(l) == 1 && grepl("^[a-z]{2}$", l)) l else "en"
        },

        .context = function() {
            if (!is.null(private$.ctx)) return(private$.ctx)
            o <- self$options
            ctx <- tryCatch({
                lib <- get_library(o$libContent, o$libName)
                sc <- use_i18n(load_scenario(lib, o$scenario))
                oid <- if (nzchar(o$outcome) && o$outcome %in% names(sc$outcomes)) o$outcome else names(sc$outcomes)[1]
                ok <- compatible_designs(sc, oid)
                designs <- intersect(unlist(o$designs), ok)
                if (length(designs) == 0)
                    stop(jmvcore::format(.("Tick at least one design that fits this outcome: {list}."),
                                         list = paste(vapply(ok, design_title, "", self), collapse = ", ")), call. = FALSE)
                m <- parse_measured(o$measured, sc, self)
                # empty text boxes keep the scenario's values; a mistyped number stops here, as a notice
                opts <- Filter(Negate(is.null), list(
                    ratio = num_opt(self, "ccRatio", .("Controls per case"), 1, 10, TRUE),
                    loss = num_opt(self, "loss", .("Loss to follow-up (% per time unit)"), 0, 50, scale = 100),
                    followup = num_opt(self, "followup", .("Follow-up"), 0.01, 100)))
                list(lib = lib, sc = sc, oc = sc$outcomes[[oid]], designs = designs, factors = m$factors, cats = m$cats,
                     lang = private$.lang(), skipped = setdiff(unlist(o$designs), ok), opts = opts)
            }, error = function(e) list(error = conditionMessage(e)))
            private$.ctx <- ctx
            ctx
        },

        .notice = function(html) {
            self$results$notice$setContent(html)
            self$results$notice$setVisible(TRUE)
        },

        # rows of the estimates table: main contrasts (the intervention, or the
        # measured factors) of each design
        .rows = function(ctx) {
            rows <- list()
            for (d in ctx$designs) {
                sp <- special_rows(d, ctx$sc, ctx$oc, ctx$lang)
                if (!is.null(sp)) {
                    for (i in seq_len(nrow(sp))) rows[[length(rows) + 1]] <- list(key = paste(d, sp$variable[i], sp$contrast[i]),
                                                                                 design = d, variable = sp$variable[i], contrast = sp$contrast[i])
                    next
                }
                iv <- intersect(names(ctx$sc$interventions), planned_columns(ctx$sc, ctx$oc, d, ctx$factors))
                for (v in if (length(iv)) iv else ctx$factors) {
                    cr <- if (!is.null(ctx$cats[[v]])) cat_rows(v, ctx$cats[[v]], ctx$lang, ctx$sc) else contrast_rows(ctx$sc, ctx$oc, v, ctx$lang)
                    for (i in seq_len(nrow(cr)))
                        rows[[length(rows) + 1]] <- list(key = paste(d, v, cr$contrast[i]), design = d, variable = v, contrast = cr$contrast[i])
                }
            }
            rows
        },

        .init = function() {
            for (nm in c("openState", "csvState"))
                if (self$results[[nm]]$rowCount == 0)
                    self$results[[nm]]$addRow(rowKey = 1, values = list(fired = ""))
            ctx <- private$.context()
            if (!is.null(ctx$error)) {
                private$.notice(sprintf("<p><b>%s</b></p>", htmlEscape(ctx$error)))
                for (nm in c("studies", "estimates", "plot", "dag")) self$results[[nm]]$setVisible(FALSE)
                return()
            }
            if (length(ctx$factors) == 0 && length(ctx$sc$interventions) == 0)
                private$.notice(sprintf("<p>%s</p>", .("No variables measured yet: tick them under Measured.")))
            if (self$options$showDag) dag_init(self, ctx)
            for (d in ctx$designs) self$results$studies$addRow(rowKey = d, values = list(design = design_title(d, self)))
            rows <- private$.rows(ctx)
            for (r in rows) self$results$estimates$addRow(rowKey = r$key, values = list(
                design = design_title(r$design, self), variable = dname(r$variable, ctx$lang), contrast = r$contrast))
            # one panel per contrast, two columns
            np <- length(unique(vapply(rows, function(r) paste(r$variable, r$contrast), "")))
            self$results$plot$setSize(760, max(260, 60 + ceiling(np / 2) * (50 + 28 * length(ctx$designs))))
        },

        .run = function() {
            ctx <- private$.context()
            if (!is.null(ctx$error)) return()
            o <- self$options
            # the diagram does not depend on the design, so it is drawn even if a study fails
            if (o$showDag) dag_fill(self, ctx)
            if (length(ctx$skipped))
                private$.notice(sprintf("<p>%s</p>", htmlEscape(jmvcore::format(.("Skipped (they do not fit this outcome): {list}."),
                    list = paste(vapply(ctx$skipped, design_title, "", self), collapse = ", ")))))
            opts <- ctx$opts
            titles <- vapply(ctx$designs, design_title, "", self)
            res <- tryCatch(compare_designs(ctx$sc, ctx$oc$id, ctx$designs, o$n, ctx$factors, seed = o$seed,
                                            student_key = o$studentKey, options = opts, lang = ctx$lang,
                                            categorize = ctx$cats, expected = o$showExpected, titles = titles),
                            error = function(e) e)
            if (inherits(res, "error")) {
                private$.notice(sprintf("<p><b>%s</b></p>", htmlEscape(conditionMessage(res))))
                return()
            }
            private$.fillStudies(ctx, res)
            private$.fillEstimates(ctx, res)
            if (o$showPlot && !is.null(res$estimates) && is.null(self$results$plot$state))
                self$results$plot$setState(list(est = res$estimates, expected = res$expected, titles = titles, lang = ctx$lang))
            private$.actions(ctx, res)
        },

        .fillStudies = function(ctx, res) {
            num <- function(x) base::format(round(x), big.mark = ",", scientific = FALSE)
            for (d in names(res$runs)) {
                f <- res$runs[[d]]$facts
                det <- c(
                    if (!is.null(f$controls)) jmvcore::format(.("{cases} cases and {controls} controls"), cases = num(f$cases), controls = num(f$controls)),
                    if (!is.null(f$subcohort)) jmvcore::format(.("subcohort {sub} of a cohort of {n}; {cases} cases"), sub = num(f$subcohort), n = num(f$cohort), cases = num(f$cases)),
                    if (!is.null(f$matched_on)) jmvcore::format(.("matched on {vars}"), vars = paste(dname(f$matched_on, ctx$lang), collapse = ", ")),
                    if (!is.null(f$events)) person_time_text(f, ctx$oc$time_units, self, num),
                    if (!is.null(f$arms)) paste(sprintf("%s: %s", dlevel(names(f$arms), ctx$lang), unlist(f$arms)), collapse = ", "),
                    if (!is.null(f$regions)) jmvcore::format(.("{k} regions of {m} people"), k = num(f$regions), m = num(f$region_size)),
                    if (!is.null(f$sequences)) jmvcore::format(.("sequences: {a} and {b} participants"), a = f$sequences[1], b = f$sequences[2]))
                self$results$studies$setRow(rowKey = d, values = list(rows = nrow(res$runs[[d]]$data), detail = paste(det, collapse = "; ")))
            }
            self$results$studies$setNote("pop", .("All designs sample the same superpopulation (same seed and student key); each study has its own participants."))
        },

        .fillEstimates = function(ctx, res) {
            tbl <- self$results$estimates
            e <- res$estimates
            if (is.null(e)) return()
            x <- res$expected
            blank <- function(v) if (is.na(v) || !is.finite(v)) "" else v
            for (i in seq_len(nrow(e))) {
                k <- paste(e$design[i], e$variable[i], e$contrast[i])
                if (!k %in% tbl$rowKeys) next
                ex <- if (!is.null(x)) x$adjusted[x$design == e$design[i] & x$variable == e$variable[i] & x$contrast == e$contrast[i]] else numeric()
                tbl$setRow(rowKey = k, values = list(truth = blank(e$true[i]), crude = blank(e$crude[i]), adjusted = blank(e$adjusted[i]),
                                                     lower = blank(e$lower[i]), upper = blank(e$upper[i]), measure = e$measure[i],
                                                     expected = if (length(ex)) blank(ex[1]) else ""))
            }
            tbl$setNote("ci", .("Adjusted for all the measured variables (and the intervention); 95% confidence interval of the adjusted estimate."))
            if (!is.null(x)) { note <- mc_note(list(x), self); if (nzchar(note)) tbl$setNote("mc", note) }
            mm <- unique(e$true_measure[e$true_measure != e$measure & e$true_measure %in% c("HR", "OR", "RR")])
            if (length(mm))
                tbl$setNote("measure", jmvcore::format(.("True values are {true}s, the measure the data were generated with (a cohort followed over time); this design estimates {est}s, which approach them when the outcome is rare."),
                                                      true = paste(mm, collapse = "/"), est = paste(unique(e$measure), collapse = "/")))
        },

        .plotDag = function(image, ggtheme, theme, ...) dag_render(image),

        .plot = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            p <- compare_plot(st$est, st$expected, st$titles, st$lang) +
                ggplot2::labs(caption = paste(strwrap(.("Filled dot and bar: adjusted estimate and 95% CI; open red circle: crude estimate; cross: large study; dashed line: true value."),
                                                      width = 110), collapse = "\n"))
            print(p)
            TRUE
        },

        .once = function(stateName, optName, fun) {
            st <- self$results[[stateName]]
            if (!isTRUE(self$options[[optName]])) {
                st$setRow(rowNo = 1, values = list(fired = ""))
                return(invisible(FALSE))
            }
            if (isTRUE(st$getCell(rowNo = 1, col = "fired")$value == "yes")) return(invisible(FALSE))
            self$options$option(optName)$perform(fun)
            st$setRow(rowNo = 1, values = list(fired = "yes"))
            invisible(TRUE)
        },

        .actions = function(ctx, res) {
            private$.once("openState", "openData", function(action) {
                list(data = res$data, title = sprintf("%s - %s", tr_text(ctx$sc$label, ctx$lang), .("Compare designs")))
            })
            private$.once("csvState", "exportCsv", function(action) {
                utils::write.csv(res$data, action$params$fullPath, row.names = FALSE, na = "", fileEncoding = "UTF-8")
                list(filename = sprintf("%s_designs.csv", ctx$sc$id))
            })
        }
    )
)
