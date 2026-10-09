# Simulated study: orchestration only. The engine (engine_*.R) knows nothing
# about jamovi; this class resolves the library and options, builds the result
# tables' rows in .init() (so they restore across clicks), fills them in .run(),
# and hands the dataset to jamovi through the two Action buttons.

simStudyClass <- R6::R6Class(
    "simStudyClass",
    inherit = simStudyBase,
    private = list(
        .ctx = NULL,

        # jamovi's language, from a marker the module catalogs translate
        .lang = function() {
            l <- tryCatch(self$options$translate("studysim-language-code"), error = function(e) "")
            if (is.character(l) && length(l) == 1 && grepl("^[a-z]{2}$", l)) l else "en"
        },

        .designTitle = function(d) {
            switch(d, case_control = .("Case-control"), matched_cc = .("Matched case-control"),
                   cohort = .("Cohort"), cross_sectional = .("Cross-sectional"), rct = .("Randomised trial"),
                   crossover = .("2x2 crossover trial"), prognosis = .("Prognosis (patient cohort)"), d)
        },

        # library, scenario, outcome, design and measured variables, or an error
        .context = function() {
            if (!is.null(private$.ctx)) return(private$.ctx)
            o <- self$options
            ctx <- tryCatch({
                lib <- get_library(o$libContent, o$libName)
                sc <- use_i18n(load_scenario(lib, o$scenario))
                oid <- if (nzchar(o$outcome) && o$outcome %in% names(sc$outcomes)) o$outcome else names(sc$outcomes)[1]
                ok <- compatible_designs(sc, oid)
                if (!o$design %in% ok)
                    stop(jmvcore::format(.("The {design} design does not fit this outcome. Possible designs: {list}."),
                                 design = private$.designTitle(o$design),
                                 list = paste(vapply(ok, private$.designTitle, ""), collapse = ", ")), call. = FALSE)
                # the panel lists the variables by their shown names (any language)
                inc <- Filter(function(m) isTRUE(m$include), o$measured)
                fac <- ids_from_names(vapply(inc, function(m) as.character(m$var), ""), sc$order)
                # grouping of measured continuous variables (form != as is)
                cats <- list()
                for (m in inc) {
                    id <- ids_from_names(m$var, sc$order)
                    if (length(id) == 0 || is.null(m$form) || m$form == "continuous") next
                    if (!sc$variables[[id]]$type %in% c("continuous", "count")) next
                    spec <- list(form = m$form, cuts = m$cuts %||% "")
                    tryCatch(cat_groups(spec), error = function(e)
                        stop(jmvcore::format(.("Cut points of {var}: {msg}"), var = m$var, msg = conditionMessage(e)), call. = FALSE))
                    cats[[id]] <- spec
                }
                list(lib = lib, sc = sc, oc = sc$outcomes[[oid]], design = o$design,
                     factors = intersect(sc$order, fac), lang = private$.lang(), ok = ok, cats = cats)
            }, error = function(e) list(error = conditionMessage(e)))
            private$.ctx <- ctx
            ctx
        },

        .designOptions = function(ctx) {
            o <- self$options
            d <- ctx$design
            opt <- list()
            if (d %in% c("case_control", "matched_cc")) opt$ratio <- o$ccRatio
            if (d %in% c("cohort", "prognosis", "rct")) {
                if (o$followup > 0) opt$followup <- o$followup
                opt$loss <- o$loss / 100
            }
            if (d %in% c("cohort", "prognosis") && o$accrual > 0) opt$accrual <- o$accrual
            if (d == "crossover") {
                if (o$period != 0) opt$period <- o$period
                if (o$carryover != 0) opt$carryover <- o$carryover
            }
            opt
        },

        .notice = function(html) {
            self$results$notice$setContent(html)
            self$results$notice$setVisible(TRUE)
        },

        .init = function() {
            for (nm in c("openState", "csvState"))
                if (self$results[[nm]]$rowCount == 0)
                    self$results[[nm]]$addRow(rowKey = 1, values = list(fired = ""))
            ctx <- private$.context()
            if (!is.null(ctx$error)) {
                private$.notice(sprintf("<p><b>%s</b></p>", htmlEscape(ctx$error)))
                for (nm in c("summary", "dictionary", "keyEffects", "keyExpected", "keyNotes"))
                    self$results[[nm]]$setVisible(FALSE)
                return()
            }
            dic <- self$results$dictionary
            for (nm in planned_columns(ctx$sc, ctx$oc, ctx$design, ctx$factors))
                dic$addRow(rowKey = nm, values = list(name = dname(nm, ctx$lang)))
            if (self$options$showDescriptives) {
                for (r in describe_plan(planned_kinds(ctx$sc, ctx$oc, ctx$design, ctx$factors, ctx$lang, ctx$cats), ctx$lang))
                    self$results$descriptives$addRow(rowKey = r$key, values = list(variable = r$variable, level = r$level))
            }
            if (self$options$showDag) private$.initDag(ctx)
            if (self$options$showKey) {
                te <- true_effects(ctx$sc, ctx$oc, ctx$lang)
                for (i in seq_len(nrow(te))) self$results$keyEffects$addRow(rowKey = i, values = list(
                    variable = dname(te$variable[i], ctx$lang), contrast = te$contrast[i]))
                for (r in private$.expectedRows(ctx))
                    self$results$keyExpected$addRow(rowKey = r$key, values = list(variable = dname(r$variable, ctx$lang), contrast = r$contrast))
            }
        },

        .expectedRows = function(ctx) {
            iv <- intersect(names(ctx$sc$interventions), planned_columns(ctx$sc, ctx$oc, ctx$design, ctx$factors))
            main <- if (length(iv)) iv else ctx$factors
            rows <- list()
            for (v in main) {
                cr <- if (!is.null(ctx$cats[[v]])) cat_rows(v, ctx$cats[[v]], ctx$lang) else contrast_rows(ctx$sc, ctx$oc, v, ctx$lang)
                for (i in seq_len(nrow(cr)))
                    rows[[length(rows) + 1]] <- list(key = paste(v, cr$contrast[i]), variable = v, contrast = cr$contrast[i])
            }
            rows
        },

        .run = function() {
            ctx <- private$.context()
            if (!is.null(ctx$error)) return()
            o <- self$options
            opt <- utils::modifyList(design_defaults(ctx$design, ctx$oc, ctx$sc), private$.designOptions(ctx))
            # The study is drawn again only when something built from the data
            # was cleared (an option it depends on changed) or a button asks
            # for the data; showing the key, the DAG or a table that is already
            # filled reuses what is on screen.
            filled <- function(tbl, col) tbl$rowCount == 0 || tbl$isFilled(col = col)
            needData <- self$results$summary$isNotFilled() ||
                (o$showDictionary && !filled(self$results$dictionary, "type")) ||
                (o$showDescriptives && !filled(self$results$descriptives, "stat")) ||
                private$.wants("openState", "openData") || private$.wants("csvState", "exportCsv")
            if (needData) {
                res <- tryCatch(simulate_study(ctx$sc, ctx$oc$id, ctx$design, o$n, ctx$factors, seed = o$seed,
                                               student_key = o$studentKey, options = private$.designOptions(ctx),
                                               missing = o$missing / 100, missing_mech = o$missingMech, lang = ctx$lang,
                                               key = FALSE, categorize = ctx$cats),
                                error = function(e) e)
                if (inherits(res, "error")) {
                    private$.notice(sprintf("<p><b>%s</b></p>", htmlEscape(conditionMessage(res))))
                    return()
                }
                opt <- res$options
                private$.fillSummary(ctx, res)
                private$.fillDictionary(res)
                if (o$showDescriptives) {
                    tbl <- self$results$descriptives
                    for (r in describe_data(res$data_raw, planned_kinds(ctx$sc, ctx$oc, ctx$design, ctx$factors, ctx$lang, ctx$cats), ctx$lang))
                        if (r$key %in% tbl$rowKeys)
                            tbl$setRow(rowKey = r$key, values = list(level = dlevel(r$level, ctx$lang), stat = r$stat, missing = r$missing))
                }
                private$.actions(ctx, res)
            } else {
                # buttons not pressed: keep their guards in step
                private$.wants("openState", "openData"); private$.wants("csvState", "exportCsv")
            }
            if (o$showDag) private$.fillDag(ctx)
            if (o$showKey) private$.fillKey(ctx, calibrate(ctx$sc, ctx$oc), opt)
        },

        # TRUE when a button was pressed and has not been served yet; resets
        # the guard when the button is off
        .wants = function(stateName, optName) {
            st <- self$results[[stateName]]
            if (!isTRUE(self$options[[optName]])) {
                st$setRow(rowNo = 1, values = list(fired = ""))
                return(FALSE)
            }
            !isTRUE(st$getCell(rowNo = 1, col = "fired")$value == "yes")
        },

        .fillSummary = function(ctx, res) {
            sc <- ctx$sc; l <- ctx$lang; f <- res$facts
            num <- function(x, d = 0) base::format(round(x, d), big.mark = ",", nsmall = 0, scientific = FALSE)
            li <- c(
                sprintf("<b>%s</b>: %s. %s", .("Scenario"), htmlEscape(tr_text(sc$label, l)), htmlEscape(tr_text(sc$population, l))),
                sprintf("<b>%s</b>: %s", .("Outcome"), htmlEscape(tr_text(ctx$oc$label, l))),
                sprintf("<b>%s</b>: %s, %s", .("Design"), private$.designTitle(ctx$design),
                        jmvcore::format(.("{n} rows"), n = num(nrow(res$data)))))
            if (!is.null(f$cases)) li <- c(li, jmvcore::format(.("{cases} cases and {controls} controls"), cases = num(f$cases), controls = num(f$controls)))
            if (!is.null(f$source_population)) li <- c(li, jmvcore::format(.("cases found among {n} people of the source population (risk {risk}%)"),
                                                                  n = num(f$source_population), risk = num(100 * f$risk_in_source, 2)))
            if (!is.null(f$matched_on)) li <- c(li, jmvcore::format(.("matched on {vars}"), vars = paste(dname(f$matched_on, l), collapse = ", ")))
            if (isTRUE(f$incomplete_sets > 0)) li <- c(li, jmvcore::format(.("{n} cases have fewer controls than asked"), n = f$incomplete_sets))
            if (!is.null(f$events)) li <- c(li, jmvcore::format(.("{events} events in {pt} person-years"), events = num(f$events), pt = num(f$person_time)))
            if (!is.null(f$arms)) li <- c(li, paste(sprintf("%s: %s", dlevel(names(f$arms), l), unlist(f$arms)), collapse = ", "))
            if (!is.null(f$sequences)) li <- c(li, jmvcore::format(.("sequences: {a} and {b} participants"), a = f$sequences[1], b = f$sequences[2]))
            um <- setdiff(names(ctx$oc$effects), c(ctx$factors, names(sc$interventions)))
            if (length(um)) li <- c(li, jmvcore::format(.("not measured (they still act): {vars}"), vars = paste(dname(um, l), collapse = ", ")))
            seedtxt <- if (nzchar(trimws(self$options$studentKey))) jmvcore::format(.("seed {seed} with student key \"{key}\""),
                         seed = self$options$seed, key = htmlEscape(trimws(self$options$studentKey))) else jmvcore::format(.("seed {seed}"), seed = self$options$seed)
            li <- c(li, seedtxt)
            if (length(ctx$factors) == 0)
                li <- c(li, paste0("<b>", .("No variables measured yet: tick them under Measured."), "</b>"))
            self$results$summary$setContent(paste0(
                "<ul>", paste0("<li>", li, "</li>", collapse = ""), "</ul>",
                "<p>", .("Press <b>Open dataset</b> to open the data in a new window, or <b>Export CSV</b> to save them."), "</p>"))
        },

        .fillDictionary = function(res) {
            dic <- self$results$dictionary
            d <- res$dictionary
            for (i in seq_len(nrow(d))) {
                if (!d$id[i] %in% dic$rowKeys) next
                type <- switch(d$type[i], integer = .("integer"), continuous = .("continuous"),
                               nominal = .("nominal"), ordinal = .("ordinal"), d$type[i])
                dic$setRow(rowKey = d$id[i], values = list(name = d$name[i], description = d$description[i], type = type,
                                                            values = d$values[i], missing = d$missing[i]))
            }
        },

        .fillKey = function(ctx, cal, opt) {
            te <- true_effects(ctx$sc, ctx$oc, ctx$lang)
            for (i in seq_len(nrow(te))) self$results$keyEffects$setRow(rowKey = i, values = list(
                measure = te$measure[i], value = te$value[i], note = te$note[i]))
            tbl <- self$results$keyExpected
            ex <- tbl$state
            if (is.null(ex)) {
                ex <- expected_estimates(ctx$sc, ctx$oc, ctx$design, opt, ctx$factors, ctx$lang, ctx$cats)
                tbl$setState(ex)
            }
            if (!is.null(ex)) {
                for (i in seq_len(nrow(ex))) {
                    k <- paste(ex$variable[i], ex$contrast[i])
                    if (!k %in% tbl$rowKeys) next
                    # no single true value (categorised or modified effect), or a
                    # coefficient the model dropped: an empty cell, not NaN
                    blank <- function(x) if (is.na(x)) "" else x
                    tbl$setRow(rowKey = k, values = list(truth = blank(ex$true[i]), crude = blank(ex$crude[i]), adjusted = blank(ex$adjusted[i]),
                                                         measure = ex$measure[i], note = ex$note[i]))
                }
                tbl$setNote("n", jmvcore::format(.("Expected values: crude and adjusted estimates from one simulated study of {n} rows with this design; adjusted for {vars}."),
                                         n = base::format(attr(ex, "n"), big.mark = ","), vars = paste(dname(attr(ex, "adjusted_for"), ctx$lang), collapse = ", ")))
                mm <- unique(ex$true_measure[ex$true_measure != ex$measure & ex$true_measure %in% c("HR", "OR", "RR")])
                if (length(mm))
                    tbl$setNote("measure", jmvcore::format(.("True values are {true}s, the measure the data were generated with (a cohort followed over time); this design estimates {est}s, which approach them when the outcome is rare."),
                                                          true = paste(mm, collapse = "/"), est = ex$measure[1]))
            }
            l <- ctx$lang
            calib <- switch(ctx$oc$type,
                binary = jmvcore::format(.("intercept {a} gives a prevalence of {p}"), a = round(cal$a, 4), p = round(cal$achieved, 4)),
                continuous = jmvcore::format(.("intercept {a}"), a = round(cal$a, 4)),
                count = jmvcore::format(.("log intercept {a}"), a = round(cal$a, 4)),
                event = jmvcore::format(.("baseline Weibull scale {l}, shape {s}: average hazard {r}, risk {q} over {t} years"),
                                l = signif(cal$lambda, 4), s = cal$shape, r = signif(cal$achieved$rate, 3),
                                q = round(cal$achieved$risk_followup, 4), t = ctx$oc$followup))
            ed <- ctx$sc$edges
            st <- if (nrow(ed)) paste(sprintf("%s → %s (%s)", ed$from, ed$to, ed$b), collapse = "; ") else "-"
            html <- paste0(
                "<p><b>", .("Teaching note"), "</b>: ", htmlEscape(tr_text(ctx$sc$teaching, l)), "</p>",
                "<p><b>", .("Calibration"), "</b>: ", calib, "</p>",
                "<p><b>", .("Structure among variables (latent paths)"), "</b>: ", htmlEscape(st), "</p>",
                if (length(ctx$sc$references)) paste0("<p><b>", .("Sources of the parameters"), "</b></p><ul>",
                    paste0("<li>", htmlEscape(ctx$sc$references), "</li>", collapse = ""), "</ul>") else "",
                "<p>", .("Effects are teaching-grade plausible values informed by the literature, not meta-analytic estimates."), "</p>")
            self$results$keyNotes$setContent(html)
        },

        # image sized to the number of layers and the most crowded one
        .initDag = function(ctx) {
            img <- self$results$dag
            g <- dag_layout(ctx$sc, ctx$oc, ctx$factors, ctx$lang)
            nl <- max(g$nodes$layer) + 1
            nk <- max(table(g$nodes$layer))
            w <- min(1200, max(600, 210 * nl))
            h <- min(1000, max(380, 58 * nk + 110))
            img$setSize(w, h)
        },

        # the state is set in .run(): jamovi restores the previous (empty)
        # state after .init(), which would wipe one set there
        .fillDag = function(ctx) {
            img <- self$results$dag
            if (is.null(img$state)) {
                g <- dag_layout(ctx$sc, ctx$oc, ctx$factors, ctx$lang)
                w <- img$width; h <- img$height
                key <- isTRUE(self$options$dagValues)
                cap <- .("Solid box: measured; dashed: not measured; grey arrows: paths among variables; dotted: correlations; dark arrows: effects on the outcome.")
                if (key) cap <- paste(cap, .("Red: raises the outcome, blue: lowers it; * the effect depends on another variable."))
                img$setState(list(g = g, w = w, h = h, key = key, caption = cap))
            }
        },

        .plotDag = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            print(dag_plot(st$g, st$w, st$h, st$key, st$caption))
            TRUE
        },

        # Action buttons: each fires once per click (hidden-cell guard)
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
            name <- sprintf("%s_%s", ctx$sc$id, ctx$design)
            private$.once("openState", "openData", function(action) {
                list(data = res$data, title = sprintf("%s - %s", tr_text(ctx$sc$label, ctx$lang), private$.designTitle(ctx$design)))
            })
            private$.once("csvState", "exportCsv", function(action) {
                utils::write.csv(res$data, action$params$fullPath, row.names = FALSE, na = "", fileEncoding = "UTF-8")
                list(filename = paste0(name, ".csv"))
            })
        }
    )
)

htmlEscape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
}
