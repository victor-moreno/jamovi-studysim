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

        .designTitle = function(d) design_title(d, self),

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
                m <- parse_measured(o$measured, sc, self)
                fac <- m$factors; cats <- m$cats
                ctx <- list(lib = lib, sc = sc, oc = sc$outcomes[[oid]], design = o$design,
                            factors = intersect(sc$order, fac), lang = private$.lang(), ok = ok, cats = cats)
                ctx$opt <- private$.designOptions(ctx)   # a mistyped number stops here, as a notice
                ctx
            }, error = function(e) list(error = conditionMessage(e)))
            private$.ctx <- ctx
            ctx
        },

        .designOptions = function(ctx) {
            o <- self$options
            d <- ctx$design
            # empty text boxes keep the scenario's (or the design's) value
            num <- function(nm, title, lo, hi, int = FALSE, scale = 1) num_opt(self, nm, title, lo, hi, int, scale)
            opt <- list()
            if (d %in% CASE_DESIGNS) opt$ratio <- num("ccRatio", .("Controls per case"), 1, 10, TRUE)
            opt$secondary <- isTRUE(o$secondary)
            if (d %in% c("cohort", "prognosis", "rct", "nested_cc", "case_cohort", "cluster_rct", "factorial")) {
                opt$followup <- num("followup", .("Follow-up"), 0.01, 100)
                opt$loss <- num("loss", .("Loss to follow-up (% per time unit)"), 0, 50, scale = 100)
            }
            if (d %in% c("cohort", "prognosis", "nested_cc", "case_cohort")) opt$accrual <- num("accrual", .("Recruitment period"), 0, 100)
            # realism that also shapes the answer key (measurement error) or the columns (noise)
            opt$reliability <- o$reliability; opt$misclass <- o$misclass / 100; opt$noise <- o$noiseVars
            opt$lod_handling <- o$lodHandling
            if (d == "diagnostic") { opt$batches <- o$batches; opt$batch_cv <- o$batchCv / 100; opt$batch_confounded <- o$batchConfounded }
            if (d == "case_cohort") opt$subcohort <- num("subcohort", .("Subcohort (% of the cohort)"), 0.1, 90, scale = 100)
            if (d == "rct") opt$looks <- o$looks
            if (d == "cluster_rct") { opt$clusters <- num("clusters", .("Clusters"), 4, 1000, TRUE); opt$icc <- num("icc", .("Intraclass correlation"), 0, 0.9) }
            if (d == "factorial") opt$interaction <- num("interaction", .("Interaction of the two interventions"), -10, 10)
            if (d == "longitudinal") { opt$visits <- num("visits", .("Follow-up visits"), 1, 12, TRUE)
                                       opt$dropout <- num("dropout", .("Dropout by the last visit (%)"), 0, 80, scale = 100) }
            if (d == "pre_post") opt$threshold <- num("threshold", .("Enrol only above percentile"), 0, 95)
            if (d == "method_comparison") {
                opt$bias <- num("mcBias", .("Bias of method B"), -Inf, Inf)
                opt$slope <- num("mcSlope", .("Proportional bias of method B (slope)"), 0.2, 5)
                opt$error_a <- num("mcErrorA", .("Error of method A (% of SD)"), 0, 100, scale = 100)
                opt$error_b <- num("mcErrorB", .("Error of method B (% of SD)"), 0, 100, scale = 100)
            }
            if (d == "ecological") { opt$regions <- num("regions", .("Regions"), 3, 500, TRUE)
                                     opt$region_size <- num("regionSize", .("People per region"), 10, 100000, TRUE)
                                     opt$region_sd <- num("regionSd", .("Differences between regions (exposures)"), 0, 3)
                                     opt$context_sd <- num("contextSd", .("Differences between regions (outcome)"), 0, 3) }
            if (d == "crossover") {
                opt$period <- num("period", .("Period effect"), -Inf, Inf)
                opt$carryover <- num("carryover", .("Carryover effect"), -Inf, Inf)
            }
            Filter(Negate(is.null), opt)
        },

        .notice = function(html) {
            self$results$notice$setContent(html)
            self$results$notice$setVisible(TRUE)
        },

        .init = function() {
            for (nm in c("openState", "csvState", "keyState"))
                if (self$results[[nm]]$rowCount == 0)
                    self$results[[nm]]$addRow(rowKey = 1, values = list(fired = ""))
            ctx <- private$.context()
            if (!is.null(ctx$error)) {
                private$.notice(sprintf("<p><b>%s</b></p>", htmlEscape(ctx$error)))
                for (nm in c("summary", "dictionary", "keyEffects", "keyExpected", "keyMarkers", "keyNotes"))
                    self$results[[nm]]$setVisible(FALSE)
                return()
            }
            dic <- self$results$dictionary
            for (nm in planned_columns(ctx$sc, ctx$oc, ctx$design, ctx$factors, ctx$opt))
                dic$addRow(rowKey = nm, values = list(name = dname(nm, ctx$lang)))
            if (self$options$showDescriptives) {
                for (r in describe_plan(planned_kinds(ctx$sc, ctx$oc, ctx$design, ctx$factors, ctx$lang, ctx$cats, ctx$opt), ctx$lang))
                    self$results$descriptives$addRow(rowKey = r$key, values = list(variable = r$variable, level = r$level))
            }
            if (self$options$showDag) private$.initDag(ctx)
            if (self$options$showKey) {
                # one block of rows per outcome (the primary's keys have no prefix)
                for (o in private$.keyOutcomes(ctx)) {
                    oc2 <- ctx$sc$outcomes[[o]]; pre <- if (o == ctx$oc$id) "" else paste0(o, "|")
                    olab <- tr_text(oc2$label, ctx$lang)
                    te <- true_effects(ctx$sc, oc2, ctx$lang)
                    for (i in seq_len(nrow(te))) self$results$keyEffects$addRow(rowKey = paste0(pre, i), values = list(
                        outcome = olab, variable = dname(te$variable[i], ctx$lang), contrast = te$contrast[i]))
                    for (r in private$.expectedRows(ctx, oc2, pre))
                        self$results$keyExpected$addRow(rowKey = r$key, values = list(outcome = olab, variable = dname(r$variable, ctx$lang), contrast = r$contrast))
                }
                if (ctx$design == "diagnostic") {
                    for (m in ctx$oc$markers) {
                        cols <- marker_cols(m)
                        parts <- if (is.null(m$block)) list(c("", 1)) else
                            Filter(Negate(is.null), list(if (m$informative > 0) c("inf", m$informative), if (m$informative < m$block) c("noise", m$block - m$informative)))
                        for (pp in parts) self$results$keyMarkers$addRow(rowKey = paste(m$id, pp[1]), values = list(
                            marker = tr_text(m$label, ctx$lang), nm = as.integer(pp[2]),
                            part = if (pp[1] == "inf") sprintf("%s-%s", cols[1], cols[m$informative])
                                   else if (pp[1] == "noise") sprintf("%s-%s", cols[m$informative + 1], cols[m$block]) else cols[1]))
                    }
                } else self$results$keyMarkers$setVisible(FALSE)
            }
        },

        # the primary outcome, and the others when the study carries them
        .keyOutcomes = function(ctx) {
            if (!isTRUE(self$options$secondary)) return(ctx$oc$id)
            cols <- secondary_columns(ctx$sc, ctx$oc, ctx$design)
            c(ctx$oc$id, intersect(setdiff(names(ctx$sc$outcomes), ctx$oc$id), cols))
        },

        .expectedRows = function(ctx, oc = ctx$oc, pre = "") {
            if (pre != "") {
                # a secondary outcome: its contrasts for the intervention or the factors
                iv <- intersect(names(ctx$sc$interventions), planned_columns(ctx$sc, ctx$oc, ctx$design, ctx$factors))
                rows <- list()
                for (v in if (length(iv)) iv else ctx$factors) {
                    cr <- if (!is.null(ctx$cats[[v]])) cat_rows(v, ctx$cats[[v]], ctx$lang, ctx$sc) else contrast_rows(ctx$sc, oc, v, ctx$lang)
                    for (i in seq_len(nrow(cr))) rows[[length(rows) + 1]] <- list(key = paste0(pre, paste(v, cr$contrast[i])), variable = v, contrast = cr$contrast[i])
                }
                return(rows)
            }
            sp <- special_rows(ctx$design, ctx$sc, ctx$oc, ctx$lang)
            if (!is.null(sp)) return(lapply(seq_len(nrow(sp)), function(i)
                list(key = paste(sp$variable[i], sp$contrast[i]), variable = sp$variable[i], contrast = sp$contrast[i])))
            iv <- intersect(names(ctx$sc$interventions), planned_columns(ctx$sc, ctx$oc, ctx$design, ctx$factors))
            main <- if (length(iv)) iv else ctx$factors
            rows <- list()
            for (v in main) {
                cr <- if (!is.null(ctx$cats[[v]])) cat_rows(v, ctx$cats[[v]], ctx$lang, ctx$sc) else contrast_rows(ctx$sc, ctx$oc, v, ctx$lang)
                for (i in seq_len(nrow(cr)))
                    rows[[length(rows) + 1]] <- list(key = paste(v, cr$contrast[i]), variable = v, contrast = cr$contrast[i])
            }
            rows
        },

        .run = function() {
            ctx <- private$.context()
            if (!is.null(ctx$error)) return()
            o <- self$options
            opt <- utils::modifyList(design_defaults(ctx$design, ctx$oc, ctx$sc), ctx$opt)
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
                                               student_key = o$studentKey, options = ctx$opt,
                                               missing = o$missing / 100, missing_mech = o$missingMech, lang = ctx$lang,
                                               key = FALSE, categorize = ctx$cats, noise = o$noiseVars, entry = o$entryErrors / 100),
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
                    for (r in describe_data(res$data_raw, planned_kinds(ctx$sc, ctx$oc, ctx$design, ctx$factors, ctx$lang, ctx$cats, ctx$opt), ctx$lang))
                        if (r$key %in% tbl$rowKeys)
                            tbl$setRow(rowKey = r$key, values = list(level = dlevel(r$level, ctx$lang), stat = r$stat, missing = r$missing))
                }
                private$.actions(ctx, res)
            } else {
                # buttons not pressed: keep their guards in step
                private$.wants("openState", "openData"); private$.wants("csvState", "exportCsv")
            }
            if (o$showDag) private$.fillDag(ctx)
            # a key that cannot be computed explains itself instead of showing R's message
            if (o$showKey) tryCatch(private$.fillKey(ctx, calibrate(ctx$sc, ctx$oc), opt), error = function(e)
                private$.notice(sprintf("<p><b>%s</b> %s</p>", .("The answer key could not be computed for these options:"),
                                        htmlEscape(conditionMessage(e)))))
            private$.once("keyState", "exportKey", function(action) {
                writeLines(private$.keyReport(ctx, opt), action$params$fullPath, useBytes = TRUE)
                list(filename = sprintf("%s_%s_answer_key.html", ctx$sc$id, ctx$design))
            })
        },

        # Teacher's answer key as one self-contained HTML page: study, true
        # model, what the design should estimate, markers, diagram, notes
        .keyReport = function(ctx, opt) {
            l <- ctx$lang; sc <- ctx$sc; oc <- ctx$oc
            esc <- htmlEscape
            num <- function(x) ifelse(is.na(x), "", formatC(signif(x, 3), format = "fg", digits = 3))
            tab <- function(head, rows) paste0("<table><tr>", paste0("<th>", esc(head), "</th>", collapse = ""), "</tr>",
                paste0(apply(rows, 1, function(r) paste0("<tr>", paste0("<td>", esc(r), "</td>", collapse = ""), "</tr>")), collapse = ""), "</table>")
            te <- true_effects(sc, oc, l)
            ex <- self$results$keyExpected$state %||% expected_all(sc, oc, ctx$design, opt, ctx$factors, l, ctx$cats)
            if (!is.data.frame(ex)) ex <- do.call(rbind, lapply(names(ex), function(o) { e <- ex[[o]]
                if (length(ex) > 1) e$contrast <- sprintf("%s: %s", tr_text(sc$outcomes[[o]]$label, l), e$contrast); e }))
            g <- dag_layout(sc, oc, ctx$factors, l)
            sz <- dag_size(g, "")
            png_file <- tempfile(fileext = ".png")
            grDevices::png(png_file, sz[["w"]] * 2, sz[["h"]] * 2, res = 144)
            print(dag_plot(g, sz[["w"]], sz[["h"]], TRUE, ""))
            grDevices::dev.off()
            img <- jsonlite::base64_enc(readBin(png_file, "raw", file.size(png_file)))
            unlink(png_file)
            o <- self$options
            facts <- c(sprintf("%s: %s", .("Scenario"), tr_text(sc$label, l)), sprintf("%s: %s", .("Outcome"), tr_text(oc$label, l)),
                       sprintf("%s: %s", .("Design"), design_title(ctx$design, self)), sprintf("%s: %s", .("Sample size"), o$n),
                       sprintf("%s: %s%s", .("Seed"), o$seed, if (nzchar(trimws(o$studentKey))) paste0(" / ", trimws(o$studentKey)) else ""),
                       sprintf("%s: %s", .("Variables"), paste(dname(ctx$factors, l), collapse = ", ")))
            html <- c("<!doctype html><html><head><meta charset='utf-8'><title>", esc(.("Answer key")), "</title><style>",
                      "body{font:14px/1.45 system-ui,sans-serif;max-width:960px;margin:24px auto;padding:0 16px;color:#222}",
                      "table{border-collapse:collapse;margin:8px 0 18px}th,td{border-bottom:1px solid #ddd;padding:4px 10px;text-align:left}",
                      "th{background:#f3f3f3}h1{font-size:22px}h2{font-size:17px;margin-top:26px}img{max-width:100%}</style></head><body>",
                      "<h1>", esc(.("Answer key")), ": ", esc(tr_text(sc$label, l)), "</h1><ul>", paste0("<li>", esc(facts), "</li>", collapse = ""), "</ul>",
                      "<h2>", esc(.("Answer key: true model")), "</h2>",
                      tab(c(.("Variable"), .("Contrast"), .("Measure"), .("True value"), .("Note")),
                          cbind(dname(te$variable, l), te$contrast, te$measure, num(te$value), te$note)))
            if (!is.null(ex) && nrow(ex)) html <- c(html, "<h2>", esc(.("Answer key: what this design should estimate")), "</h2>",
                tab(c(.("Variable"), .("Contrast"), .("True"), .("Crude"), .("Adjusted"), .("Estimate"), .("Note")),
                    cbind(dname(ex$variable, l), ex$contrast, num(ex$true), num(ex$crude), num(ex$adjusted), ex$measure, ex$note)))
            mc <- mc_note(list(ex), self)
            if (nzchar(mc)) html <- c(html, "<p>", esc(mc), "</p>")
            if (ctx$design == "diagnostic") {
                mt <- self$results$keyMarkers$state %||% expected_markers(sc, oc, opt)
                html <- c(html, "<h2>", esc(.("Answer key: markers")), "</h2>",
                          tab(c(.("Marker"), .("Columns"), .("True AUC"), .("Large study")), cbind(mt$marker, mt$part, num(mt$true), num(mt$auc))))
            }
            html <- c(html, "<h2>", esc(.("Causal diagram")), "</h2><img alt='DAG' src='data:image/png;base64,", img, "'>",
                      "<h2>", esc(.("Teaching note")), "</h2><p>", esc(tr_text(sc$teaching, l)), "</p>",
                      if (length(sc$references)) c("<h2>", esc(.("Sources of the parameters")), "</h2><ul>",
                                                   paste0("<li>", esc(sc$references), "</li>", collapse = ""), "</ul>"),
                      "</body></html>")
            paste(html, collapse = "")
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
            dot <- design_options_text(ctx$design, res$options, ctx$oc, self, l)
            if (nzchar(dot)) li <- c(li, dot)
            if (!is.null(f$controls)) li <- c(li, jmvcore::format(.("{cases} cases and {controls} controls"), cases = num(f$cases), controls = num(f$controls)))
            if (!is.null(f$source_population)) li <- c(li, jmvcore::format(.("cases found among {n} people of the source population (risk {risk}%)"),
                                                                  n = num(f$source_population), risk = num(100 * f$risk_in_source, 2)))
            if (!is.null(f$subcohort)) li <- c(li, jmvcore::format(.("a random subcohort of {sub} people from a cohort of {n}, plus every case: {cases} cases, {insub} of them in the subcohort"),
                                                          sub = num(f$subcohort), n = num(f$cohort), cases = num(f$cases), insub = num(f$cases_in_subcohort)))
            if (ctx$design == "nested_cc") li <- c(li, .("controls drawn from each case's risk set: a person can be a control more than once, and a control can later be a case"))
            if (!is.null(f$matched_on)) li <- c(li, jmvcore::format(.("matched on {vars}"), vars = paste(dname(f$matched_on, l), collapse = ", ")))
            if (isTRUE(f$incomplete_sets > 0)) li <- c(li, jmvcore::format(.("{n} cases have fewer controls than asked"), n = f$incomplete_sets))
            if (!is.null(f$events)) li <- c(li, person_time_text(f, ctx$oc$time_units, self, num))
            if (!is.null(f$competing)) li <- c(li, jmvcore::format(.("{n} competing events ({label}), status 2"), n = num(f$competing),
                                                          label = htmlEscape(tr_text(ctx$oc$competing$label, l))))
            if (!is.null(f$arms)) li <- c(li, paste(sprintf("%s: %s", dlevel(names(f$arms), l), unlist(f$arms)), collapse = ", "))
            if (!is.null(f$clusters)) li <- c(li, jmvcore::format(.("{k} clusters randomised ({by} by arm), intraclass correlation {icc}"),
                                                         k = f$clusters[1], by = paste(f$clusters[-1], collapse = "/"), icc = f$icc))
            if (!is.null(f$factorial)) li <- c(li, jmvcore::format(.("2x2 factorial: {a} and {b} randomised independently; {cells} participants per combination"),
                                                          a = dname(f$factorial[1], l), b = dname(f$factorial[2], l), cells = paste(f$cells, collapse = "/")))
            if (!is.null(f$markers)) li <- c(li, jmvcore::format(.("prevalence {p}%; markers measured on everybody: {list}"), p = num(100 * f$prevalence, 1),
                                                        list = htmlEscape(paste(vapply(ctx$oc$markers, function(m) {
                                                            cc <- marker_cols(m); if (length(cc) > 1) sprintf("%s-%s", cc[1], cc[length(cc)]) else cc }, ""), collapse = ", "))))
            if (!is.null(f$regions)) li <- c(li, jmvcore::format(.("{k} regions of {m} people (the sample size is not used): only totals, means and percentages per region are kept"),
                                                        k = num(f$regions), m = num(f$region_size)))
            if (ctx$design == "pre_post") li <- c(li, if (!is.null(f$threshold))
                jmvcore::format(.("everybody treated; only people above the {p}th percentile before were enrolled ({n} screened)"), p = f$threshold, n = num(f$screened))
                else .("everybody treated, measured before and after; no control group"))
            if (ctx$design == "method_comparison") li <- c(li, .("the outcome measured twice in each person, by methods A and B"))
            if (isTRUE(self$options$secondary) && length(res$secondary))
                li <- c(li, jmvcore::format(.("other outcomes of the same people: {list}"), list = paste(vapply(res$secondary, function(x) tr_text(sc$outcomes[[x]]$label, l), ""), collapse = "; ")))
            if (!is.null(f$attending)) li <- c(li, jmvcore::format(.("baseline and {k} follow-up visits; participants seen at each visit: {n}"),
                                                          k = f$visits, n = paste(f$attending, collapse = ", ")))
            if (!is.null(f$looks)) li <- c(li, jmvcore::format(.("{k} interim looks after {ns} participants; O'Brien-Fleming boundaries |z| ≥ {b}"),
                                                      k = length(f$looks$n), ns = paste(cumsum(f$looks$n), collapse = ", "),
                                                      b = paste(sprintf("%.2f", f$looks$bounds), collapse = ", ")))
            if (!is.null(f$sequences)) li <- c(li, jmvcore::format(.("sequences: {a} and {b} participants"), a = f$sequences[1], b = f$sequences[2]))
            o <- self$options
            if (o$reliability < 1 || o$misclass > 0)
                li <- c(li, jmvcore::format(.("measurement error: reliability {r} for numeric variables, {m}% of categories misclassified"),
                                            r = o$reliability, m = o$misclass))
            if (ctx$design == "diagnostic" && o$batches >= 2)
                li <- c(li, jmvcore::format(.("markers assayed in {k} batches ({cv}% variation between batches){conf}"), k = o$batches, cv = o$batchCv,
                                            conf = if (o$batchConfounded) .(", cases and controls in separate batches") else ""))
            if (isTRUE(f$noise > 0)) li <- c(li, jmvcore::format(.("{k} extra variables (extra_01, …) with no effect"), k = f$noise))
            if (isTRUE(f$entry_errors > 0)) li <- c(li, jmvcore::format(.("{n} data-entry errors to find and correct"), n = f$entry_errors))
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
            for (o in private$.keyOutcomes(ctx)) {
                pre <- if (o == ctx$oc$id) "" else paste0(o, "|")
                te <- true_effects(ctx$sc, ctx$sc$outcomes[[o]], ctx$lang)
                for (i in seq_len(nrow(te))) if (paste0(pre, i) %in% self$results$keyEffects$rowKeys)
                    self$results$keyEffects$setRow(rowKey = paste0(pre, i), values = list(measure = te$measure[i], value = te$value[i], note = te$note[i]))
            }
            tbl <- self$results$keyExpected
            exl <- tbl$state
            if (is.data.frame(exl)) exl <- stats::setNames(list(exl), ctx$oc$id)   # a state saved by v0.3
            if (is.null(exl)) {
                exl <- expected_all(ctx$sc, ctx$oc, ctx$design, opt, ctx$factors, ctx$lang, ctx$cats)
                tbl$setState(exl)
            }
            ex <- exl[[ctx$oc$id]]
            blank <- function(x) if (is.na(x)) "" else x
            for (o in names(exl)) {
                e <- exl[[o]]; pre <- if (o == ctx$oc$id) "" else paste0(o, "|")
                for (i in seq_len(NROW(e))) {
                    k <- paste0(pre, paste(e$variable[i], e$contrast[i]))
                    if (!k %in% tbl$rowKeys) next
                    # no single true value (categorised or modified effect), or a
                    # coefficient the model dropped: an empty cell, not NaN
                    tbl$setRow(rowKey = k, values = list(truth = blank(e$true[i]), crude = blank(e$crude[i]), adjusted = blank(e$adjusted[i]),
                                                         measure = e$measure[i], note = e$note[i]))
                }
            }
            if (!is.null(ex)) {
                tbl$setNote("n", jmvcore::format(.("Expected values: crude and adjusted estimates from one simulated study of {n} rows with this design; adjusted for {vars}."),
                                         n = base::format(attr(ex, "n"), big.mark = ","), vars = paste(dname(attr(ex, "adjusted_for"), ctx$lang), collapse = ", ")))
                note <- mc_note(exl, self)
                if (nzchar(note)) tbl$setNote("mc", note)
                if (ctx$design == "factorial" && opt$interaction != 0)
                    tbl$setNote("interaction", .("With an interaction, a model of the two main effects estimates each intervention's effect averaged over the other; the true values hold when the other intervention is at its reference level."))
                if (!is.null(ctx$oc$competing) && ctx$design %in% c("cohort", "prognosis", "rct", "cluster_rct", "factorial"))
                    tbl$setNote("competing", .("Status 2 is a competing event: a Cox model that censors it estimates the true cause-specific hazard ratios; 1 - Kaplan-Meier then overstates the cumulative incidence of the event (use the cumulative incidence function)."))
                if (opt$reliability < 1 || opt$misclass > 0)
                    tbl$setNote("error", .("The measured variables have measurement error, also in this large study: the estimates are diluted towards no effect, and confounding is only partly removed."))
                if (ctx$design == "ecological")
                    tbl$setNote("ecological", .("Ecological regression on the regions: a percentage is compared all versus none of the region, a mean per unit. The true values are effects on individuals; the gap is the ecological bias."))
                if (ctx$design == "longitudinal")
                    tbl$setNote("longitudinal", .("Crude: difference at the last visit among those who attended it (completers). Adjusted: linear mixed model with all visits (random intercept, an effect per visit), valid when dropout depends on values already seen."))
                if (ctx$design == "cluster_rct")
                    tbl$setNote("cluster", .("True values are conditional on the cluster; a model that ignores the clusters estimates a population-average effect (closer to the null for OR and HR) and needs cluster-robust standard errors."))
                mm <- unique(ex$true_measure[ex$true_measure != ex$measure & ex$true_measure %in% c("HR", "OR", "RR")])
                if (length(mm))
                    tbl$setNote("measure", jmvcore::format(.("True values are {true}s, the measure the data were generated with (a cohort followed over time); this design estimates {est}s, which approach them when the outcome is rare."),
                                                          true = paste(mm, collapse = "/"), est = ex$measure[1]))
            }
            if (ctx$design == "diagnostic") {
                mk <- self$results$keyMarkers
                mt <- mk$state
                if (is.null(mt)) { mt <- expected_markers(ctx$sc, ctx$oc, opt); mk$setState(mt) }
                for (i in seq_len(nrow(mt))) {
                    m <- ctx$oc$markers[[mt$marker[i]]]
                    k <- paste(mt$marker[i], if (is.null(m$block)) "" else if (mt$true[i] > 0.5 || m$informative == m$block) "inf" else "noise")
                    if (k %in% mk$rowKeys) mk$setRow(rowKey = k, values = list(truth = mt$true[i], expected = mt$auc[i]))
                }
                mk$setNote("auc", .("AUC of each marker alone (a block: the mean over its columns). Values are higher in the diseased unless the scenario says lower; the large study has 20,000 people."))
            }
            l <- ctx$lang
            calib <- switch(ctx$oc$type,
                binary = jmvcore::format(.("intercept {a} gives a prevalence of {p}"), a = round(cal$a, 4), p = round(cal$achieved, 4)),
                continuous = jmvcore::format(.("intercept {a}"), a = round(cal$a, 4)),
                count = jmvcore::format(.("log intercept {a}"), a = round(cal$a, 4)),
                event = jmvcore::format(.("baseline Weibull scale {l}, shape {s}: average hazard {r}, risk {q} over {t} {units}"),
                                l = signif(cal$lambda, 4), s = cal$shape, r = signif(cal$achieved$rate, 3),
                                q = round(cal$achieved$risk_followup, 4), t = ctx$oc$followup, units = unit_text(ctx$oc$time_units, l)))
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

        # causal diagram: shared with Compare designs (dag_init / dag_fill below)
        .initDag = function(ctx) dag_init(self, ctx),
        .fillDag = function(ctx) dag_fill(self, ctx),
        .plotDag = function(image, ggtheme, theme, ...) dag_render(image),

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

# The causal diagram of an analysis with options showDag/dagValues and an
# Image result `dag` (Simulated study, Compare designs). `self` is the
# analysis: .() translates through it.
dag_caption <- function(self) {
    cap <- .("Solid box: measured; dashed: not measured; grey arrows: paths among variables; dotted: correlations; dark arrows: effects on the outcome.")
    if (isTRUE(self$options$dagValues)) cap <- paste(cap, .("Red: raises the outcome, blue: lowers it; * the effect depends on another variable."))
    cap
}

# in .init(): the image sized to the layout (pixels) and its caption
dag_init <- function(self, ctx) {
    g <- dag_layout(ctx$sc, ctx$oc, ctx$factors, ctx$lang)
    sz <- dag_size(g, dag_caption(self))
    self$results$dag$setSize(sz[["w"]], sz[["h"]])
}

# in .run(): jamovi restores the previous (empty) state after .init(), which
# would wipe one set there; a state saved by v0.1 has no routed points
dag_fill <- function(self, ctx) {
    img <- self$results$dag
    if (is.null(img$state) || is.null(img$state$g$points)) {
        g <- dag_layout(ctx$sc, ctx$oc, ctx$factors, ctx$lang)
        img$setState(list(g = g, w = img$width, h = img$height, key = isTRUE(self$options$dagValues),
                          caption = dag_caption(self)))
    }
}

dag_render <- function(image) {
    st <- image$state
    if (is.null(st)) return(FALSE)
    print(dag_plot(st$g, st$w, st$h, st$key, st$caption))
    TRUE
}

# The design options a study used (the scenario's unless typed in the panel),
# for the summary: "Follow-up: 10 years; Loss to follow-up: 2% per year"
design_options_text <- function(design, opt, oc, self, lang = "en") {
    u <- unit_text(oc$time_units, lang)
    f <- function(x) base::format(signif(x, 4))
    it <- c(
        if (!is.null(opt$followup) && design %in% c("cohort", "prognosis", "rct", "nested_cc", "case_cohort", "cluster_rct", "factorial"))
            c(sprintf("%s: %s %s", .("Follow-up"), f(opt$followup), u), sprintf("%s: %s%% %s", .("Loss to follow-up"), f(100 * opt$loss), per_unit_text(oc$time_units, lang))),
        if (!is.null(opt$accrual) && opt$accrual > 0 && design %in% c("cohort", "prognosis", "nested_cc", "case_cohort"))
            sprintf("%s: %s %s", .("Recruitment period"), f(opt$accrual), u),
        if (design == "factorial") sprintf("%s: %s", .("Interaction of the two interventions"), f(opt$interaction %||% 0)),
        if (design == "longitudinal") sprintf("%s: %s%%", .("Dropout by the last visit (%)"), f(100 * opt$dropout)),
        if (design == "ecological") c(sprintf("%s: %s", .("Differences between regions (exposures)"), f(opt$region_sd)),
                                      sprintf("%s: %s", .("Differences between regions (outcome)"), f(opt$context_sd))),
        if (design == "method_comparison") c(sprintf("%s: %s", .("Bias of method B"), f(opt$bias)), sprintf("%s: %s", .("Proportional bias of method B (slope)"), f(opt$slope)),
                                             sprintf("%s: %s", .("Error of method A (% of SD)"), f(100 * opt$error_a)), sprintf("%s: %s", .("Error of method B (% of SD)"), f(100 * opt$error_b))),
        if (design == "crossover") c(sprintf("%s: %s", .("Period effect"), f(opt$period)), sprintf("%s: %s", .("Carryover effect"), f(opt$carryover))))
    paste(it, collapse = "; ")
}

# A design option typed in a text box: NULL when empty (the scenario's value),
# else a number in [lo, hi] (a decimal comma is accepted) divided by `scale`
num_opt <- function(self, name, title, lo, hi, int = FALSE, scale = 1) {
    x <- self$options[[name]]
    if (is.null(x) || (is.character(x) && !nzchar(trimws(x)))) return(NULL)
    v <- suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(x)))))
    if (length(v) != 1 || is.na(v) || v < lo || v > hi || (int && v != round(v)))
        stop(jmvcore::format(.("{title}: write a number between {lo} and {hi}, or leave it empty for the scenario's value."),
                             title = title, lo = if (is.finite(lo)) lo else "-", hi = if (is.finite(hi)) hi else "-"), call. = FALSE)
    v / scale
}

# `self` is the calling analysis: .() translates through it
design_title <- function(d, self) {
    switch(d, case_control = .("Case-control"), matched_cc = .("Matched case-control"),
           cohort = .("Cohort"), cross_sectional = .("Cross-sectional"), rct = .("Randomised trial"),
           crossover = .("2x2 crossover trial"), prognosis = .("Prognosis (patient cohort)"),
           nested_cc = .("Nested case-control"),
           case_cohort = .("Case-cohort"),
           cluster_rct = .("Cluster randomised trial"),
           factorial = .("2x2 factorial trial"),
           longitudinal = .("Longitudinal trial (repeated visits)"),
           ecological = .("Ecological (regions)"),
           diagnostic = .("Diagnostic accuracy"),
           pre_post = .("Before-after (single group)"),
           method_comparison = .("Method comparison"), d)
}

# the panel's Variables list -> measured factor ids and the grouping of the
# measured continuous ones (form other than continuous); names may be shown
# names in any language
parse_measured <- function(measured, sc, self) {
    # no list at all (e.g. called from R): every variable is measured
    if (length(measured) == 0) return(list(factors = sc$order, cats = list()))
    inc <- Filter(function(m) isTRUE(m$include), measured)
    fac <- ids_from_names(vapply(inc, function(m) as.character(m$var), ""), sc$order)
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
    list(factors = intersect(sc$order, fac), cats = cats)
}

# "{events} events in {pt} person-years", in the outcome's time units
person_time_text <- function(f, units, self, num) {
    msg <- switch(units %||% "years", months = .("{events} events in {pt} person-months"), weeks = .("{events} events in {pt} person-weeks"),
                  days = .("{events} events in {pt} person-days"), .("{events} events in {pt} person-years"))
    jmvcore::format(msg, events = num(f$events), pt = num(f$person_time))
}

# Note on the Monte Carlo uncertainty of the expected values (a list of
# estimate tables); "" for tables saved without intervals (before v0.4)
mc_note <- function(tables, self) {
    tables <- Filter(function(e) !is.null(e$lower), tables)
    if (!length(tables)) return("")
    mc <- mc_halfwidth(do.call(rbind, lapply(tables, function(e) e[, c("lower", "upper", "measure")])))
    out <- c(if (!is.na(mc$pct)) jmvcore::format(.("The expected values come from one simulated study: their 95% Monte Carlo uncertainty is up to ±{pct}% for the ratios."),
                                                 pct = formatC(mc$pct, format = "f", digits = 1)),
             if (!is.na(mc$diff)) jmvcore::format(.("The expected values come from one simulated study: their 95% Monte Carlo uncertainty is up to ±{d} for the differences."),
                                                  d = formatC(signif(mc$diff, 2), format = "fg")))
    paste(out, collapse = " ")
}

htmlEscape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
}
