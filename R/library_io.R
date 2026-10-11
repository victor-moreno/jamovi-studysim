# The library: a dictionary of shared variables plus scenarios, in one data
# model with three serialisations -- YAML (one file per scenario plus
# variables.yaml), an Excel workbook (one sheet per table) and JSON (the whole
# library in one object, which the options panel can read). All three go
# through the nested list form used by normalize_scenario().

LANGS <- c("en", "es", "ca")

# YAML 1.1 reads yes/no/on/off as booleans; a library writes them as levels
yaml_read <- function(text) {
    keep <- function(x) x
    yaml::yaml.load(text, handlers = list("bool#yes" = keep, "bool#no" = keep), eval.expr = FALSE)
}

read_yaml_file <- function(path) yaml_read(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))

# A YAML document may be a dictionary (variables.yaml), one scenario (has
# `id` and `outcomes`), a whole library (`variables:` + `scenarios:`, maybe
# `i18n:`) or a language dictionary (`names:`/`levels:`, file named <lang>.yaml).
add_yaml_doc <- function(lib, doc, name = "") {
    base <- sub("\\.(ya?ml|json)$", "", basename(name), ignore.case = TRUE)
    if (is.null(doc$outcomes) && (!is.null(doc$scenarios) || !is.null(doc$i18n) ||
        (!is.null(doc$variables) && is.null(doc$id)))) {
        lib$dictionary <- shallow_merge(lib$dictionary, doc$variables %||% list())
        for (s in doc$scenarios %||% list()) lib$scenarios[[s$id]] <- s
        if (!is.null(doc$scenarios) && !is.null(names(doc$scenarios)))
            for (k in names(doc$scenarios)) { s <- doc$scenarios[[k]]; s$id <- s$id %||% k; lib$scenarios[[s$id]] <- s }
        lib$i18n <- merge_i18n(lib$i18n, doc$i18n)
    } else if (!is.null(doc$outcomes)) {
        doc$id <- doc$id %||% base
        lib$scenarios[[doc$id]] <- doc
    } else if ((!is.null(doc$names) || !is.null(doc$levels)) && grepl("^[a-z]{2}$", base)) {
        lib$i18n <- merge_i18n(lib$i18n, stats::setNames(list(doc[c("names", "levels")]), base))
    } else {
        lib$dictionary <- shallow_merge(lib$dictionary, doc)
    }
    lib
}

empty_library <- function() list(dictionary = list(), scenarios = list(), i18n = list())

# translations: list(<lang> = list(names = list(id = name), levels = list(level = label)));
# entries of `b` win, language by language and section by section
merge_i18n <- function(a, b) {
    out <- a %||% list()
    for (l in names(b)) for (k in c("names", "levels"))
        if (length(b[[l]][[k]])) out[[l]][[k]] <- shallow_merge(out[[l]][[k]] %||% list(), b[[l]][[k]])
    out
}

read_library_dir <- function(dir) {
    lib <- empty_library()
    f <- file.path(dir, "variables.yaml")
    if (file.exists(f)) lib$dictionary <- read_yaml_file(f)
    for (p in sort(list.files(file.path(dir, "scenarios"), "\\.ya?ml$", full.names = TRUE)))
        lib <- add_yaml_doc(lib, read_yaml_file(p), p)
    for (p in sort(list.files(file.path(dir, "i18n"), "^[a-z]{2}\\.ya?ml$", full.names = TRUE)))
        lib <- add_yaml_doc(lib, read_yaml_file(p), p)
    lib
}

.builtin <- new.env()

builtin_library <- function() {
    if (is.null(.builtin$lib)) {
        dir <- system.file("library", package = "studysim")
        if (!nzchar(dir)) dir <- file.path("inst", "library")
        .builtin$lib <- read_library_dir(dir)
    }
    .builtin$lib
}

# custom ids win over built-in ones; dictionaries and translations likewise
merge_libraries <- function(base, custom) {
    list(dictionary = shallow_merge(base$dictionary, custom$dictionary),
         scenarios = shallow_merge(base$scenarios, custom$scenarios),
         i18n = merge_i18n(base$i18n, custom$i18n))
}

# ---- tabular model (the Excel sheets) ---------------------------------------

LIST_SEP <- ", "

# a label-like field <-> columns field_en, field_es, ...
ml_cols <- function(x, field) {
    out <- list()
    for (l in LANGS) out[[paste0(field, "_", l)]] <- if (is.null(x)) NA_character_ else
        if (is.list(x)) as.character(x[[l]] %||% NA_character_) else if (l == "en") as.character(x) else NA_character_
    out
}

ml_from <- function(row, field) {
    v <- list()
    for (l in LANGS) {
        k <- paste0(field, "_", l)
        if (!is.null(row[[k]]) && !is.na(row[[k]]) && nzchar(row[[k]])) v[[l]] <- as.character(row[[k]])
    }
    if (length(v) == 0) NULL else if (length(v) == 1 && names(v) == "en") v$en else v
}

csv1 <- function(x) if (is.null(x)) NA_character_ else paste(unlist(x), collapse = LIST_SEP)
uncsv <- function(x) if (is.null(x) || is.na(x) || !nzchar(trimws(x))) NULL else trimws(strsplit(as.character(x), ",")[[1]])
na1 <- function(x) if (is.null(x)) NA else unlist(x)[1]
rbind_rows <- function(rows) {
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) return(NULL)
    cols <- unique(unlist(lapply(rows, names)))
    do.call(rbind, lapply(rows, function(r) {
        for (k in setdiff(cols, names(r))) r[[k]] <- NA
        as.data.frame(r[cols], stringsAsFactors = FALSE, check.names = FALSE)
    }))
}

VAR_FIELDS <- c("from", "type", "dist", "p", "maf", "round", "formula", "only_if", "otherwise", "lod")

var_row <- function(scenario, id, v) {
    r <- list(scenario = scenario, variable = id)
    for (f in VAR_FIELDS) r[[f]] <- if (is.null(v[[f]])) NA else as.character(na1(v[[f]]))
    r$levels <- csv1(v$levels); r$probs <- csv1(v$probs); r$alleles <- csv1(v$alleles)
    r$range <- csv1(v$range)
    c(r, ml_cols(v$units, "units"), ml_cols(v$label, "label"), ml_cols(v$description, "description"))
}

MARKER_FIELDS <- c("dist", "round", "auc", "direction", "block", "informative", "rho", "lod")

OUTCOME_FIELDS <- c("type", "scale", "prevalence", "rate", "median", "shape", "mean", "sd", "within_cor",
                    "size", "window", "followup", "round", "time_units", "marker_cor")

library_to_tables <- function(lib) {
    sc <- lib$scenarios
    t <- list()
    t$scenarios <- rbind_rows(lapply(sc, function(s) c(
        list(scenario = s$id, domain = s$domain %||% NA, null_variables = csv1(s$null_variables)),
        ml_cols(s$label, "label"), ml_cols(s$population, "population"), ml_cols(s$teaching, "teaching"))))
    t$variables <- rbind_rows(c(
        lapply(names(lib$dictionary), function(v) var_row("*", v, lib$dictionary[[v]])),
        unlist(lapply(sc, function(s) lapply(names(s$variables), function(v) var_row(s$id, v, s$variables[[v]] %||% list()))),
               recursive = FALSE)))
    t$interventions <- rbind_rows(unlist(lapply(sc, function(s) lapply(names(s$interventions), function(i) {
        x <- s$interventions[[i]]
        c(list(scenario = s$id, intervention = i, levels = csv1(x$levels), ratio = csv1(x$ratio)), ml_cols(x$label, "label"))
    })), recursive = FALSE))
    # a competing event is a row of its own, named <outcome>:competing
    with_competing <- function(s) {
        out <- list()
        for (o in names(s$outcomes)) {
            out[[o]] <- s$outcomes[[o]]
            if (!is.null(s$outcomes[[o]]$competing)) out[[paste0(o, ":competing")]] <- s$outcomes[[o]]$competing
            if (!is.null(s$outcomes[[o]]$after_event)) out[[paste0(o, ":after_event")]] <- s$outcomes[[o]]$after_event
        }
        out
    }
    t$outcomes <- rbind_rows(unlist(lapply(sc, function(s) { oo <- with_competing(s); lapply(names(oo), function(o) {
        x <- oo[[o]]
        r <- list(scenario = s$id, outcome = o)
        for (f in OUTCOME_FIELDS) r[[f]] <- if (is.null(x[[f]])) NA else as.character(na1(x[[f]]))
        r$risk <- if (is.null(x$risk)) NA else as.character(x$risk$value)
        r$risk_at <- if (is.null(x$risk)) NA else as.character(x$risk$at)
        r$levels <- csv1(x$levels); r$range <- csv1(x$range); r$designs <- csv1(x$designs)
        r$link <- if (is.null(x$link)) NA else x$link$outcome
        r$link_rho <- if (is.null(x$link)) NA else as.character(x$link$rho)
        r$after <- x$after %||% NA
        c(r, ml_cols(x$units, "units"), ml_cols(x$label, "label"), ml_cols(x$description, "description"))
    }) }), recursive = FALSE))
    edge_rows <- function(s, key, kind) lapply(s[[key]], function(e) {
        e <- unlist(e); list(scenario = s$id, from = e[1], to = e[2], strength = e[3], kind = kind) })
    t$structure <- rbind_rows(unlist(lapply(sc, function(s) c(edge_rows(s, "structure", "path"),
                                                             edge_rows(s, "correlations", "correlation"))), recursive = FALSE))
    t$effects <- rbind_rows(unlist(lapply(sc, function(s) { oo <- with_competing(s); unlist(lapply(names(oo), function(o) {
        eff <- oo[[o]]$effects
        lapply(names(eff), function(v) {
            e <- eff[[v]]; m <- intersect(names(e), names(MEASURES))[1]
            mod <- if (is.null(e$by)) NA else names(e$by)[1]
            list(scenario = s$id, outcome = o, variable = v, measure = m, value = csv1(e[[m]]),
                 per = if (is.null(e$per)) NA else as.character(e$per), model = e$model %||% NA,
                 center = if (is.null(e$center)) NA else as.character(e$center),
                 modifier = mod,
                 modifier_values = if (is.na(mod)) NA else paste(sprintf("%s: %s", names(e$by[[mod]]), unlist(e$by[[mod]])), collapse = LIST_SEP))
        })
    }), recursive = FALSE) }), recursive = FALSE))
    t$markers <- rbind_rows(unlist(lapply(sc, function(s) unlist(lapply(names(s$outcomes), function(o) {
        mk <- s$outcomes[[o]]$markers
        lapply(names(mk), function(m) {
            x <- mk[[m]]
            r <- list(scenario = s$id, outcome = o, marker = m)
            for (f in MARKER_FIELDS) r[[f]] <- if (is.null(x[[f]])) NA else as.character(na1(x[[f]]))
            r$range <- csv1(x$range)
            c(r, ml_cols(x$units, "units"), ml_cols(x$label, "label"))
        })
    }), recursive = FALSE)), recursive = FALSE))
    t$designs <- rbind_rows(unlist(lapply(sc, function(s) unlist(lapply(names(s$designs), function(d)
        lapply(names(s$designs[[d]]), function(k) list(scenario = s$id, design = d, option = k, value = csv1(s$designs[[d]][[k]])))),
        recursive = FALSE)), recursive = FALSE))
    t$references <- rbind_rows(unlist(lapply(sc, function(s) lapply(unlist(s$references), function(r)
        list(scenario = s$id, reference = r))), recursive = FALSE))
    tr_rows <- function(scen, i18n) {
        keys <- list()
        for (l in names(i18n)) for (k in c("names", "levels")) for (x in names(i18n[[l]][[k]]))
            keys[[paste(k, x, sep = "\r")]] <- c(kind = sub("s$", "", k), key = x)
        lapply(keys, function(kk) {
            k <- paste0(kk[["kind"]], "s")
            r <- list(scenario = scen, kind = kk[["kind"]], key = kk[["key"]])
            for (l in LANGS) r[[l]] <- as.character(i18n[[l]][[k]][[kk[["key"]]]] %||% NA)
            r
        })
    }
    t$translations <- rbind_rows(c(tr_rows("*", lib$i18n),
                                   unlist(lapply(sc, function(s) tr_rows(s$id, s$i18n)), recursive = FALSE)))
    Filter(Negate(is.null), t)
}

# translations sheet -> i18n lists (scenario "*" = the whole library)
tables_to_i18n <- function(df, scenario) {
    out <- list()
    for (r in rows_of(df, scenario)) {
        k <- paste0(cell(r, "kind") %||% "name", "s")
        if (!k %in% c("names", "levels")) next
        for (l in LANGS) if (!is.null(cell(r, l))) out[[l]][[k]][[cell(r, "key")]] <- cell(r, l)
    }
    out
}

# numbers stay as text in the tables, so "2.5/1000" and "log(1.04)" survive
cell <- function(row, k) { v <- row[[k]]; if (is.null(v) || length(v) == 0 || is.na(v) || !nzchar(trimws(as.character(v)))) NULL else trimws(as.character(v)) }

rows_of <- function(df, scenario) {
    if (is.null(df) || nrow(df) == 0) return(list())
    d <- df[as.character(df$scenario) == scenario, , drop = FALSE]
    lapply(seq_len(nrow(d)), function(i) as.list(d[i, , drop = FALSE]))
}

table_var <- function(r) {
    v <- list()
    for (f in VAR_FIELDS) if (!is.null(cell(r, f))) v[[f]] <- cell(r, f)
    if (!is.null(v$round)) v$round <- as.integer(v$round)
    for (f in c("levels", "probs", "alleles", "range")) if (!is.null(uncsv(cell(r, f)))) v[[f]] <- uncsv(cell(r, f))
    for (f in c("units", "label", "description")) if (!is.null(ml_from(r, f))) v[[f]] <- ml_from(r, f)
    v
}

tables_to_library <- function(t) {
    lib <- empty_library()
    for (r in rows_of(t$variables, "*")) lib$dictionary[[r$variable]] <- table_var(r)
    lib$i18n <- tables_to_i18n(t$translations, "*")
    ids <- unique(as.character(t$scenarios$scenario))
    for (id in ids) {
        r0 <- rows_of(t$scenarios, id)[[1]]
        s <- list(id = id)
        for (f in c("label")) if (!is.null(ml_from(r0, f))) s[[f]] <- ml_from(r0, f)
        if (!is.null(cell(r0, "domain"))) s$domain <- cell(r0, "domain")
        if (!is.null(ml_from(r0, "population"))) s$population <- ml_from(r0, "population")
        s$variables <- list()
        for (r in rows_of(t$variables, id)) s$variables[[r$variable]] <- table_var(r)
        st <- rows_of(t$structure, id)
        edges <- function(kind) lapply(Filter(function(r) (cell(r, "kind") %||% "path") == kind, st),
                                       function(r) c(r$from, r$to, as.character(r$strength)))
        if (length(edges("path"))) s$structure <- edges("path")
        if (length(edges("correlation"))) s$correlations <- edges("correlation")
        for (r in rows_of(t$interventions, id)) {
            x <- list(levels = uncsv(cell(r, "levels")))
            if (!is.null(uncsv(cell(r, "ratio")))) x$ratio <- uncsv(cell(r, "ratio"))
            if (!is.null(ml_from(r, "label"))) x$label <- ml_from(r, "label")
            s$interventions[[r$intervention]] <- x
        }
        s$outcomes <- list()
        for (r in rows_of(t$outcomes, id)) {
            o <- list()
            for (f in OUTCOME_FIELDS) if (!is.null(cell(r, f))) o[[f]] <- cell(r, f)
            if (!is.null(o$round)) o$round <- as.integer(o$round)
            if (!is.null(cell(r, "risk"))) o$risk <- list(value = cell(r, "risk"), at = cell(r, "risk_at"))
            for (f in c("levels", "range", "designs")) if (!is.null(uncsv(cell(r, f)))) o[[f]] <- uncsv(cell(r, f))
            for (f in c("units", "label", "description")) if (!is.null(ml_from(r, f))) o[[f]] <- ml_from(r, f)
            if (!is.null(cell(r, "link"))) o$link <- list(outcome = cell(r, "link"), rho = cell(r, "link_rho"))
            if (!is.null(cell(r, "after"))) o$after <- cell(r, "after")
            o$effects <- list()
            for (e in Filter(function(e) e$outcome == r$outcome, rows_of(t$effects, id))) {
                x <- list(); x[[e$measure]] <- uncsv(cell(e, "value"))
                if (!is.null(cell(e, "per"))) x$per <- cell(e, "per")
                if (!is.null(cell(e, "model"))) x$model <- cell(e, "model")
                if (!is.null(cell(e, "center"))) x$center <- cell(e, "center")
                if (!is.null(cell(e, "modifier"))) {
                    kv <- uncsv(cell(e, "modifier_values"))
                    vals <- stats::setNames(as.list(trimws(sub("^[^:]*:", "", kv))), trimws(sub(":.*$", "", kv)))
                    x$by <- stats::setNames(list(vals), cell(e, "modifier"))
                }
                o$effects[[e$variable]] <- x
            }
            for (m in Filter(function(m) m$outcome == r$outcome, rows_of(t$markers, id))) {
                x <- list()
                for (f in MARKER_FIELDS) if (!is.null(cell(m, f))) x[[f]] <- cell(m, f)
                if (!is.null(uncsv(cell(m, "range")))) x$range <- uncsv(cell(m, "range"))
                for (f in c("units", "label")) if (!is.null(ml_from(m, f))) x[[f]] <- ml_from(m, f)
                o$markers[[m$marker]] <- x
            }
            s$outcomes[[r$outcome]] <- o
        }
        for (part in c("competing", "after_event")) for (nm in grep(sprintf(":%s$", part), names(s$outcomes), value = TRUE)) {
            s$outcomes[[sub(sprintf(":%s$", part), "", nm)]][[part]] <- s$outcomes[[nm]]
            s$outcomes[[nm]] <- NULL
        }
        if (!is.null(uncsv(cell(r0, "null_variables")))) s$null_variables <- uncsv(cell(r0, "null_variables"))
        for (r in rows_of(t$designs, id)) {
            v <- uncsv(cell(r, "value"))
            s$designs[[r$design]][[r$option]] <- v
        }
        refs <- vapply(rows_of(t$references, id), function(r) as.character(r$reference), "")
        if (length(refs)) s$references <- refs
        if (!is.null(ml_from(r0, "teaching"))) s$teaching <- ml_from(r0, "teaching")
        tr <- tables_to_i18n(t$translations, id)
        if (length(tr)) s$i18n <- tr
        lib$scenarios[[id]] <- s
    }
    lib
}

# ---- files in and out ----------------------------------------------------------

write_library_xlsx <- function(lib, path, help = NULL) {
    t <- library_to_tables(lib)
    wb <- openxlsx::createWorkbook()
    if (!is.null(help)) {
        openxlsx::addWorksheet(wb, "help")
        openxlsx::writeData(wb, "help", data.frame(help = help), colNames = FALSE)
        openxlsx::setColWidths(wb, "help", 1, 140)
    }
    for (nm in names(t)) {
        openxlsx::addWorksheet(wb, nm)
        openxlsx::writeData(wb, nm, t[[nm]], headerStyle = openxlsx::createStyle(textDecoration = "bold"))
        openxlsx::freezePane(wb, nm, firstRow = TRUE)
    }
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    invisible(path)
}

read_library_xlsx <- function(path) {
    sheets <- setdiff(openxlsx::getSheetNames(path), "help")
    t <- lapply(stats::setNames(sheets, sheets), function(s) {
        d <- openxlsx::read.xlsx(path, sheet = s, colNames = TRUE, skipEmptyRows = TRUE)
        d[] <- lapply(d, as.character)
        d
    })
    tables_to_library(t)
}

# With compatible = TRUE each outcome carries `_compatible`, the designs that
# fit it computed by compatible_designs(), so the panel never re-implements
# that rule (it falls back to its own copy for JSON files written without it)
library_to_json <- function(lib, pretty = FALSE, compatible = FALSE) {
    sc <- lib$scenarios
    if (compatible) for (id in names(sc)) {
        s <- tryCatch(load_scenario(lib, id), error = function(e) NULL)
        if (is.null(s)) next
        for (o in intersect(names(sc[[id]]$outcomes), names(s$outcomes)))
            sc[[id]]$outcomes[[o]][["_compatible"]] <- I(compatible_designs(s, o))
    }
    jsonlite::toJSON(list(variables = lib$dictionary, scenarios = sc, i18n = lib$i18n %||% list()), auto_unbox = TRUE,
                     pretty = pretty, null = "null", digits = NA)
}

library_from_json <- function(text) {
    doc <- jsonlite::fromJSON(text, simplifyVector = FALSE)
    add_yaml_doc(empty_library(), doc)
}

# YAML: variables.yaml + scenarios/<id>.yaml, zipped
write_library_yaml_zip <- function(lib, path) {
    tmp <- tempfile("studysim-lib"); dir.create(file.path(tmp, "scenarios"), recursive = TRUE)
    on.exit(unlink(tmp, recursive = TRUE))
    if (length(lib$dictionary)) yaml::write_yaml(lib$dictionary, file.path(tmp, "variables.yaml"))
    for (s in lib$scenarios) yaml::write_yaml(s, file.path(tmp, "scenarios", paste0(s$id, ".yaml")))
    if (length(lib$i18n)) dir.create(file.path(tmp, "i18n"))
    for (l in names(lib$i18n)) yaml::write_yaml(lib$i18n[[l]], file.path(tmp, "i18n", paste0(l, ".yaml")))
    files <- list.files(tmp, recursive = TRUE)
    zip::zip(normalizePath(path, mustWork = FALSE), files, root = tmp)
    invisible(path)
}

# any supported file (by extension) -> library
read_library_file <- function(path, name = basename(path)) {
    ext <- tolower(tools::file_ext(name))
    switch(ext,
        yaml = , yml = add_yaml_doc(empty_library(), read_yaml_file(path), name),
        json = library_from_json(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")),
        xlsx = read_library_xlsx(path),
        zip = {
            tmp <- tempfile("studysim-zip"); on.exit(unlink(tmp, recursive = TRUE))
            zip::unzip(path, exdir = tmp)
            lib <- empty_library()
            for (p in sort(list.files(tmp, "\\.(ya?ml|json)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)))
                lib <- merge_libraries(lib, read_library_file(p))
            lib
        },
        stop(sprintf("'%s': a library is a .yaml, .yml, .json, .xlsx or .zip file", name), call. = FALSE))
}

# base64 payload from the panel's browse button -> library
read_library_payload <- function(b64, name) {
    if (!grepl("^[A-Za-z0-9+/=\\s]*$", b64, perl = TRUE)) stop("the library payload is not valid base64", call. = FALSE)
    raw <- jsonlite::base64_dec(gsub("\\s", "", b64))
    tmp <- tempfile(fileext = paste0(".", tools::file_ext(name)))
    on.exit(unlink(tmp))
    writeBin(raw, tmp)
    read_library_file(tmp, name)
}

load_scenario <- function(lib, id, strict = TRUE) {
    raw <- lib$scenarios[[id]]
    if (is.null(raw)) stop(sprintf("the library has no scenario '%s'", id), call. = FALSE)
    sc <- normalize_scenario(raw, lib$dictionary, strict = strict)
    # what students see: the library's translations, then the scenario's own
    sc$i18n <- merge_i18n(lib$i18n, raw$i18n)
    sc
}

# Validate every scenario; also checks names usable as column names.
check_library <- function(lib) {
    prev <- .langdict$active
    on.exit(.langdict$active <- prev)
    out <- list()
    for (id in names(lib$scenarios)) {
        sc <- tryCatch(load_scenario(lib, id, strict = FALSE), error = function(e) {
            list(problems = data.frame(where = sprintf("scenario '%s'", id), level = "error",
                                       message = conditionMessage(e), stringsAsFactors = FALSE)) })
        p <- sc$problems
        bad <- names(lib$scenarios[[id]]$variables)[!grepl("^[A-Za-z][A-Za-z0-9_]*$", names(lib$scenarios[[id]]$variables))]
        for (b in bad) p[nrow(p) + 1, ] <- list(sprintf("variable '%s'", b), "error",
                                                 "names must start with a letter and use only letters, digits and _")
        # i18n is by language first: i18n: {es: {names: ..., levels: ...}}
        for (k in setdiff(names(lib$scenarios[[id]]$i18n), names(lib$scenarios[[id]]$i18n)[grepl("^[a-z]{2}$", names(lib$scenarios[[id]]$i18n))]))
            p[nrow(p) + 1, ] <- list("i18n", "warning", sprintf(
                "'%s' is not a language code: write i18n: {es: {names: {...}, levels: {...}}}", k))
        if (!is.null(sc$order) && length(sc$rule_order) && !any(p$level == "error")) {
            # rules are only fully checked by running them
            err <- tryCatch({ with_seed(1, gen_covariates(sc, 200)); NULL }, error = function(e) conditionMessage(e))
            if (!is.null(err)) p[nrow(p) + 1, ] <- list("rules", "error", err)
        }
        if (!is.null(sc$order)) {
            # translated names become column names: valid and unique per language
            use_i18n(sc)
            ids <- c(sc$order, names(sc$outcomes), names(sc$interventions), "id", "set", "case", "time", "sequence", "period")
            for (l in union(LANGS, names(sc$i18n))) {
                nm <- dname(ids, l)
                for (b in unique(nm[!grepl("^[A-Za-z][A-Za-z0-9_]*$", nm)]))
                    p[nrow(p) + 1, ] <- list(sprintf("translations (%s)", l), "error", sprintf(
                        "the name '%s' must start with a letter and use only letters, digits and _", b))
                for (d in unique(nm[duplicated(nm)]))
                    p[nrow(p) + 1, ] <- list(sprintf("translations (%s)", l), "error", sprintf(
                        "'%s' names two columns (%s)", d, paste(ids[nm == d], collapse = ", ")))
            }
        }
        if (nrow(p)) { p$scenario <- id; out[[id]] <- p }
    }
    if (length(out) == 0) return(data.frame(scenario = character(), where = character(), level = character(),
                                            message = character(), stringsAsFactors = FALSE))
    res <- do.call(rbind, out); rownames(res) <- NULL
    res[, c("scenario", "where", "level", "message")]
}

.libcache <- new.env()

# built-in library, merged with a custom one sent by the panel (cached by content)
get_library <- function(payload = "", name = "") {
    if (!nzchar(payload)) return(builtin_library())
    key <- digest::digest(list(payload, name))
    if (is.null(.libcache[[key]]))
        .libcache[[key]] <- merge_libraries(builtin_library(), read_library_payload(payload, name))
    .libcache[[key]]
}

# one scenario with the dictionary entries it draws on
library_subset <- function(lib, ids) {
    sc <- lib$scenarios[ids]
    used <- unique(unlist(lapply(sc, function(s) names(Filter(function(v) is.null(v$type) || identical(v$from, "dictionary"), s$variables)))))
    list(dictionary = lib$dictionary[intersect(names(lib$dictionary), used)], scenarios = sc, i18n = lib$i18n)
}

# a readable one-line description of a variable's marginal distribution
describe_variable <- function(v) {
    rule <- paste(c(if (!is.null(v$formula)) paste("=", v$formula$text),
                    if (!is.null(v$p_rule)) paste0("p = ", v$p_rule$text),
                    if (!is.null(v$only_if)) sprintf("only if %s, else %s", v$only_if$text, v$otherwise)), collapse = "; ")
    if (!is.null(v$formula)) return(rule)
    if (!is.null(v$p_rule)) return(paste0(v$levels[2], ": ", rule))
    base <- describe_marginal(v)
    if (nzchar(rule)) paste0(base, "; ", rule) else base
}

describe_marginal <- function(v) {
    switch(v$type,
        continuous = , count = paste0(v$dist$family, "(", paste(signif(v$dist$args, 4), collapse = ", "), ")",
                                      if (!is.null(v$range)) sprintf(" in [%s, %s]", v$range[1], v$range[2]) else ""),
        binary = sprintf("%s %.3g", v$levels[2], v$p),
        genotype = sprintf("MAF %.3g (%s)", v$maf, paste(v$levels, collapse = "/")),
        paste(sprintf("%s %.3g", v$levels, v$probs), collapse = ", "))
}

# ---- names and levels shown to students (inst/library/i18n/<lang>.yaml) -------

.langdict <- new.env()

# The translations in force: those of the scenario being simulated (set by
# use_i18n), else the built-in library's. Each analysis sets them before it
# formats anything, so a custom library's names never leak into another run.
use_i18n <- function(sc = NULL) {
    .langdict$active <- if (is.null(sc)) NULL else sc$i18n %||% list()
    invisible(sc)
}

lang_dict <- function(lang = "en") {
    d <- if (is.null(.langdict$active)) builtin_library()$i18n else .langdict$active
    d[[lang]] %||% list(names = list(), levels = list())
}

# column name of an id, and label of a level, in `lang` (unchanged when absent)
dname <- function(id, lang = "en") {
    n <- lang_dict(lang)$names
    vapply(as.character(id), function(x) as.character(n[[x]] %||% x), "", USE.NAMES = FALSE)
}

dlevel <- function(x, lang = "en") {
    l <- lang_dict(lang)$levels
    one <- function(s) {
        if (!is.null(l[[s]])) return(as.character(l[[s]]))
        # a crossover sequence "placebo-drug" is translated part by part
        if (grepl("-", s, fixed = TRUE)) return(paste(vapply(strsplit(s, "-", fixed = TRUE)[[1]], one, ""), collapse = "-"))
        s
    }
    vapply(as.character(x), one, "", USE.NAMES = FALSE)
}

# the panel sends the measured variables by their shown names (any language)
ids_from_names <- function(x, ids) {
    x <- as.character(unlist(x))
    out <- character()
    for (s in x) {
        if (s %in% ids) { out <- c(out, s); next }
        for (l in LANGS) {
            m <- ids[dname(ids, l) == s]
            if (length(m)) { out <- c(out, m[1]); break }
        }
    }
    unique(out)
}

# rename the columns and relabel the factor levels of a formatted study
localise_data <- function(df, lang = "en") {
    for (nm in names(df)) if (is.factor(df[[nm]])) {
        d <- attr(df[[nm]], "jmv-desc")
        levels(df[[nm]]) <- dlevel(levels(df[[nm]]), lang)
        attr(df[[nm]], "jmv-desc") <- d
    }
    # an ecological per-level column: translated variable name and level
    # ... and a measurement of the outcome (before, method A): outcome name and suffix
    eco <- lapply(df, function(x) attr(x, "eco") %||% attr(x, "rename"))
    nms <- dname(names(df), lang)
    for (j in which(!vapply(eco, is.null, TRUE))) {
        suf <- eco[[j]][2]
        suf <- if (suf %in% c("a", "b")) suf else gtext(suf, lang) %||% dlevel(suf, lang)
        nms[j] <- paste0(dname(eco[[j]][1], lang), "_", gsub("[^[:alnum:]]+", "_", if (nzchar(suf)) suf else dlevel(eco[[j]][2], lang)))
    }
    names(df) <- nms
    df
}
