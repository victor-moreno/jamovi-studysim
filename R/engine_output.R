# From the engine's internal frame to the dataset students see: readable
# names, factors with their level labels (ordered for ordinal variables),
# column descriptions (label + units) and a data dictionary.

# generic labels the engine adds itself (scenario labels come from the library)
GENERIC <- list(
    case = list(en = "Case status", es = "Caso o control", ca = "Cas o control"),
    control = list(en = "control", es = "control", ca = "control"),
    caselevel = list(en = "case", es = "caso", ca = "cas"),
    time = list(en = "Follow-up time", es = "Tiempo de seguimiento", ca = "Temps de seguiment"),
    event = list(en = "1 = event, 0 = censored", es = "1 = evento, 0 = censurado", ca = "1 = esdeveniment, 0 = censurat"),
    set = list(en = "Matched set", es = "Grupo emparejado", ca = "Grup aparellat"),
    riskset = list(en = "Risk set (one case and its controls)", es = "Conjunto de riesgo (un caso y sus controles)",
                   ca = "Conjunt de risc (un cas i els seus controls)"),
    settime = list(en = "Time at which the set was sampled (the case's event time)", es = "Tiempo en que se muestreó el conjunto (el del evento del caso)",
                   ca = "Temps en què es va mostrejar el conjunt (el de l'esdeveniment del cas)"),
    subcohort = list(en = "Member of the random subcohort", es = "Miembro de la subcohorte aleatoria", ca = "Membre de la subcohort aleatòria"),
    visit = list(en = "Visit (0 = baseline, before treatment)", es = "Visita (0 = basal, antes del tratamiento)", ca = "Visita (0 = basal, abans del tractament)"),
    cluster = list(en = "Cluster (unit of randomisation)", es = "Conglomerado (unidad de aleatorización)", ca = "Conglomerat (unitat d'aleatorització)"),
    look = list(en = "First interim look that includes the participant", es = "Primer análisis intermedio que incluye al participante",
                ca = "Primera anàlisi intermèdia que inclou el participant"),
    sequence = list(en = "Treatment sequence", es = "Secuencia de tratamiento", ca = "Seqüència de tractament"),
    period = list(en = "Period", es = "Periodo", ca = "Període"),
    id = list(en = "Participant identifier", es = "Identificador del participante", ca = "Identificador del participant"),
    per = list(en = "per", es = "por", ca = "per"),
    meansd = list(en = "mean (SD)", es = "media (DE)", ca = "mitjana (DE)"),
    cat_median = list(en = "split at the median", es = "dividida por la mediana", ca = "dividida per la mediana"),
    cat_tertiles = list(en = "tertiles", es = "terciles", ca = "terços"),
    cat_quartiles = list(en = "quartiles", es = "cuartiles", ca = "quartils"),
    cat_quintiles = list(en = "quintiles", es = "quintiles", ca = "quintils"),
    cat_cuts = list(en = "grouped by cut points", es = "agrupada por puntos de corte", ca = "agrupada per punts de tall"),
    categorised = list(en = "categorised; the true effect is per unit (see true model)",
                       es = "categorizada; el efecto verdadero es por unidad (ver modelo verdadero)",
                       ca = "categoritzada; l'efecte veritable és per unitat (vegeu model veritable)"),
    mediqr = list(en = "median [IQR]", es = "mediana [RIC]", ca = "mediana [RIQ]"),
    vs = list(en = "vs", es = "frente a", ca = "enfront de"))

gtext <- function(key, lang) tr_text(GENERIC[[key]], lang)

with_desc <- function(x, desc) { attr(x, "jmv-desc") <- desc; x }

var_desc <- function(v, lang) {
    d <- tr_text(v$label, lang)
    if (!is.null(v$units)) d <- sprintf("%s (%s)", d, tr_text(v$units, lang))
    d
}

code_to_column <- function(x, v) {
    switch(v$type,
        continuous = as.numeric(x),
        count = as.integer(x),
        binary = factor(v$levels[x + 1L], levels = v$levels),
        ordinal = factor(v$levels[x], levels = v$levels, ordered = TRUE),
        factor(v$levels[x], levels = v$levels))
}

# res: list(data, facts) from a design; factors: the scenario variables to show
format_study <- function(sc, oc, design, res, factors, lang = "en") {
    d <- res$data
    n <- nrow(d)
    out <- list()
    if (design %in% c("crossover", "longitudinal")) {
        out$id <- with_desc(as.integer(factor(d$.pid, levels = unique(d$.pid))), gtext("id", lang))
    } else {
        out$id <- with_desc(seq_len(n), gtext("id", lang))
    }
    iv <- intersect(names(sc$interventions), sub("^\\.", "", names(d)))
    if (design == "matched_cc") out$set <- with_desc(as.integer(d$.set), gtext("set", lang))
    if (design == "nested_cc") out$set <- with_desc(as.integer(d$.set), gtext("riskset", lang))
    if (design == "cluster_rct") out$cluster <- with_desc(as.integer(d$.cluster), gtext("cluster", lang))
    if (design == "case_cohort") out$subcohort <- with_desc(factor(c("no", "yes")[d$.sub + 1L], levels = c("no", "yes")), gtext("subcohort", lang))
    if (design == "crossover") {
        lv <- sc$interventions[[iv]]$levels
        seqs <- c(paste(lv, collapse = "-"), paste(rev(lv), collapse = "-"))
        out$sequence <- with_desc(factor(seqs[d$.seq], levels = seqs), gtext("sequence", lang))
        out$period <- with_desc(as.integer(d$.period), gtext("period", lang))
    }
    for (v in iv) {
        x <- sc$interventions[[v]]
        out[[v]] <- with_desc(factor(x$levels[d[[paste0(".", v)]]], levels = x$levels), tr_text(x$label, lang))
    }
    if (!is.null(d$.look)) out$look <- with_desc(as.integer(d$.look), gtext("look", lang))
    if (!is.null(d$.visit)) out$visit <- with_desc(as.integer(d$.visit), gtext("visit", lang))
    olab <- var_desc(oc, lang)
    if (design %in% CASE_DESIGNS) {
        lv <- c("control", "case")   # shown in the student's language by localise_data()
        out$case <- with_desc(factor(lv[d$.y + 1L], levels = lv), sprintf("%s: %s", gtext("case", lang), tr_text(oc$label, lang)))
        if (design == "nested_cc") out$time <- with_desc(round(d$.at, 3), sprintf("%s (%s)", gtext("settime", lang), oc$time_units))
    } else if (!is.null(d$.time)) {
        out$time <- with_desc(round(d$.time, 3), sprintf("%s (%s)", gtext("time", lang), oc$time_units))
        out[[oc$id]] <- with_desc(as.integer(d$.status), sprintf("%s: %s", tr_text(oc$label, lang), gtext("event", lang)))
    } else if (oc$type %in% c("binary", "event")) {
        out[[oc$id]] <- with_desc(factor(oc$levels[d$.y + 1L], levels = oc$levels), olab)
    } else if (oc$type == "count") {
        out[[oc$id]] <- with_desc(as.integer(d$.y), olab)
    } else {
        out[[oc$id]] <- with_desc(as.numeric(d$.y), olab)
    }
    for (v in factors) {
        vv <- sc$variables[[v]]
        out[[v]] <- with_desc(code_to_column(d[[v]], vv), var_desc(vv, lang))
    }
    df <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
    for (nm in names(out)) attr(df[[nm]], "jmv-desc") <- attr(out[[nm]], "jmv-desc")
    df
}

# df is the localised dataset, ids the engine's names of its columns (same order)
data_dictionary <- function(sc, df, factors, ids = names(df)) {
    rows <- lapply(seq_along(df), function(j) {
        x <- df[[j]]; id <- ids[j]
        role <- if (id %in% factors) "factor" else if (id %in% c("id", "set", "sequence", "period", "subcohort", "cluster", "look", "visit")) "design"
                else if (id %in% names(sc$interventions)) "intervention" else "outcome"
        type <- if (is.ordered(x)) "ordinal" else if (is.factor(x)) "nominal" else if (is.integer(x)) "integer" else "continuous"
        values <- if (is.factor(x)) paste(levels(x), collapse = ", ")
                  else sprintf("%s \u2013 %s", base::format(min(x, na.rm = TRUE)), base::format(max(x, na.rm = TRUE)))
        data.frame(id = id, name = names(df)[j], description = attr(x, "jmv-desc") %||% "", role = role, type = type,
                   values = values, missing = sum(is.na(x)), stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
}

# Missing values in the factor columns: MCAR, or MAR driven by the outcome
# (e.g. cases answer more completely), at an overall rate `pct` (0-1).
apply_missing <- function(df, cols, pct, mech, seed, driver = NULL, unit = df$id) {
    if (pct <= 0 || length(cols) == 0) return(df)
    ids <- unique(unit)
    first <- match(ids, unit)
    drv <- NULL
    if (mech == "mar" && !is.null(driver)) {
        drv <- as.numeric(df[[driver]][first])
        drv <- (drv - mean(drv, na.rm = TRUE)) / (stats::sd(drv, na.rm = TRUE) %||% 1)
        drv[is.na(drv)] <- 0
    }
    with_seed(seed, {
        for (v in cols) {
            p <- if (is.null(drv)) rep(pct, length(ids)) else {
                # odds of missingness x3 per SD of the outcome, calibrated to pct
                a <- stats::uniroot(function(a) mean(stats::plogis(a - log(3) * drv)) - pct, c(-20, 20))$root
                stats::plogis(a - log(3) * drv)
            }
            miss_ids <- ids[stats::runif(length(ids)) < p]
            keep <- attr(df[[v]], "jmv-desc")
            df[[v]][unit %in% miss_ids] <- NA
            attr(df[[v]], "jmv-desc") <- keep
        }
    })
    df
}

# The dataset's column names, known before any data are drawn (the results
# tables need their rows in .init()); mirrors format_study().
planned_columns <- function(sc, oc, design, factors, opt = list()) {
    iv <- if (design %in% c("rct", "crossover", "cluster_rct", "longitudinal")) trial_iv(sc, oc)$id
          else if (design == "factorial") names(factorial_ivs(sc, oc)) else character()
    looks <- design == "rct" && (opt$looks %||% 0) >= 2 && oc$type != "event"
    out <- c("id", if (design %in% c("matched_cc", "nested_cc")) "set", if (design == "case_cohort") "subcohort",
             if (design == "cluster_rct") "cluster", if (design == "crossover") c("sequence", "period"), iv,
             if (looks) "look", if (design == "longitudinal") "visit")
    out <- c(out, if (design %in% CASE_DESIGNS) c("case", if (design == "nested_cc") "time")
                  else if (oc$type == "event" && design %in% c("cohort", "prognosis", "rct", "case_cohort", "cluster_rct", "factorial")) c("time", oc$id)
                  else oc$id)
    c(out, intersect(sc$order, factors))
}

# What the descriptives table shows for each column, known before the data
# exist (rows are built in .init()): "cont", or the factor levels. id and set
# are identifiers and are skipped.
planned_kinds <- function(sc, oc, design, factors, lang = "en", categorize = list(), opt = list()) {
    cols <- planned_columns(sc, oc, design, factors, opt)
    out <- list()
    for (nm in setdiff(cols, c("id", "set", "cluster", "look", "visit"))) {
        out[[nm]] <- if (nm == "sequence") {
            lv <- trial_iv(sc, oc)$levels
            c(paste(lv, collapse = "-"), paste(rev(lv), collapse = "-"))
        } else if (nm == "period") c("1", "2")
        else if (nm %in% names(sc$interventions)) sc$interventions[[nm]]$levels
        else if (nm == "case") c("control", "case")
        else if (nm == "subcohort") c("no", "yes")
        else if (nm == "time") "cont"
        else if (nm == oc$id) {
            if ("time" %in% cols) c("0", "1") else if (oc$type %in% c("binary", "event")) oc$levels else "cont"
        } else {
            v <- sc$variables[[nm]]
            spec <- categorize[[nm]]
            if (!is.null(spec) && spec$form != "continuous" && v$type %in% c("continuous", "count"))
                structure(paste0(cat_prefix(spec), seq_len(cat_groups(spec))), dynamic = TRUE)
            else if (v$type %in% c("continuous", "count")) "cont" else v$levels
        }
    }
    out
}

# Univariate descriptives: mean (SD) and median [IQR] for numbers, n (%) per level.
# Rows keyed "<column>|<i>", as planned by planned_kinds().
describe_data <- function(df, kinds, lang = "en") {
    rows <- list()
    dec <- function(s) if (!is.finite(s) || s >= 10) 1 else if (s >= 1) 2 else 3
    for (nm in names(kinds)) {
        x <- df[[nm]]
        miss <- sum(is.na(x))
        if (identical(kinds[[nm]], "cont")) {
            x <- as.numeric(x[!is.na(x)])
            d <- dec(stats::sd(x))
            q <- stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
            f <- function(v) formatC(v, format = "f", digits = d)
            rows[[length(rows) + 1]] <- list(key = paste0(nm, "|1"), variable = nm, level = gtext("meansd", lang),
                                             stat = sprintf("%s (%s)", f(mean(x)), f(stats::sd(x))), missing = miss)
            rows[[length(rows) + 1]] <- list(key = paste0(nm, "|2"), variable = nm, level = gtext("mediqr", lang),
                                             stat = sprintf("%s [%s, %s]", f(q[2]), f(q[1]), f(q[3])), missing = NA)
        } else {
            # categorised variables: the group labels hold the sample's cut points
            lv <- if (isTRUE(attr(kinds[[nm]], "dynamic"))) levels(x) else kinds[[nm]]
            tab <- table(factor(as.character(x), levels = lv))
            n <- sum(tab)
            for (i in seq_along(lv))
                rows[[length(rows) + 1]] <- list(key = paste0(nm, "|", i), variable = nm, level = lv[i],
                                                 stat = sprintf("%d (%.1f%%)", tab[[i]], if (n) 100 * tab[[i]] / n else 0),
                                                 missing = if (i == 1) miss else NA)
        }
    }
    rows
}

# planned rows of the descriptives table (two per number, one per level),
# with the variable and level as the student sees them
describe_plan <- function(kinds, lang = "en") {
    rows <- list()
    for (nm in names(kinds)) {
        cont <- identical(kinds[[nm]], "cont")
        lv <- if (cont) c(gtext("meansd", lang), gtext("mediqr", lang)) else dlevel(kinds[[nm]], lang)
        for (i in seq_along(lv)) rows[[length(rows) + 1]] <- list(key = paste0(nm, "|", i), variable = dname(nm, lang), level = lv[i])
    }
    rows
}

# ---- categorising continuous variables -----------------------------------------

CAT_FORMS <- c("continuous", "median", "tertiles", "quartiles", "quintiles", "cuts")

# "18.5, 25, 30" -> values; "10%, 90%" -> percentiles; with a decimal comma,
# separate by ";" ("18,5; 25")
parse_cuts <- function(txt) {
    s <- trimws(as.character(txt %||% ""))
    if (!nzchar(s)) stop("cut points are empty: write e.g. 18.5, 25, 30 or 10%, 90%", call. = FALSE)
    parts <- if (grepl(";", s)) gsub(",", ".", strsplit(s, ";")[[1]]) else strsplit(s, "[,[:space:]]+")[[1]]
    parts <- trimws(parts[nzchar(trimws(parts))])
    pct <- grepl("%$", parts)
    if (any(pct) && !all(pct)) stop(sprintf("'%s': use either values or percentiles, not both", s), call. = FALSE)
    x <- suppressWarnings(as.numeric(sub("%$", "", parts)))
    if (anyNA(x)) stop(sprintf("'%s' is not a list of numbers", s), call. = FALSE)
    if (all(pct) && any(x <= 0 | x >= 100)) stop(sprintf("'%s': percentiles must be between 0 and 100", s), call. = FALSE)
    # labels keep the decimals the user typed (25 -> "25", 18.5 -> "18.5")
    dec <- max(0, nchar(sub("^[^.]*\\.?", "", sub("%$", "", parts))))
    list(type = if (all(pct)) "pct" else "value", x = sort(unique(x)), decimals = dec)
}

# number of groups a categorisation spec gives (known before the data exist)
cat_groups <- function(spec) switch(spec$form, median = 2L, tertiles = 3L, quartiles = 4L, quintiles = 5L,
                                    cuts = length(parse_cuts(spec$cuts)$x) + 1L, 1L)

cat_prefix <- function(spec) if (spec$form == "cuts" && parse_cuts(spec$cuts)$type == "value") "G" else "Q"

# x -> ordered factor "Q1: <=24.0", "Q2: 24.0-27.3", "Q3: >27.3" (cut points
# from the sample for quantiles and percentiles, as a study would compute them)
categorize_column <- function(x, spec, digits = NULL, integer = FALSE) {
    b <- switch(spec$form,
        median = stats::quantile(x, 0.5, na.rm = TRUE, names = FALSE),
        tertiles = stats::quantile(x, 1:2 / 3, na.rm = TRUE, names = FALSE),
        quartiles = stats::quantile(x, 1:3 / 4, na.rm = TRUE, names = FALSE),
        quintiles = stats::quantile(x, 1:4 / 5, na.rm = TRUE, names = FALSE),
        cuts = { cu <- parse_cuts(spec$cuts)
                 if (cu$type == "pct") stats::quantile(x, cu$x / 100, na.rm = TRUE, names = FALSE) else cu$x })
    b <- sort(unique(b))
    # decimals of the labels: as typed for value cut points, otherwise the
    # variable's own (none for counts and whole numbers)
    d <- if (spec$form == "cuts" && parse_cuts(spec$cuts)$type == "value") parse_cuts(spec$cuts)$decimals
         else if (integer || all(abs(x[is.finite(x)] - round(x[is.finite(x)])) < 1e-8)) 0
         else if (!is.null(digits)) digits else 2
    f <- function(v) formatC(round(v, d), format = "f", digits = d)
    p <- cat_prefix(spec)
    k <- length(b) + 1L
    lab <- vapply(seq_len(k), function(i)
        if (i == 1) sprintf("%s1: ≤%s", p, f(b[1]))
        else if (i == k) sprintf("%s%d: >%s", p, i, f(b[k - 1]))
        else sprintf("%s%d: %s–%s", p, i, f(b[i - 1]), f(b[i])), "")
    g <- findInterval(x, b, left.open = TRUE) + 1L
    factor(lab[g], levels = lab, ordered = TRUE)
}

# apply a list(id = list(form, cuts)) to a formatted study (engine names)
categorize_study <- function(df, sc, categorize, lang = "en") {
    for (v in intersect(names(categorize), names(df))) {
        spec <- categorize[[v]]
        if (spec$form == "continuous" || !sc$variables[[v]]$type %in% c("continuous", "count")) next
        desc <- attr(df[[v]], "jmv-desc")
        df[[v]] <- categorize_column(df[[v]], spec, sc$variables[[v]]$round, sc$variables[[v]]$type == "count")
        attr(df[[v]], "jmv-desc") <- sprintf("%s, %s", desc, gtext(paste0("cat_", spec$form), lang))
    }
    df
}

# answer-key rows of a categorised variable: each group against the first
cat_rows <- function(v, spec, lang = "en") {
    k <- cat_groups(spec); p <- cat_prefix(spec)
    data.frame(variable = v, contrast = sprintf("%s%d %s %s1", p, 2:k, gtext("vs", lang), p), idx = 2:k,
               stringsAsFactors = FALSE)
}
