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
    sequence = list(en = "Treatment sequence", es = "Secuencia de tratamiento", ca = "Seqüència de tractament"),
    period = list(en = "Period", es = "Periodo", ca = "Període"),
    id = list(en = "Participant identifier", es = "Identificador del participante", ca = "Identificador del participant"),
    per = list(en = "per", es = "por", ca = "per"),
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
    if (design == "crossover") {
        out$id <- with_desc(as.integer(factor(d$.pid, levels = unique(d$.pid))), gtext("id", lang))
    } else {
        out$id <- with_desc(seq_len(n), gtext("id", lang))
    }
    iv <- intersect(names(sc$interventions), sub("^\\.", "", names(d)))
    if (design == "matched_cc") out$set <- with_desc(as.integer(d$.set), gtext("set", lang))
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
    olab <- var_desc(oc, lang)
    if (design %in% c("case_control", "matched_cc")) {
        lv <- c(gtext("control", lang), gtext("caselevel", lang))
        out$case <- with_desc(factor(lv[d$.y + 1L], levels = lv), sprintf("%s: %s", gtext("case", lang), tr_text(oc$label, lang)))
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

data_dictionary <- function(sc, df, factors) {
    rows <- lapply(names(df), function(nm) {
        x <- df[[nm]]
        role <- if (nm %in% factors) "factor" else if (nm %in% c("id", "set", "sequence", "period")) "design"
                else if (nm %in% names(sc$interventions)) "intervention" else "outcome"
        type <- if (is.ordered(x)) "ordinal" else if (is.factor(x)) "nominal" else if (is.integer(x)) "integer" else "continuous"
        values <- if (is.factor(x)) paste(levels(x), collapse = ", ")
                  else sprintf("%s \u2013 %s", base::format(min(x, na.rm = TRUE)), base::format(max(x, na.rm = TRUE)))
        data.frame(name = nm, description = attr(x, "jmv-desc") %||% "", role = role, type = type,
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
planned_columns <- function(sc, oc, design, factors) {
    iv <- if (design %in% c("rct", "crossover")) trial_iv(sc, oc)$id else character()
    out <- c("id", if (design == "matched_cc") "set", if (design == "crossover") c("sequence", "period"), iv)
    out <- c(out, if (design %in% c("case_control", "matched_cc")) "case"
                  else if (oc$type == "event" && design %in% c("cohort", "prognosis", "rct")) c("time", oc$id)
                  else oc$id)
    c(out, intersect(sc$order, factors))
}
