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
    event2 = list(en = "1 = event, 2 = {c}, 0 = censored", es = "1 = evento, 2 = {c}, 0 = censurado", ca = "1 = esdeveniment, 2 = {c}, 0 = censurat"),
    set = list(en = "Matched set", es = "Grupo emparejado", ca = "Grup aparellat"),
    riskset = list(en = "Risk set (one case and its controls)", es = "Conjunto de riesgo (un caso y sus controles)",
                   ca = "Conjunt de risc (un cas i els seus controls)"),
    settime = list(en = "Time at which the set was sampled (the case's event time)", es = "Tiempo en que se muestreó el conjunto (el del evento del caso)",
                   ca = "Temps en què es va mostrejar el conjunt (el de l'esdeveniment del cas)"),
    subcohort = list(en = "Member of the random subcohort", es = "Miembro de la subcohorte aleatoria", ca = "Membre de la subcohort aleatòria"),
    region = list(en = "Region", es = "Región", ca = "Regió"),
    population = list(en = "People in the region", es = "Personas en la región", ca = "Persones a la regió"),
    cases_in = list(en = "Cases in {w} years", es = "Casos en {w} años", ca = "Casos en {w} anys"),
    mean_of = list(en = "Mean", es = "Media", ca = "Mitjana"),
    pct_of = list(en = "% with", es = "% con", ca = "% amb"),
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
    cat_scale = list(en = "divided by", es = "dividida por", ca = "dividida per"),
    before = list(en = "before", es = "antes", ca = "abans"),
    method_a = list(en = "method_a", es = "metodo_a", ca = "metode_a"),
    method_b = list(en = "method_b", es = "metodo_b", ca = "metode_b"),
    meas_before = list(en = "before the intervention", es = "antes de la intervención", ca = "abans de la intervenció"),
    meas_after = list(en = "after the intervention", es = "después de la intervención", ca = "després de la intervenció"),
    by_method = list(en = "measured by method", es = "medida con el método", ca = "mesurada amb el mètode"),
    change = list(en = "after - before", es = "después - antes", ca = "després - abans"),
    prepost_note = list(en = "mean change: the effect plus the natural trend and regression to the mean",
                        es = "cambio medio: el efecto más la tendencia natural y la regresión a la media",
                        ca = "canvi mitjà: l'efecte més la tendència natural i la regressió a la mitjana"),
    mc_diff = list(en = "mean difference (B - A)", es = "diferencia media (B - A)", ca = "diferència mitjana (B - A)"),
    mc_low = list(en = "lower limit of agreement", es = "límite de acuerdo inferior", ca = "límit d'acord inferior"),
    mc_high = list(en = "upper limit of agreement", es = "límite de acuerdo superior", ca = "límit d'acord superior"),
    mc_slope = list(en = "B per unit of A (regression)", es = "B por unidad de A (regresión)", ca = "B per unitat d'A (regressió)"),
    mc_note = list(en = "the regression slope is attenuated by A's own error; the true slope is per unit of the true value",
                   es = "la pendiente de regresión se atenúa por el error del propio A; la pendiente verdadera es por unidad del valor verdadero",
                   ca = "el pendent de regressió s'atenua per l'error del mateix A; el pendent veritable és per unitat del valor veritable"),
    batch = list(en = "Assay batch", es = "Lote de análisis", ca = "Lot d'anàlisi"),
    lod_missing = list(en = "missing", es = "falta", ca = "falta"),
    after_x = list(en = "after %s", es = "tras %s", ca = "després de %s"),
    without_x = list(en = "without %s", es = "sin %s", ca = "sense %s"),
    illness_death = list(en = "death after a first event (illness-death): no single true hazard ratio; see the transitions in the true model",
                         es = "muerte tras un primer evento (enfermedad-muerte): no hay un único hazard ratio verdadero; vea las transiciones en el modelo verdadero",
                         ca = "mort després d'un primer esdeveniment (malaltia-mort): no hi ha un únic hazard ratio veritable; vegeu les transicions al model veritable"),
    extra = list(en = "Extra variable", es = "Variable adicional", ca = "Variable addicional"),
    cat_log2 = list(en = "log2", es = "log2", ca = "log2"),
    cat_log10 = list(en = "log10", es = "log10", ca = "log10"),
    per_log2 = list(en = "per doubling", es = "por cada duplicación", ca = "per cada duplicació"),
    per_log10 = list(en = "per 10-fold increase", es = "por cada aumento de 10 veces", ca = "per cada augment de 10 vegades"),
    transformed = list(en = "log-transformed; the true effect is per unit (see true model)",
                       es = "transformada a logaritmo; el efecto verdadero es por unidad (ver modelo verdadero)",
                       ca = "transformada a logaritme; l'efecte veritable és per unitat (vegeu model veritable)"),
    categorised = list(en = "categorised; the true effect is per unit (see true model)",
                       es = "categorizada; el efecto verdadero es por unidad (ver modelo verdadero)",
                       ca = "categoritzada; l'efecte veritable és per unitat (vegeu model veritable)"),
    not_est_matched = list(en = "not estimable: matched on it", es = "no estimable: es variable de emparejamiento",
                           ca = "no estimable: és variable d'aparellament"),
    not_est_alias = list(en = "not estimable: determined by other measured variables (e.g. derived from the same one)",
                         es = "no estimable: la determinan otras variables medidas (p. ej. derivadas de la misma)",
                         ca = "no estimable: la determinen altres variables mesurades (p. ex. derivades de la mateixa)"),
    log_zero = list(en = "the outcome has zeros: analysed as log(y + %s), which pulls the ratio towards 1",
                    es = "el resultado tiene ceros: analizado como log(y + %s), lo que acerca la razón a 1",
                    ca = "el resultat té zeros: analitzat com a log(y + %s), cosa que acosta la raó a 1"),
    by = list(en = "by", es = "a", ca = "a"),
    risk = list(en = "Risk", es = "Riesgo", ca = "Risc"),
    rd = list(en = "Risk difference", es = "Diferencia de riesgos", ca = "Diferència de riscos"),
    nnt = list(en = "Number needed to treat for one event fewer", es = "Número necesario a tratar para un evento menos", ca = "Nombre necessari a tractar per a un esdeveniment menys"),
    nnh = list(en = "Number needed to treat for one event more", es = "Número necesario a tratar para un evento más", ca = "Nombre necessari a tractar per a un esdeveniment més"),
    rr = list(en = "Risk ratio", es = "Razón de riesgos", ca = "Raó de riscos"),
    or_marg = list(en = "Odds ratio, marginal (population)", es = "Odds ratio marginal (poblacional)", ca = "Odds ratio marginal (poblacional)"),
    or_cond = list(en = "Odds ratio, conditional (true model)", es = "Odds ratio condicional (modelo verdadero)", ca = "Odds ratio condicional (model veritable)"),
    hr_marg = list(en = "Hazard ratio, marginal (population)", es = "Hazard ratio marginal (poblacional)", ca = "Hazard ratio marginal (poblacional)"),
    hr_cond = list(en = "Hazard ratio, conditional (true model)", es = "Hazard ratio condicional (modelo verdadero)", ca = "Hazard ratio condicional (model veritable)"),
    null_var = list(en = "no effect (null variable)", es = "sin efecto (variable nula)", ca = "sense efecte (variable nul·la)"),
    no_direct = list(en = "no direct effect; acts through %s", es = "sin efecto directo; actúa a través de %s", ca = "sense efecte directe; actua a través de %s"),
    also_through = list(en = "also acts through %s", es = "también actúa a través de %s", ca = "també actua a través de %s"),
    at_center = list(en = "at %s = %s", es = "con %s = %s", ca = "amb %s = %s"),
    mediqr = list(en = "median [IQR]", es = "mediana [RIC]", ca = "mediana [RIQ]"),
    vs = list(en = "vs", es = "frente a", ca = "enfront de"))

gtext <- function(key, lang) tr_text(GENERIC[[key]], lang)

# time units in the student's language (the library writes them in English)
TIME_UNITS <- list(years = list(en = "years", es = "años", ca = "anys"), months = list(en = "months", es = "meses", ca = "mesos"),
                   weeks = list(en = "weeks", es = "semanas", ca = "setmanes"), days = list(en = "days", es = "días", ca = "dies"))
unit_text <- function(u, lang) if (is.null(TIME_UNITS[[u %||% "years"]])) u else tr_text(TIME_UNITS[[u %||% "years"]], lang)
# "per year" in the student's language, for rates
TIME_PER <- list(years = list(en = "per year", es = "al año", ca = "l'any"), months = list(en = "per month", es = "al mes", ca = "al mes"),
                 weeks = list(en = "per week", es = "por semana", ca = "per setmana"), days = list(en = "per day", es = "al día", ca = "al dia"))
per_unit_text <- function(u, lang) if (is.null(TIME_PER[[u %||% "years"]])) paste("/", u) else tr_text(TIME_PER[[u %||% "years"]], lang)

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

# One row per region: totals, the outcome, and the region's mean or percentage
# of each measured variable. A per-level column keeps its variable and level
# in an attribute, so localise_data() can translate its name.
format_ecological <- function(sc, oc, res, factors, lang = "en") {
    d <- res$data
    out <- list(region = with_desc(as.integer(d$.region), gtext("region", lang)),
                population = with_desc(as.integer(d$.pop), gtext("population", lang)))
    out[[oc$id]] <- if (oc$type %in% c("binary", "event"))
        with_desc(as.integer(d$.y), sprintf("%s: %s", tr_text(oc$label, lang), sub("{w}", base::format(oc$window), gtext("cases_in", lang), fixed = TRUE)))
    else with_desc(round(d$.y, (oc$round %||% 2) + 1), sprintf("%s: %s", gtext("mean_of", lang), var_desc(oc, lang)))
    for (v in factors) {
        x <- sc$variables[[v]]
        if (x$type %in% c("continuous", "count")) {
            out[[v]] <- with_desc(round(d[[v]], (x$round %||% 1) + 1), sprintf("%s: %s", gtext("mean_of", lang), var_desc(x, lang)))
        } else if (x$type == "binary") {
            out[[v]] <- with_desc(round(100 * d[[v]], 1), sprintf("%s %s: %s", gtext("pct_of", lang), dlevel(x$levels[2], lang), tr_text(x$label, lang)))
        } else for (k in seq_along(x$levels)[-1]) {
            nm <- eco_col(v, x$levels[k])
            col <- with_desc(round(100 * d[[nm]], 1), sprintf("%s %s: %s", gtext("pct_of", lang), dlevel(x$levels[k], lang), tr_text(x$label, lang)))
            attr(col, "eco") <- c(v, x$levels[k])
            out[[nm]] <- col
        }
    }
    df <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
    for (nm in names(out)) for (a in c("jmv-desc", "eco")) attr(df[[nm]], a) <- attr(out[[nm]], a)
    df
}

# res: list(data, facts) from a design; factors: the scenario variables to show
format_study <- function(sc, oc, design, res, factors, lang = "en") {
    if (design == "ecological") return(format_ecological(sc, oc, res, factors, lang))
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
    # before-after and method comparison: two measurements of the outcome's quantity
    if (design == "pre_post") {
        out[[paste0(oc$id, "_before")]] <- structure(with_desc(as.numeric(d$.y0), sprintf("%s, %s", olab, gtext("meas_before", lang))), rename = c(oc$id, "before"))
        out[[oc$id]] <- with_desc(as.numeric(d$.y), sprintf("%s, %s", olab, gtext("meas_after", lang)))
    } else if (design == "method_comparison") {
        for (m in c("a", "b")) out[[paste0(oc$id, "_", m)]] <- structure(with_desc(as.numeric(d[[paste0(".m", m)]]),
            sprintf("%s, %s %s", olab, gtext("by_method", lang), toupper(m))), rename = c(oc$id, m))
    } else
    if (design %in% CASE_DESIGNS) {
        lv <- c("control", "case")   # shown in the student's language by localise_data()
        out$case <- with_desc(factor(lv[d$.y + 1L], levels = lv), sprintf("%s: %s", gtext("case", lang), tr_text(oc$label, lang)))
        if (design == "nested_cc") out$time <- with_desc(round(d$.at, 3), sprintf("%s (%s)", gtext("settime", lang), unit_text(oc$time_units, lang)))
    } else if (!is.null(d$.time)) {
        out$time <- with_desc(round(d$.time, 3), sprintf("%s (%s)", gtext("time", lang), unit_text(oc$time_units, lang)))
        ev <- if (is.null(oc$competing) || design %in% c("case_cohort")) gtext("event", lang)
              else sub("{c}", tr_text(oc$competing$label, lang), gtext("event2", lang), fixed = TRUE)
        out[[oc$id]] <- with_desc(as.integer(d$.status), sprintf("%s: %s", tr_text(oc$label, lang), ev))
    } else if (oc$type %in% c("binary", "event")) {
        out[[oc$id]] <- with_desc(factor(oc$levels[d$.y + 1L], levels = oc$levels), olab)
    } else if (oc$type == "count") {
        out[[oc$id]] <- with_desc(as.integer(d$.y), olab)
    } else {
        out[[oc$id]] <- with_desc(as.numeric(d$.y), olab)
    }
    # the scenario's other outcomes for the same people (Simulated study option)
    for (o in res$secondary) {
        o2 <- sc$outcomes[[o]]
        st <- d[[paste0(".sec_", o)]]
        if (!is.null(d[[paste0(".sec_time_", o)]])) {
            out[[paste0("time_", o)]] <- with_desc(round(d[[paste0(".sec_time_", o)]], 3), sprintf("%s (%s): %s", gtext("time", lang), unit_text(o2$time_units, lang), tr_text(o2$label, lang)))
            ev <- if (is.null(o2$competing)) gtext("event", lang) else sub("{c}", tr_text(o2$competing$label, lang), gtext("event2", lang), fixed = TRUE)
            out[[o]] <- with_desc(as.integer(st), sprintf("%s: %s", tr_text(o2$label, lang), ev))
        } else if (o2$type %in% c("binary", "event")) out[[o]] <- with_desc(factor(o2$levels[st + 1L], levels = o2$levels), var_desc(o2, lang))
        else if (o2$type == "count") out[[o]] <- with_desc(as.integer(st), var_desc(o2, lang))
        else out[[o]] <- with_desc(as.numeric(st), var_desc(o2, lang))
    }
    for (m in oc$markers) if (!is.null(res$markers)) for (nm in marker_cols(m))
        out[[nm]] <- with_desc(res$markers[[nm]], var_desc(m, lang))
    for (v in factors) {
        vv <- sc$variables[[v]]
        out[[v]] <- with_desc(code_to_column(d[[v]], vv), var_desc(vv, lang))
    }
    df <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
    for (nm in names(out)) for (a in c("jmv-desc", "rename")) attr(df[[nm]], a) <- attr(out[[nm]], a)
    df
}

# df is the localised dataset, ids the engine's names of its columns (same order)
data_dictionary <- function(sc, df, factors, ids = names(df)) {
    rows <- lapply(seq_along(df), function(j) {
        x <- df[[j]]; id <- ids[j]
        role <- if (id %in% factors || !is.null(attr(x, "eco")) || grepl("^extra_[0-9]+$", id)) "factor"
                else if (id %in% c("id", "set", "sequence", "period", "subcohort", "cluster", "look", "visit", "region", "population", "batch")) "design"
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
    own <- mech == "mnar"   # missing not at random: driven by each variable's own value
    if (mech == "mar" && !is.null(driver)) {
        drv <- as.numeric(df[[driver]][first])
        drv <- (drv - mean(drv, na.rm = TRUE)) / (stats::sd(drv, na.rm = TRUE) %||% 1)
        drv[is.na(drv)] <- 0
    }
    with_seed(seed, {
        for (v in cols) {
            if (own) {
                drv <- as.numeric(df[[v]][first])
                drv <- (drv - mean(drv, na.rm = TRUE)) / (stats::sd(drv, na.rm = TRUE) %||% 1)
                drv[!is.finite(drv)] <- 0
                drv <- -drv   # higher values more often missing (sign below)
            }
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
    if (design == "ecological")
        return(c("region", "population", oc$id, unlist(lapply(intersect(sc$order, factors), function(v) eco_cols(sc, v)))))
    iv <- if (design %in% c("rct", "crossover", "cluster_rct", "longitudinal")) trial_iv(sc, oc)$id
          else if (design == "factorial") names(factorial_ivs(sc, oc)) else character()
    looks <- design == "rct" && (opt$looks %||% 0) >= 2 && oc$type != "event"
    out <- c("id", if (design %in% c("matched_cc", "nested_cc")) "set", if (design == "case_cohort") "subcohort",
             if (design == "cluster_rct") "cluster", if (design == "crossover") c("sequence", "period"), iv,
             if (looks) "look", if (design == "longitudinal") "visit")
    out <- c(out, if (design == "pre_post") c(paste0(oc$id, "_before"), oc$id)
                  else if (design == "method_comparison") paste0(oc$id, c("_a", "_b"))
                  else if (design %in% CASE_DESIGNS) c("case", if (design == "nested_cc") "time")
                  else if (oc$type == "event" && design %in% c("cohort", "prognosis", "rct", "case_cohort", "cluster_rct", "factorial")) c("time", oc$id)
                  else oc$id)
    if (isTRUE(opt$secondary)) out <- c(out, secondary_columns(sc, oc, design))
    if (design == "diagnostic") out <- c(out, unname(unlist(lapply(oc$markers, marker_cols))), if ((opt$batches %||% 0) >= 2) "batch")
    c(out, intersect(sc$order, factors), noise_names(opt$noise %||% 0))
}

# What the descriptives table shows for each column, known before the data
# exist (rows are built in .init()): "cont", or the factor levels. id and set
# are identifiers and are skipped.
planned_kinds <- function(sc, oc, design, factors, lang = "en", categorize = list(), opt = list()) {
    cols <- planned_columns(sc, oc, design, factors, opt)
    out <- list()
    iv_pp <- if (design == "pre_post") trial_iv(sc, oc)$id else character()   # everybody treated: not described
    for (nm in setdiff(cols, c("id", "set", "cluster", "look", "visit", "region", "population", "batch", iv_pp))) {
        if (design == "ecological") {
            # region totals, means and percentages; a categorised mean has groups
            spec <- categorize[[nm]]
            out[[nm]] <- if (is_grouping(spec) && !is.null(sc$variables[[nm]]) &&
                             sc$variables[[nm]]$type %in% c("continuous", "count"))
                structure(paste0(cat_prefix(spec), seq_len(cat_groups(spec))), dynamic = TRUE) else "cont"
            next
        }
        out[[nm]] <- if (nm == "sequence") {
            lv <- trial_iv(sc, oc)$levels
            c(paste(lv, collapse = "-"), paste(rev(lv), collapse = "-"))
        } else if (nm == "period") c("1", "2")
        else if (nm %in% names(sc$interventions)) sc$interventions[[nm]]$levels
        else if (nm == "case") c("control", "case")
        else if (nm == "subcohort") c("no", "yes")
        else if (grepl("^time_", nm) && sub("^time_", "", nm) %in% names(sc$outcomes)) "cont"
        else if (nm %in% setdiff(names(sc$outcomes), oc$id)) {
            o2 <- sc$outcomes[[nm]]
            if (paste0("time_", nm) %in% cols) { if (is.null(o2$competing)) c("0", "1") else c("0", "1", "2") }
            else if (o2$type %in% c("binary", "event")) o2$levels else "cont"
        }
        else if (grepl("^extra_[0-9]+$", nm)) { if (as.integer(sub("extra_", "", nm)) %% 2 == 1) "cont" else c("no", "yes") }
        else if (nm == "time" || nm %in% unlist(lapply(oc$markers, marker_cols)) || nm %in% paste0(oc$id, c("_before", "_a", "_b"))) "cont"
        else if (nm == oc$id) {
            if ("time" %in% cols) { if (!is.null(oc$competing) && design != "case_cohort") c("0", "1", "2") else c("0", "1") }
            else if (oc$type %in% c("binary", "event")) oc$levels else "cont"
        } else {
            v <- sc$variables[[nm]]
            spec <- categorize[[nm]]
            if (is_grouping(spec) && v$type %in% c("continuous", "count"))
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

CAT_FORMS <- c("continuous", "median", "tertiles", "quartiles", "quintiles", "cuts", "log2", "log10", "scale")
LOG_FORMS <- c("log2", "log10")

# a spec that groups the variable (not continuous, not a transform)
is_grouping <- function(spec) !is.null(spec) && !spec$form %in% c("continuous", LOG_FORMS, "scale")

# the divisor of the scale form, typed in the Cuts column ("10", "2,5")
scale_of <- function(spec) {
    k <- suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(spec$cuts %||% "")))))
    if (length(k) != 1 || is.na(k) || k <= 0) stop("write the divisor in Cuts, a positive number such as 10", call. = FALSE)
    k
}

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
                                    scale = { scale_of(spec); 1L },
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
        if (spec$form == "scale") {
            # in units of k: an effect per unit of the new column is per k original units
            k <- scale_of(spec)
            df[[v]] <- round(df[[v]] / k, 6)
            attr(df[[v]], "jmv-desc") <- sprintf("%s, %s %s", desc, gtext("cat_scale", lang), base::format(k))
            next
        }
        if (spec$form %in% LOG_FORMS) {
            # log2 or log10; with zeros or negatives, of x + 1
            x <- df[[v]]
            shift <- if (any(x <= 0, na.rm = TRUE)) 1 else 0
            df[[v]] <- round(if (spec$form == "log2") log2(x + shift) else log10(x + shift), 4)
            attr(df[[v]], "jmv-desc") <- sprintf("%s, %s%s", desc, gtext(paste0("cat_", spec$form), lang), if (shift) "(x + 1)" else "")
            next
        }
        df[[v]] <- categorize_column(df[[v]], spec, sc$variables[[v]]$round, sc$variables[[v]]$type == "count")
        attr(df[[v]], "jmv-desc") <- sprintf("%s, %s", desc, gtext(paste0("cat_", spec$form), lang))
    }
    df
}

# answer-key rows of a categorised variable: each group against the first
cat_rows <- function(v, spec, lang = "en", sc = NULL) {
    # a scaled variable keeps one row, per k of its units
    if (spec$form == "scale") {
        u <- sc$variables[[v]]$units
        return(data.frame(variable = v, contrast = sprintf("%s %s%s", gtext("per", lang), base::format(scale_of(spec)),
                                                           if (is.null(u)) "" else paste0(" ", tr_text(u, lang))),
                          idx = NA_integer_, stringsAsFactors = FALSE))
    }
    # a log transform keeps one row: per doubling or per 10-fold increase
    if (spec$form %in% LOG_FORMS)
        return(data.frame(variable = v, contrast = gtext(paste0("per_", spec$form), lang), idx = NA_integer_, stringsAsFactors = FALSE))
    k <- cat_groups(spec); p <- cat_prefix(spec)
    data.frame(variable = v, contrast = sprintf("%s%d %s %s1", p, 2:k, gtext("vs", lang), p), idx = 2:k,
               stringsAsFactors = FALSE)
}
