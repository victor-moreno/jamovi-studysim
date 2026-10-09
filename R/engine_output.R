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
    meansd = list(en = "mean (SD)", es = "media (DE)", ca = "mitjana (DE)"),
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
        lv <- c("control", "case")   # shown in the student's language by localise_data()
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

# df is the localised dataset, ids the engine's names of its columns (same order)
data_dictionary <- function(sc, df, factors, ids = names(df)) {
    rows <- lapply(seq_along(df), function(j) {
        x <- df[[j]]; id <- ids[j]
        role <- if (id %in% factors) "factor" else if (id %in% c("id", "set", "sequence", "period")) "design"
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
planned_columns <- function(sc, oc, design, factors) {
    iv <- if (design %in% c("rct", "crossover")) trial_iv(sc, oc)$id else character()
    out <- c("id", if (design == "matched_cc") "set", if (design == "crossover") c("sequence", "period"), iv)
    out <- c(out, if (design %in% c("case_control", "matched_cc")) "case"
                  else if (oc$type == "event" && design %in% c("cohort", "prognosis", "rct")) c("time", oc$id)
                  else oc$id)
    c(out, intersect(sc$order, factors))
}

# What the descriptives table shows for each column, known before the data
# exist (rows are built in .init()): "cont", or the factor levels. id and set
# are identifiers and are skipped.
planned_kinds <- function(sc, oc, design, factors, lang = "en") {
    cols <- planned_columns(sc, oc, design, factors)
    out <- list()
    for (nm in setdiff(cols, c("id", "set"))) {
        out[[nm]] <- if (nm == "sequence") {
            lv <- trial_iv(sc, oc)$levels
            c(paste(lv, collapse = "-"), paste(rev(lv), collapse = "-"))
        } else if (nm == "period") c("1", "2")
        else if (nm %in% names(sc$interventions)) sc$interventions[[nm]]$levels
        else if (nm == "case") c("control", "case")
        else if (nm == "time") "cont"
        else if (nm == oc$id) {
            if ("time" %in% cols) c("0", "1") else if (oc$type %in% c("binary", "event")) oc$levels else "cont"
        } else {
            v <- sc$variables[[nm]]
            if (v$type %in% c("continuous", "count")) "cont" else v$levels
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
            lv <- kinds[[nm]]
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

# Node and edge coordinates of the scenario's causal diagram: layers by longest
# path along the structure edges, the outcome in the last layer, nodes ordered
# within a layer by the mean position of their parents (fewer crossings).
dag_layout <- function(sc, oc, factors, lang = "en") {
    vars <- sc$order
    ivs <- intersect(names(oc$effects), names(sc$interventions))
    depth <- stats::setNames(integer(length(vars)), vars)
    for (v in vars) {
        pa <- sc$edges$from[sc$edges$to == v]
        if (length(pa)) depth[v] <- max(depth[pa]) + 1L
    }
    nodes <- data.frame(id = c(ivs, vars, oc$id), layer = c(rep(0L, length(ivs)), depth, max(depth) + 1L),
                        stringsAsFactors = FALSE)
    nodes$kind <- ifelse(nodes$id == oc$id, "outcome", ifelse(nodes$id %in% ivs, "intervention",
                  ifelse(nodes$id %in% factors, "measured", "unmeasured")))
    nodes$null <- !nodes$id %in% c(names(oc$effects), oc$id)
    # initial order: null variables last; then barycentre sweeps (down by
    # parents, up by children other than the outcome) to reduce crossings
    parents <- function(v) if (v == oc$id) names(oc$effects) else sc$edges$from[sc$edges$to == v]
    children <- function(v) sc$edges$to[sc$edges$from == v]
    nodes$y <- 0
    place <- function(i) { k <- length(i); nodes$y[i] <<- if (k == 1) 0 else seq(1, -1, length.out = k) * (k - 1) / 2 }
    layers <- sort(unique(nodes$layer))
    for (L in layers) { i <- which(nodes$layer == L); place(i[order(nodes$null[i])]) }
    bary <- function(ids, nb) vapply(ids, function(v) {
        py <- nodes$y[nodes$id %in% nb(v)]
        if (length(py)) mean(py) else nodes$y[nodes$id == v] }, 0)
    for (sweep in 1:4) {
        for (L in layers[-1]) { i <- which(nodes$layer == L); place(i[order(-bary(nodes$id[i], parents))]) }
        for (L in rev(layers[-length(layers)])) {
            i <- which(nodes$layer == L)
            b <- bary(nodes$id[i], children)
            place(i[order(nodes$null[i], -b)])
        }
    }
    nodes$x <- nodes$layer
    nodes$label <- dname(nodes$id, lang)
    pos <- function(id) nodes[match(id, nodes$id), c("x", "y")]
    edges <- list()
    if (nrow(sc$edges)) {
        a <- pos(sc$edges$from); b <- pos(sc$edges$to)
        edges[[1]] <- data.frame(from = sc$edges$from, to = sc$edges$to, x = a$x, y = a$y, xend = b$x, yend = b$y,
                                 type = "path", label = sprintf("%.2f", sc$edges$b), dir = sign(sc$edges$b),
                                 stringsAsFactors = FALSE)
    }
    R <- sc$R_exo
    if (length(R) > 1) for (i in seq_len(nrow(R) - 1)) for (j in (i + 1):nrow(R)) if (R[i, j] != 0) {
        a <- pos(rownames(R)[i]); b <- pos(rownames(R)[j])
        edges[[length(edges) + 1]] <- data.frame(from = rownames(R)[i], to = rownames(R)[j], x = a$x, y = a$y,
                                                 xend = b$x, yend = b$y, type = "correlation",
                                                 label = sprintf("%.2f", R[i, j]), dir = sign(R[i, j]),
                                                 stringsAsFactors = FALSE)
    }
    eff <- names(oc$effects)
    if (length(eff)) {
        a <- pos(eff); b <- pos(oc$id)
        lab <- vapply(oc$effects, function(e) {
            v <- if (length(e$value) > 1) paste(signif(e$value[-1], 3), collapse = "/") else signif(e$value, 3)
            paste0(e$measure, " ", v, if (!is.null(e$by)) "*" else "")
        }, "")
        edges[[length(edges) + 1]] <- data.frame(from = eff, to = oc$id, x = a$x, y = a$y, xend = b$x, yend = b$y,
                                                 type = "effect", label = lab,
                                                 dir = vapply(oc$effects, function(e) sign(sum(e$coef)), 0),
                                                 stringsAsFactors = FALSE)
    }
    list(nodes = nodes, edges = if (length(edges)) do.call(rbind, edges) else NULL)
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
