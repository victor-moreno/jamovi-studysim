# A scenario as read from the library (a nested list, YAML-shaped) is turned
# into the engine's normalised form: variables with parsed marginals, a
# topological order, the implied latent correlation matrix, and per-outcome
# effects as coefficients on the linear-predictor scale.
#
# Problems are collected, not thrown one at a time, so the library manager can
# list them all; normalize_scenario() stops only if asked to (strict = TRUE).

VAR_TYPES <- c("continuous", "count", "binary", "nominal", "ordinal", "genotype")
OUTCOME_TYPES <- c("binary", "event", "continuous", "count")
MEASURES <- c(OR = "log", HR = "log", RR = "log", ratio = "log", beta = "identity")

# pick a label in `lang` from a string or a list(en =, es =, ...)
tr_text <- function(x, lang = "en") {
    if (is.null(x)) return("")
    if (is.list(x)) {
        v <- x[[lang]] %||% x[["en"]] %||% x[[1]]
        return(as.character(v))
    }
    as.character(x)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# override whole fields (a range replaces a range), unlike modifyList, which
# would merge two unnamed lists element by element
shallow_merge <- function(a, b) { a[names(b)] <- b; a }

# design options from the library: numbers, except the matching variables
normalize_design_defaults <- function(x, pr) {
    out <- list()
    for (d in names(x)) {
        if (!d %in% DESIGNS) { add_problem(pr, "designs", sprintf("unknown design '%s' (known: %s)", d, paste(DESIGNS, collapse = ", "))); next }
        out[[d]] <- list()
        for (k in names(x[[d]])) {
            v <- unlist(x[[d]][[k]])
            out[[d]][[k]] <- if (k == "match") as.character(v) else try_at(pr, sprintf("designs: %s %s", d, k), num_expr(v[1]))
        }
    }
    out
}

new_problems <- function() {
    e <- new.env()
    e$items <- data.frame(where = character(), level = character(), message = character(),
                          stringsAsFactors = FALSE)
    e
}

add_problem <- function(pr, where, message, level = "error") {
    pr$items[nrow(pr$items) + 1, ] <- list(where, level, message)
    invisible(NULL)
}

# run expr; a condition becomes a problem located at `where`
try_at <- function(pr, where, expr) {
    tryCatch(expr, error = function(e) { add_problem(pr, where, conditionMessage(e)); NULL })
}

num_vec <- function(x) vapply(as.list(unlist(x)), num_expr, numeric(1))

normalize_variable <- function(id, v, dict, pr) {
    where <- sprintf("variable '%s'", id)
    if (!is.null(dict[[id]]) && (isTRUE(v$from == "dictionary") || is.null(v$type)))
        v <- shallow_merge(dict[[id]], v[setdiff(names(v), "from")])
    v$from <- NULL
    if (is.null(v$type) && !is.null(v$dist)) v$type <- "continuous"
    if (is.null(v$type) || !v$type %in% VAR_TYPES) {
        add_problem(pr, where, sprintf("type must be one of %s%s", paste(VAR_TYPES, collapse = ", "),
                                       if (is.null(dict[[id]])) " (it is not in the shared dictionary either)" else ""))
        return(NULL)
    }
    out <- list(id = id, type = v$type, label = v$label %||% id, description = v$description,
                units = v$units, round = v$round)
    if (!is.null(v$range)) out$range <- try_at(pr, where, num_vec(v$range))
    # rules (engine_rules.R), parsed once all the variables are known
    out$rules_raw <- list(formula = v$formula, only_if = v$only_if, p = v$p)
    if (!is.null(v$only_if))
        out$otherwise <- if (v$type %in% c("continuous", "count")) try_at(pr, where, num_expr(v$otherwise %||% 0))
                         else if (!is.null(v$otherwise)) as.character(unlist(v$otherwise))
    if (!is.null(v$formula) && v$type %in% c("continuous", "count", "binary")) {
        if (v$type == "binary") out$levels <- as.character(unlist(v$levels %||% c("no", "yes")))
        return(out)
    }
    if (v$type %in% c("continuous", "count")) {
        if (is.null(v$dist)) { add_problem(pr, where, "needs a dist, e.g. normal(25, 4), or a formula"); return(NULL) }
        out$dist <- try_at(pr, where, parse_dist(v$dist))
        if (is.null(out$dist)) return(NULL)
        if (v$type == "count" && !out$dist$family %in% c("poisson", "negbin"))
            add_problem(pr, where, "a count needs poisson() or negbin()")
        if (!is.null(out$range) && !is.null(out$dist))
            try_at(pr, where, qdist(0.5, out$dist, out$range))
    } else if (v$type == "binary") {
        # p is a number, or an expression of other variables (a rule)
        out$p <- tryCatch(num_expr(v$p %||% NA), error = function(e) NULL)
        if (is.null(out$p) && is.character(v$p)) out$p <- 0.5
        else if (is.null(out$p) || out$p <= 0 || out$p >= 1) {
            add_problem(pr, where, "needs p, a proportion between 0 and 1, or an expression such as plogis((age - 51) / 1.5)"); return(NULL) }
        out$levels <- as.character(unlist(v$levels %||% c("no", "yes")))
        if (length(out$levels) != 2) add_problem(pr, where, "a binary variable has exactly 2 levels")
    } else if (v$type %in% c("nominal", "ordinal")) {
        out$levels <- as.character(unlist(v$levels))
        out$probs <- try_at(pr, where, num_vec(v$probs))
        if (length(out$levels) < 2 || length(out$probs) != length(out$levels)) {
            add_problem(pr, where, "needs levels and probs of the same length (at least 2)"); return(NULL) }
        if (any(out$probs <= 0) || abs(sum(out$probs) - 1) > 0.01)
            add_problem(pr, where, sprintf("probs must be > 0 and add up to 1 (they add up to %.3f)", sum(out$probs)))
        out$probs <- out$probs / sum(out$probs)
    } else if (v$type == "genotype") {
        out$maf <- try_at(pr, where, num_expr(v$maf %||% NA))
        if (is.null(out$maf) || out$maf <= 0 || out$maf > 0.5) {
            add_problem(pr, where, "needs maf, the minor allele frequency (0 to 0.5)"); return(NULL) }
        al <- as.character(unlist(v$alleles %||% c("A", "B")))
        out$levels <- c(paste0(al[1], al[1]), paste0(al[1], al[2]), paste0(al[2], al[2]))
        q <- out$maf
        out$probs <- c((1 - q)^2, 2 * q * (1 - q), q^2)
    }
    out
}

# Kahn's algorithm over the structure edges; NULL on a cycle
topo_order <- function(ids, edges) {
    indeg <- stats::setNames(integer(length(ids)), ids)
    for (t in edges$to) indeg[t] <- indeg[t] + 1L
    out <- character(); queue <- ids[indeg == 0]
    while (length(queue)) {
        v <- queue[1]; queue <- queue[-1]; out <- c(out, v)
        for (t in edges$to[edges$from == v]) {
            indeg[t] <- indeg[t] - 1L
            if (indeg[t] == 0) queue <- c(queue, t)
        }
    }
    if (length(out) < length(ids)) NULL else out
}

# Positive-definite repair of a correlation matrix by clipping eigenvalues.
near_pd <- function(R, eps = 1e-6) {
    e <- eigen(R, symmetric = TRUE)
    if (min(e$values) > eps) return(R)
    v <- pmax(e$values, eps)
    R2 <- e$vectors %*% diag(v, length(v)) %*% t(e$vectors)
    d <- sqrt(diag(R2))
    R2 <- R2 / outer(d, d)
    dimnames(R2) <- dimnames(R)
    R2
}

# Latent model: exogenous Z ~ MVN(0, R_exo); endogenous Z_j = b'Z_parents + e_j,
# with var(Z_j) = 1. Returns the implied correlation matrix and residual sds.
implied_latent <- function(order, edges, R_exo, pr = NULL) {
    p <- length(order)
    R <- diag(p); dimnames(R) <- list(order, order)
    ex <- rownames(R_exo)
    R[ex, ex] <- R_exo
    resid <- stats::setNames(rep(1, p), order)
    for (j in seq_along(order)) {
        v <- order[j]
        pe <- edges[edges$to == v, , drop = FALSE]
        if (nrow(pe) == 0) next
        b <- pe$b; P <- pe$from
        expl <- as.numeric(t(b) %*% R[P, P, drop = FALSE] %*% b)
        if (expl >= 0.99) {
            if (!is.null(pr)) add_problem(pr, sprintf("structure into '%s'", v), sprintf(
                "the edges into '%s' explain %.0f%% of its variance; strengths must leave room for noise (lower them)",
                v, 100 * expl))
            b <- b * sqrt(0.9 / expl); expl <- 0.9
            edges$b[edges$to == v] <- b
        }
        resid[v] <- sqrt(1 - expl)
        earlier <- order[seq_len(j - 1)]
        cv <- as.numeric(t(b) %*% R[P, earlier, drop = FALSE])
        R[v, earlier] <- cv; R[earlier, v] <- cv
    }
    list(R = R, resid = resid, edges = edges)
}

normalize_effect <- function(oc_id, var_id, e, vars, interventions, pr) {
    where <- sprintf("outcome '%s', effect of '%s'", oc_id, var_id)
    meas <- intersect(names(e), names(MEASURES))
    if (length(meas) != 1) {
        add_problem(pr, where, "give exactly one of OR, HR, RR, ratio or beta"); return(NULL) }
    v <- vars[[var_id]]
    iv <- interventions[[var_id]]
    if (is.null(v) && is.null(iv)) {
        # a declared variable that failed already has its own message
        if (var_id %in% pr$declared) return(NULL)
        add_problem(pr, where, sprintf("'%s' is not a variable or intervention of this scenario", var_id)); return(NULL) }
    vals <- try_at(pr, where, num_vec(e[[meas]]))
    if (is.null(vals)) return(NULL)
    log_scale <- MEASURES[[meas]] == "log"
    if (log_scale && any(vals <= 0)) { add_problem(pr, where, sprintf("%s must be > 0", meas)); return(NULL) }
    tf <- function(x) if (log_scale) log(x) else x
    levels <- if (!is.null(iv)) iv$levels else v$levels
    type <- if (!is.null(iv)) "intervention" else v$type
    out <- list(var = var_id, measure = meas, log = log_scale, per = 1, model = "additive", by = NULL,
                type = type, value = vals)
    if (type %in% c("nominal", "ordinal", "intervention")) {
        K <- length(levels)
        if (length(vals) == K - 1) vals <- c(if (log_scale) 1 else 0, vals)
        if (length(vals) != K) {
            add_problem(pr, where, sprintf("needs %d values, one per level (%s), the first being the reference",
                                           K, paste(levels, collapse = ", "))); return(NULL) }
        out$coef <- tf(vals) - tf(vals[1])
    } else {
        if (length(vals) != 1) { add_problem(pr, where, "needs a single value"); return(NULL) }
        out$coef <- tf(vals)
        if (!is.null(e$per)) out$per <- try_at(pr, where, num_expr(sub("^\\s*([-0-9.eE+/*()]+).*$", "\\1", as.character(e$per)))) %||% 1
        if (type == "genotype") {
            out$model <- e$model %||% "additive"
            if (!out$model %in% c("additive", "dominant", "recessive"))
                add_problem(pr, where, "model must be additive, dominant or recessive")
        }
    }
    if (!is.null(e$by)) {
        mod <- names(e$by)[1]
        mv <- vars[[mod]]
        if (is.null(mv) || is.null(mv$levels)) {
            add_problem(pr, where, sprintf("modifier '%s' must be a categorical variable of the scenario", mod))
        } else if (!type %in% c("continuous", "count", "binary", "genotype")) {
            add_problem(pr, where, "effect modification (by:) is supported for numeric, binary and genotype effects")
        } else {
            bv <- e$by[[mod]]
            co <- rep(out$coef, length(mv$levels)); names(co) <- mv$levels
            for (lv in names(bv)) {
                if (!lv %in% mv$levels) { add_problem(pr, where, sprintf("'%s' is not a level of '%s'", lv, mod)); next }
                x <- num_expr(bv[[lv]])
                co[lv] <- tf(x)
            }
            out$by <- list(var = mod, coef = unname(co), binary = mv$type == "binary")
        }
    }
    out
}

normalize_outcome <- function(id, o, vars, interventions, pr) {
    where <- sprintf("outcome '%s'", id)
    if (is.null(o$type) || !o$type %in% OUTCOME_TYPES) {
        add_problem(pr, where, sprintf("type must be one of %s", paste(OUTCOME_TYPES, collapse = ", "))); return(NULL) }
    out <- list(id = id, type = o$type, label = o$label %||% id, description = o$description,
                units = o$units, round = o$round, designs = unlist(o$designs),
                window = num_expr(o$window %||% 10), followup = num_expr(o$followup %||% (o$window %||% 10)))
    g <- function(x) try_at(pr, where, num_expr(x))
    if (o$type == "binary") {
        out$prevalence <- g(o$prevalence %||% NA)
        if (is.null(out$prevalence) || out$prevalence <= 0 || out$prevalence >= 1)
            add_problem(pr, where, "a binary outcome needs prevalence (0 to 1)")
        out$levels <- as.character(unlist(o$levels %||% c("no", "yes")))
    } else if (o$type == "event") {
        out$shape <- g(o$shape %||% 1)
        out$levels <- as.character(unlist(o$levels %||% c("no", "yes")))
        if (!is.null(o$rate)) {
            out$target <- list(kind = "rate", value = g(o$rate))
        } else if (!is.null(o$risk)) {
            out$target <- list(kind = "risk", value = g(o$risk$value), at = g(o$risk$at))
        } else if (!is.null(o$median)) {
            out$target <- list(kind = "median", value = g(o$median))
        } else add_problem(pr, where, "an event outcome needs rate, risk {value, at} or median")
        out$time_units <- o$time_units %||% "years"
    } else if (o$type == "continuous") {
        out$mean <- g(o$mean %||% NA); out$sd <- g(o$sd %||% NA)
        if (is.null(out$sd) || out$sd <= 0) add_problem(pr, where, "a continuous outcome needs mean and sd (residual sd > 0)")
        if (!is.null(o$range)) out$range <- try_at(pr, where, num_vec(o$range))
        out$within_cor <- g(o$within_cor %||% 0.6)
        # scale: log -> generated on the log scale: mean is the geometric mean,
        # sd the sd of log(y), effects are ratios (e.g. bioequivalence)
        out$log <- identical(o$scale, "log")
        if (out$log && !is.null(out$mean) && !is.na(out$mean) && out$mean <= 0)
            add_problem(pr, where, "a log-scale outcome needs a positive (geometric) mean")
    } else if (o$type == "count") {
        out$mean <- g(o$mean %||% NA)
        out$size <- if (is.null(o$size)) NULL else g(o$size)
        if (is.null(out$mean) || out$mean <= 0) add_problem(pr, where, "a count outcome needs mean > 0")
    }
    eff <- o$effects %||% list()
    want <- switch(o$type, binary = "OR", event = c("HR", "OR", "RR"), count = "RR",
                   continuous = if (isTRUE(out$log)) "ratio" else "beta")
    for (v in names(eff)) {
        m <- intersect(names(eff[[v]]), names(MEASURES))
        if (length(m) == 1 && !m %in% want)
            add_problem(pr, sprintf("outcome '%s', effect of '%s'", id, v), sprintf(
                "%s is unusual for a %s outcome (expected %s); it is used as given", m,
                if (isTRUE(out$log)) "log-scale" else o$type, paste(want, collapse = "/")), "warning")
    }
    out$effects <- Filter(Negate(is.null), lapply(names(eff), function(v)
        normalize_effect(id, v, eff[[v]], vars, interventions, pr)))
    names(out$effects) <- vapply(out$effects, `[[`, "", "var")
    out
}

# Outcome type -> designs it can feed (scenario-first menu, task_plan.md);
# trials need an intervention. An outcome's own `designs:` list overrides.
DESIGNS <- c("cross_sectional", "cohort", "case_control", "matched_cc", "rct", "crossover", "prognosis")

compatible_designs <- function(sc, outcome) {
    oc <- sc$outcomes[[outcome]]
    if (length(oc$designs)) return(intersect(oc$designs, DESIGNS))
    has_iv <- length(sc$interventions) > 0 && any(names(oc$effects) %in% names(sc$interventions))
    d <- switch(oc$type,
        binary = c("cross_sectional", "cohort", "case_control", "matched_cc"),
        event = c("cohort", "case_control", "matched_cc"),
        continuous = c("cross_sectional", "cohort"),
        count = c("cross_sectional", "cohort"))
    if (has_iv) {
        d <- c(d, "rct")
        if (oc$type == "continuous") d <- c(d, "crossover")
    }
    d
}

normalize_scenario <- function(raw, dictionary = list(), strict = TRUE) {
    pr <- new_problems()
    pr$declared <- names(raw$variables)
    id <- raw$id %||% "scenario"
    ivs <- list()
    for (iv in names(raw$interventions)) {
        x <- raw$interventions[[iv]]
        ivs[[iv]] <- list(id = iv, label = x$label %||% iv,
                          levels = as.character(unlist(x$levels %||% c("control", "treatment"))),
                          ratio = num_vec(x$ratio %||% rep(1, length(unlist(x$levels %||% 1:2)))))
    }
    vars <- list()
    for (v in names(raw$variables)) {
        nv <- normalize_variable(v, raw$variables[[v]] %||% list(), dictionary, pr)
        if (!is.null(nv)) vars[[v]] <- nv
    }
    ids <- names(vars)
    for (v in ids) {
        rr <- vars[[v]]$rules_raw
        vars[[v]]$rules_raw <- NULL
        where <- sprintf("variable '%s'", v)
        if (!is.null(rr$formula)) vars[[v]]$formula <- try_at(pr, where, parse_rule(rr$formula, setdiff(ids, v)))
        if (!is.null(rr$only_if)) {
            vars[[v]]$only_if <- try_at(pr, where, parse_rule(rr$only_if, setdiff(ids, v)))
            if (is.null(vars[[v]]$otherwise) && !vars[[v]]$type %in% c("continuous", "count"))
                add_problem(pr, where, "only_if needs otherwise: the level to use when the condition is false")
            else if (!is.null(vars[[v]]$levels) && !as.character(vars[[v]]$otherwise) %in% vars[[v]]$levels)
                add_problem(pr, where, sprintf("otherwise: '%s' is not one of its levels (%s)", vars[[v]]$otherwise,
                                               paste(vars[[v]]$levels, collapse = ", ")))
        }
        if (vars[[v]]$type == "binary" && is.character(rr$p) && is.null(tryCatch(num_expr(rr$p), error = function(e) NULL)))
            vars[[v]]$p_rule <- try_at(pr, where, parse_rule(rr$p, setdiff(ids, v)))
    }
    rule_vars <- ids[vapply(vars, has_rules, TRUE)]
    rule_edges <- do.call(rbind, c(list(data.frame(from = character(), to = character(), stringsAsFactors = FALSE)),
        lapply(rule_vars, function(v) { d <- rule_deps(vars[[v]])
            if (length(d)) data.frame(from = d, to = v, stringsAsFactors = FALSE) else NULL })))
    rule_order <- character()
    if (length(rule_vars)) {
        ro <- topo_order(ids, rule_edges)
        if (is.null(ro)) add_problem(pr, "rules", "the rules (p, formula, only_if) refer to each other in a circle")
        else rule_order <- intersect(ro, rule_vars)
    }
    as_edges <- function(x, what) {
        if (length(x) == 0) return(data.frame(from = character(), to = character(), b = numeric()))
        rows <- lapply(x, function(e) {
            e <- unlist(e)
            if (length(e) == 3 && all(e[1:2] %in% pr$declared) && !all(e[1:2] %in% ids)) return(NULL)
            if (length(e) != 3 || !all(e[1:2] %in% ids)) {
                add_problem(pr, what, sprintf("[%s] must be [variable, variable, strength] with known variables",
                                              paste(e, collapse = ", ")))
                return(NULL)
            }
            b <- try_at(pr, what, num_expr(e[3]))
            if (is.null(b) || abs(b) >= 1) { add_problem(pr, what, sprintf("strength of [%s] must be between -1 and 1",
                                                                      paste(e, collapse = ", "))); return(NULL) }
            data.frame(from = e[1], to = e[2], b = b, stringsAsFactors = FALSE)
        })
        do.call(rbind, c(list(data.frame(from = character(), to = character(), b = numeric())), rows))
    }
    edges <- as_edges(raw$structure, "structure")
    cors <- as_edges(raw$correlations, "correlations")
    ord <- topo_order(ids, edges)
    if (is.null(ord)) { add_problem(pr, "structure", "the edges form a cycle; the structure must be a DAG"); ord <- ids }
    exo <- setdiff(ids, edges$to)
    R_exo <- diag(length(exo)); dimnames(R_exo) <- list(exo, exo)
    for (i in seq_len(nrow(cors))) {
        a <- cors$from[i]; b <- cors$to[i]
        if (!all(c(a, b) %in% exo)) {
            add_problem(pr, "correlations", sprintf("[%s, %s]: correlations are only for variables without incoming edges (use structure for the others)", a, b))
            next
        }
        R_exo[a, b] <- R_exo[b, a] <- cors$b[i]
    }
    if (length(exo) && min(eigen(R_exo, symmetric = TRUE, only.values = TRUE)$values) <= 1e-6) {
        add_problem(pr, "correlations", "the correlations are not mutually consistent (not positive definite); repaired to the nearest valid set", "warning")
        R_exo <- near_pd(R_exo)
    }
    lat <- implied_latent(ord, edges, R_exo, pr)
    outcomes <- list()
    for (o in names(raw$outcomes)) {
        no <- normalize_outcome(o, raw$outcomes[[o]], vars, ivs, pr)
        if (!is.null(no)) outcomes[[o]] <- no
    }
    if (length(outcomes) == 0) add_problem(pr, sprintf("scenario '%s'", id), "has no usable outcome")
    clash <- intersect(names(ivs), ids)
    if (length(clash)) add_problem(pr, "interventions", sprintf("'%s' is both a variable and an intervention", clash[1]))
    for (nv in unlist(raw$null_variables)) {
        if (!nv %in% ids) add_problem(pr, "null_variables", sprintf("'%s' is not a variable", nv), "warning")
        for (o in outcomes) if (nv %in% names(o$effects))
            add_problem(pr, "null_variables", sprintf("'%s' is listed as null but has an effect on '%s'", nv, o$id), "warning")
    }
    sc <- list(id = id, label = raw$label %||% id, domain = raw$domain %||% "", population = raw$population,
               variables = vars, order = ord, edges = lat$edges, R_exo = R_exo, exogenous = exo,
               R = lat$R, resid = lat$resid, outcomes = outcomes, interventions = ivs,
               rule_order = rule_order, rule_edges = rule_edges,
               null_variables = unlist(raw$null_variables), references = unlist(raw$references),
               teaching = raw$teaching, design_defaults = normalize_design_defaults(raw$designs, pr),
               problems = pr$items)
    class(sc) <- "studysim_scenario"
    errs <- pr$items[pr$items$level == "error", ]
    if (strict && nrow(errs))
        stop(sprintf("scenario '%s' has problems:\n- %s", id,
                     paste(sprintf("%s: %s", errs$where, errs$message), collapse = "\n- ")), call. = FALSE)
    sc
}
