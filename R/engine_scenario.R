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
            out[[d]][[k]] <- if (k %in% c("match", "between")) as.character(v)
                             else if (k == "profile") try_at(pr, sprintf("designs: %s %s", d, k), num_vec(v))
                             else try_at(pr, sprintf("designs: %s %s", d, k), num_expr(v[1]))
            if (k == "between" && length(v) %% 3 != 0)
                add_problem(pr, sprintf("designs: %s between", d), "between is a list of triples: variable, variable, correlation")
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
    if (!is.null(v$lod)) out$lod <- try_at(pr, where, num_expr(v$lod))   # limit of detection of an assay
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
        } else if (!type %in% c("continuous", "count", "binary", "genotype") && !(type == "intervention" && length(levels) == 2)) {
            add_problem(pr, where, "effect modification (by:) is supported for numeric, binary, genotype and two-arm intervention effects")
        } else {
            bv <- e$by[[mod]]
            # a two-arm intervention: the effect of its second arm, per level of the modifier
            co <- rep(out$coef[length(out$coef)], length(mv$levels)); names(co) <- mv$levels
            for (lv in names(bv)) {
                if (!lv %in% mv$levels) { add_problem(pr, where, sprintf("'%s' is not a level of '%s'", lv, mod)); next }
                x <- num_expr(bv[[lv]])
                co[lv] <- tf(x)
            }
            out$by <- list(var = mod, coef = unname(co), binary = mv$type == "binary")
            # a numeric effect modified by another variable acts on (x - center):
            # the modifier's own effect is then the one at x = center, not at
            # x = 0 (default: the population mean, filled by normalize_scenario)
            if (type %in% c("continuous", "count") && !is.null(e$center))
                out$center <- try_at(pr, where, num_expr(e$center))
        }
    }
    out
}

# Default `center` of the numeric effects with `by:` (the population mean of
# x, 3 significant digits) and a warning when a center lies outside x's values
fill_centers <- function(sc, pr) {
    need <- function(oc) Filter(function(e) !is.null(e$by) && e$type %in% c("continuous", "count"),
                                c(oc$effects, oc$competing$effects, oc$after_event$effects))
    effs <- unlist(lapply(sc$outcomes, need), recursive = FALSE)
    deps <- unlist(lapply(sc$outcomes, function(oc) lapply(oc$markers, `[[`, "depends")), recursive = FALSE)
    if (!length(effs) && !length(unlist(deps))) return(sc)
    cov <- tryCatch(with_seed(derive_seed("center", sc$id), gen_covariates(sc, 20000L)), error = function(e) NULL)
    if (is.null(cov)) return(sc)
    # markers' depends: the variable standardised by its population mean and SD
    for (o in names(sc$outcomes)) for (m in names(sc$outcomes[[o]]$markers)) for (v in names(sc$outcomes[[o]]$markers[[m]]$depends)) {
        x <- as.numeric(cov[[v]])
        sc$outcomes[[o]]$markers[[m]]$depends[[v]]$mean <- mean(x)
        sc$outcomes[[o]]$markers[[m]]$depends[[v]]$sd <- max(stats::sd(x), 1e-8)
    }
    if (!length(effs)) return(sc)
    fix <- function(oc) {
        for (k in c("effects", "competing", "after_event")) {
            if (k != "effects") { if (!is.null(oc[[k]])) oc[[k]] <- fix(oc[[k]]); next }
            for (v in names(oc$effects)) {
                e <- oc$effects[[v]]
                if (is.null(e$by) || !e$type %in% c("continuous", "count")) next
                x <- cov[[v]]
                if (is.null(e$center)) e$center <- signif(mean(x), 3)
                else if (e$center < min(x) || e$center > max(x))
                    add_problem(pr, sprintf("outcome '%s', effect of '%s'", oc$id, v), sprintf(
                        "center %s is outside the values of '%s' (%s to %s): '%s' gets a large implied effect of its own; use a typical value such as %s",
                        base::format(e$center), v, base::format(signif(min(x), 3)), base::format(signif(max(x), 3)),
                        e$by$var, base::format(signif(mean(x), 3))), "warning")
                oc$effects[[v]] <- e
            }
        }
        oc
    }
    sc$outcomes <- lapply(sc$outcomes, fix)
    sc
}

# A marker of a binary outcome (diagnostic design): binormal, i.e. a latent
# N(0, 1) in people without the disease shifted by delta = sqrt(2) qnorm(auc)
# in the diseased, then mapped to `dist` (the marker's distribution in the
# non-diseased) by its quantiles, which keeps the AUC. A `block` is p markers
# (standard-normal values, exchangeable correlation rho) of which the first
# `informative` have the AUC and the others none.
normalize_marker <- function(oc_id, id, m, pr, vars = list()) {
    where <- sprintf("outcome '%s', marker '%s'", oc_id, id)
    g <- function(x, default) { v <- try_at(pr, where, num_expr(x %||% default)); if (is.null(v)) default else v }
    out <- list(id = id, label = m$label %||% id, units = m$units, round = if (!is.null(m$round)) as.integer(m$round), auc = g(m$auc, NA),
                dir = if (identical(m$direction, "lower")) -1 else 1)
    if (is.na(out$auc) || out$auc < 0.5 || out$auc >= 1) {
        add_problem(pr, where, "needs auc, the area under the ROC curve (0.5 to 1)"); return(NULL) }
    if (!is.null(m$direction) && !m$direction %in% c("higher", "lower"))
        add_problem(pr, where, "direction must be higher or lower (values in the diseased)")
    # severity_sd: the diseased differ in how far the marker moves (a disease
    # severity shared by the markers); the shift is widened so that `auc`
    # stays the population AUC: delta = sqrt(2 + s^2) qnorm(auc)
    out$severity_sd <- g(m$severity_sd, 0)
    if (out$severity_sd < 0) add_problem(pr, where, "severity_sd must be 0 or more")
    out$delta <- sqrt(2 + out$severity_sd^2) * stats::qnorm(out$auc)
    if (!is.null(m$cutoff)) out$cutoff <- g(m$cutoff, NA)   # test positive beyond it (Youden's point otherwise)
    # depends: {age: 0.3}: the marker's latent value moves by b per SD of the
    # variable, in diseased and non-diseased alike (auc is then the AUC at
    # fixed values of those variables); moments filled by normalize_scenario
    if (length(m$depends)) {
        out$depends <- list()
        for (v in names(m$depends)) {
            if (is.null(vars[[v]]) || !vars[[v]]$type %in% c("continuous", "count", "binary", "ordinal"))
                add_problem(pr, where, sprintf("depends: '%s' must be a numeric, binary or ordinal variable of the scenario", v))
            else out$depends[[v]] <- list(var = v, b = g(m$depends[[v]], 0))
        }
    }
    if (!is.null(m$block)) {
        out$block <- as.integer(g(m$block, 0)); out$informative <- as.integer(g(m$informative, 0)); out$rho <- g(m$rho, 0)
        if (out$block < 2 || out$informative < 0 || out$informative > out$block || out$rho < 0 || out$rho >= 1)
            add_problem(pr, where, "a block needs block (number of markers, at least 2), informative (0 to block) and rho (0 to 1)")
        return(out)
    }
    if (!is.null(m$range)) out$range <- try_at(pr, where, num_vec(m$range))
    if (!is.null(m$dist)) out$dist <- try_at(pr, where, parse_dist(m$dist))
    if (!is.null(m$lod)) out$lod <- g(m$lod, NA)   # limit of detection
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
        if (length(o$markers)) {
            # correlation of the single markers within disease groups (a shared factor)
            out$marker_cor <- g(o$marker_cor %||% 0)
            if (out$marker_cor < 0 || out$marker_cor >= 1) add_problem(pr, where, "marker_cor must be between 0 and 1")
            out$markers <- Filter(Negate(is.null), lapply(names(o$markers), function(m) normalize_marker(id, m, o$markers[[m]], pr, vars)))
            names(out$markers) <- vapply(out$markers, `[[`, "", "id")
        }
    } else if (o$type == "event") {
        out$shape <- g(o$shape %||% 1)
        out$levels <- as.character(unlist(o$levels %||% c("no", "yes")))
        if (!is.null(o$rate)) {
            out$target <- list(kind = "rate", value = g(o$rate))
        } else if (!is.null(o$risk)) {
            out$target <- list(kind = "risk", value = g(o$risk$value), at = g(o$risk$at))
        } else if (!is.null(o$median)) {
            out$target <- list(kind = "median", value = g(o$median))
        } else if (is.null(o[["after"]])) add_problem(pr, where, "an event outcome needs rate, risk {value, at} or median")
        # illness-death: death (this outcome) after a first event X; its own
        # target, if any, is the hazard without X (else X's competing event),
        # after_event the hazard once X has happened
        if (!is.null(o[["after"]])) {
            out$after <- as.character(o[["after"]])[1]
            if (is.null(o$after_event)) add_problem(pr, where, "after needs after_event: {median, risk or rate; shape; effects}")
            else {
                ae <- o$after_event; ae$type <- "event"; ae$followup <- ae$followup %||% o$followup %||% o$window
                out$after_event <- normalize_outcome(paste0(id, "_after"), ae, vars, interventions, pr)
            }
        }
        out$time_units <- o$time_units %||% "years"
        # a competing event: an event outcome of its own (target, shape,
        # effects) whose occurrence first ends the follow-up for this one
        if (!is.null(o$competing)) {
            cc <- o$competing
            cc$type <- "event"; cc$followup <- cc$followup %||% o$followup %||% o$window
            out$competing <- normalize_outcome(paste0(id, "_competing"), cc, vars, interventions, pr)
        }
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
DESIGNS <- c("cross_sectional", "cohort", "case_control", "matched_cc", "rct", "crossover", "prognosis",
             "nested_cc", "case_cohort", "cluster_rct", "factorial", "longitudinal", "ecological", "diagnostic",
             "pre_post", "method_comparison")

compatible_designs <- function(sc, outcome) {
    oc <- sc$outcomes[[outcome]]
    # an illness-death outcome needs people followed from the start
    if (!is.null(oc[["after"]])) return(intersect(oc$designs %||% c("cohort", "prognosis", "rct"), c("cohort", "prognosis", "rct")))
    if (length(oc$designs)) return(intersect(oc$designs, DESIGNS))
    has_iv <- length(sc$interventions) > 0 && any(names(oc$effects) %in% names(sc$interventions))
    d <- switch(oc$type,
        binary = c("cross_sectional", "cohort", "case_control", "matched_cc", "ecological"),
        event = c("cohort", "case_control", "matched_cc", "nested_cc", "case_cohort", "ecological"),
        continuous = c("cross_sectional", "cohort", "ecological", "method_comparison"),
        count = c("cross_sectional", "cohort", "ecological"))
    if (length(oc$markers)) d <- c(d, "diagnostic")
    if (has_iv) {
        d <- c(d, "rct", "cluster_rct")
        if (oc$type == "continuous") d <- c(d, "crossover", "longitudinal", "pre_post")
        two <- Filter(function(v) length(sc$interventions[[v]]$levels) == 2, intersect(names(oc$effects), names(sc$interventions)))
        if (length(two) >= 2) d <- c(d, "factorial")
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
    for (o in names(outcomes)) if (!is.null(outcomes[[o]]$after)) {
        x <- outcomes[[outcomes[[o]]$after]]
        where <- sprintf("outcome '%s', after", o)
        if (is.null(x) || x$type != "event" || x$id == o) add_problem(pr, where, "after must name another event outcome of the scenario")
        else if (is.null(outcomes[[o]]$target) && is.null(x$competing))
            add_problem(pr, where, sprintf("give '%s' a target (death without %s) or give '%s' a competing event", o, x$id, x$id))
    }
    # links between outcomes (correlated noise): to an outcome without a link of its own
    for (o in names(outcomes)) {
        lk <- raw$outcomes[[o]]$link
        if (is.null(lk)) next
        where <- sprintf("outcome '%s', link", o)
        rho <- tryCatch(num_expr(lk$rho), error = function(e) NA)
        if (is.null(lk$outcome) || !lk$outcome %in% setdiff(names(outcomes), o)) add_problem(pr, where, "outcome must be another outcome of the scenario")
        else if (!is.null(raw$outcomes[[lk$outcome]]$link)) add_problem(pr, where, sprintf("'%s' is linked itself; link both to the same outcome", lk$outcome))
        else if (is.na(rho) || abs(rho) >= 1) add_problem(pr, where, "rho must be between -1 and 1")
        else outcomes[[o]]$link <- list(outcome = lk$outcome, rho = rho)
    }
    # a translated text whose keys are not language codes was split at a comma
    # inside {...} (the YAML trap in schema.md)
    check_text <- function(x, where) {
        if (!is.list(x)) return(invisible())
        for (k in names(x)) {
            v <- x[[k]]
            if (k %in% c("label", "units", "population", "description") && is.list(v) && !all(grepl("^[a-z]{2}$", names(v))))
                add_problem(pr, where, sprintf("%s looks split at a comma; put the text in quotes", k), "warning")
            else if (is.list(v)) check_text(v, if (is.null(names(x))) where else sprintf("%s %s", where, k))
        }
    }
    check_text(raw[c("variables", "outcomes", "interventions", "label", "population")], sprintf("scenario '%s'", id))
    # names become columns and model terms: R's reserved words cannot be used
    reserved <- c("if", "else", "repeat", "while", "function", "for", "next", "break", "in", "TRUE", "FALSE",
                  "NULL", "Inf", "NaN", "NA")
    for (nm in intersect(c(names(raw$variables), names(raw$outcomes), names(raw$interventions)), reserved))
        add_problem(pr, sprintf("name '%s'", nm), sprintf("'%s' is a reserved word; choose another name", nm))
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
    if (!any(pr$items$level == "error")) { sc <- fill_centers(sc, pr); sc$problems <- pr$items }
    class(sc) <- "studysim_scenario"
    errs <- pr$items[pr$items$level == "error", ]
    if (strict && nrow(errs))
        stop(sprintf("scenario '%s' has problems:\n- %s", id,
                     paste(sprintf("%s: %s", errs$where, errs$message), collapse = "\n- ")), call. = FALSE)
    sc
}
