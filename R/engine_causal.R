# Causal reading of a scenario for the answer key: which variables a variable
# causes (its descendants through the structure and the rules), the mediators
# of its effect on an outcome, the adjustment set the diagram supports (the
# variables it does not cause), and its total effect.
#
# The library gives direct (conditional) effects: the effect of a variable with
# every other one fixed. A variable that acts through others (education ->
# parity -> breast cancer) has a total effect too, which is what an analysis
# adjusted for its confounders but not for its mediators estimates.

# directed edges among variables: the latent paths and the rules (p, formula, only_if)
causal_edges <- function(sc) rbind(sc$edges[, c("from", "to")],
                                   sc$rule_edges %||% data.frame(from = character(), to = character(), stringsAsFactors = FALSE))

descendants <- function(sc, v) {
    ed <- causal_edges(sc)
    out <- character(); front <- v
    while (length(front)) {
        nx <- setdiff(unique(ed$to[ed$from %in% front]), c(out, v))
        out <- c(out, nx); front <- nx
    }
    out
}

# a derived variable (formula) is a function of its inputs: adjusting for them,
# or for other variables derived from them, leaves it nothing to vary
formula_family <- function(sc, v) {
    f <- sc$variables[[v]]$formula
    if (is.null(f)) return(character())
    sib <- Filter(function(w) !is.null(sc$variables[[w]]$formula) && length(intersect(sc$variables[[w]]$formula$deps, f$deps)), sc$order)
    setdiff(unique(c(f$deps, sib)), v)
}

# the variables a valid analysis of v adjusts for, among `available`: those v
# does not cause (descendants are mediators or colliders)
adjustment_set <- function(sc, v, available) {
    if (v %in% names(sc$interventions)) return(intersect(available, sc$order))
    setdiff(intersect(available, sc$order), c(v, descendants(sc, v), formula_family(sc, v)))
}

# the variables through which v also acts on the outcome
mediators <- function(sc, oc, v) {
    if (v %in% names(sc$interventions)) return(character())
    intersect(descendants(sc, v), names(oc$effects))
}

.total_cache <- new.env()

# Total effects of the variables that act through mediators: the regression of
# the outcome on v and the variables v does not cause, in a large simulated
# population (a cohort for event outcomes, cross-sectional otherwise). For the
# other variables the total effect is the direct one. Returns a data frame
# (variable, row = contrast index, total) and the population size as "n".
total_effects <- function(sc, oc) {
    med <- Filter(function(v) length(mediators(sc, oc, v)) > 0, setdiff(sc$order, names(sc$interventions)))
    empty <- data.frame(variable = character(), row = integer(), total = numeric(), stringsAsFactors = FALSE)
    if (!length(med) || !is.null(oc[["after"]])) return(structure(empty, n = 0))
    key <- digest::digest(list("total", sc$id, sc$variables, sc$edges, sc$rule_edges, sc$R_exo, oc))
    if (!is.null(.total_cache[[key]])) return(.total_cache[[key]])
    cal <- calibrate(sc, oc)
    base <- if (oc$type == "event") "cohort" else "cross_sectional"
    opt <- list(followup = oc$followup, loss = 0, accrual = 0)
    p_ev <- if (oc$type == "event") cal$achieved$risk_followup else if (oc$type == "binary") min(oc$prevalence, 1 - oc$prevalence) else NA
    n <- if (is.na(p_ev)) 40000 else min(200000, max(40000, ceiling(4000 / p_ev)))
    big <- run_design(base, sc, oc, cal, derive_seed("total", sc$id, oc$id), n, opt)
    fd <- format_study(sc, oc, base, big, sc$order, "en")
    rows <- lapply(med, function(v) {
        fit <- fit_design_model(base, sc, oc, fd, c(v, adjustment_set(sc, v, sc$order)))
        cr <- contrast_rows(sc, oc, v)
        b <- fit$coef[cr$coefname] * cr$per
        data.frame(variable = v, row = seq_len(nrow(cr)), total = unname(if (fit$log) exp(b) else b), stringsAsFactors = FALSE)
    })
    out <- structure(do.call(rbind, c(list(empty), rows)), n = nrow(fd))
    .total_cache[[key]] <- out
    out
}

# does any of `vars` act through mediators (the total effect differs from the direct one)?
any_mediated <- function(sc, oc, vars = sc$order) {
    if (!is.null(oc[["after"]])) return(FALSE)
    any(vapply(setdiff(vars, names(sc$interventions)), function(v) length(mediators(sc, oc, v)) > 0, TRUE))
}

# does the diagram's adjustment of some measured factor differ from adjusting for
# all of them (a measured descendant or derived sibling)? Not for the trials,
# whose main contrast is the randomised intervention
dag_differs <- function(sc, design, factors, has_iv = FALSE) {
    if (has_iv || design %in% c("crossover", "pre_post", "method_comparison")) return(FALSE)
    any(vapply(factors, function(v) length(intersect(factors, c(descendants(sc, v), formula_family(sc, v)))) > 0, TRUE))
}
