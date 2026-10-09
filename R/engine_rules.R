# Rules that tie a variable to others, beyond the latent correlations:
#   p: "plogis((age - 51) / 1.5)"        binary probability from other variables
#   only_if: smoking == "current"         otherwise: 0   (structural values)
#   formula: weight / (height / 100)^2    deterministic, derived variable
# Expressions are evaluated in a closed environment: the referenced variables
# (numbers, or level labels for categorical ones) and a short list of
# functions. Anything else is refused when the library is read, so a library
# file can never run arbitrary code.

RULE_FUNS <- list(
    `+` = `+`, `-` = `-`, `*` = `*`, `/` = `/`, `^` = `^`, `(` = `(`,
    `==` = `==`, `!=` = `!=`, `<` = `<`, `>` = `>`, `<=` = `<=`, `>=` = `>=`,
    `&` = `&`, `|` = `|`, `!` = `!`, `%in%` = `%in%`, c = c,
    log = log, exp = exp, sqrt = sqrt, abs = abs, round = round,
    pmin = pmin, pmax = pmax, ifelse = ifelse, plogis = stats::plogis, qlogis = stats::qlogis)

# text -> list(expr, deps); stops with a plain message on anything not allowed
parse_rule <- function(text, ids) {
    e <- tryCatch(parse(text = as.character(text), keep.source = FALSE),
                  error = function(err) stop(sprintf("cannot read '%s'", text), call. = FALSE))
    if (length(e) != 1) stop(sprintf("'%s' must be a single expression", text), call. = FALSE)
    e <- e[[1]]
    fun_names <- setdiff(all.names(e), all.vars(e))
    bad_f <- setdiff(fun_names, names(RULE_FUNS))
    if (length(bad_f)) stop(sprintf("'%s' uses %s, which is not allowed (allowed: arithmetic, comparisons, %s)",
                                    text, paste(bad_f, collapse = ", "),
                                    paste(setdiff(names(RULE_FUNS), c("+", "-", "*", "/", "^", "(", "==", "!=", "<", ">", "<=", ">=", "&", "|", "!", "%in%")), collapse = ", ")),
                            call. = FALSE)
    deps <- all.vars(e)
    bad_v <- setdiff(deps, ids)
    if (length(bad_v)) stop(sprintf("'%s' names %s, which %s not a variable of this scenario", text,
                                    paste(bad_v, collapse = ", "), if (length(bad_v) > 1) "are" else "is"), call. = FALSE)
    list(expr = e, text = as.character(text), deps = deps)
}

has_rules <- function(v) !is.null(v$p_rule) || !is.null(v$formula) || !is.null(v$only_if)

rule_deps <- function(v) unique(c(v$p_rule$deps, v$formula$deps, v$only_if$deps))

# the values a rule sees: numbers, or the library's level labels
rule_env <- function(cov, sc, deps) {
    vals <- lapply(stats::setNames(deps, deps), function(d) {
        x <- cov[[d]]; v <- sc$variables[[d]]
        switch(v$type, continuous = , count = x, binary = v$levels[x + 1L], v$levels[x])
    })
    list2env(c(RULE_FUNS, vals), parent = emptyenv())
}

# value of a level label (or number) as the variable's internal code
value_code <- function(val, v) {
    if (v$type %in% c("continuous", "count")) return(as.numeric(val))
    k <- match(as.character(val), v$levels)
    if (anyNA(k)) stop(sprintf("'%s' is not a level of '%s' (%s)", val[is.na(k)][1], v$id,
                               paste(v$levels, collapse = ", ")), call. = FALSE)
    if (v$type == "binary") k - 1L else k
}

# Apply the rules, in dependency order, to covariates drawn from the copula.
# z holds the latent draws, so a rule-driven binary keeps its latent ordering.
apply_rules <- function(cov, Z, sc) {
    n <- nrow(cov)
    for (id in sc$rule_order) {
        v <- sc$variables[[id]]
        env <- rule_env(cov, sc, rule_deps(v))
        ev <- function(r) {
            x <- eval(r$expr, env)
            if (length(x) == 1) x <- rep(x, n)
            if (length(x) != n) stop(sprintf("'%s' does not give one value per person", r$text), call. = FALSE)
            x
        }
        if (!is.null(v$formula)) {
            x <- ev(v$formula)
            cov[[id]] <- switch(v$type,
                binary = as.integer(as.logical(x)),
                count = as.integer(round(x)),
                { x <- as.numeric(x)
                  if (!is.null(v$range)) x <- pmin(pmax(x, v$range[1]), v$range[2])
                  if (!is.null(v$round)) round(x, v$round) else x })
        } else if (!is.null(v$p_rule)) {
            p <- pmin(pmax(as.numeric(ev(v$p_rule)), 0), 1)
            cov[[id]] <- as.integer(stats::pnorm(Z[, id]) > 1 - p)
        }
        if (!is.null(v$only_if)) {
            keep <- as.logical(ev(v$only_if))
            keep[is.na(keep)] <- FALSE
            cov[[id]][!keep] <- value_code(v$otherwise, v)
        }
    }
    cov
}
