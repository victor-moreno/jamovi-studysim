# Biases a study can be given on purpose (Simulated study, Biases), each
# applied to the student's data and to the answer key's large study alike, so
# the key shows what the biased design estimates next to the truth:
# - recall: unexposed cases report the exposure (case-control);
# - control selection: controls sampled with odds depending on the exposure
#   (hospital controls), in the samplers (engine_designs.R);
# - collider: `admitted`, caused by the outcome and the exposure;
# - non-adherence (trials), informative loss to follow-up and immortal time,
#   in the samplers and below.
# `vars` are the exposures: the variables ticked in the Exposure column.

.moments_cache <- new.env()

# population mean and SD of each numeric variable (a fixed sample of 20,000)
pop_moments <- function(sc) {
    key <- digest::digest(list("moments", sc$id, sc$variables, sc$edges, sc$R_exo))
    if (!is.null(.moments_cache[[key]])) return(.moments_cache[[key]])
    cov <- with_seed(derive_seed("center", sc$id), gen_covariates(sc, 20000L))
    out <- lapply(stats::setNames(sc$order, sc$order), function(v) {
        x <- as.numeric(cov[[v]]); list(mean = mean(x), sd = max(stats::sd(x), 1e-8)) })
    .moments_cache[[key]] <- out
    out
}

# the exposure score of variable v: per SD for numbers, exposed (any level
# but the reference) or not for categories. x is an engine code (0/1, 1..K)
# or a formatted column.
exposure_score <- function(sc, v, x) {
    t <- sc$variables[[v]]$type
    if (t %in% c("continuous", "count")) {
        m <- pop_moments(sc)[[v]]
        return((as.numeric(x) - m$mean) / m$sd)
    }
    k <- as.integer(x)
    if (is.factor(x) || t != "binary") k <- k - 1L   # factors 1..K and categorical codes 1..K -> 0..K-1
    as.numeric(k > 0)
}

score_sum <- function(sc, d, vars) {
    s <- numeric(nrow(d))
    for (v in intersect(vars, names(d))) s <- s + exposure_score(sc, v, d[[v]])
    s[is.na(s)] <- 0
    s
}

# Recall bias in a formatted case-control study: each unexposed case reports
# the next level with probability p; a numeric exposure is reported p higher
recall_bias <- function(fd, sc, vars, p, seed) {
    if (p <= 0 || is.null(fd$case)) return(fd)
    case <- !is.na(fd$case) & fd$case == "case"
    with_seed(seed, for (v in intersect(vars, names(fd))) {
        x <- fd[[v]]; desc <- attr(x, "jmv-desc"); def <- sc$variables[[v]]
        if (is.factor(x)) {
            k <- as.integer(x)
            hit <- case & !is.na(k) & k == 1L & stats::runif(length(k)) < p
            k[hit] <- 2L
            x <- factor(levels(x)[k], levels = levels(x), ordered = is.ordered(x))
        } else if (is.numeric(x)) {
            y <- ifelse(case, x * (1 + p), x)
            if (!is.null(def$range)) y <- pmin(y, def$range[2])
            x <- if (def$type == "count") as.integer(round(y)) else if (!is.null(def$round)) round(y, def$round) else y
        }
        attr(x, "jmv-desc") <- desc
        fd[[v]] <- x
    })
    fd
}

# The outcome as a number for the collider: an event or case 1, else 0; a
# continuous or count outcome standardised
outcome_indicator <- function(fd, oc, design) {
    if (design %in% CASE_DESIGNS) return(as.numeric(fd$case == "case"))
    y <- fd[[oc$id]]
    if (is.factor(y)) return(as.numeric(as.integer(y) == 2L))
    if (!is.null(fd$time)) return(as.numeric(y == 1))
    (y - mean(y, na.rm = TRUE)) / stats::sd(y, na.rm = TRUE)
}

# A collider: admitted to hospital during the study, more often with the
# outcome (odds x4) and with the exposure (x3 if exposed, or per SD); 20% overall
add_collider <- function(fd, sc, oc, design, vars, seed, lang = "en") {
    lp <- log(4) * outcome_indicator(fd, oc, design) + log(3) * score_sum(sc, fd, vars)
    lp[is.na(lp)] <- 0
    a <- stats::uniroot(function(a) mean(stats::plogis(a + lp)) - 0.2, c(-30, 30))$root
    u <- with_seed(seed, stats::runif(nrow(fd)))
    fd$admitted <- with_desc(factor(c("no", "yes")[1 + (u < stats::plogis(a + lp))], levels = c("no", "yes")), gtext("admitted", lang))
    fd
}

# Immortal time: a treatment started at a random time after entry (half the
# people by half the follow-up), with no effect on the outcome. `treated`
# says whether it started before the end of follow-up, so the treated had to
# survive until the start: a baseline comparison makes the treatment look protective.
add_immortal <- function(fd, opt, seed, lang = "en") {
    if (is.null(fd$time)) return(fd)
    fu <- opt$followup
    ts <- with_seed(seed, stats::rexp(nrow(fd), log(2) / (fu / 2)))
    ever <- ts < fd$time
    fd$treated <- with_desc(factor(c("no", "yes")[1 + ever], levels = c("no", "yes")), gtext("treated", lang))
    fd$treatment_start <- with_desc(ifelse(ever, round(ts, 3), NA_real_), gtext("treatment_start", lang))
    fd
}

# the biases of a formatted study, in the order a study would meet them
apply_biases <- function(fd, sc, oc, design, opt, seed, lang = "en") {
    vars <- opt$exposures %||% character()
    if (design %in% c("case_control", "matched_cc") && isTRUE(opt$recall > 0))
        fd <- recall_bias(fd, sc, vars, opt$recall, derive_seed(seed, "recall"))
    if (isTRUE(opt$collider) && design %in% c("cross_sectional", "cohort", "case_control") && length(vars))
        fd <- add_collider(fd, sc, oc, design, vars, derive_seed(seed, "collider"), lang)
    if (isTRUE(opt$immortal) && design %in% c("cohort", "prognosis") && oc$type == "event")
        fd <- add_immortal(fd, opt, derive_seed(seed, "immortal"), lang)
    fd
}
