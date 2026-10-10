# Design samplers: each turns the superpopulation into one study. They return
# an internal frame (`.pid`, covariate codes, design columns starting with a
# dot, outcome in `.y` or `.time`/`.status`) and a list of facts for the summary.
# `n` is always the number of people in the study (a crossover has 2 rows each).

MAX_SCAN <- 400L   # chunks, i.e. 2 million people, before "outcome too rare"

# designs whose outcome column is case/control status
CASE_DESIGNS <- c("case_control", "matched_cc", "nested_cc")

design_defaults <- function(design, oc, sc) {
    d <- switch(design,
        cross_sectional = list(),
        cohort = list(followup = oc$followup, loss = 0.02, accrual = 0),
        prognosis = list(followup = oc$followup, loss = 0.02, accrual = 2),
        case_control = list(ratio = 1),
        matched_cc = list(ratio = 1, match = intersect(c("age", "sex"), names(sc$variables)), caliper = 2),
        rct = list(followup = oc$followup, loss = 0.02, block = 4),
        crossover = list(period = 0, carryover = 0),
        nested_cc = list(ratio = 1, followup = oc$followup, loss = 0.02, accrual = 0),
        cluster_rct = list(followup = oc$followup, loss = 0.02, clusters = 20, icc = 0.05),
        factorial = list(followup = oc$followup, loss = 0.02, block = 4, interaction = 0),
        longitudinal = list(block = 4, visits = 4, dropout = 0.2, trend = 0),
        ecological = list(regions = 30, region_size = 2000, region_sd = 0.5, context_sd = 0.2),
        diagnostic = list(),
        pre_post = list(threshold = 0, trend = 0),
        method_comparison = list(bias = 0, slope = 1, error_a = 0.10, error_b = 0.15),
        case_cohort = list(subcohort = 0.1, followup = oc$followup, loss = 0.02, accrual = 0))
    utils::modifyList(d, sc$design_defaults[[design]] %||% list())
}

# event outcome read as a status: an event within the outcome's risk window
# (and before a competing event, if the outcome has one)
event_status <- function(oc, cal, lp, noise, cov) {
    t <- draw_outcome(oc, cal, lp, noise)
    as.integer(t <= oc$window & t < competing_time(oc, cal, noise, cov))
}

# binary status for any outcome a status-based design can use
status_of <- function(oc, cal, lp, noise, cov) {
    if (oc$type == "event") event_status(oc, cal, lp, noise, cov) else draw_outcome(oc, cal, lp, noise)
}

# follow-up of an event outcome: time to the first of the event, the competing
# event and censoring; status 1 = event, 2 = competing event, 0 = censored
event_follow_up <- function(oc, cal, lp, noise, cov, opt, trt = NULL) {
    t1 <- draw_outcome(oc, cal, lp, noise)
    t2 <- competing_time(oc, cal, noise, cov, trt)
    fu <- follow_up(pmin(t1, t2), noise, opt$followup, opt$loss, opt$accrual %||% 0)
    fu$status <- ifelse(fu$status == 1L, ifelse(t1 <= t2, 1L, 2L), 0L)
    fu
}

# follow-up with administrative censoring (staggered entry over `accrual`)
# and exponential loss to follow-up at rate `loss` per time unit
follow_up <- function(t, noise, followup, loss, accrual) {
    admin <- followup + accrual * (1 - noise$u3)
    cens <- if (loss > 0) -log(noise$u2) / loss else Inf
    end <- pmin(admin, cens)
    list(time = pmin(t, end), status = as.integer(t <= end))
}

sim_cross_sectional <- function(sc, oc, cal, seed, n, opt) {
    pop <- population(sc, oc$id, seed, n)
    lp <- linear_predictor(oc, pop$cov)
    d <- pop$cov
    d$.y <- if (oc$type %in% c("binary", "event")) status_of(oc, cal, lp, pop$noise, pop$cov) else draw_outcome(oc, cal, lp, pop$noise)
    list(data = d, facts = list())
}

# lp_add: extra terms of the linear predictor (a cluster effect, an
# interaction); noise_z: replaces the person's noise of a continuous outcome
sim_cohort <- function(sc, oc, cal, seed, n, opt, trt = NULL, lp_add = 0, noise_z = NULL, pop = NULL) {
    if (is.null(pop)) pop <- population(sc, oc$id, seed, n)
    if (!is.null(noise_z)) pop$noise$z <- noise_z
    lp <- linear_predictor(oc, pop$cov, trt) + lp_add
    d <- pop$cov
    if (!is.null(trt)) for (v in names(trt)) d[[paste0(".", v)]] <- trt[[v]]
    if (oc$type == "event") {
        fu <- event_follow_up(oc, cal, lp, pop$noise, pop$cov, opt, trt)
        d$.time <- fu$time; d$.status <- fu$status
        facts <- list(events = sum(fu$status == 1L), person_time = sum(fu$time))
        if (!is.null(oc$competing)) facts$competing <- sum(fu$status == 2L)
    } else {
        d$.y <- draw_outcome(oc, cal, lp, pop$noise)
        facts <- list()
    }
    list(data = d, facts = facts)
}

# Scan the superpopulation chunk by chunk until there are enough cases.
scan_cases <- function(sc, oc, cal, seed, n_cases) {
    cov <- list(); st <- list(); have <- 0L; k <- 0L
    while (have < n_cases) {
        k <- k + 1L
        if (k > MAX_SCAN)
            stop(sprintf("the outcome is too rare: %d cases in %s people. Ask for fewer cases, or raise its frequency.",
                         have, base::format(MAX_SCAN * CHUNK, big.mark = ",")), call. = FALSE)
        ch <- pop_chunk(sc, oc$id, seed, k)
        s <- status_of(oc, cal, linear_predictor(oc, ch$cov), ch$noise, ch$cov)
        cov[[k]] <- ch$cov; st[[k]] <- s; have <- have + sum(s)
    }
    d <- do.call(rbind, cov); d$.y <- unlist(st)
    rownames(d) <- NULL
    d
}

sim_case_control <- function(sc, oc, cal, seed, n, opt) {
    ratio <- opt$ratio
    n_cases <- max(1L, round(n / (1 + ratio)))
    pool <- scan_cases(sc, oc, cal, seed, n_cases)
    cases <- which(pool$.y == 1)[seq_len(n_cases)]
    scanned <- max(cases)
    nonc <- which(pool$.y[seq_len(scanned)] == 0)
    n_ctrl <- n - n_cases
    if (length(nonc) < n_ctrl) {
        extra <- which(pool$.y == 0 & seq_len(nrow(pool)) > scanned)
        nonc <- c(nonc, extra)
    }
    ctrl <- with_seed(derive_seed(seed, sc$id, oc$id, "controls", n), sample(nonc, n_ctrl))
    d <- pool[c(cases, ctrl), , drop = FALSE]
    list(data = d, facts = list(cases = n_cases, controls = n_ctrl, source_population = scanned,
                                risk_in_source = mean(pool$.y[seq_len(scanned)])))
}

sim_matched_cc <- function(sc, oc, cal, seed, n, opt) {
    ratio <- opt$ratio
    mv <- intersect(unlist(opt$match), names(sc$variables))
    n_cases <- max(1L, round(n / (1 + ratio)))
    pool <- scan_cases(sc, oc, cal, seed, n_cases)
    cases <- which(pool$.y == 1)[seq_len(n_cases)]
    # controls come from a source population 4x the one the cases came from, so
    # most cases find their matches
    while (nrow(pool) < max(cases) * 4L && nrow(pool) < MAX_SCAN * CHUNK) {
        k <- nrow(pool) %/% CHUNK + 1L
        ch <- pop_chunk(sc, oc$id, seed, k)
        ch$cov$.y <- status_of(oc, cal, linear_predictor(oc, ch$cov), ch$noise, ch$cov)
        pool <- rbind(pool, ch$cov)
    }
    rownames(pool) <- NULL
    exact <- mv[vapply(mv, function(v) sc$variables[[v]]$type %in% c("binary", "nominal", "ordinal", "genotype"), TRUE)]
    near <- setdiff(mv, exact)
    # one group for all when matching only on caliper variables (a list cannot be indexed by "")
    key <- if (length(exact)) paste0("k", do.call(paste, c(pool[exact], sep = "\r"))) else rep("all", nrow(pool))
    avail <- pool$.y == 0
    # candidates grouped by the exact-match key and sorted on the first
    # caliper variable, so each case looks only at a window of its group
    sortv <- if (length(near)) near[1] else NULL
    groups <- split(which(avail), key[avail])
    if (!is.null(sortv)) groups <- lapply(groups, function(g) g[order(pool[[sortv]][g])])
    sets <- list(); incomplete <- 0L
    with_seed(derive_seed(seed, sc$id, oc$id, "matching", n), {
        for (i in seq_along(cases)) {
            ci <- cases[i]
            ok <- groups[[key[ci]]]
            if (!is.null(sortv) && length(ok)) {
                x <- pool[[sortv]][ok]
                lo <- findInterval(pool[[sortv]][ci] - opt$caliper, x, left.open = TRUE) + 1L
                hi <- findInterval(pool[[sortv]][ci] + opt$caliper, x)
                ok <- if (hi >= lo) ok[lo:hi] else integer()
            }
            ok <- ok[avail[ok]]
            for (v in near[-1]) ok <- ok[abs(pool[[v]][ok] - pool[[v]][ci]) <= opt$caliper]
            take <- if (length(ok) > ratio) ok[sample.int(length(ok), ratio)] else ok
            if (length(take) < ratio) incomplete <- incomplete + 1L
            avail[take] <- FALSE
            sets[[i]] <- data.frame(row = c(ci, take), set = i)
        }
    })
    s <- do.call(rbind, sets)
    d <- pool[s$row, , drop = FALSE]
    d$.set <- s$set
    list(data = d, facts = list(cases = n_cases, controls = nrow(d) - n_cases, matched_on = mv,
                                caliper = if (length(near)) opt$caliper else NULL, incomplete_sets = incomplete))
}

# A followed cohort (event outcome), chunk by chunk, until enough(d) is TRUE
# for the cohort so far; d has the covariates, .time, .status and .u (a
# uniform per person for designs that sample at random, e.g. a subcohort).
scan_cohort <- function(sc, oc, cal, seed, opt, enough) {
    parts <- list(); k <- 0L
    repeat {
        k <- k + 1L
        if (k > MAX_SCAN)
            stop(sprintf("the outcome is too rare: too few events among %s people. Ask for a smaller study, or raise its frequency.",
                         base::format(MAX_SCAN * CHUNK, big.mark = ",")), call. = FALSE)
        ch <- pop_chunk(sc, oc$id, seed, k)
        fu <- event_follow_up(oc, cal, linear_predictor(oc, ch$cov), ch$noise, ch$cov, opt)
        d <- ch$cov
        # a competing event ends follow-up like censoring here
        d$.time <- fu$time; d$.status <- as.integer(fu$status == 1L)
        d$.u <- with_seed(derive_seed(seed, sc$id, "sampling", k), stats::runif(CHUNK))
        parts[[k]] <- d
        all <- do.call(rbind, parts)
        m <- enough(all)
        if (!is.na(m)) { all <- all[seq_len(m), , drop = FALSE]; rownames(all) <- NULL; return(all) }
    }
}

# Nested case-control: every event of a followed cohort is a case; its controls
# are drawn from its risk set (people still followed at the case's event time,
# future cases included), so a person can be a control more than once and a
# control can later become a case. The OR of conditional logistic regression
# estimates the hazard ratio, with no rare-disease assumption.
sim_nested_cc <- function(sc, oc, cal, seed, n, opt) {
    if (oc$type != "event") stop("a nested case-control study needs a time-to-event outcome", call. = FALSE)
    ratio <- opt$ratio
    n_cases <- max(1L, round(n / (1 + ratio)))
    coh <- scan_cohort(sc, oc, cal, seed, opt, function(d) {
        m <- match(n_cases, cumsum(d$.status)); if (is.na(m)) NA else m })
    o <- order(coh$.time)
    t_sorted <- coh$.time[o]
    cases <- which(coh$.status == 1)
    sets <- list(); short <- 0L
    with_seed(derive_seed(seed, sc$id, oc$id, "risksets", n), {
        for (i in seq_along(cases)) {
            ci <- cases[i]
            # risk set: followed at least as long as the case, the case excluded
            rs <- o[t_sorted >= coh$.time[ci]]
            rs <- rs[rs != ci]
            take <- if (length(rs) > ratio) rs[sample.int(length(rs), ratio)] else rs
            if (length(take) < ratio) short <- short + 1L
            sets[[i]] <- data.frame(row = c(ci, take), set = i, case = c(1L, rep(0L, length(take))), at = coh$.time[ci])
        }
    })
    s <- do.call(rbind, sets)
    d <- coh[s$row, , drop = FALSE]
    d$.set <- s$set; d$.y <- s$case; d$.at <- s$at
    rownames(d) <- NULL
    list(data = d, facts = list(cases = n_cases, controls = nrow(d) - n_cases, source_population = nrow(coh),
                                risk_in_source = mean(coh$.status), person_time = sum(coh$.time),
                                incomplete_sets = short))
}

# Case-cohort: a random subcohort of the followed cohort plus every case
# outside it. The cohort is as large as needed for the study to have n people.
sim_case_cohort <- function(sc, oc, cal, seed, n, opt) {
    if (oc$type != "event") stop("a case-cohort study needs a time-to-event outcome", call. = FALSE)
    f <- opt$subcohort
    if (!is.numeric(f) || f <= 0 || f >= 1) stop("the subcohort fraction must be between 0 and 1", call. = FALSE)
    coh <- scan_cohort(sc, oc, cal, seed, opt, function(d) {
        size <- cumsum(d$.u < f | d$.status == 1)
        m <- match(n, size); if (is.na(m)) NA else m })
    coh$.sub <- as.integer(coh$.u < f)
    d <- coh[coh$.sub == 1 | coh$.status == 1, , drop = FALSE]
    rownames(d) <- NULL
    list(data = d, facts = list(cohort = nrow(coh), subcohort = sum(coh$.sub), cases = sum(coh$.status),
                                cases_in_subcohort = sum(coh$.sub == 1 & coh$.status == 1)))
}

trial_iv <- function(sc, oc) {
    iv <- intersect(names(oc$effects), names(sc$interventions))
    if (length(iv) == 0) stop(sprintf("outcome '%s' has no intervention effect, so it cannot be a trial", oc$id), call. = FALSE)
    sc$interventions[[iv[1]]]
}

# the two interventions of a 2x2 factorial trial (two levels each)
factorial_ivs <- function(sc, oc) {
    iv <- intersect(names(oc$effects), names(sc$interventions))
    iv <- iv[vapply(iv, function(v) length(sc$interventions[[v]]$levels) == 2, TRUE)]
    if (length(iv) < 2) stop(sprintf("a 2x2 factorial trial needs two interventions with two levels acting on '%s'", oc$id), call. = FALSE)
    sc$interventions[iv[1:2]]
}

# O'Brien-Fleming boundaries (two-sided 5%) for K equally spaced looks:
# |z| >= c_K sqrt(K / k) at look k (Jennison & Turnbull 2000, table 2.3)
obf_bounds <- function(K) {
    cK <- c(`2` = 1.977, `3` = 2.004, `4` = 2.024, `5` = 2.040)[[as.character(K)]]
    cK * sqrt(K / seq_len(K))
}

# permuted-block randomisation of n people to the intervention's levels
randomise <- function(iv, n, block, seed) {
    unit <- rep(seq_along(iv$levels), round(iv$ratio))
    reps <- max(1L, round(block / length(unit)))
    blk <- rep(unit, reps)
    with_seed(seed, unlist(lapply(seq_len(ceiling(n / length(blk))), function(i) sample(blk))))[seq_len(n)]
}

sim_rct <- function(sc, oc, cal, seed, n, opt) {
    iv <- trial_iv(sc, oc)
    arm <- randomise(iv, n, opt$block, derive_seed(seed, sc$id, "randomisation", n))
    trt <- stats::setNames(list(arm), iv$id)
    r <- sim_cohort(sc, oc, cal, seed, n, opt, trt)
    r$facts$arms <- stats::setNames(as.list(tabulate(arm, length(iv$levels))), iv$levels)
    # interim looks: participants in order of enrolment, each analysed from the
    # first look after their outcome is known (not for time-to-event outcomes,
    # whose information grows with follow-up)
    K <- opt$looks %||% 0
    if (K >= 2 && oc$type != "event") {
        r$data$.look <- ceiling(K * seq_len(n) / n)
        r$facts$looks <- list(n = as.integer(table(r$data$.look)), bounds = obf_bounds(K))
    }
    r
}

# Cluster-randomised trial: n people in `clusters` clusters of (nearly) equal
# size, arms randomised by cluster, a cluster random effect giving the
# intraclass correlation `icc` (on the latent logistic scale for non-continuous
# outcomes). The true effects are conditional on the cluster.
sim_cluster_rct <- function(sc, oc, cal, seed, n, opt) {
    iv <- trial_iv(sc, oc)
    # a small study has fewer clusters (at least 2 people each); the summary reports them
    K <- min(as.integer(opt$clusters), n %/% 2L)
    if (K < 4) stop("a cluster trial needs at least 4 clusters and 2 people per cluster", call. = FALSE)
    if (opt$icc < 0 || opt$icc >= 1) stop("the intraclass correlation must be between 0 and 1", call. = FALSE)
    cl <- sort(rep(seq_len(K), length.out = n))
    carm <- randomise(iv, K, length(iv$levels) * 2, derive_seed(seed, sc$id, "cluster randomisation", n, K))
    b <- with_seed(derive_seed(seed, sc$id, oc$id, "clusters", n, K), stats::rnorm(K))
    pop <- population(sc, oc$id, seed, n)
    trt <- stats::setNames(list(carm[cl]), iv$id)
    r <- if (oc$type == "continuous")
        sim_cohort(sc, oc, cal, seed, n, opt, trt, noise_z = sqrt(1 - opt$icc) * pop$noise$z + sqrt(opt$icc) * b[cl], pop = pop)
    else sim_cohort(sc, oc, cal, seed, n, opt, trt, lp_add = sqrt(opt$icc / (1 - opt$icc) * pi^2 / 3) * b[cl], pop = pop)
    r$data$.cluster <- cl
    r$facts$arms <- stats::setNames(as.list(tabulate(carm[cl], length(iv$levels))), iv$levels)
    r$facts$clusters <- c(K, tabulate(carm, length(iv$levels)))
    r$facts$icc <- opt$icc
    r
}

# 2x2 factorial trial: two interventions randomised independently (permuted
# blocks of the four combinations); `interaction` is added to the linear
# predictor of those given both (beta, or log OR/HR/RR)
sim_factorial <- function(sc, oc, cal, seed, n, opt) {
    ivs <- factorial_ivs(sc, oc)
    combo <- randomise(list(levels = 1:4, ratio = rep(1, 4)), n, max(4, opt$block), derive_seed(seed, sc$id, "factorial", n))
    a1 <- (combo - 1L) %% 2L + 1L; a2 <- (combo - 1L) %/% 2L + 1L
    trt <- stats::setNames(list(a1, a2), names(ivs))
    r <- sim_cohort(sc, oc, cal, seed, n, opt, trt, lp_add = (opt$interaction %||% 0) * (a1 == 2 & a2 == 2))
    r$facts$cells <- tabulate(combo, 4)
    r$facts$factorial <- names(ivs)
    r
}

sim_crossover <- function(sc, oc, cal, seed, n, opt) {
    if (oc$type != "continuous") stop("a crossover trial needs a continuous outcome", call. = FALSE)
    iv <- trial_iv(sc, oc)
    if (length(iv$levels) != 2) stop("a 2x2 crossover needs an intervention with two levels", call. = FALSE)
    pop <- population(sc, oc$id, seed, n)
    sq <- randomise(list(levels = 1:2, ratio = c(1, 1)), n, 4, derive_seed(seed, sc$id, "sequence", n))
    rho <- oc$within_cor
    zp <- with_seed(derive_seed(seed, sc$id, oc$id, "periods", n), matrix(stats::rnorm(2 * n), n))
    rows <- lapply(1:2, function(p) {
        t <- if (p == 1) sq else 3L - sq
        lp <- linear_predictor(oc, pop$cov, stats::setNames(list(t), iv$id))
        y <- cal$a + lp + oc$sd * (sqrt(rho) * pop$noise$z + sqrt(1 - rho) * zp[, p]) +
            (p == 2) * opt$period + (p == 2 & sq == 2) * opt$carryover
        if (isTRUE(oc$log)) y <- exp(y)
        if (!is.null(oc$range)) y <- pmin(pmax(y, oc$range[1]), oc$range[2])
        if (!is.null(oc$round)) y <- round(y, oc$round)
        d <- pop$cov
        d$.seq <- sq; d$.period <- p; d[[paste0(".", iv$id)]] <- t; d$.y <- y
        d$.row <- seq_len(n)
        d
    })
    d <- do.call(rbind, rows)
    d <- d[order(d$.row, d$.period), ]
    d$.row <- NULL
    rownames(d) <- NULL
    list(data = d, facts = list(sequences = tabulate(sq, 2), within_cor = rho))
}

# a continuous outcome's value from its linear predictor and noise (log
# scale, range, rounding), as draw_outcome() does
finish_continuous <- function(y, oc) {
    if (isTRUE(oc$log)) y <- exp(y)
    if (!is.null(oc$range)) y <- pmin(pmax(y, oc$range[1]), oc$range[2])
    if (!is.null(oc$round)) y <- round(y, oc$round)
    y
}

# Longitudinal trial: the outcome at baseline (visit 0, before treatment) and
# at `visits` follow-up visits; the treatment effect grows linearly to its full
# value at the last visit; a subject random intercept gives the correlation
# `within_cor` between visits. Dropout is monotone and missing at random: the
# odds of leaving before the next visit double per SD of the last observed
# value, calibrated so that `dropout` of the participants miss the last visit.
sim_longitudinal <- function(sc, oc, cal, seed, n, opt) {
    if (oc$type != "continuous") stop("a longitudinal trial needs a continuous outcome", call. = FALSE)
    iv <- trial_iv(sc, oc)
    K <- as.integer(opt$visits)
    if (K < 1) stop("a longitudinal trial needs at least one follow-up visit", call. = FALSE)
    pop <- population(sc, oc$id, seed, n)
    arm <- randomise(iv, n, opt$block, derive_seed(seed, sc$id, "randomisation", n))
    lp0 <- linear_predictor(oc, pop$cov)
    tr <- linear_predictor(oc, pop$cov, stats::setNames(list(arm), iv$id)) - lp0
    rho <- oc$within_cor
    e <- with_seed(derive_seed(seed, sc$id, oc$id, "visits", n, K), matrix(stats::rnorm(n * (K + 1)), n))
    # the treatment effect at each visit as a multiple of its value at the last
    # visit: linear by default, or the scenario's `profile` (given at equally
    # spaced visits, interpolated to these); `trend`: change in the reference
    # arm from baseline to the last visit (the outcome's mean is at the last)
    prof <- opt$profile
    mult <- if (length(prof)) stats::approx(c(0, seq_along(prof) / length(prof)), c(0, prof), xout = (0:K) / K)$y else (0:K) / K
    trend <- opt$trend %||% 0
    Y <- sapply(0:K, function(j) cal$a + lp0 + tr * mult[j + 1] + trend * (j / K - 1) +
                                 oc$sd * (sqrt(rho) * pop$noise$z + sqrt(1 - rho) * e[, j + 1]))
    # dropout after visit j (0..K-1), driven by the value seen at visit j
    U <- with_seed(derive_seed(seed, sc$id, oc$id, "dropout", n, K), matrix(stats::runif(n * K), n))
    Z <- scale(Y[, 1:K, drop = FALSE])
    last <- function(alpha) {
        drop <- U < stats::plogis(alpha + log(2) * Z)
        apply(cbind(drop, TRUE), 1, function(x) which(x)[1]) - 1L   # last visit attended
    }
    lastv <- if (opt$dropout <= 0) rep(K, n) else {
        alpha <- stats::uniroot(function(a) mean(last(a) < K) - opt$dropout, c(-30, 10))$root
        last(alpha)
    }
    rows <- lapply(0:K, function(j) {
        keep <- lastv >= j
        d <- pop$cov[keep, , drop = FALSE]
        d[[paste0(".", iv$id)]] <- arm[keep]
        d$.visit <- j; d$.y <- finish_continuous(Y[keep, j + 1], oc); d$.row <- which(keep)
        d
    })
    d <- do.call(rbind, rows)
    d <- d[order(d$.row, d$.visit), ]
    d$.row <- NULL
    rownames(d) <- NULL
    list(data = d, facts = list(arms = stats::setNames(as.list(tabulate(arm, length(iv$levels))), iv$levels),
                                visits = K, attending = vapply(0:K, function(j) sum(lastv >= j), 0L),
                                completers_by_arm = tabulate(arm[lastv == K], length(iv$levels))))
}

# Ecological study: `regions` regions of `region_size` people each (whole
# regions: the study size n is not used). Regions differ in
# the means of the exogenous latents (SD `region_sd`, correlated as the
# variables are), which the structure carries to the others, and in an
# unexplained region effect on the outcome (SD `context_sd`, linear predictor
# scale). Only region totals and means are kept: population, cases (binary or
# event outcomes, within the risk window) or the mean outcome, the mean of
# each numeric variable and the proportion in each non-reference level.
sim_ecological <- function(sc, oc, cal, seed, n, opt) {
    m <- as.integer(opt$region_size)
    n <- as.integer(opt$regions)
    if (n < 3) stop("an ecological study needs at least 3 regions", call. = FALSE)
    if (m < 10) stop("regions need at least 10 people", call. = FALSE)
    exo <- sc$exogenous
    # between-region correlations: the individual ones, unless `between`
    # (triples variable, variable, r) sets them (contextual confounding)
    Rb <- sc$R_exo[exo, exo, drop = FALSE]
    if (length(opt$between)) {
        b <- matrix(opt$between, ncol = 3, byrow = TRUE)
        for (i in seq_len(nrow(b))) if (all(b[i, 1:2] %in% exo)) Rb[b[i, 1], b[i, 2]] <- Rb[b[i, 2], b[i, 1]] <- as.numeric(b[i, 3])
        Rb <- near_pd(Rb)
    }
    Lb <- chol(Rb)
    rows <- lapply(seq_len(n), function(g) {
        shift <- with_seed(derive_seed(seed, sc$id, "region", g), {
            z <- stats::rnorm(length(exo))
            stats::setNames(opt$region_sd * as.numeric(z %*% Lb), exo)
        })
        ctx <- with_seed(derive_seed(seed, sc$id, oc$id, "context", g), stats::rnorm(1)) * opt$context_sd
        cov <- with_seed(derive_seed(seed, sc$id, "region people", g), gen_covariates(sc, m, shift))
        noise <- with_seed(derive_seed(seed, sc$id, oc$id, "region noise", g),
                           data.frame(u = stats::runif(m), z = stats::rnorm(m), u2 = stats::runif(m), u3 = stats::runif(m),
                                      u4 = stats::runif(m)))
        lp <- linear_predictor(oc, cov) + ctx
        y <- if (oc$type %in% c("binary", "event")) status_of(oc, cal, lp, noise, cov) else draw_outcome(oc, cal, lp, noise)
        out <- list(.region = g, .pop = m, .y = if (oc$type %in% c("binary", "event")) sum(y) else mean(y))
        for (v in sc$order) {
            x <- sc$variables[[v]]
            if (x$type %in% c("continuous", "count", "binary")) out[[v]] <- mean(cov[[v]])
            else for (k in seq_along(x$levels)[-1]) out[[eco_col(v, x$levels[k])]] <- mean(cov[[v]] == k)
        }
        as.data.frame(out, check.names = FALSE)
    })
    d <- do.call(rbind, rows)
    list(data = d, facts = list(regions = n, region_size = m, population = n * m,
                                total = if (oc$type %in% c("binary", "event")) sum(d$.y) else NULL))
}

# column of the proportion in one level of a categorical variable (engine name)
eco_col <- function(v, level) paste0(v, "_", gsub("[^A-Za-z0-9]+", "_", level))

# region-level columns standing for a variable
eco_cols <- function(sc, v) {
    x <- sc$variables[[v]]
    if (is.null(x) || x$type %in% c("continuous", "count", "binary")) v else eco_col(v, x$levels[-1])
}

# column names of a marker (one, or the block's p)
marker_cols <- function(m) if (is.null(m$block)) m$id else sprintf("%s%0*d", m$id, nchar(m$block), seq_len(m$block))

# Diagnostic accuracy study: a cross-sectional sample (disease status by the
# outcome model) with the outcome's markers measured on everybody
sim_diagnostic <- function(sc, oc, cal, seed, n, opt) {
    if (length(oc$markers) == 0) stop(sprintf("outcome '%s' has no markers, so it cannot be a diagnostic study", oc$id), call. = FALSE)
    r <- sim_cross_sectional(sc, oc, cal, seed, n, opt)
    y <- r$data$.y
    mk <- with_seed(derive_seed(seed, sc$id, oc$id, "markers", n), {
        cols <- list()
        rc <- oc$marker_cor %||% 0
        shared <- if (rc > 0) stats::rnorm(n) else 0
        for (m in oc$markers) {
            if (!is.null(m$block)) {
                f <- stats::rnorm(n)
                E <- sqrt(m$rho) * f + sqrt(1 - m$rho) * matrix(stats::rnorm(n * m$block), n)
                k <- seq_len(m$informative)
                E[, k] <- E[, k] + m$dir * m$delta * y
                nm <- marker_cols(m)
                for (j in seq_len(m$block)) cols[[nm[j]]] <- round(E[, j], 3)
            } else {
                z <- sqrt(rc) * shared + sqrt(1 - rc) * stats::rnorm(n) + m$dir * m$delta * y
                x <- if (is.null(m$dist)) z else qdist(stats::pnorm(z), m$dist, m$range)
                cols[[m$id]] <- if (!is.null(m$round)) round(x, m$round) else round(x, 3)
            }
        }
        as.data.frame(cols, check.names = FALSE)
    })
    r$markers <- mk
    r$facts$prevalence <- mean(y)
    r$facts$markers <- names(oc$markers)
    r
}

# empirical AUC of x for status d (1 = diseased), Mann-Whitney
auc_of <- function(x, d, dir = 1) {
    ok <- !is.na(x) & !is.na(d)
    x <- dir * x[ok]; d <- d[ok] == 1
    n1 <- sum(d); n0 <- sum(!d)
    if (n1 == 0 || n0 == 0) return(NA_real_)
    (sum(rank(x)[d]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# per marker (a block as its informative and its noise markers): the true AUC
# and the AUC in a formatted study fd (status from the outcome column)
marker_table <- function(oc, fd) {
    d <- as.integer(fd[[oc$id]]) - 1L
    rows <- list()
    for (m in oc$markers) {
        if (is.null(m$block)) {
            rows[[length(rows) + 1]] <- data.frame(marker = m$id, part = "", n_markers = 1L, true = m$auc,
                                                   auc = auc_of(fd[[m$id]], d, m$dir), stringsAsFactors = FALSE)
        } else {
            nm <- marker_cols(m); k <- m$informative
            a <- vapply(nm, function(v) auc_of(fd[[v]], d, m$dir), 0)
            if (k > 0) rows[[length(rows) + 1]] <- data.frame(marker = m$id, part = sprintf("%s-%s", nm[1], nm[k]), n_markers = k,
                                                              true = m$auc, auc = mean(a[seq_len(k)]), stringsAsFactors = FALSE)
            if (k < m$block) rows[[length(rows) + 1]] <- data.frame(marker = m$id, part = sprintf("%s-%s", nm[k + 1], nm[m$block]),
                                                                    n_markers = m$block - k, true = 0.5, auc = mean(a[-seq_len(k)]),
                                                                    stringsAsFactors = FALSE)
        }
    }
    do.call(rbind, rows)
}

# Before-after study without a control group: everybody gets the
# intervention; the outcome is measured before (without it) and after.
# `threshold` (percentile, 0-99) enrols only people whose value before is
# above it, as when patients are recruited for being high: their values fall
# on remeasurement anyway (regression to the mean). `trend`: change that would
# happen without treatment.
sim_pre_post <- function(sc, oc, cal, seed, n, opt) {
    if (oc$type != "continuous") stop("a before-after study needs a continuous outcome", call. = FALSE)
    iv <- trial_iv(sc, oc)
    rho <- oc$within_cor
    draw <- function(k) {
        ch <- pop_chunk(sc, oc$id, seed, k)
        lp0 <- linear_predictor(oc, ch$cov)
        tr <- linear_predictor(oc, ch$cov, stats::setNames(list(rep(length(iv$levels), CHUNK)), iv$id)) - lp0
        e <- with_seed(derive_seed(seed, sc$id, oc$id, "prepost", k), matrix(stats::rnorm(2 * CHUNK), CHUNK))
        y0 <- cal$a + lp0 + oc$sd * (sqrt(rho) * ch$noise$z + sqrt(1 - rho) * e[, 1])
        y1 <- cal$a + lp0 + tr + (opt$trend %||% 0) + oc$sd * (sqrt(rho) * ch$noise$z + sqrt(1 - rho) * e[, 2])
        d <- ch$cov; d$.y0 <- y0; d$.y <- y1; d
    }
    th <- opt$threshold %||% 0
    cut <- if (th > 0) stats::quantile(draw(1)$.y0, th / 100, names = FALSE) else -Inf
    parts <- list(); have <- 0L; k <- 0L
    while (have < n) {
        k <- k + 1L
        if (k > MAX_SCAN) stop("too few people above the entry threshold; lower it", call. = FALSE)
        d <- draw(k); d <- d[d$.y0 > cut, , drop = FALSE]
        parts[[k]] <- d; have <- have + nrow(d)
    }
    d <- do.call(rbind, parts)[seq_len(n), , drop = FALSE]
    d$.y0 <- finish_continuous(d$.y0, oc); d$.y <- finish_continuous(d$.y, oc)
    rownames(d) <- NULL   # everybody treated: no treatment column
    list(data = d, facts = list(screened = if (th > 0) k * CHUNK else NULL, threshold = if (th > 0) th else NULL))
}

# Method comparison: the outcome's quantity measured by two methods in the
# same people: A = true + error, B = bias + slope x true + error (errors as a
# share of the outcome's SD).
sim_method_comparison <- function(sc, oc, cal, seed, n, opt) {
    if (oc$type != "continuous") stop("a method comparison needs a continuous outcome", call. = FALSE)
    pop <- population(sc, oc$id, seed, n)
    true <- draw_outcome(oc, cal, linear_predictor(oc, pop$cov), pop$noise)
    e <- with_seed(derive_seed(seed, sc$id, oc$id, "methods", n), matrix(stats::rnorm(2 * n), n))
    s <- stats::sd(true)
    a <- true + opt$error_a * s * e[, 1]
    b <- opt$bias + opt$slope * true + opt$error_b * s * e[, 2]
    rd <- function(x) if (!is.null(oc$round)) round(x, oc$round + 1) else x
    d <- pop$cov; d$.ma <- rd(a); d$.mb <- rd(b)
    list(data = d, facts = list(method_bias = opt$bias, method_slope = opt$slope))
}

run_design <- function(design, sc, oc, cal, seed, n, opt) {
    f <- switch(design,
        cross_sectional = sim_cross_sectional, cohort = sim_cohort, prognosis = sim_cohort,
        case_control = sim_case_control, matched_cc = sim_matched_cc,
        rct = sim_rct, crossover = sim_crossover, nested_cc = sim_nested_cc, case_cohort = sim_case_cohort,
        cluster_rct = sim_cluster_rct, factorial = sim_factorial, longitudinal = sim_longitudinal,
        ecological = sim_ecological, diagnostic = sim_diagnostic, pre_post = sim_pre_post,
        method_comparison = sim_method_comparison,
        stop(sprintf("unknown design '%s'", design), call. = FALSE))
    f(sc, oc, cal, seed, n, opt)
}
