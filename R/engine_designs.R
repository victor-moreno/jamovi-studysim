# Design samplers: each turns the superpopulation into one study. They return
# an internal frame (`.pid`, covariate codes, design columns starting with a
# dot, outcome in `.y` or `.time`/`.status`) and a list of facts for the summary.
# `n` is always the number of people in the study (a crossover has 2 rows each).

MAX_SCAN <- 400L   # chunks, i.e. 2 million people, before "outcome too rare"

design_defaults <- function(design, oc, sc) {
    d <- switch(design,
        cross_sectional = list(),
        cohort = list(followup = oc$followup, loss = 0.02, accrual = 0),
        prognosis = list(followup = oc$followup, loss = 0.02, accrual = 2),
        case_control = list(ratio = 1),
        matched_cc = list(ratio = 1, match = intersect(c("age", "sex"), names(sc$variables)), caliper = 2),
        rct = list(followup = oc$followup, loss = 0.02, block = 4),
        crossover = list(period = 0, carryover = 0))
    utils::modifyList(d, sc$design_defaults[[design]] %||% list())
}

# event outcome read as a status: an event within the outcome's risk window
event_status <- function(oc, cal, lp, noise) {
    t <- draw_outcome(oc, cal, lp, noise)
    as.integer(t <= oc$window)
}

# binary status for any outcome a status-based design can use
status_of <- function(oc, cal, lp, noise) {
    if (oc$type == "event") event_status(oc, cal, lp, noise) else draw_outcome(oc, cal, lp, noise)
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
    d$.y <- if (oc$type %in% c("binary", "event")) status_of(oc, cal, lp, pop$noise) else draw_outcome(oc, cal, lp, pop$noise)
    list(data = d, facts = list())
}

sim_cohort <- function(sc, oc, cal, seed, n, opt, trt = NULL) {
    pop <- population(sc, oc$id, seed, n)
    lp <- linear_predictor(oc, pop$cov, trt)
    d <- pop$cov
    if (!is.null(trt)) for (v in names(trt)) d[[paste0(".", v)]] <- trt[[v]]
    if (oc$type == "event") {
        fu <- follow_up(draw_outcome(oc, cal, lp, pop$noise), pop$noise, opt$followup, opt$loss, opt$accrual %||% 0)
        d$.time <- fu$time; d$.status <- fu$status
        facts <- list(events = sum(fu$status), person_time = sum(fu$time))
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
        s <- status_of(oc, cal, linear_predictor(oc, ch$cov), ch$noise)
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
        ch$cov$.y <- status_of(oc, cal, linear_predictor(oc, ch$cov), ch$noise)
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

trial_iv <- function(sc, oc) {
    iv <- intersect(names(oc$effects), names(sc$interventions))
    if (length(iv) == 0) stop(sprintf("outcome '%s' has no intervention effect, so it cannot be a trial", oc$id), call. = FALSE)
    sc$interventions[[iv[1]]]
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

run_design <- function(design, sc, oc, cal, seed, n, opt) {
    f <- switch(design,
        cross_sectional = sim_cross_sectional, cohort = sim_cohort, prognosis = sim_cohort,
        case_control = sim_case_control, matched_cc = sim_matched_cc,
        rct = sim_rct, crossover = sim_crossover,
        stop(sprintf("unknown design '%s'", design), call. = FALSE))
    f(sc, oc, cal, seed, n, opt)
}
