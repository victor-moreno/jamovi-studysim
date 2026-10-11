# Outcome models on the superpopulation: a linear predictor from the
# scenario's conditional effects, an intercept / baseline hazard calibrated to
# the scenario's prevalence, rate, risk, median or mean, and the outcome drawn
# from the person's own noise (so a treated and an untreated version of the
# same person share it: potential outcomes, used by the trial designs).

# Contribution of one effect to the linear predictor. `trt` holds intervention
# codes (1 = reference arm); without it everybody is in the reference arm.
effect_contrib <- function(eff, cov, trt = NULL) {
    if (eff$type == "intervention") {
        x <- trt[[eff$var]]
        if (is.null(x)) return(0)
        if (is.null(eff$by)) return(eff$coef[x])
        m <- cov[[eff$by$var]]
        return(eff$by$coef[if (eff$by$binary) m + 1L else m] * (x == 2L))
    }
    x <- cov[[eff$var]]
    if (eff$type %in% c("nominal", "ordinal")) return(eff$coef[x])
    # centred when modified (`center`, engine_scenario.R): the modifier's effect is the one at x = center
    if (!is.null(eff$center)) x <- x - eff$center
    base <- switch(eff$type,
        continuous = x / eff$per,
        count = x / eff$per,
        binary = x,
        genotype = {
            g <- x - 1L
            switch(eff$model, additive = g, dominant = as.numeric(g >= 1), recessive = as.numeric(g == 2))
        })
    coef <- if (is.null(eff$by)) eff$coef else {
        m <- cov[[eff$by$var]]
        # binary codes are 0/1, categorical ones 1..K
        eff$by$coef[if (eff$by$binary) m + 1L else m]
    }
    coef * base
}

linear_predictor <- function(oc, cov, trt = NULL) {
    lp <- numeric(nrow(cov))
    for (e in oc$effects) lp <- lp + effect_contrib(e, cov, trt)
    lp
}

.calib_cache <- new.env()

# Intercept (binary, continuous, count) or baseline Weibull scale (event),
# solved on a fixed calibration sample of 100 000 people so that it depends on
# the scenario only, never on the seed or the design.
calibrate <- function(sc, oc) {
    key <- digest::digest(list(sc$variables, sc$edges, sc$R_exo, oc$effects, oc[setdiff(names(oc), "effects")]))
    if (!is.null(.calib_cache[[key]])) return(.calib_cache[[key]])
    cov <- with_seed(derive_seed("calibration", sc$id), gen_covariates(sc, 100000L))
    lp <- linear_predictor(oc, cov)
    out <- switch(oc$type,
        binary = {
            a <- stats::uniroot(function(a) mean(stats::plogis(a + lp)) - oc$prevalence, c(-40, 40), tol = 1e-10)$root
            list(a = a, achieved = mean(stats::plogis(a + lp)))
        },
        continuous = list(a = (if (isTRUE(oc$log)) log(oc$mean) else oc$mean) - mean(lp), achieved = oc$mean),
        count = list(a = log(oc$mean) - log(mean(exp(lp))), achieved = oc$mean),
        event = if (is.null(oc$target)) list(achieved = list()) else {
            g <- oc$shape; elp <- exp(lp); t <- oc$target
            if (t$kind == "rate" && g == 1) {
                lam <- t$value / mean(elp)
            } else {
                # rate with a non-exponential shape: read as the risk over the follow-up
                if (t$kind == "rate") t <- list(kind = "risk", value = 1 - exp(-t$value * oc$followup), at = oc$followup)
                f <- switch(t$kind,
                    risk = function(ll) mean(1 - exp(-exp(ll) * t$at^g * elp)) - t$value,
                    median = function(ll) mean(exp(-exp(ll) * t$value^g * elp)) - 0.5)
                lam <- exp(stats::uniroot(f, c(-60, 30), tol = 1e-10)$root)
            }
            list(lambda = lam, shape = g,
                 achieved = list(rate = lam * mean(elp),
                                 risk_followup = mean(1 - exp(-lam * oc$followup^g * elp))))
        })
    # a competing event has its own baseline, calibrated the same way
    if (oc$type == "event" && !is.null(oc$competing)) out$competing <- calibrate(sc, oc$competing)
    if (oc$type == "event" && !is.null(oc$after_event)) out$after <- calibrate(sc, oc$after_event)
    .calib_cache[[key]] <- out
    out
}

# time to the competing event of an event outcome (Inf when it has none)
competing_time <- function(oc, cal, noise, cov, trt = NULL) {
    cc <- oc$competing
    if (is.null(cc)) return(rep(Inf, nrow(noise)))
    lp <- linear_predictor(cc, cov, trt)
    (-log(noise$u4) / (cal$competing$lambda * exp(lp)))^(1 / cal$competing$shape)
}

# Illness-death: time to death for an outcome `after: X`. X happens at t1
# (its own model and noise); death without X at t0 (this outcome's own model,
# or X's competing event); after X, death comes after a further time from
# after_event. nz: this outcome's noise, nzx: X's.
after_event_times <- function(sc, oc, cal, cov, nz, nzx, trt = NULL) {
    ox <- sc$outcomes[[oc[["after"]]]]; calx <- calibrate(sc, ox)
    t1 <- draw_outcome(ox, calx, linear_predictor(ox, cov, trt), nzx)
    if (!is.null(oc$target)) {
        t0 <- draw_outcome(oc, cal, linear_predictor(oc, cov, trt), nz); upost <- nz$u4
    } else {
        t0 <- competing_time(ox, calx, nzx, cov, trt); upost <- nz$u
    }
    ae <- oc$after_event
    tp <- (-log(upost) / (cal$after$lambda * exp(linear_predictor(ae, cov, trt))))^(1 / cal$after$shape)
    ifelse(t1 < t0, t1 + tp, t0)
}

# Draw the outcome from the linear predictor and the person's noise
draw_outcome <- function(oc, cal, lp, noise) {
    switch(oc$type,
        binary = as.integer(noise$u < stats::plogis(cal$a + lp)),
        continuous = {
            y <- cal$a + lp + oc$sd * noise$z
            if (isTRUE(oc$log)) y <- exp(y)
            if (!is.null(oc$range)) y <- pmin(pmax(y, oc$range[1]), oc$range[2])
            if (!is.null(oc$round)) y <- round(y, oc$round)
            y
        },
        count = {
            mu <- exp(cal$a + lp)
            if (is.null(oc$size)) stats::qpois(noise$u, mu) else stats::qnbinom(noise$u, size = oc$size, mu = mu)
        },
        event = (-log(noise$u) / (cal$lambda * exp(lp)))^(1 / cal$shape))
}
