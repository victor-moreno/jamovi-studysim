test_that("nested case-control draws controls from the risk set and estimates the HR", {
  sc <- toy()
  r <- simulate_study(sc, "ev", "nested_cc", n = 600, factors = c("age", "smoke"), key = FALSE)
  d <- r$data
  expect_identical(names(d)[1:4], c("id", "set", "case", "time"))
  expect_true(all(table(d$set) == 2))
  expect_equal(sum(d$case == "case"), 300)
  expect_true(all(tapply(d$time, d$set, function(x) length(unique(x))) == 1))
  big <- simulate_study(sc, "ev", "nested_cc", n = 6000, factors = c("age", "smoke"), key = FALSE)
  e <- estimate_table(sc, sc$outcomes$ev, "nested_cc", big$data_raw, c("age", "smoke"), ci = TRUE)
  expect_true(all(e$lower < e$true & e$true < e$upper))
})

test_that("case-cohort keeps a random subcohort plus all cases and estimates the HR", {
  sc <- toy()
  r <- simulate_study(sc, "ev", "case_cohort", n = 3000, factors = c("age", "smoke"), options = list(subcohort = 0.2), key = FALSE)
  f <- r$facts
  expect_equal(nrow(r$data), 3000)
  expect_equal(f$subcohort / f$cohort, 0.2, tolerance = 0.03)
  expect_equal(sum(r$data$ev), f$cases)
  expect_equal(sum(r$data$subcohort == "yes"), f$subcohort)
  e <- estimate_table(sc, sc$outcomes$ev, "case_cohort", r$data_raw, c("age", "smoke"), ci = TRUE)
  expect_true(all(e$lower < e$true & e$true < e$upper))
})

trial_toy <- function() {
  raw <- toy_raw()
  raw$interventions$salt <- list(levels = c("usual", "lowsalt"))
  raw$outcomes$sbp$effects$salt <- list(beta = c(0, -4))
  normalize_scenario(raw)
}

test_that("a cluster trial randomises clusters and gives the intended ICC", {
  sc <- trial_toy()
  expect_true(all(c("cluster_rct", "factorial") %in% compatible_designs(sc, "sbp")))
  r <- simulate_study(sc, "sbp", "cluster_rct", 3000, c("age"), options = list(clusters = 60, icc = 0.1), key = FALSE)
  d <- r$data_raw
  expect_identical(names(d)[1:3], c("id", "cluster", "drug"))
  expect_true(all(tapply(as.integer(d$drug), d$cluster, function(x) length(unique(x))) == 1))
  a <- anova(lm(sbp ~ factor(cluster), d)); m <- 50
  expect_equal((a[1, 3] - a[2, 3]) / (a[1, 3] + (m - 1) * a[2, 3]), 0.1, tolerance = 0.05)
  naive <- summary(lm(sbp ~ drug + age, d))$coef[2, 2]
  e <- estimate_table(sc, sc$outcomes$sbp, "cluster_rct", d, "age", ci = TRUE)
  expect_gt((e$upper - e$lower) / (2 * 1.96), 1.5 * naive)
})

test_that("a 2x2 factorial trial balances the four cells and averages over an interaction", {
  sc <- trial_toy()
  r <- simulate_study(sc, "sbp", "factorial", 4000, "age", options = list(interaction = 4), key = FALSE)
  expect_equal(r$facts$cells, rep(1000, 4))
  e <- estimate_table(sc, sc$outcomes$sbp, "factorial", r$data_raw, "age")
  expect_equal(e$adjusted, c(-8, -4) + 2, tolerance = 0.15)
})

test_that("interim looks split a trial by enrolment", {
  sc <- trial_toy()
  r <- simulate_study(sc, "sbp", "rct", 300, "age", options = list(looks = 3), key = FALSE)
  expect_equal(as.integer(table(r$data$look)), c(100, 100, 100))
  expect_equal(round(r$facts$looks$bounds, 2), c(3.47, 2.45, 2.00))
  expect_false("look" %in% names(simulate_study(sc, "ev", "rct", 300, "age", options = list(looks = 3), key = FALSE)$data))
})

test_that("a longitudinal trial has monotone dropout near the target and the mixed model recovers the effect", {
  sc <- trial_toy()
  r <- simulate_study(sc, "sbp", "longitudinal", 2000, "age", options = list(visits = 3, dropout = 0.3), key = FALSE)
  d <- r$data_raw
  expect_identical(names(d)[1:3], c("id", "drug", "visit"))
  expect_equal(r$facts$attending[1], 2000)
  expect_true(all(diff(r$facts$attending) <= 0))
  expect_equal(1 - r$facts$attending[4] / 2000, 0.3, tolerance = 0.1)
  expect_true(all(tapply(d$visit, d$id, function(v) identical(v, seq(0, length(v) - 1)))))
  e <- estimate_table(sc, sc$outcomes$sbp, "longitudinal", d, "age", ci = TRUE)
  expect_true(e$lower < -8 && -8 < e$upper)
})

test_that("an ecological study keeps region totals and percentages", {
  sc <- toy()
  r <- simulate_study(sc, "dis", "ecological", 100, c("age", "sex", "smoke"), options = list(regions = 12, region_size = 500),
                      lang = "es", key = FALSE)
  d <- r$data_raw
  expect_equal(nrow(d), 12)
  expect_identical(names(d), c("region", "population", "dis", "age", "sex", "smoke_former", "smoke_current"))
  expect_true(all(d$dis <= d$population))
  expect_true(all(d$smoke_former + d$smoke_current < 100))
  expect_true(sd(d$age) > 1)
  e <- estimate_table(sc, sc$outcomes$dis, "ecological", d, c("age", "sex", "smoke"), ci = TRUE)
  expect_equal(nrow(e), 4)
  expect_true(all(is.finite(e$adjusted)))
  expect_identical(names(r$data)[6], "smoke_former")
})

test_that("a competing event gives status 2 and cause-specific Cox recovers the HRs", {
  raw <- toy_raw()
  raw$outcomes$ev$competing <- list(label = "Other death", risk = list(value = 0.3, at = 10), shape = 1.2,
                                    effects = list(age = list(HR = 1.08)))
  sc <- normalize_scenario(raw)
  r <- simulate_study(sc, "ev", "cohort", 8000, c("age", "smoke"), key = FALSE)
  expect_setequal(unique(r$data$ev), 0:2)
  expect_equal(r$facts$competing, sum(r$data$ev == 2))
  e <- estimate_table(sc, sc$outcomes$ev, "cohort", r$data_raw, c("age", "smoke"), ci = TRUE)
  expect_true(all(e$lower < e$true & e$true < e$upper))
  cc <- simulate_study(sc, "ev", "case_cohort", 600, "age", key = FALSE)
  expect_setequal(unique(cc$data$ev), 0:1)
  base <- simulate_study(toy(), "ev", "cohort", 500, "age", key = FALSE)$data
  expect_lt(sum(r$data$ev[1:500] == 1), sum(base$ev == 1) + 1)
})

test_that("diagnostic markers have the target AUC, keep their marginal and a block mixes signal and noise", {
  raw <- toy_raw()
  raw$outcomes$dis$markers <- list(
    crp = list(dist = "lognormal(3, 2)", auc = 0.75, round = 2),
    alb = list(dist = "normal(40, 4)", auc = 0.65, direction = "lower"),
    omic = list(block = 20, informative = 3, auc = 0.7, rho = 0.3))
  sc <- normalize_scenario(raw)
  expect_true("diagnostic" %in% compatible_designs(sc, "dis"))
  r <- simulate_study(sc, "dis", "diagnostic", 8000, "age", key = FALSE)
  d <- r$data_raw
  expect_true(all(c("crp", "alb", "omic01", "omic20") %in% names(d)))
  mt <- marker_table(sc$outcomes$dis, d)
  expect_equal(mt$auc, mt$true, tolerance = 0.03)
  healthy <- d$dis == "no"
  expect_equal(mean(d$alb[healthy]), 40, tolerance = 0.01)
  expect_equal(cor(d$omic10, d$omic11), 0.3, tolerance = 0.1)
  expect_error(simulate_study(toy(), "dis", "diagnostic", 100), "does not fit")
})

test_that("the PSA scenario runs a diagnostic study through simStudy", {
  r <- simStudy(data = data.frame(), scenario = "prostate_cancer_psa", outcome = "prostate_cancer", design = "diagnostic", n = 300,
                measured = list(list(var = "age", include = TRUE)), showKey = TRUE)
  k <- r$keyMarkers$asDF
  expect_equal(nrow(k), 4)
  expect_true(all(!is.na(k$expected)))
  expect_match(r$summary$content, "markers measured")
})

test_that("a two-arm intervention effect can depend on a modifier", {
  raw <- toy_raw()
  raw$outcomes$dis$effects$drug <- list(OR = c(1, 0.8), by = list(smoke = list(never = 1.3, former = 0.8, current = 0.5)))
  sc <- normalize_scenario(raw)
  r <- simulate_study(sc, "dis", "rct", 60000, sc$order, key = FALSE)$data_raw
  ors <- vapply(c("never", "former", "current"), function(l) {
    f <- glm(dis ~ drug + age + sex + bmi + snp, binomial, r[r$smoke == l, ]); exp(coef(f)[["drugdrug"]]) }, 0)
  expect_equal(unname(ors), c(1.3, 0.8, 0.5), tolerance = 0.12)
})

test_that("realism: measurement error dilutes, MNAR hides high values, noise and entry errors appear", {
  sc <- toy()
  f <- c("age", "bmi", "sex", "smoke")
  r0 <- simulate_study(sc, "sbp", "cross_sectional", 3000, f, key = FALSE)
  r1 <- simulate_study(sc, "sbp", "cross_sectional", 3000, f, key = FALSE, options = list(reliability = 0.5, misclass = 0.2))
  b <- function(d) coef(lm(sbp ~ bmi + age, d))[["bmi"]]
  expect_lt(b(r1$data_raw), 0.8 * b(r0$data_raw))
  expect_gt(cor(r0$data_raw$bmi, r1$data_raw$bmi), 0.6)
  expect_lt(mean(r1$data_raw$sex == r0$data_raw$sex), 0.9)
  m <- simulate_study(sc, "sbp", "cross_sectional", 3000, f, key = FALSE, missing = 0.3, missing_mech = "mnar")
  expect_lt(mean(m$data_raw$bmi, na.rm = TRUE), mean(r0$data_raw$bmi) - 0.3)
  x <- simulate_study(sc, "sbp", "cross_sectional", 500, f, key = FALSE, noise = 3, entry = 0.05)
  expect_true(all(c("extra_01", "extra_02", "extra_03") %in% names(x$data)))
  expect_gt(x$facts$entry_errors, 20)
  expect_identical(tail(planned_columns(sc, sc$outcomes$sbp, "cross_sectional", f, list(noise = 3)), 3), c("extra_01", "extra_02", "extra_03"))
})

test_that("before-after shows regression to the mean; method comparison recovers bias and slope", {
  sc <- trial_toy()
  a <- simulate_study(sc, "sbp", "pre_post", 4000, "age", key = FALSE)
  b <- simulate_study(sc, "sbp", "pre_post", 4000, "age", options = list(threshold = 80), key = FALSE)
  ch <- function(r) mean(r$data_raw$sbp - r$data_raw$sbp_before)
  expect_equal(ch(a), -8, tolerance = 0.1)
  expect_lt(ch(b), ch(a) - 2)
  expect_false("drug" %in% names(a$data))
  m <- simulate_study(sc, "sbp", "method_comparison", 4000, "age", options = list(bias = 5, slope = 1.1, error_a = 0.05, error_b = 0.1), key = FALSE)
  e <- estimate_table(sc, sc$outcomes$sbp, "method_comparison", m$data_raw, "age", opt = m$options)
  expect_equal(e$crude[1], e$true[1], tolerance = 0.02)
  expect_equal(e$crude[4], 1.1, tolerance = 0.03)
  r <- simStudy(data = data.frame(), scenario = "antihypertensive_trial", outcome = "sbp", design = "method_comparison", n = 200, showKey = TRUE)
  expect_equal(nrow(r$keyExpected$asDF), 4)
  expect_identical(r$dictionary$asDF$name[2:3], c("sbp_a", "sbp_b"))
})

test_that("assays: values below the limit of detection and batches", {
  sc <- load_scenario(builtin_library(), "troponin_mi")
  r <- simulate_study(sc, "ami", "diagnostic", 1000, "age", options = list(lod_handling = "half"), key = FALSE)
  expect_equal(min(r$data_raw$troponin), 0.005)
  r2 <- simulate_study(sc, "ami", "diagnostic", 1000, "age", options = list(batches = 5, batch_confounded = TRUE), key = FALSE)
  expect_identical(names(r2$data_raw), planned_columns(sc, sc$outcomes$ami, "diagnostic", "age", list(batches = 5)))
  expect_true(any(is.na(r2$data_raw$troponin)))
  tb <- table(r2$data_raw$batch, r2$data_raw$ami)
  expect_true(sum(apply(tb, 1, min) == 0) >= 3)
})
