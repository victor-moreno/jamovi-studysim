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
