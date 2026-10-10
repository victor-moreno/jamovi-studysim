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
