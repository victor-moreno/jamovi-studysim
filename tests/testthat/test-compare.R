test_that("compare_designs draws each design from the same population and stacks them", {
  sc <- toy()
  r <- compare_designs(sc, "dis", c("cross_sectional", "case_control", "matched_cc", "rct"), n = 300,
                       factors = c("age", "sex", "smoke"))
  expect_setequal(names(r$runs), c("cross_sectional", "case_control", "matched_cc"))
  expect_identical(names(r$data)[1], "design")
  expect_equal(nrow(r$data), sum(vapply(r$runs, function(x) nrow(x$data), 0L)))
  expect_true(all(c("case", "set", "dis") %in% names(r$data)))
  e <- r$estimates
  expect_true(all(e$lower <= e$adjusted & e$adjusted <= e$upper, na.rm = TRUE))
  expect_setequal(unique(e$design), names(r$runs))
  cs <- simulate_study(sc, "dis", "cross_sectional", 300, c("age", "sex", "smoke"), key = FALSE)$data
  expect_equal(r$runs$cross_sectional$data$age, cs$age)
})

test_that("matching on caliper variables only finds controls", {
  sc <- toy()
  r <- simulate_study(sc, "dis", "matched_cc", 200, c("age", "smoke"), options = list(match = "age"), key = FALSE)
  expect_equal(r$facts$controls, 100)
  expect_equal(r$facts$incomplete_sets, 0)
})

test_that("simCompare runs headlessly", {
  r <- simCompare(data = data.frame(), scenario = "chd_incidence", outcome = "chd",
                  designs = c("cohort", "case_control", "crossover"), n = 400,
                  measured = lapply(c("age", "smoking"), function(v) list(var = v, include = TRUE)))
  expect_identical(r$studies$asDF$design, c("Cohort", "Case-control"))
  expect_match(r$notice$content, "Skipped")
  e <- r$estimates$asDF
  expect_equal(nrow(e), 2 * 3)
  expect_true(all(!is.na(e$adjusted)))
})
