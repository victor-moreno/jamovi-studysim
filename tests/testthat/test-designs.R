test_that("every compatible design runs and formats", {
  sc <- toy()
  for (o in names(sc$outcomes)) for (d in compatible_designs(sc, o)) {
    r <- simulate_study(sc, o, d, n = 300, seed = 5, key = FALSE)
    expect_true(is.data.frame(r$data), info = paste(o, d))
    expect_true(nrow(r$data) %in% c(300, 600), info = paste(o, d))
    expect_false(is.null(attr(r$data$age, "jmv-desc")), info = paste(o, d))
  }
  expect_error(simulate_study(sc, "sbp", "case_control", 100), "does not fit")
})

test_that("case-control samples cases and controls; matched sets match", {
  sc <- toy()
  r <- simulate_study(sc, "dis", "case_control", n = 600, options = list(ratio = 2), key = FALSE)
  expect_equal(as.numeric(table(r$data$case)), c(400, 200))
  m <- simulate_study(sc, "ev", "matched_cc", n = 300, options = list(ratio = 2, match = c("age", "sex")), key = FALSE)
  d <- m$data
  expect_true(all(tapply(as.character(d$sex), d$set, function(x) length(unique(x))) == 1))
  expect_true(all(tapply(d$age, d$set, function(x) diff(range(x))) <= 4))
  expect_equal(sum(d$case == "case"), 100)
})

test_that("cohort and rct have follow-up columns; crossover is long", {
  sc <- toy()
  r <- simulate_study(sc, "ev", "rct", n = 400, key = FALSE)
  expect_true(all(c("drug", "time", "ev") %in% names(r$data)))
  expect_equal(as.numeric(table(r$data$drug)), c(200, 200))
  expect_true(all(r$data$time <= 10 + 1e-9))
  x <- simulate_study(sc, "sbp", "crossover", n = 50, key = FALSE)
  expect_equal(nrow(x$data), 100)
  expect_equal(as.numeric(table(table(x$data$id))), 50)
  expect_true(all(c("sequence", "period", "drug", "sbp") %in% names(x$data)))
})

test_that("the same seed gives the same study; a student key changes it", {
  sc <- toy()
  a <- simulate_study(sc, "dis", "cross_sectional", n = 200, seed = 3, key = FALSE)
  b <- simulate_study(sc, "dis", "cross_sectional", n = 200, seed = 3, key = FALSE)
  k <- simulate_study(sc, "dis", "cross_sectional", n = 200, seed = 3, student_key = "S123", key = FALSE)
  expect_identical(a$data, b$data)
  expect_false(identical(a$data$age, k$data$age))
  co <- simulate_study(sc, "dis", "cohort", n = 200, seed = 3, key = FALSE)
  expect_identical(a$data$age, co$data$age)
})

test_that("missing data hit the requested rate", {
  sc <- toy()
  r <- simulate_study(sc, "dis", "cross_sectional", n = 4000, missing = 0.1, key = FALSE)
  expect_equal(mean(is.na(r$data$bmi)), 0.1, tolerance = 0.02)
  expect_equal(sum(is.na(r$data$dis)), 0)
  m <- simulate_study(sc, "dis", "cross_sectional", n = 4000, missing = 0.1, missing_mech = "mar", key = FALSE)
  pm <- tapply(is.na(m$data$bmi), m$data$dis, mean)
  expect_gt(pm[["no"]], pm[["yes"]])
})

test_that("answer key recovers the truth and shows confounding", {
  sc <- toy()
  r <- simulate_study(sc, "dis", "cross_sectional", n = 200, factors = c("age", "bmi", "smoke", "sex", "snp", "coffee"))
  e <- r$key$expected
  adj <- setNames(e$adjusted, paste(e$variable, e$contrast))
  tru <- setNames(e$true, paste(e$variable, e$contrast))
  expect_equal(adj[["bmi per 1"]], tru[["bmi per 1"]], tolerance = 0.03)
  expect_equal(adj[["smoke current vs never"]], 2, tolerance = 0.15)
  expect_equal(tru[["snp TT vs CC"]], 1.3^2)
  expect_equal(tru[["coffee yes vs no"]], 1)
  expect_true(abs(log(e$crude[e$variable == "bmi"])) > abs(log(1.08)) + 0.005)
  expect_identical(r$key$unmeasured, character(0))
  r2 <- simulate_study(sc, "ev", "rct", n = 100, factors = c("age", "bmi"))
  e2 <- r2$key$expected
  expect_equal(e2$measure[1], "HR")
  expect_equal(e2$adjusted[e2$variable == "drug"], 0.7, tolerance = 0.12)
  expect_true("smoke" %in% r2$key$unmeasured)
  r3 <- simulate_study(sc, "sbp", "crossover", n = 30, options = list(period = 2))
  expect_equal(r3$key$expected$adjusted, -8, tolerance = 0.1)
  r4 <- simulate_study(sc, "ev", "matched_cc", n = 300, factors = c("age", "smoke"))
  expect_equal(r4$key$expected$measure[1], "OR")
})

test_that("planned columns match the formatted data", {
  sc <- toy()
  for (o in names(sc$outcomes)) for (d in compatible_designs(sc, o)) {
    f <- c("age", "smoke", "snp")
    r <- simulate_study(sc, o, d, n = 100, factors = f, key = FALSE)
    expect_identical(planned_columns(sc, sc$outcomes[[o]], d, f), names(r$data), info = paste(o, d))
  }
})
