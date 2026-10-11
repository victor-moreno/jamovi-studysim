centred_raw <- function(center = NULL) {
  r <- list(id = "centred",
    variables = list(x = list(type = "continuous", dist = "normal(50, 10)"),
                     m = list(type = "binary", p = 0.5, levels = c("a", "b"))),
    outcomes = list(y = list(type = "binary", prevalence = 0.3,
                             effects = list(x = list(OR = 1.05, by = list(m = list(a = 1, b = 1.1)))))))
  if (!is.null(center)) r$outcomes$y$effects$x$center <- center
  r
}

test_that("a modified numeric effect is centred at the population mean", {
  sc <- normalize_scenario(centred_raw())
  e <- sc$outcomes$y$effects$x
  expect_equal(e$center, 50, tolerance = 0.01)
  cal <- calibrate(sc, sc$outcomes$y)
  big <- run_design("cross_sectional", sc, sc$outcomes$y, cal, 3, 60000, list())
  fd <- format_study(sc, sc$outcomes$y, "cross_sectional", big, sc$order)
  fd$xc <- fd$x - e$center
  f <- stats::glm(y ~ xc * m, stats::binomial(), fd)
  expect_lt(abs(stats::coef(f)[["mb"]]), 0.06)
  expect_equal(exp(stats::coef(f)[["xc:mb"]]), 1.1, tolerance = 0.02)
  te <- true_effects(sc, sc$outcomes$y)
  expect_match(te$note[1], "m=a: 1; m=b: 1.1")
})

test_that("a center outside the variable's values is reported", {
  sc <- normalize_scenario(centred_raw(0), strict = FALSE)
  expect_true(any(grepl("center 0 is outside", sc$problems$message)))
  sc2 <- normalize_scenario(centred_raw(45), strict = FALSE)
  expect_equal(nrow(sc2$problems), 0)
  expect_equal(sc2$outcomes$y$effects$x$center, 45)
})

test_that("the modifier's expected effect says where it holds", {
  sc <- load_scenario(builtin_library(), "breast_cancer_risk")
  oc <- sc$outcomes$breast_cancer
  ex <- expected_estimates(sc, oc, "case_control", design_defaults("case_control", oc, sc), c("age", "menopause", "hrt", "bmi"))
  expect_match(ex$note[ex$variable == "menopause"], "at bmi = 26.6")
  expect_lt(abs(log(ex$adjusted[ex$variable == "menopause"])), 0.2)
})

test_that("a continuous outcome with a range is truncated, not piled at the bounds", {
  raw <- list(id = "trunc", variables = list(x = list(type = "continuous", dist = "normal(0, 1)")),
              outcomes = list(y = list(type = "continuous", mean = 95, sd = 10, range = c(0, 100),
                                       effects = list(x = list(beta = 2)))))
  sc <- normalize_scenario(raw)
  y <- simulate_study(sc, "y", "cross_sectional", 3000, key = FALSE)$data_raw$y
  expect_lt(mean(y >= 100), 0.002)
  expect_true(all(y >= 0 & y <= 100))
  expect_gt(stats::cor(y, simulate_study(sc, "y", "cross_sectional", 3000, key = FALSE)$data_raw$x), 0.1)
})

test_that("the panel catalogue carries the designs computed in R", {
  lib <- builtin_library()
  j <- jsonlite::fromJSON(library_to_json(library_subset(lib, "colon_adjuvant"), compatible = TRUE), simplifyVector = FALSE)
  sc <- load_scenario(lib, "colon_adjuvant")
  expect_identical(unlist(j$scenarios$colon_adjuvant$outcomes$death[["_compatible"]]), compatible_designs(sc, "death"))
  back <- library_from_json(library_to_json(library_subset(lib, "colon_adjuvant"), compatible = TRUE))
  expect_equal(nrow(check_library(back)), 0)
})
