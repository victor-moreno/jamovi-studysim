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

test_that("empty design options keep the scenario's values, typed ones override them", {
  r <- simStudy(data = data.frame(), scenario = "esoph_cancer", design = "case_control", n = 300)
  expect_match(r$summary$content, "60 cases and 240 controls")
  r2 <- simStudy(data = data.frame(), scenario = "esoph_cancer", design = "case_control", n = 300, ccRatio = "1")
  expect_match(r2$summary$content, "150 cases and 150 controls")
  r3 <- simStudy(data = data.frame(), scenario = "lead_succimer", outcome = "blood_lead", design = "longitudinal", n = 40, dropout = "x")
  expect_match(r3$notice$content, "Dropout by the last visit \\(%\\): write a number")
  r4 <- simStudy(data = data.frame(), scenario = "breast_cancer_risk", design = "cohort", n = 200, loss = "1,5")
  expect_match(r4$summary$content, "Loss to follow-up: 1.5% per year")
})

test_that("engine errors reach the user translated, R's own errors with a preface", {
  fake <- list(options = list(translate = function(x, n = 1) paste0("[", x, "]")))
  expect_match(friendly_error(simpleError("the outcome is too rare: 3 cases in 2,000,000 people. Ask"), fake),
               "^\\[The outcome is too rare: 3 cases in 2,000,000 people")
  expect_match(friendly_error(simpleError("boom", call = quote(uniroot(f))), fake), "^\\[The calculation failed.*\\] boom$")
  expect_identical(friendly_error(simpleError("Ya traducido"), fake), "Ya traducido")
  m <- list(list(var = "age", include = TRUE, form = "cuts", cuts = "a, b"))
  r <- simStudy(data = data.frame(), scenario = "breast_cancer_risk", design = "cohort", n = 100, measured = m)
  expect_match(r$notice$content, "Cut points of age: 'a, b' is not a list of numbers.")
})

test_that("the key separates direct and total effects and adjusts as the diagram says", {
  sc <- load_scenario(builtin_library(), "chd_incidence")
  oc <- sc$outcomes$chd
  expect_true("hdl" %in% mediators(sc, oc, "sex"))
  expect_false("hdl" %in% adjustment_set(sc, "sex", sc$order))
  te <- true_effects(sc, oc, total = TRUE)
  edu <- te[te$variable == "education", ]
  expect_match(edu$note[1], "no direct effect; acts through")
  expect_true(all(edu$value == 1) && all(edu$total < 1))
  expect_equal(te$total[te$variable == "sbp"], te$value[te$variable == "sbp"])
  expect_gt(te$total[te$variable == "sex"], te$value[te$variable == "sex"])
  ex <- expected_estimates(sc, oc, "cohort", design_defaults("cohort", oc, sc), sc$order)
  s <- ex[ex$variable == "sex", ]
  expect_lt(abs(log(s$dag / s$total)), abs(log(s$adjusted / s$total)))
  expect_equal(ex$dag[ex$variable == "sbp"], ex$adjusted[ex$variable == "sbp"])
})

test_that("a variable acting only through others gets a row of its own", {
  sc <- load_scenario(builtin_library(), "breast_cancer_risk")
  te <- true_effects(sc, sc$outcomes$breast_cancer, total = TRUE)
  m <- te[te$variable == "menopause", ]
  expect_equal(nrow(m), 1)
  expect_match(m$note, "acts through hrt")
  expect_gt(m$total, 1.05)
})

test_that("trials report absolute effects and the marginal ratio is closer to 1", {
  sc <- load_scenario(builtin_library(), "streptomycin_tb")
  oc <- sc$outcomes[[1]]
  a <- trial_absolute(sc, oc, "rct", design_defaults("rct", oc, sc), n = 40000)
  v <- stats::setNames(a$value, sub(" [0-9]+$", "", sub("^[^ ]+ ", "", a$key)))
  expect_equal(v[["rd"]], diff(a$value[1:2]))
  expect_equal(v[["nnt"]], 1 / abs(v[["rd"]]))
  expect_lt(v[["or_m"]], v[["or_c"]])
  expect_gt(v[["or_m"]], 1)
  expect_identical(a$key, trial_absolute_rows(sc, oc, "rct"))
  expect_null(trial_absolute(sc, oc, "cohort", list()))
})
