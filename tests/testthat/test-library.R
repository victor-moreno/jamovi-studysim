lib_dir <- function() {
  d <- system.file("library", package = "studysim")
  if (!nzchar(d)) d <- testthat::test_path("..", "..", "inst", "library")
  d
}

test_that("the built-in library is valid", {
  lib <- read_library_dir(lib_dir())
  expect_gte(length(lib$scenarios), 6)
  p <- check_library(lib)
  expect_equal(nrow(p[p$level == "error", ]), 0, info = paste(p$scenario, p$where, p$message, collapse = "\n"))
  expect_identical(lib$dictionary$diabetes$levels, c("no", "yes"))
})

test_that("every built-in scenario, outcome and design simulates", {
  lib <- read_library_dir(lib_dir())
  for (id in names(lib$scenarios)) {
    sc <- load_scenario(lib, id)
    for (o in names(sc$outcomes)) for (d in compatible_designs(sc, o)) {
      r <- simulate_study(sc, o, d, n = 200, seed = 2, key = FALSE)
      expect_gt(nrow(r$data), 0)
    }
  }
})

test_that("calibration targets are met for the built-in scenarios", {
  lib <- read_library_dir(lib_dir())
  sc <- load_scenario(lib, "chd_incidence")
  cal <- calibrate(sc, sc$outcomes$chd)
  expect_equal(cal$achieved$risk_followup, 0.08, tolerance = 1e-6)
  sc <- load_scenario(lib, "bioequivalence")
  r <- simulate_study(sc, "auc", "crossover", n = 24, seed = 1)
  expect_equal(r$key$expected$measure, "ratio")
  expect_equal(r$key$expected$adjusted, 0.96, tolerance = 0.02)
  expect_true(all(r$data$auc > 0))
})

same_scenarios <- function(a, b) {
  for (id in names(a$scenarios)) {
    x <- load_scenario(a, id); y <- load_scenario(b, id)
    for (k in c("variables", "order", "edges", "R", "outcomes", "interventions", "null_variables", "references"))
      expect_equal(y[[k]], x[[k]], info = paste(id, k), ignore_attr = TRUE)
    expect_equal(tr_text(y$teaching, "es"), tr_text(x$teaching, "es"))
    expect_equal(y$design_defaults, x$design_defaults, info = id, ignore_attr = TRUE)
  }
}

test_that("Excel, JSON and YAML-zip round trips keep every scenario", {
  lib <- read_library_dir(lib_dir())
  tmp <- tempfile(fileext = ".xlsx")
  write_library_xlsx(lib, tmp)
  same_scenarios(lib, read_library_file(tmp))
  js <- library_to_json(lib)
  same_scenarios(lib, library_from_json(js))
  z <- tempfile(fileext = ".zip")
  write_library_yaml_zip(lib, z)
  same_scenarios(lib, read_library_file(z))
  b64 <- jsonlite::base64_enc(charToRaw(enc2utf8(as.character(js))))
  same_scenarios(lib, read_library_payload(b64, "lib.json"))
  expect_error(read_library_payload("@@@", "x.json"), "base64")
})

test_that("effect modification by a binary variable is generated as specified", {
  lib <- read_library_dir(lib_dir())
  sc <- load_scenario(lib, "breast_cancer_risk")
  oc <- sc$outcomes$breast_cancer
  cal <- calibrate(sc, oc)
  pop <- population(sc, oc$id, 4, 200000)
  t <- draw_outcome(oc, cal, linear_predictor(oc, pop$cov), pop$noise)
  d <- pop$cov; d$ev <- as.integer(t <= 10); d$tt <- pmin(t, 10)
  f <- survival::coxph(survival::Surv(tt, ev) ~ age + I(bmi / 5) * menopause + parity + alcohol, data = d)
  b <- coef(f)
  expect_equal(unname(exp(b["I(bmi/5)"])), 0.93, tolerance = 0.06)
  expect_equal(unname(exp(b["I(bmi/5)"] + b["I(bmi/5):menopause"])), 1.12, tolerance = 0.06)
})
