test_that("a log-scale outcome with zeros still gets an answer key", {
  sc <- load_scenario(builtin_library(), "early_af_cryoablation")
  r <- simulate_study(sc, "af_burden", "rct", 300, sc$order, seed = 1)
  ex <- r$key$expected
  expect_true(all(is.finite(ex$adjusted)))
  expect_match(ex$note[1], "log\\(y \\+ 0.005\\)")
})

test_that("missingness depending on the outcome works in a method comparison", {
  sc <- load_scenario(builtin_library(), "metformin_hba1c")
  r <- simulate_study(sc, "hba1c", "method_comparison", 200, sc$order, missing = 0.2, missing_mech = "mar", key = FALSE)
  expect_gt(sum(is.na(r$data_raw[[sc$order[1]]])), 0)
})

test_that("case-control rows do not reveal the case status", {
  sc <- load_scenario(builtin_library(), "breast_cancer_risk")
  d <- simulate_study(sc, "breast_cancer", "case_control", 400, sc$order, seed = 1, key = FALSE)$data_raw
  first <- sum(d$case[1:200] == "case")
  expect_true(first > 60 && first < 140)
})

test_that("the student key ignores capitals and surrounding spaces", {
  sc <- load_scenario(builtin_library(), "chd_incidence")
  a <- simulate_study(sc, "chd", "cohort", 100, sc$order, student_key = " Anna ", key = FALSE)$data
  b <- simulate_study(sc, "chd", "cohort", 100, sc$order, student_key = "anna", key = FALSE)$data
  expect_identical(a, b)
})

test_that("coefficients that cannot be estimated say why", {
  sc <- load_scenario(builtin_library(), "chd_incidence")
  ex <- simulate_study(sc, "chd", "matched_cc", 300, sc$order, seed = 1)$key$expected
  expect_match(ex$note[is.na(ex$adjusted) & ex$variable == "sex"], "matched")
  sc <- load_scenario(builtin_library(), "msi_crc_pathology")
  ex <- simulate_study(sc, "msi", "cross_sectional", 300, sc$order, seed = 1)$key$expected
  expect_match(ex$note[ex$variable == "til"], "not estimable")
})

test_that("expected values carry their Monte Carlo uncertainty", {
  sc <- load_scenario(builtin_library(), "indomethacin_ercp")
  ex <- simulate_study(sc, "pancreatitis", "rct", 300, sc$order, seed = 1)$key$expected
  mc <- mc_halfwidth(ex)
  expect_true(mc$pct > 0 && mc$pct < 20)
  expect_true(is.na(mc$diff))
})

test_that("follow-up time is labelled in the outcome's own units and language", {
  sc <- load_scenario(builtin_library(), "tebentafusp_uveal_melanoma")
  r <- simulate_study(sc, "os", "rct", 100, sc$order, seed = 1, lang = "es", key = FALSE)
  expect_match(attr(r$data_raw$time, "jmv-desc"), "\\(meses\\)")
  expect_identical(unit_text("years", "ca"), "anys")
  expect_identical(unit_text("fortnights", "es"), "fortnights")
})
