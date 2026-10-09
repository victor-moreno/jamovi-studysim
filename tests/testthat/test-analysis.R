test_that("simStudy runs headlessly and fills its tables", {
  r <- simStudy(data = data.frame(), scenario = "chd_incidence", outcome = "chd", design = "matched_cc",
                n = 300, ccRatio = 2, factors = list("age", "sex", "sbp", "smoking"), showKey = TRUE)
  d <- r$dictionary$asDF
  expect_identical(d$name, c("id", "set", "case", "age", "sex", "smoking", "sbp"))
  expect_true(all(!is.na(d$missing)))
  k <- r$keyExpected$asDF
  expect_equal(nrow(k), 5)
  expect_true(all(k$measure == "OR"))
  expect_match(r$summary$content, "100 cases and 200 controls")
})

test_that("an incompatible design or unknown scenario gives a notice, not an error", {
  r <- simStudy(data = data.frame(), scenario = "chd_incidence", outcome = "chd", design = "crossover")
  expect_true(r$notice$visible)
  expect_match(r$notice$content, "does not fit")
  r2 <- simStudy(data = data.frame(), scenario = "nope")
  expect_match(r2$notice$content, "no scenario 'nope'")
})

test_that("a custom JSON library from the panel is merged with the built-in one", {
  lib <- builtin_library()
  s <- lib$scenarios$bioequivalence
  s$id <- "bioeq_custom"; s$label <- "Custom BE"
  js <- jsonlite::toJSON(list(scenarios = list(bioeq_custom = s)), auto_unbox = TRUE, digits = NA)
  b64 <- jsonlite::base64_enc(charToRaw(enc2utf8(as.character(js))))
  r <- simStudy(data = data.frame(), libContent = b64, libName = "mine.json", scenario = "bioeq_custom",
                outcome = "auc", design = "crossover", n = 24)
  expect_false(r$notice$visible)
  expect_identical(r$dictionary$asDF$name[1:5], c("id", "sequence", "period", "formulation", "auc"))
})

test_that("the analysis serialises as jamovi does", {
  skip_if_not_installed("RProtoBuf")
  jmvcore:::initProtoBuf()
  a <- simStudyClass$new(options = simStudyOptions$new(scenario = "breast_cancer_risk", outcome = "breast_cancer",
                                                        design = "case_control", n = 200, showKey = TRUE),
                         data = data.frame(), datasetId = "d", analysisId = "1", revision = 1)
  a$init(); a$run()
  pb <- RProtoBuf::serialize(a$asProtoBuf(final = FALSE, includeState = TRUE), NULL)
  expect_gt(length(pb), 1000)
})
