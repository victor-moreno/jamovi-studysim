test_that("simStudy runs headlessly and fills its tables", {
  r <- simStudy(data = data.frame(), scenario = "chd_incidence", outcome = "chd", design = "matched_cc",
                n = 300, ccRatio = 2, measured = lapply(c("age", "sex", "sbp", "smoking", "coffee"),
                function(v) list(var = v, include = v != "coffee")), showKey = TRUE)
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

test_that("simLibrary lists, checks and details the built-in library", {
  r <- simLibrary(data = data.frame(), source = "builtin", scenario = "chd_incidence")
  s <- r$scenarios$asDF
  expect_gte(nrow(s), 6)
  expect_true(all(s$origin == "built-in"))
  expect_equal(nrow(r$problems$asDF), 0)
  expect_true("coffee" %in% r$variables$asDF$name)
  expect_true(any(r$structure$asDF$from == "smoking" & r$structure$asDF$to == "coffee"))
  expect_match(r$about$content, "Coronary")
})

test_that("simLibrary reads a custom Excel library and reports its problems", {
  lib <- library_subset(builtin_library(), "antihypertensive_trial")
  lib$scenarios$antihypertensive_trial$id <- "bad_trial"
  names(lib$scenarios) <- "bad_trial"
  lib$scenarios$bad_trial$outcomes$sbp$effects$salt <- list(beta = 0.6, per = 1)
  lib$scenarios$bad_trial$variables$salt$dist <- "normal(9)"
  f <- tempfile(fileext = ".xlsx"); write_library_xlsx(lib, f)
  b64 <- jsonlite::base64_enc(readBin(f, "raw", file.size(f)))
  r <- simLibrary(data = data.frame(), libContent = b64, libName = "mine.xlsx", source = "custom", scenario = "bad_trial")
  expect_identical(r$scenarios$asDF$origin, "custom")
  p <- r$problems$asDF
  expect_true(any(grepl("takes 2 values", p$message)))
})

test_that("measured variables may come by their Spanish or Catalan names", {
  r <- simStudy(data = data.frame(), scenario = "chd_incidence", outcome = "chd", design = "cohort", n = 100,
                measured = list(list(var = "edad", include = TRUE), list(var = "tabac", include = TRUE),
                                list(var = "sexo", include = FALSE)))
  expect_identical(r$dictionary$asDF$name, c("id", "time", "chd", "age", "smoking"))
  r0 <- simStudy(data = data.frame(), scenario = "chd_incidence", outcome = "chd", design = "cohort", n = 100)
  expect_match(r0$summary$content, "No variables measured yet")
})

test_that("the panel's form and cut points reach the data, descriptives and key", {
  r <- simStudy(data = data.frame(), scenario = "chd_incidence", outcome = "chd", design = "cohort", n = 600, showKey = TRUE,
                measured = list(list(var = "sbp", include = TRUE, form = "quartiles", cuts = ""),
                                list(var = "bmi", include = TRUE, form = "cuts", cuts = "25, 30"),
                                list(var = "age", include = TRUE, form = "continuous", cuts = ""),
                                list(var = "sex", include = TRUE, form = "tertiles", cuts = "")))
  d <- r$descriptives$asDF
  expect_true(any(grepl("^Q4: >", d$level)))
  expect_true(any(d$level == "G3: >30.0"))
  k <- r$keyExpected$asDF
  expect_identical(k$contrast[k$variable == "sbp"], c("Q2 vs Q1", "Q3 vs Q1", "Q4 vs Q1"))
  expect_true(all(!is.na(k$adjusted)))
  bad <- simStudy(data = data.frame(), scenario = "chd_incidence", outcome = "chd", design = "cohort", n = 100,
                  measured = list(list(var = "bmi", include = TRUE, form = "cuts", cuts = "25, x")))
  expect_match(bad$notice$content, "Cut points of bmi")
})
