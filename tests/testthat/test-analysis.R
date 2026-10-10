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
  r0 <- simStudy(data = data.frame(), scenario = "chd_incidence", outcome = "chd", design = "cohort", n = 100,
                 measured = list(list(var = "age", include = FALSE)))
  expect_match(r0$summary$content, "No variables measured yet")
  rall <- simStudy(data = data.frame(), scenario = "chd_incidence", outcome = "chd", design = "cohort", n = 100)
  expect_true(all(c("age", "sex", "coffee") %in% rall$dictionary$asDF$name))
})

test_that("the panel's form and cut points reach the data, descriptives and key", {
  r <- simStudy(data = data.frame(), scenario = "chd_incidence", outcome = "chd", design = "cohort", n = 600, showKey = TRUE,
                measured = list(list(var = "sbp", include = TRUE, form = "quartiles", cuts = ""),
                                list(var = "bmi", include = TRUE, form = "cuts", cuts = "25, 30"),
                                list(var = "age", include = TRUE, form = "continuous", cuts = ""),
                                list(var = "sex", include = TRUE, form = "tertiles", cuts = "")))
  d <- r$descriptives$asDF
  expect_true(any(grepl("^Q4: >", d$level)))
  expect_true(any(d$level == "G3: >30"))
  k <- r$keyExpected$asDF
  expect_identical(k$contrast[k$variable == "sbp"], c("Q2 vs Q1", "Q3 vs Q1", "Q4 vs Q1"))
  expect_true(all(!is.na(k$adjusted)))
  bad <- simStudy(data = data.frame(), scenario = "chd_incidence", outcome = "chd", design = "cohort", n = 100,
                  measured = list(list(var = "bmi", include = TRUE, form = "cuts", cuts = "25, x")))
  expect_match(bad$notice$content, "Cut points of bmi")
})

test_that("log2 and log10 forms transform a measured variable and the key reports per doubling", {
  r <- simStudy(data = data.frame(), scenario = "chd_incidence", outcome = "chd", design = "cohort", n = 400, showKey = TRUE,
                measured = list(list(var = "age", include = TRUE, form = "continuous"),
                                list(var = "total_chol", include = TRUE, form = "log2", cuts = "")))
  d <- r$dictionary$asDF
  expect_match(d$description[d$name == "total_chol"], "log2")
  k <- r$keyExpected$asDF
  expect_identical(k$contrast[k$variable == "total_chol"], "per doubling")
  expect_true(is.finite(k$adjusted[k$variable == "total_chol"]))
})

test_that("the Library lists only the scenarios that fit a design filter", {
  r <- simLibrary(data = data.frame(), source = "builtin", scenario = "isis2", designFilter = "factorial")
  expect_setequal(r$scenarios$asDF$id, c("antihypertensive_trial", "physicians_health", "isis2"))
})

test_that("the scale form divides a variable and re-expresses the true effect", {
  r <- simCompare(data = data.frame(), scenario = "breast_cancer_risk", outcome = "breast_cancer", designs = c("case_control", "cohort"),
                  n = 600, measured = list(list(var = "age", include = TRUE), list(var = "alcohol", include = TRUE, form = "scale", cuts = "1")))
  e <- r$estimates$asDF
  a <- e[e$variable == "alcohol", ]
  expect_true(all(a$contrast == "per 1 g/day"))
  expect_equal(unique(a$truth), 1.07^(1 / 10), tolerance = 1e-6)
  bad <- simStudy(data = data.frame(), scenario = "chd_incidence", outcome = "chd", design = "cohort", n = 100,
                  measured = list(list(var = "bmi", include = TRUE, form = "scale", cuts = "")))
  expect_match(bad$notice$content, "divisor")
})

test_that("the teacher's answer-key report is a self-contained HTML page", {
  a <- simStudyClass$new(options = simStudyOptions$new(scenario = "troponin_mi", outcome = "ami", design = "diagnostic", n = 200, showKey = TRUE),
                         data = data.frame(), datasetId = "d", analysisId = "1", revision = 1)
  a$init(); a$run()
  p <- a$.__enclos_env__$private
  ctx <- p$.context()
  html <- p$.keyReport(ctx, utils::modifyList(design_defaults("diagnostic", ctx$oc, ctx$sc), list()))
  expect_match(html, "data:image/png;base64,")
  expect_match(html, "AUC")
  expect_match(html, "Reichlin")
})
