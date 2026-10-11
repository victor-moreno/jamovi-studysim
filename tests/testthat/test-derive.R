test_that("deriving from simulated data recovers the scenario (round trip)", {
  lib <- builtin_library()
  sc <- load_scenario(lib, "chd_incidence")
  r <- simulate_study(sc, "chd", "cohort", n = 15000, seed = 9, factors = c("age", "sex", "smoking", "sbp", "hdl", "coffee"),
                      options = list(loss = 0), key = FALSE)
  d <- r$data
  dv <- derive_scenario(d, "chd", "event", covs = c("age", "sex", "smoking", "sbp", "hdl", "coffee"), time = "time",
                        id = "chd_copy", source = "a simulated cohort")
  expect_identical(dv$type, "event")
  v <- dv$scenario$variables
  expect_identical(v$sex$type, "binary"); expect_identical(v$smoking$type, "nominal")
  expect_identical(v$coffee$type, "count"); expect_match(v$hdl$dist, "^lognormal|^gamma")
  e <- dv$estimates
  hr <- setNames(e$estimate, paste(e$variable, e$contrast))
  expect_equal(hr[["age per 1"]], 1.05, tolerance = 0.03)
  expect_equal(hr[["sex male vs female"]], 1.6, tolerance = 0.25)
  cr <- dv$correlations
  expect_equal(cr$latent[cr$a == "smoking" & cr$b == "coffee"], 0.5, tolerance = 0.12)
  sc2 <- normalize_scenario(dv$scenario)
  expect_s3_class(sc2, "studysim_scenario")
  ex <- derive_check(dv)
  sim <- setNames(ex$adjusted, paste(ex$variable, ex$contrast))
  expect_equal(unname(sim["age per 1"]), hr[["age per 1"]], tolerance = 0.03)
  y <- yaml::as.yaml(dv$scenario)
  back <- add_yaml_doc(empty_library(), yaml_read(y), "chd_copy.yaml")
  expect_true("chd_copy" %in% names(back$scenarios))
  expect_equal(nrow(check_library(back)[check_library(back)$level == "error", ]), 0)
})

test_that("binary and continuous outcomes derive too", {
  set.seed(1)
  n <- 3000
  d <- data.frame(Age = round(runif(n, 30, 70)), Group = factor(sample(c("a", "b", "c"), n, TRUE)))
  d$`Blood pressure` <- 100 + 0.5 * d$Age + c(0, 3, 6)[as.integer(d$Group)] + rnorm(n, 0, 8)
  d$ill <- factor(ifelse(runif(n) < plogis(-5 + 0.06 * d$Age), "yes", "no"), levels = c("no", "yes"))
  d$Age[1:30] <- NA
  b <- derive_scenario(d, "ill", covs = c("Age", "Group"))
  expect_identical(b$type, "binary")
  expect_equal(b$estimates$estimate[1], exp(0.06), tolerance = 0.02)
  expect_equal(b$scenario$derivation$complete_rows, n - 30)
  cdv <- derive_scenario(d, "Blood pressure", covs = c("Age", "Group"))
  expect_identical(cdv$type, "continuous"); expect_identical(cdv$outcome_id, "blood_pressure")
  expect_equal(cdv$estimates$estimate[cdv$estimates$contrast == "c vs a"], 6, tolerance = 0.15)
  expect_equal(cdv$scenario$outcomes$blood_pressure$sd, 8, tolerance = 0.05)
  expect_s3_class(normalize_scenario(cdv$scenario), "studysim_scenario")
})

test_that("simDerive runs headlessly on a dataset", {
  sc <- load_scenario(builtin_library(), "breast_cancer_risk")
  r <- simulate_study(sc, "breast_cancer", "case_control", n = 1500, seed = 2,
                      factors = c("age", "parity", "family_hist", "alcohol", "education"), key = FALSE)
  a <- simDerive(data = r$data, outcome = "case", covs = c("age", "parity", "family_hist", "alcohol", "education"))
  e <- a$effects$asDF
  expect_true(all(c("Adjusted", "Re-estimated") %in% c("Adjusted", "Re-estimated")))
  expect_equal(nrow(e), 6)
  expect_true(all(!is.na(e$simulated)))
  expect_equal(e$estimate[e$variable == "family_hist"], e$simulated[e$variable == "family_hist"], tolerance = 0.25)
  expect_match(a$yaml$content, "derivation:")
  expect_match(a$summary$content, "binary")
  expect_identical(a$variables$asDF$type[a$variables$asDF$name == "education"], "ordinal")
})

test_that("Derive handles missing covariates by complete cases, mean, single or multiple imputation", {
  sc <- load_scenario(builtin_library(), "chd_incidence")
  f <- c("age", "sex", "smoking", "sbp")
  full <- simulate_study(sc, "chd", "cohort", 4000, f, seed = 3, key = FALSE)$data
  r <- simulate_study(sc, "chd", "cohort", 4000, f, seed = 3, key = FALSE, missing = 0.25, missing_mech = "mar")$data
  ref <- suppressWarnings(derive_scenario(full, "chd", "event", f, time = "time"))$estimates$estimate
  mi <- suppressWarnings(derive_scenario(r, "chd", "event", f, time = "time", missing = "multiple", m = 5))
  mn <- suppressWarnings(derive_scenario(r, "chd", "event", f, time = "time", missing = "mean"))
  expect_lt(max(abs(log(mi$estimates$estimate / ref))), 0.15)
  expect_gt(max(abs(log(mn$estimates$estimate / ref))), 0.3)
  expect_true(all(is.finite(mi$estimates$complete_case)))
  expect_identical(mi$scenario$derivation$missing_method, "multiple")
  expect_true(any(grepl("complete for the model", mi$warnings)))
  a <- simDerive(data = r, outcome = "chd", time = "time", covs = c("age", "sex", "smoking", "sbp"), missingMethod = "single", validate = FALSE)
  expect_true(all(!is.na(a$effects$asDF$complete_case)))
  expect_match(a$notice$content, "Check before")
})

test_that("Derive warns about a variable recorded only after the outcome", {
  set.seed(1)
  d <- data.frame(y = rbinom(500, 1, 0.3), x = rnorm(500))
  d$after <- ifelse(d$y == 1, rnorm(500), NA)
  w <- suppressWarnings(derive_scenario(d, "y", "binary", c("x", "after")))$warnings
  expect_true(any(grepl("recorded after the outcome", w)))
})

test_that("Derive finds codes, structural values and orders", {
  set.seed(2)
  n <- 1500
  d <- data.frame(age = round(runif(n, 25, 60)), user = sample(c("no", "yes"), n, TRUE, prob = c(0.6, 0.4)))
  d$start <- ifelse(d$user == "yes", round(runif(n, 16, 24)), NA)
  d$years <- ifelse(d$user == "yes", rpois(n, 6) + 1, 0)
  d$screen <- sample(c("no", "yes"), n, TRUE)
  d$age_screen <- ifelse(d$screen == "yes", pmin(d$age, round(runif(n, 20, 50))), 99)
  d$y <- rbinom(n, 1, plogis(-1 + 0.1 * d$years))
  r <- suppressWarnings(derive_scenario(d, "y", "binary", c("age", "user", "start", "years", "screen", "age_screen")))
  v <- r$scenario$variables
  expect_identical(v$start$only_if, "user == \"yes\"")
  expect_identical(v$years$only_if, "user == \"yes\"")
  expect_identical(v$age_screen$only_if, "screen == \"yes\"")
  expect_equal(v$years$otherwise, 0)
  expect_identical(r$rows_model, r$complete)
  expect_true(any(grepl("values 99 read as missing", r$warnings)))
  expect_true(any(grepl("'age_screen' <= 'age'", r$warnings)))
  pairs <- apply(r$correlations[c("a", "b")], 1, function(p) paste(sort(p), collapse = "-"))
  expect_false(any(c("start-user", "user-years") %in% pairs))
  sc <- normalize_scenario(r$scenario, strict = TRUE)
  s <- simulate_study(sc, "y", "cross_sectional", 2000, sc$order, key = FALSE)$data_raw
  expect_true(all(s$years[s$user == "no"] == 0))
  b <- exp(coef(glm(y ~ years, binomial, s)))[["years"]]
  expect_equal(b, exp(0.1), tolerance = 0.05)
})
