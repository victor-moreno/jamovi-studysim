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

test_that("the three language dictionaries name every column uniquely and validly", {
  lib <- read_library_dir(lib_dir())
  for (l in c("en", "es", "ca")) {
    d <- lang_dict(l)
    for (id in names(lib$scenarios)) {
      sc <- load_scenario(lib, id)
      for (o in names(sc$outcomes)) for (des in compatible_designs(sc, o)) {
        cols <- planned_columns(sc, sc$outcomes[[o]], des, sc$order)
        cols <- setdiff(cols, unlist(lapply(sc$order, function(v) setdiff(eco_cols(sc, v), v))))
        nm <- dname(cols, l)
        expect_true(all(cols %in% names(d$names)), info = paste(l, id, setdiff(cols, names(d$names))))
        expect_false(anyDuplicated(nm) > 0, info = paste(l, id, o, des))
        expect_true(all(grepl("^[A-Za-z][A-Za-z0-9_]*$", nm)), info = paste(l, id))
      }
      for (v in sc$variables) if (!is.null(v$levels) && v$type != "genotype")
        expect_true(all(v$levels %in% names(d$levels)), info = paste(l, id, v$id))
    }
  }
  sc <- load_scenario(lib, "chd_incidence")
  r <- simulate_study(sc, "chd", "case_control", n = 100, factors = c("sex", "smoking"), lang = "es", key = FALSE)
  expect_identical(names(r$data), c("id", "caso", "sexo", "tabaco"))
  expect_identical(levels(r$data$tabaco), c("nunca", "exfumador", "fumador"))
  expect_identical(levels(r$data$caso), c("control", "caso"))
  expect_false(is.null(attr(r$data$tabaco, "jmv-desc")))
  expect_identical(ids_from_names(list("tabac", "sexo", "age"), sc$order), c("smoking", "sex", "age"))
})

test_that("a custom library carries its own translations, in every format", {
  lib <- library_subset(builtin_library(), "antihypertensive_trial")
  s <- lib$scenarios$antihypertensive_trial
  s$id <- "salt_trial"; s$variables$potassium <- list(type = "binary", p = 0.3, levels = c("low", "high"))
  s$i18n <- list(es = list(names = list(potassium = "potasio", salt = "sal_diaria"), levels = list(low = "bajo", high = "alto")),
                 ca = list(names = list(potassium = "potassi"), levels = list(low = "baix", high = "alt")))
  custom <- list(dictionary = lib$dictionary, scenarios = list(salt_trial = s),
                 i18n = list(es = list(names = list(drug = "brazo"))))
  m <- merge_libraries(builtin_library(), custom)
  sc <- load_scenario(m, "salt_trial")
  r <- simulate_study(sc, "sbp", "rct", n = 40, factors = c("potassium", "salt", "age"), lang = "es", key = FALSE)
  expect_identical(names(r$data), c("id", "brazo", "pas", "edad", "sal_diaria", "potasio"))
  expect_identical(levels(r$data$potasio), c("bajo", "alto"))
  expect_identical(levels(r$data$brazo), c("placebo", "amlodipino"))
  rc <- simulate_study(sc, "sbp", "rct", n = 40, factors = "potassium", lang = "ca", key = FALSE)
  expect_identical(names(rc$data)[4], "potassi")
  expect_identical(ids_from_names(list("potasio", "potassi", "sal_diaria"), sc$order), c("potassium", "salt"))
  for (f in c("xlsx", "json", "zip")) {
    tmp <- tempfile(fileext = paste0(".", f))
    switch(f, xlsx = write_library_xlsx(custom, tmp), json = writeLines(library_to_json(custom), tmp),
           zip = write_library_yaml_zip(custom, tmp))
    back <- read_library_file(tmp)
    expect_equal(back$scenarios$salt_trial$i18n$es$names$potassium, "potasio", info = f)
    expect_equal(back$i18n$es$names$drug, "brazo", info = f)
  }
  bad <- custom
  bad$scenarios$salt_trial$i18n$es$names$salt <- "pas"
  bad$scenarios$salt_trial$i18n$ca$names$age <- "edat actual"
  p <- check_library(merge_libraries(builtin_library(), bad))
  expect_true(any(grepl("'pas' names two columns", p$message)))
  expect_true(any(grepl("edat actual", p$message)))
  use_i18n(NULL)
  expect_equal(dname("age", "es"), "edad")
})

test_that("rules tie variables: probability from age, structural values, formulas", {
  lib <- read_library_dir(lib_dir())
  sc <- load_scenario(lib, "breast_cancer_risk")
  cov <- with_seed(3, gen_covariates(sc, 50000))
  expect_lt(mean(cov$menopause[cov$age >= 60]), 1.0001)
  expect_gt(mean(cov$menopause[cov$age >= 60]), 0.99)
  expect_lt(mean(cov$menopause[cov$age <= 42]), 0.01)
  expect_true(all(cov$hrt[cov$menopause == 0] == 0))
  expect_equal(mean(cov$hrt[cov$menopause == 1]), 0.25, tolerance = 0.03)
  sc2 <- load_scenario(lib, "chd_incidence")
  c2 <- with_seed(4, gen_covariates(sc2, 20000))
  expect_true(all(c2$cigarettes[c2$smoking != 3] == 0))
  expect_true(all(c2$cigarettes[c2$smoking == 3] >= 1))
  r <- toy_raw()
  r$variables$height <- list(type = "continuous", dist = "normal(168, 9)")
  r$variables$bmi2 <- list(type = "continuous", formula = "weight_kg / (height / 100)^2")
  r$variables$weight_kg <- list(type = "continuous", dist = "normal(72, 11)")
  r$variables$obese <- list(type = "binary", formula = "bmi2 >= 30", levels = c("no", "yes"))
  s3 <- normalize_scenario(r)
  c3 <- with_seed(5, gen_covariates(s3, 2000))
  expect_equal(c3$bmi2, c3$weight_kg / (c3$height / 100)^2)
  expect_identical(c3$obese, as.integer(c3$bmi2 >= 30))
  bad <- toy_raw()
  bad$variables$x <- list(type = "binary", p = "system('ls')")
  bad$variables$y <- list(type = "count", dist = "poisson(2)", only_if = "nope > 1")
  bad$variables$z <- list(type = "binary", p = 0.2, only_if = "sex == 'male'")
  msg <- paste(normalize_scenario(bad, strict = FALSE)$problems$message, collapse = " | ")
  expect_match(msg, "system.*not allowed")
  expect_match(msg, "nope, which is not a variable")
  expect_match(msg, "only_if needs otherwise")
  expect_equal(nrow(check_library(lib)[check_library(lib)$level == "error", ]), 0)
})
