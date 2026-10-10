test_that("every compatible design runs and formats", {
  sc <- toy()
  for (o in names(sc$outcomes)) for (d in compatible_designs(sc, o)) {
    r <- simulate_study(sc, o, d, n = 300, seed = 5, key = FALSE)
    expect_true(is.data.frame(r$data), info = paste(o, d))
    expect_true(nrow(r$data) %in% c(300, 600) || length(unique(r$data$id)) == 300 || d == "ecological", info = paste(o, d))
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

test_that("planned descriptives match the data, and the DAG layout is complete", {
  sc <- toy()
  for (o in names(sc$outcomes)) for (d in compatible_designs(sc, o)) {
    f <- c("age", "smoke", "snp", "kids")
    r <- simulate_study(sc, o, d, n = 120, factors = f, missing = 0.1, key = FALSE)
    k <- planned_kinds(sc, sc$outcomes[[o]], d, f)
    rows <- describe_data(r$data, k)
    expect_true(all(names(k) %in% names(r$data)), info = paste(o, d))
    for (nm in names(k)) if (!identical(k[[nm]], "cont"))
      expect_true(all(stats::na.omit(as.character(r$data[[nm]])) %in% k[[nm]]), info = paste(o, d, nm))
    expect_false(any(grepl("NA", vapply(rows, `[[`, "", "stat"))), info = paste(o, d))
  }
  g <- dag_layout(sc, sc$outcomes$ev, c("age", "smoke"))
  expect_setequal(g$nodes$id, c("drug", sc$order, "ev"))
  expect_equal(g$nodes$kind[g$nodes$id == "bmi"], "unmeasured")
  expect_true(all(g$edges$x[g$edges$type == "path"] < g$edges$xend[g$edges$type == "path"]))
  expect_equal(sum(g$edges$type == "effect"), 3)
})

test_that("continuous variables can be categorised by quantiles or cut points", {
  sc <- toy()
  expect_equal(parse_cuts("18,5; 25; 30")$x, c(18.5, 25, 30))
  expect_equal(parse_cuts("10%, 90%")$type, "pct")
  expect_equal(parse_cuts("18.5, 25")$decimals, 1)
  expect_identical(levels(categorize_column(c(0, 0, 1, 3, 5), list(form = "cuts", cuts = "0"), NULL, TRUE)), c("G1: ≤0", "G2: >0"))
  expect_identical(levels(categorize_column(c(20.1, 26.3, 31.2), list(form = "cuts", cuts = "25, 30"), 1)), c("G1: ≤25", "G2: 25–30", "G3: >30"))
  expect_identical(levels(categorize_column(c(0, 1, 2, 2, 3, 4, 6), list(form = "median"), NULL, TRUE)), c("Q1: ≤2", "Q2: >2"))
  expect_error(parse_cuts("10%, 25"), "not both")
  expect_error(parse_cuts("a, b"), "not a list of numbers")
  cats <- list(bmi = list(form = "tertiles"), age = list(form = "cuts", cuts = "50, 60"),
               kids = list(form = "median"), smoke = list(form = "quartiles"))
  r <- simulate_study(sc, "dis", "cross_sectional", n = 3000, factors = c("bmi", "age", "kids", "smoke"),
                      categorize = cats)
  d <- r$data
  expect_true(is.ordered(d$bmi) && nlevels(d$bmi) == 3)
  expect_equal(as.numeric(prop.table(table(d$bmi))), rep(1 / 3, 3), tolerance = 0.02)
  expect_identical(levels(d$age), c("G1: ≤50", "G2: 50–60", "G3: >60"))
  expect_true(all(d$age[r$data_raw$age == r$data_raw$age][1:10] %in% levels(d$age)))
  expect_false(is.ordered(d$smoke))
  expect_match(attr(d$bmi, "jmv-desc"), "tertiles")
  k <- planned_kinds(sc, sc$outcomes$dis, "cross_sectional", c("bmi", "age"), categorize = cats)
  rows <- describe_data(r$data_raw, k)
  expect_true(any(grepl("^Q2: ", vapply(rows, `[[`, "", "level"))))
  e <- r$key$expected
  b <- e[e$variable == "bmi", ]
  expect_identical(b$contrast, c("Q2 vs Q1", "Q3 vs Q1"))
  expect_true(all(b$adjusted > 1.2) && all(is.na(b$true)))
  expect_true(b$adjusted[2] > b$adjusted[1])
  expect_identical(e$contrast[e$variable == "age"], c("G2 vs G1", "G3 vs G1"))
})
