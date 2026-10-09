toy_raw <- function() list(
  id = "toy", label = list(en = "Toy", es = "Juguete"),
  variables = list(
    age = list(type = "continuous", dist = "uniform(35, 75)", round = 0, units = "years"),
    sex = list(type = "binary", p = 0.5, levels = c("female", "male")),
    bmi = list(type = "continuous", dist = "lognormal(26, 4)", range = c(16, 50), round = 1),
    smoke = list(type = "nominal", levels = c("never", "former", "current"), probs = c(0.5, 0.3, 0.2)),
    diet = list(type = "ordinal", levels = c("low", "mid", "high"), probs = c(0.3, 0.4, 0.3)),
    kids = list(type = "count", dist = "poisson(1.9)", range = c(0, 8)),
    snp = list(type = "genotype", maf = 0.3, alleles = c("C", "T")),
    coffee = list(type = "binary", p = 0.6)),
  structure = list(c("age", "bmi", 0.3), c("diet", "bmi", 0.3), c("age", "kids", 0.25)),
  correlations = list(c("smoke", "coffee", 0.3)),
  interventions = list(drug = list(levels = c("placebo", "drug"))),
  outcomes = list(
    dis = list(type = "binary", prevalence = 0.2,
               effects = list(age = list(OR = 1.05), bmi = list(OR = 1.08), smoke = list(OR = c(1, 1.3, 2)),
                              sex = list(OR = 1.5), snp = list(OR = 1.3))),
    ev = list(type = "event", rate = "5/1000", followup = 10,
              effects = list(age = list(HR = 1.06), smoke = list(HR = c(1.4, 2.5)), drug = list(HR = 0.7))),
    sbp = list(type = "continuous", mean = 130, sd = 12, round = 0,
               effects = list(age = list(beta = 0.5), bmi = list(beta = 1.2, per = 1), drug = list(beta = -8)))),
  null_variables = list("coffee"))
toy <- function() normalize_scenario(toy_raw())
