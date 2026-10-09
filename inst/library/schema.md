# studysim library format

A library is a **dictionary** of shared variables plus **scenarios** (one clinical
question each). The same content can be written three ways, and the Library
manager converts between them:

- **YAML**: `variables.yaml` + `scenarios/<id>.yaml` (or one file with
  `variables:` and `scenarios:`), zipped for exchange;
- **Excel**: one sheet per table (below), one row per item;
- **JSON**: one object `{"variables": {...}, "scenarios": {...}}`. This is the only
  form the *Simulated study* panel can read to fill its pickers.

Numbers may be written as small expressions: `2.5/1000`, `log(1.04)`.
Labels may be plain text or translated: `label: {en: Age, es: Edad, ca: Edat}`.
Names of variables, outcomes and interventions start with a letter and use only
letters, digits and `_` (they become column names).

> **YAML trap.** Inside `{ ... }` a comma separates fields, so quote distributions
> there: `age: {from: dictionary, dist: "uniform(35, 75)"}`. On their own line no
> quotes are needed: `dist: uniform(35, 75)`.

## Variables (dictionary entries and scenario variables)

| field | for | meaning |
|---|---|---|
| `from: dictionary` | any | start from the dictionary entry of the same name; other fields override it |
| `type` | any | `continuous`, `count`, `binary`, `nominal`, `ordinal`, `genotype` |
| `dist` | continuous, count | `normal(mean, sd)`, `lognormal(mean, sd)` (natural-scale mean and sd), `gamma(mean, sd)`, `beta(mean, sd)`, `uniform(min, max)`, `exponential(mean)`; counts: `poisson(mean)`, `negbin(mean, size)` |
| `range: [min, max]` | continuous, count | truncation (exact, by rescaling the quantile) |
| `round` | continuous | decimals kept |
| `p` | binary | proportion in the second level |
| `levels` | binary, nominal, ordinal | level labels, reference first (binary default `[no, yes]`) |
| `probs` | nominal, ordinal | one proportion per level, adding up to 1 |
| `maf`, `alleles` | genotype | minor allele frequency (Hardy-Weinberg); alleles default `[A, B]` → levels AA, AB, BB |
| `units`, `label`, `description` | any | shown in the data and the dictionary |

## Structure among variables

```yaml
structure:                 # directed edges, latent standardized path coefficients (-1..1)
  - [age, bmi, 0.20]
correlations:              # undirected latent correlations between variables with no incoming edge
  - [smoking, coffee, 0.30]
```

All variables are generated from latent standard normals: variables without
incoming edges are jointly normal with the given correlations, the others are
linear in their parents plus noise, and each latent is mapped to its marginal by
its quantile function (a Gaussian copula). The edges must form a DAG, and the
edges into a variable must leave room for noise (their explained variance < 1).

## Outcomes

```yaml
outcomes:
  breast_cancer:
    type: event            # binary | event | continuous | count
    rate: 2.5/1000         # event: rate per time unit (exponential), or
    # risk: {value: 0.08, at: 10}   cumulative risk at a time, or
    # median: 5                     median survival
    shape: 1               # event: Weibull shape (1 = exponential)
    window: 10             # event: years at risk behind a case/control status
    followup: 10           # event: default follow-up of cohorts and trials
    designs: [case_control, matched_cc, cohort]   # optional; otherwise derived from the type
    effects:
      age: {HR: 1.04, per: 1}
      smoking: {HR: [1, 1.2, 1.9]}               # one value per level, reference first
      bmi: {HR: 1.12, per: 5, by: {menopause: {pre: 0.93, post: 1.12}}}
      fgfr2: {HR: 1.26, model: additive}         # genotype: additive, dominant or recessive
```

| type | target | effects | analysed with |
|---|---|---|---|
| `binary` | `prevalence` | `OR` (logistic) | logistic regression |
| `event` | `rate`, `risk` or `median`; `shape` | `HR` (proportional hazards; `OR`/`RR` read as HR) | Cox (cohort), logistic (case-control) |
| `continuous` | `mean`, `sd` (residual), `within_cor` (crossover), `round`, `range` | `beta` (difference per unit) | linear regression |
| `continuous` with `scale: log` | `mean` = geometric mean, `sd` of log values | `ratio` | linear regression on logs |
| `count` | `mean`, optional `size` (negative binomial) | `RR` | Poisson regression |

The intercept (or baseline hazard) is calibrated on 100 000 simulated people so
the population hits the target. Effects are **conditional** (adjusted for every
other variable in the scenario); the answer key shows what crude and adjusted
analyses of each design are expected to give.

Default designs by outcome type: binary → cross-sectional, cohort, case-control,
matched case-control; event → cohort, case-control, matched case-control;
continuous and count → cross-sectional, cohort; plus randomised trial (and a 2×2
crossover for continuous outcomes) when an effect names an intervention.
`prognosis` (patient cohort with staggered entry) must be listed in `designs:`.

## Interventions (trials)

```yaml
interventions:
  drug: {levels: [placebo, amlodipine], ratio: [1, 1], label: Treatment arm}
```

An intervention is assigned by the design (permuted blocks), never drawn from the
population; observational designs leave everybody in the reference level.

## Other scenario fields

`id`, `label`, `domain`, `population` (text), `null_variables` (candidate
factors with no effect), `designs` (default options per design, e.g.
`matched_cc: {match: [age, sex], caliper: 2}`, `prognosis: {accrual: 4,
followup: 8, loss: 0.02}`, `crossover: {period: -1.5, carryover: 0}`),
`references`, `teaching` (the note shown with the answer key).

## Excel sheets

| sheet | one row per | columns |
|---|---|---|
| `scenarios` | scenario | scenario, domain, null_variables, label_en/es/ca, population_*, teaching_* |
| `variables` | variable (scenario `*` = dictionary) | scenario, variable, from, type, dist, p, maf, round, levels, probs, alleles, range, units_*, label_*, description_* |
| `interventions` | intervention | scenario, intervention, levels, ratio, label_* |
| `outcomes` | outcome | scenario, outcome, type, scale, prevalence, rate, median, shape, mean, sd, within_cor, size, window, followup, round, time_units, risk, risk_at, levels, range, designs, units_*, label_*, description_* |
| `structure` | edge | scenario, from, to, strength, kind (`path` or `correlation`) |
| `effects` | effect | scenario, outcome, variable, measure, value, per, model, modifier, modifier_values (`pre: 0.93, post: 1.12`) |
| `designs` | design option | scenario, design, option, value |
| `references` | reference | scenario, reference |

Lists inside a cell are comma-separated (`never, former, current`).

## Names and categories in each language

`inst/library/i18n/en.yaml`, `es.yaml` and `ca.yaml` rename what students see:

```yaml
names:            # column names (letters, digits and _)
  age: edad
  smoking: tabaco
levels:           # category labels, by the library's level
  current: fumador
  "yes": sí       # quote yes/no keys
```

The library keeps its own ids and levels; a missing entry is shown as written in
the library. Names must stay unique within a dataset.

A custom library can carry its own translations, which win over the built-in
ones: an `i18n:` section in a library file (or in one scenario file, for that
scenario only), files `i18n/<lang>.yaml` in a zip, or a `translations` sheet in
Excel (scenario `*` = the whole library; kind `name` or `level`; key; en, es, ca).

```yaml
i18n:
  es:
    names: {potassium: potasio}
    levels: {low: bajo, high: alto}
```

The Library check reports translated names that are not valid column names or
that name two columns.
