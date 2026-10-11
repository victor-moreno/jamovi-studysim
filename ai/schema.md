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

## Rules: variables tied to others

Beyond the latent correlations, a variable can be tied to others by a rule:

```yaml
menopause:
  type: binary
  levels: [pre, post]
  p: plogis((age - 51) / 1.5)      # probability from other variables
hrt:
  type: binary
  p: 0.25                          # among those allowed by only_if
  levels: [never, ever]
  only_if: menopause == "post"     # structural value otherwise
  otherwise: never
cigarettes:
  type: count
  dist: negbin(15, 4)
  only_if: smoking == "current"    # structural zero
  otherwise: 0
bmi:
  type: continuous
  formula: weight / (height / 100)^2   # derived, no dist needed
```

Rules see numbers for continuous and count variables and the library's level
labels (in quotes) for the others. Allowed: arithmetic, comparisons, `&`, `|`,
`!`, `%in%`, `c()`, `log`, `exp`, `sqrt`, `abs`, `round`, `pmin`, `pmax`, `ifelse`,
`plogis`, `qlogis`. Rules may refer to each other but not in a circle; the check
runs them on a small sample. In Excel they are the `p`, `formula`, `only_if` and
`otherwise` columns of the `variables` sheet.

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

A numeric effect modified by another variable (`by:`) acts on `x - center`:
the modifier's own effect (none, unless it has an effect of its own) holds at
`x = center`. The default `center` is the population mean of x, so an
unmodified-looking modifier does not pick up a large effect at x = 0; write
`center: 0` when 0 is a meaningful value (never used, no births). The check
warns when a center lies outside the variable's values.

| type | target | effects | analysed with |
|---|---|---|---|
| `binary` | `prevalence` | `OR` (logistic) | logistic regression |
| `event` | `rate`, `risk` or `median`; `shape` | `HR` (proportional hazards; `OR`/`RR` read as HR) | Cox (cohort), logistic (case-control) |
| `continuous` | `mean`, `sd` (residual), `within_cor` (crossover), `round`, `range` (truncation, exact) | `beta` (difference per unit) | linear regression |
| `continuous` with `scale: log` | `mean` = geometric mean, `sd` of log values | `ratio` | linear regression on logs |
| `count` | `mean`, optional `size` (negative binomial) | `RR` | Poisson regression |

The intercept (or baseline hazard) is calibrated on 100 000 simulated people so
the population hits the target. Effects are **conditional** (adjusted for every
other variable in the scenario); the answer key shows what crude and adjusted
analyses of each design are expected to give.

Default designs by outcome type (used when an outcome has no `designs:` list):

| outcome | designs |
|---|---|
| binary | cross_sectional, cohort, case_control, matched_cc, ecological; diagnostic when it has `markers:` |
| event | cohort, case_control, matched_cc, nested_cc, case_cohort, ecological |
| continuous, count | cross_sectional, cohort, ecological |
| any, with an intervention effect | + rct, cluster_rct; continuous: + crossover, longitudinal, pre_post; two 2-level interventions: + factorial |
| continuous | + method_comparison (two methods measuring the outcome) |

`prognosis` (patient cohort with staggered entry) must be listed in `designs:`.

### Competing event (event outcomes)

```yaml
  bc_death:
    type: event
    risk: {value: 0.14, at: 10}
    effects: {...}
    competing:                       # whichever comes first ends follow-up
      label: Death from other causes
      risk: {value: 0.08, at: 10}    # as if the main event could not happen
      shape: 1.5
      effects:
        age: {HR: 1.10, per: 1}
```

Cohort, prognosis and trial data then code the status 0 = censored, 1 = event,
2 = competing event; the key gives cause-specific hazard ratios (Cox censoring
status 2). Case-control style designs count only events before a competing one.

### Several outcomes for the same people

A scenario can have several outcomes; *Simulated study* can put them all in one
dataset (option *Other outcomes of the scenario*), for cross-sectional, cohort,
prognosis and trial designs. Two refinements:

```yaml
  controlled:
    type: binary
    link: {outcome: hba1c, rho: -0.85}   # correlated with another outcome
  death:
    type: event
    after: recurrence                    # illness-death: death after a first event
    after_event:                         # hazard of death once it has happened
      median: 1.0
      effects: {treatment: {HR: [1, 1.13, 1.41]}}
```

`link` correlates an outcome's noise with another's on a latent scale where
higher means more of the outcome (a higher value, an event, an earlier event):
`rho` > 0 when they go together, < 0 when one goes with less of the other. Link
to an outcome that has no link of its own.

`after: X` builds overall survival from a first event X (e.g. recurrence):
death without X is the outcome's own target (rate, risk or median, with its
effects) or, without one, X's competing event; after X, death follows
`after_event`. The time to death is then never shorter than the time to X, and
there is no single true hazard ratio (the key gives the transitions). Such
outcomes fit cohort, prognosis and randomised trial designs.

### Markers (binary outcomes, diagnostic design)

```yaml
    markers:
      psa:
        dist: lognormal(5.5, 3.5)    # values in people without the disease
        auc: 0.68                    # area under the ROC curve
        direction: higher            # higher (default) or lower in the diseased
        units: ng/mL
        round: 1
        range: [0.5, 100]
      gene:                          # a block of p markers g01..g40
        block: 40
        informative: 4               # the first 4 have the AUC, the others 0.5
        auc: 0.62
        rho: 0.2                     # correlation among the block's markers
      troponin:
        dist: lognormal(0.012, 0.012)
        auc: 0.90
        lod: 0.01                    # limit of detection (also for variables)
      ca125:
        dist: lognormal(15, 10)
        auc: 0.80
        cutoff: 35                   # test positive beyond it (default: Youden's point)
        severity_sd: 1               # the diseased differ in how far it moves
        depends: {age: 0.3}          # latent shift per SD of age, in everybody
```

A marker is binormal: a latent N(0, 1) in the non-diseased, shifted by
√2·Φ⁻¹(AUC) in the diseased, mapped to `dist` by its quantiles (which keeps the
AUC). Markers depend on the disease only (conditionally independent of the
other variables) unless it `depends` on some of them; `auc` is then the AUC at
fixed values of those variables, and the key gives the population AUC. With
`severity_sd` the diseased differ in a severity shared by the markers, and the
shift is widened so that `auc` stays the population AUC (a study of the most
severe cases overstates it: spectrum bias). The key reports sensitivity,
specificity and predictive values at `cutoff` (by default the latent midpoint
between the groups, Youden's point). A block has standard-normal values. The outcome's
`marker_cor` (default 0) correlates its single markers within disease groups
(markers that measure related things).

## Interventions (trials)

```yaml
interventions:
  drug: {levels: [placebo, amlodipine], ratio: [1, 1], label: Treatment arm}
```

An intervention is assigned by the design (permuted blocks), never drawn from the
population; observational designs leave everybody in the reference level.

## Other scenario fields

`id`, `label`, `domain`, `population` (text), `null_variables` (candidate
factors with no effect), `designs` (default options per design, below),
`references`, `teaching` (the note shown with the answer key).

| design | options (defaults) |
|---|---|
| `cohort`, `prognosis`, `rct` | `followup` (outcome's), `loss` 0.02 per year, `accrual` (0; prognosis 2); rct: `block` 4, `looks` 0 |
| `case_control`, `matched_cc` | `ratio` 1; matched: `match` [age, sex], `caliper` 2 |
| `nested_cc` | `ratio` 1, `followup`, `loss`, `accrual` |
| `case_cohort` | `subcohort` 0.1 (fraction of the cohort), `followup`, `loss`, `accrual` |
| `cluster_rct` | `clusters` 20, `icc` 0.05, `followup`, `loss` |
| `factorial` | `interaction` 0 (beta, or log OR/HR/RR, for those given both), `followup`, `loss` |
| `longitudinal` | `visits` 4, `dropout` 0.2 (fraction missing the last visit), `trend` 0 (change in the reference arm from baseline to the last visit; the outcome's `mean` is the last visit's), `profile` (the treatment effect at equally spaced visits as multiples of its last-visit value, e.g. `[4.7, 3.6, 1]` for an effect that fades; default linear) |
| `ecological` | `regions` 30, `region_size` 2000, `region_sd` 0.5, `context_sd` 0.2, `between` (triples `[a, b, r, ...]`: correlations of the regions' shifts of exogenous variables, when regions differ more coherently than people) |
| `crossover` | `period` 0, `carryover` 0 |
| `pre_post` | `threshold` 0 (enrol only above this percentile of the value before: regression to the mean), `trend` 0 |
| `method_comparison` | `bias` 0, `slope` 1 (method B = bias + slope x true + error), `error_a` 0.10, `error_b` 0.15 (as shares of the outcome's SD) |

For example `matched_cc: {match: [age, sex], caliper: 2}`, `prognosis: {accrual: 4,
followup: 8, loss: 0.02}`, `crossover: {period: -1.5, carryover: 0}`.

## Locking the answer key

```yaml
lock: 2bb80d537b1da3e38bd30361aa855686bde0eacd7162fef6a25fe97bf527a25b   # SHA-256 of the teacher code
```

A library with `lock:` (written by the Library analysis when a teacher code
is typed before exporting) hides, while it is loaded, the answer key, its
report, the class export, the effect sizes on the diagram, the true values of
Compare designs and the effects in Library, until the same code is typed
under *Teacher code*. It is a deterrent for a class, not protection: the
library file itself holds the parameters. In Excel it is a row of a
`settings` sheet (setting `lock`, value the hash).

## Excel sheets

| sheet | one row per | columns |
|---|---|---|
| `scenarios` | scenario | scenario, domain, null_variables, label_en/es/ca, population_*, teaching_* |
| `variables` | variable (scenario `*` = dictionary) | scenario, variable, from, type, dist, p, maf, round, levels, probs, alleles, range, units_*, label_*, description_* |
| `interventions` | intervention | scenario, intervention, levels, ratio, label_* |
| `outcomes` | outcome | scenario, outcome, type, scale, prevalence, rate, median, shape, mean, sd, within_cor, size, window, followup, round, time_units, risk, risk_at, levels, range, designs, units_*, label_*, description_* |
| `structure` | edge | scenario, from, to, strength, kind (`path` or `correlation`) |
| `effects` | effect | scenario, outcome, variable, measure, value, per, model, center, modifier, modifier_values (`pre: 0.93, post: 1.12`) |
| `markers` | marker | scenario, outcome, marker, dist, round, auc, direction, block, informative, rho, lod, cutoff, severity_sd, depends (`age: 0.3`), range, units_*, label_* |
| `designs` | design option | scenario, design, option, value |
| `references` | reference | scenario, reference |

Lists inside a cell are comma-separated (`never, former, current`). A competing
event is a row of `outcomes` (and its effects rows of `effects`) named
`<outcome>:competing`.

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
