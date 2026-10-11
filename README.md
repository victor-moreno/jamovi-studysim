# studysim: generate simulated medical studies for jamovi

A jamovi module for teaching epidemiology and clinical research. Pick a clinical
**scenario**, an **outcome**, a **study design** that fits it and a sample size, and
get a plausible dataset in a new jamovi window, with confounders, mediators, effect
modifiers and null variables built in. A hidden **answer key** gives the true
parameters and what crude and adjusted analyses of that design should find.

**Tutorial: <https://victor-moreno.github.io/jamovi-studysim/>**

## Analyses (menu: Simulate)

- **Simulated study**: scenario → outcome → design; choose the measured
  variables and group numeric ones; missing data; seed and student keys; data
  dictionary, descriptives, causal diagram and answer key; *Open dataset* or
  *Export CSV*. Designs:
  - observational: cross-sectional, cohort, case-control, matched case-control,
    nested case-control (risk sets), case-cohort, ecological (regions),
    prognosis (with competing risks);
  - trials: randomised (with interim looks), cluster randomised, 2×2 factorial,
    2×2 crossover, longitudinal with dropout, before-after (single group);
  - diagnostic accuracy (markers with a target AUC, marker panels for p ≫ n)
    and method comparison (Bland-Altman);
  - several outcomes for the same people (linked outcomes, illness-death);
  - realism: missing data (MCAR, MAR, MNAR), measurement error, detection
    limits and assay batches, data-entry errors, extra null variables; an
    answer-key report for the teacher.
- **Compare designs**: one scenario under several designs drawn from the same
  population: estimates with 95% CI per design against the truth (optionally the
  large-study expectation), a forest plot, and all studies stacked in one dataset
  with a design column.
- **Library**: lists, checks, explores and exports scenarios; reads custom
  libraries in YAML, Excel or JSON.
- **Derive scenario**: estimates a scenario from a real dataset (marginals, latent
  correlations, conditional effects; missing covariates by complete cases, mean,
  single or multiple imputation; codes such as 99 or impossible zeros read as
  missing; values that do not apply in some people written as rules) and checks
  it by simulation; an AI prompt and
  a Claude skill in `ai/` turn it into a teaching scenario.

## Scenarios

65 scenarios, most replicating a real study: their parameters were estimated
with the module's own *Derive* engine from open patient-level data (R
packages survival, MASS, datasets, HSAUR3, TH.data, pROC, medicaldata,
aplore3, NHANES; the UCI repository) or from the author's teaching datasets,
or taken from the published tables of the study (including papers chosen for
critical reading); each was
then given a causal structure and a teaching note (English, Spanish, Catalan).
The files store parameters only, never data. The format is in
`inst/library/schema.md`. Effect sizes reproduce the source's main estimates
as teaching values, not meta-analytic estimates. The
[scenario catalogue](https://victor-moreno.github.io/jamovi-studysim/scenarios.html)
explains the method and filters the scenarios by domain, design and topic.

## Install

Download the `.jmo` for your computer from the [Releases](../../releases) page
(`x64` for Intel/AMD, `arm64` for Apple silicon) and in jamovi choose
**Modules (⊕) → Sideload**. Requires jamovi 28.4 or newer; no other packages.

## Developers

```sh
Rscript tools/build-library-js.R      # after editing inst/library
bash tools/build-jmo.sh current       # dist/*.jmo
bash tools/build-ai-skill.sh          # dist/studysim-scenario-skill.zip
Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat")'
```

The engine (`R/engine_*.R`, `R/derive.R`) is plain R, independent of jamovi.
`tests/ui/*.py` and `tutorial/screenshots/capture.py` drive jamovi in Docker with
the jamovi-automation helpers; the tutorial is a Quarto site in `tutorial/`,
rendered to `docs/`.

## Acknowledgements

Designed and built together with Claude Code, over several rounds of design,
implementation and testing in jamovi. Ideas come from simstudy, simDAG, PRIME-CVD
and The Islands; no code was copied. Survival models use the R package `survival`
by Terry Therneau; mixed models in the answer key use `nlme` (Pinheiro, Bates and
the R Core Team). Scenario parameters were estimated from datasets distributed
with the R packages survival (Therneau), MASS (Venables and Ripley), datasets
(R Core Team), HSAUR3 (Everitt and Hothorn), TH.data (Hothorn), pROC (Robin et
al.), medicaldata (Higgins), aplore3 (Braglia) and NHANES (Pruim); we thank the
original investigators, cited in each scenario.

This module has been developed with support of the Instituto de Salud Carlos III
(ISCIII), “Programa FORTALECE del Ministerio de Ciencia e Innovación”, through the
project number FORT23/00032 and the Consortium for Biomedical Research in
Epidemiology and Public Health (CIBERESP), action Genrisk.

## Licence

GPL (>= 2).
