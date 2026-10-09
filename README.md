# studysim: generate simulated medical studies for jamovi

A jamovi module for teaching epidemiology and clinical research. Pick a clinical
**scenario**, an **outcome**, a **study design** that fits it and a sample size, and
get a plausible dataset in a new jamovi window, with confounders, mediators, effect
modifiers and null variables built in. A hidden **answer key** gives the true
parameters and what crude and adjusted analyses of that design should find.

**Tutorial: <https://victor-moreno.github.io/jamovi-studysim/>**

## Analyses (menu: Simulate)

- **Simulated study**: scenario → outcome → design (case-control, matched
  case-control, cohort, cross-sectional, randomised trial, 2×2 crossover,
  prognosis); choose the measured variables and group numeric ones; missing data;
  seed and student keys; data dictionary, descriptives, causal diagram and answer
  key; *Open dataset* or *Export CSV*.
- **Library**: lists, checks, explores and exports scenarios; reads custom
  libraries in YAML, Excel or JSON.
- **Derive scenario**: estimates a scenario from a real dataset (marginals, latent
  correlations, conditional effects) and checks it by simulation; an AI prompt and
  a Claude skill in `ai/` turn it into a teaching scenario.

Six scenarios ship with version 0.1 (breast cancer risk and prognosis, coronary
heart disease, an antihypertensive trial, metformin, bioequivalence); the format is
in `inst/library/schema.md`. Effect sizes are teaching-grade plausible values
informed by the cited literature, not meta-analytic estimates.

The options, results, built-in scenarios and dataset column names are available in
English, Spanish and Catalan, following jamovi's language setting.

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
by Terry Therneau.

This module has been developed with support of the Instituto de Salud Carlos III
(ISCIII), “Programa FORTALECE del Ministerio de Ciencia e Innovación”, through the
project number FORT23/00032 and the Consortium for Biomedical Research in
Epidemiology and Public Health (CIBERESP), action Genrisk.

## Licence

GPL (>= 2).
