# studysim: generate simulated medical studies for jamovi

A jamovi module for teaching epidemiology and clinical research. Pick a clinical
**scenario**, an **outcome**, a **study design** and a sample size, and get a
plausible dataset in a new jamovi window. A hidden **answer key** gives the true
parameters and what the analyses should find.

- **Tutorial:** <https://victor-moreno.github.io/jamovi-studysim/>
- **Scenario catalogue:** <https://victor-moreno.github.io/jamovi-studysim/scenarios.html>
  (65 scenarios, most replicating a real study)

## Analyses (menu: Simulate)

- **Simulated study**: one dataset from a scenario, outcome and design
  (observational, trials, diagnostic), with optional realism and biases, and
  its answer key.
- **Compare designs**: one scenario under several designs, estimates against
  the truth.
- **Library**: lists, checks, explores and exports scenarios.
- **Derive scenario**: estimates a new scenario from a real dataset.

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
