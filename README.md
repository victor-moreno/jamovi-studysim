# studysim: generate simulated medical studies for jamovi

A jamovi module for teaching epidemiology and clinical research. The student picks a
**clinical scenario** from a library, one of its **outcomes**, a **study design**
that fits that outcome, a sample size and some realism, and gets a **plausible
dataset** in a new jamovi window: mixed variable types, confounders, mediators,
effect modifiers and null variables, so that the multivariable analysis matters. A
hidden **answer key** gives the true parameters and what crude and adjusted analyses
of that design are expected to estimate.

It needs no packages beyond those bundled with jamovi: `survival`, `yaml`,
`jsonlite`, `openxlsx`, `zip`, `digest`, `jmvcore` and `R6`.

## Analyses (menu: Simulate)

**Simulated study**
- Scenario → outcome → design pickers; only the designs that fit the outcome are
  offered (case-control, matched case-control, cohort, cross-sectional, randomised
  trial, 2×2 crossover, prognosis).
- *Variables* checklist, with each variable's units or categories: tick the
  scenario's variables to include in the data (none at start); the others stay unmeasured but still act (residual confounding
  exercises). *Form* (numeric variables only) groups a continuous variable: median split, tertiles,
  quartiles, quintiles (cut points from the sample, labels such as `Q2: 24.0–27.3`)
  or *cuts* typed as values (`18.5, 25, 30`, or `18,5; 25` with a decimal
  comma; labels keep the typed decimals) or percentiles (`10%, 90%`); the answer key then gives the expected
  group contrasts.
- Design options: controls per case, follow-up, loss to follow-up, recruitment
  period, period and carryover effects. Realism: missing values completely at random
  or depending on the outcome.
- *Open dataset* opens the data in a new window (proper nominal/ordinal/ID types and
  variable descriptions); *Export CSV* saves them.
- Results: study summary (cases and controls, events and person-years, arms…),
  data dictionary, univariate descriptives (mean (SD), median [IQR], n (%)), the
  causal diagram (DAG: measured variables in solid boxes, unmeasured ones dashed,
  effect sizes on the arrows as an option), and the answer key: the true model, the expected crude and
  adjusted estimates under this design (from one large simulated study), the
  calibration, the latent structure, the teaching note and the sources.
- *Seed* and *Student key*: the same seed always gives the same study; a student key
  (e.g. a student ID) gives each student a different dataset from the same
  assignment, which the teacher can regenerate.
- The superpopulation depends only on the scenario and the seed, so the same seed
  under another design samples the same people. Showing the key, the DAG or a
  table does not draw the study again; only options that change the data do.

**Library**
- Lists the scenarios, checks them (plain-language problems located by scenario
  and field), and shows one scenario's variables, effects, structure, teaching note
  and references.
- Reads a custom library (YAML, Excel, JSON or a zip of them) and exports the
  library or one scenario as Excel, JSON or zipped YAML, or an Excel template with
  a worked example and the format reference.

**Derive scenario**
- From the open dataset: choose the outcome (binary, continuous, count, or an
  event with its follow-up time) and the variables. It estimates each marginal
  (normal, log-normal, gamma or uniform; Poisson or negative binomial; category
  proportions), the latent correlations (by inverting the same Gaussian copula
  the simulator uses, so they are reproduced) and the conditional effects
  (logistic, linear, Poisson or Cox, with 95% CI and crude estimates), plus the
  prevalence, mean, risk or Weibull shape for calibration.
- *Check by simulating*: a large study simulated from the derived scenario is
  analysed with the same model; the re-estimated effects should match the data.
- *Export scenario* as YAML (with a `derivation:` block: source, original names,
  missing data, estimates and correlations), Excel or JSON.
- The data cannot tell causes from correlates, so the scenario has undirected
  correlations only. Finish it by hand, or with an AI (next section).

## From a derived scenario to a teaching scenario with an AI
- `ai/prompt-derive-to-scenario.md`: a prompt for any AI assistant. Attach the
  exported YAML and `inst/library/schema.md`; it returns the scenario with
  names and translations, a causal structure, rules, rounded effects, teaching
  note and references (marked to verify), plus a report of its decisions.
- `ai/studysim-scenario/`: the same as a Claude skill
  (`bash tools/build-ai-skill.sh` → `dist/studysim-scenario-skill.zip`), with
  `scripts/check_scenario.R`, which validates the scenario, simulates a large
  study and compares the re-estimated effects with the data's (following
  renamed variables and changed reference categories).

## The library
Six scenarios ship with v0.1: breast cancer risk factors, breast cancer prognosis,
coronary heart disease incidence (Framingham-like), an antihypertensive drug vs
placebo trial, metformin vs placebo on HbA1c, and bioequivalence of two
formulations. Each is a YAML file in `inst/library/scenarios/` built on a shared
dictionary (`inst/library/variables.yaml`); the format is described in
`inst/library/schema.md`. Besides correlations, variables can be tied by rules:
a probability that depends on other variables (menopause from age), structural
values (no cigarettes for non-smokers, no HRT before menopause) and formulas
(derived variables). Effect sizes are teaching-grade plausible values informed
by the cited literature, not meta-analytic estimates.

Teachers can write their own scenarios in YAML or Excel. The *Simulated study* panel
lists custom scenarios when the library is JSON (the *Library* analysis converts
YAML and Excel to JSON); with a YAML or Excel library the scenario id is typed.

## How the data are generated
Covariates come from a Gaussian copula structured as a DAG: latent standard normals
(exogenous ones correlated, the others linear in their parents), each mapped to its
marginal (normal, log-normal, gamma, beta, uniform, Poisson, negative binomial,
binary, nominal, ordinal, genotype in Hardy-Weinberg). Outcomes follow logistic,
linear (also on the log scale), Poisson/negative binomial or Weibull proportional
hazards models whose intercept or baseline hazard is calibrated to the scenario's
prevalence, rate, risk or median. Designs sample from that superpopulation: case
status from events within a risk window, matching within calipers, permuted-block
randomisation, staggered entry and loss to follow-up. The engine (`R/engine_*.R`)
does not depend on jamovi.

## Languages
The options, tables, notes and the built-in scenarios are available in English,
Spanish and Catalan; jamovi uses the language chosen in its settings. The
dataset follows the language too: column names and category labels come from
three editable dictionaries, `inst/library/i18n/{en,es,ca}.yaml` (e.g. `age` →
`edad` / `edat`, `current` → `fumador`). The engine keeps working with the
library's ids, so a saved analysis reopens in any language. A custom library can
bring its own translations (an `i18n:` section, `i18n/<lang>.yaml` in a zip, or a
`translations` sheet in Excel); see `inst/library/schema.md`. The interface
translations are in `jamovi/i18n/*.po` (`python3 tools/fill-translations.py`
after `jmc --i18n . --update`).

## Build and install (developers)
```sh
Rscript tools/build-library-js.R      # after editing inst/library: embeds it in the panel
bash tools/install.sh desktop         # jamovi.app, via jmvtools::install()
bash tools/build-jmo.sh current       # release: dist/*_current_R4.6.0_{x64,arm64}.jmo
```
Tests: `Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat")'`
(engine, designs, library round trips, analyses), and `tests/ui/*.py`, which drive
both panels in a jamovi 28.4 container with the jamovi-automation helpers.
Requires jamovi ≥ 28.4.

## Acknowledgements
This module was designed and built together with Claude Code, over several rounds of
design, implementation and testing in jamovi.

Ideas come from simstudy (definition tables), simDAG (DAG-based generation),
PRIME-CVD (clean vs messy synthetic cohorts) and The Islands (recruiting from a
virtual population); no code was copied. Survival models in the answer key use the R
package `survival` by Terry Therneau.

This module has been developed with support of the Instituto de Salud Carlos III
(ISCIII), “Programa FORTALECE del Ministerio de Ciencia e Innovación”, through the
project number FORT23/00032 and the Consortium for Biomedical Research in
Epidemiology and Public Health (CIBERESP), action Genrisk.

## Licence
GPL (>= 2).
