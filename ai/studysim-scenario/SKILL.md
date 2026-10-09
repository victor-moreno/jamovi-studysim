---
name: studysim-scenario
description: Turn a scenario estimated by studysim's "Derive scenario" (jamovi, Simulate menu) into a teaching scenario for the studysim library - meaningful names and translations, a causal structure (DAG) from domain knowledge, rules for impossible combinations, rounded conditional effects, teaching note and references - and validate it by simulation. Use when the user uploads a derived studysim YAML (it has a `derivation:` block) or asks to write or fix a studysim scenario.
---

# studysim scenario from derived data

studysim simulates medical studies for teaching from a library of scenarios
(YAML). Its jamovi analysis *Derive scenario* estimates a scenario from a real
dataset; the result is statistically faithful but causally blank (undirected
correlations, raw column names). This skill finishes it.

## Inputs

- the derived YAML (required): `derivation:` holds the source, original names,
  missing data, adjusted and crude estimates with CI, and all correlations;
- optional: what the study was and the teaching goal. Ask for them if the
  variables' meaning is unclear; do not guess the meaning of a cryptic column.

## Steps

Follow `references/prompt.md` (the same steps as the module's portable prompt),
with the format in `references/schema.md` and `references/example.yaml` (a
finished built-in scenario) as the model to imitate.

Then validate, if R is available:

```sh
Rscript scripts/check_scenario.R scenario.yaml        # needs the studysim package or its source (STUDYSIM_SRC=/path)
```

It reports schema problems (fix all errors), runs the rules, simulates a large
study of a suitable design and compares the re-estimated adjusted effects with
the derivation's (through `derivation: renamed:`). Differences beyond ~15% mean
a structure or rounding change moved the scenario away from the data: fix it
or explain it in the report.

## Output

One YAML file (the scenario) and a short report of decisions and things to
verify. Never present literature references as checked unless you checked
them; mark them `(to verify)`.
