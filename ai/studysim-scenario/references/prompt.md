# Prompt: from a *Derive scenario* file to a teaching scenario

Use this prompt with any AI assistant. Attach (or paste) two files:

1. the YAML exported by **Simulate ▸ Derive scenario** in jamovi (it has a
   `derivation:` block with the estimates and their context);
2. `schema.md`, the library format (in the module: `inst/library/schema.md`).

Optionally add a few lines about the study the data came from (population,
design, why it was collected) and the teaching goal.

---

You are helping a teacher of epidemiology and clinical research turn a scenario
**estimated from real data** into a **teaching scenario** for the studysim
jamovi module. The attached YAML was produced by studysim's *Derive scenario*:
its marginal distributions, latent correlations and conditional effects were
estimated from a dataset, and its `derivation:` block records the source, the
original column names, missing data, the adjusted and crude estimates with 95%
CI, and every observed and latent correlation. The format of the result is
described in the attached `schema.md`; follow it exactly.

Data cannot tell causes from correlates, so the derived file has only undirected
`correlations:`. Your job is to add the causal and teaching knowledge the data
lack, without drifting from what the data say. Do the following, in order:

1. **Names.** Give each variable, outcome and intervention a short, meaningful
   id (letters, digits, `_`; lower case), a `label` in English, Spanish and
   Catalan (`{en: ..., es: ..., ca: ...}`) and `units` where they apply. Add an
   `i18n:` section with the column names (`names:`) and category labels
   (`levels:`) in es and ca. Record every renaming in
   `derivation: {renamed: {old_id: new_id}}`.
2. **Levels.** Put the reference category first (e.g. `never` before `former`
   and `current`; `no` before `yes`) and reorder `probs` and categorical effect
   values accordingly (the reference value is 1 for OR/HR/RR, 0 for beta; the
   others are each level against the new reference: divide ratios by the new
   reference's old value).
3. **Causal structure.** For each latent correlation of the derivation with
   |r| ≥ 0.05, decide from domain knowledge whether one variable causes the
   other (e.g. age → blood pressure, smoking → lung function), both share a
   cause, or no direction is defensible. Write directed pairs under
   `structure:` as `[cause, effect, strength]`, with the strength equal to the
   latent correlation when the effect has a single parent; when it has several,
   reduce the strengths so that their combined explained variance stays below
   0.9 and the pairwise correlations stay close to the derivation (say so in
   the report). Keep under `correlations:` only pairs of variables that have no
   incoming `structure` edge and no defensible direction. Never create cycles.
4. **Rules.** Add `p:`, `only_if:`/`otherwise:` or `formula:` where a
   combination is impossible or a value is structural (menopause and age,
   pregnancy and sex, cigarettes per day only for current smokers, BMI from
   weight and height, …). Rules may use only the operators and functions listed
   in `schema.md`.
5. **Effects.** Keep the **adjusted** estimates of the derivation as the
   conditional effects, rounded to teaching-friendly values that stay inside
   their 95% CI. Choose a sensible `per` for continuous variables (per 10 years,
   per 5 kg/m2, …) and convert the value (a ratio per 1 unit to the power
   `per`; a beta times `per`). If a variable is a mediator in your structure,
   say in the teaching note that its adjusted effect is a direct effect. Do not
   invent effects for variables the derivation did not estimate. A variable
   whose estimate is close to the null with a CI that includes it may go to
   `null_variables:` with no effect, if that is plausible; say so.
6. **Outcome.** Keep the type and the calibration target (prevalence, mean and
   residual sd, rate, risk at a time, shape). Set `designs:` to the designs that
   make sense for this outcome (see the schema) and `window`/`followup` for
   events.
7. **Population, references, teaching.** Write `population` (en/es/ca). Under
   `references:` cite the data source (keep the derivation's line) and, only if
   you are sure they exist, literature supporting the main effects, each marked
   `(to verify)` unless you checked it. Write `teaching:` in en/es/ca: which
   variables confound which associations, which are mediators or colliders,
   what the crude versus adjusted analyses should show, and any modification
   or rule the students should notice.
8. **Keep** the `derivation:` block (plus `renamed:`), `domain:` and the `id`
   the teacher gave, unless asked otherwise.

Return:

- **the complete scenario as one YAML file**, valid against `schema.md`
  (quote distributions inside `{ }`, quote `yes`/`no` keys);
- **a short report**: every structural decision and its reason, every
  rounding that moved an estimate by more than 10%, rules added, variables left
  undirected, and anything the teacher should verify.

If you can run R, validate before answering: with the studysim package
installed (or its source), run `ai/studysim-scenario/scripts/check_scenario.R
scenario.yaml` and fix every error; report its comparison of simulated and data
estimates.
