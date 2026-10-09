# Prompt: from a *Derive scenario* file to a teaching scenario

Use this prompt with any AI assistant. Attach (or paste) two files:

1. the YAML exported by **Simulate ▸ Derive scenario** in jamovi (it has a
   `derivation:` block with the estimates and their context);
2. `schema.md`, the library format (in the module: `inst/library/schema.md`).
   Do not skip it: without it the assistant guesses the format (e.g. the
   `i18n:` layout) and the library check rejects or silently ignores parts.

Optionally add a few lines about the study the data came from (population,
design, why it was collected) and the teaching goal.

Before deriving, give *Derive* only **baseline** covariates: leave out anything
recorded after or because of the outcome (site of metastasis, treatment of a
relapse, cause of death), which also empties the complete cases.

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

0. **Check the derivation first.** Stop and tell the teacher (do not repair
   silently) if `complete_rows` is well below `rows`, or a variable is missing
   mostly in one outcome group or is recorded only after the outcome: the
   teacher should run *Derive* again without it. Count events (or cases) per
   estimated coefficient: below ~10 the adjusted estimates are overfitted.
   Then shrink **all** log ratios (betas) by one common factor (0.5-0.7, or
   (LR - df) / LR if the teacher gives the model's likelihood-ratio statistic)
   rather than copying them. Shrinking one variable alone breaks the
   confounding between them. An estimate that is NA, outside 0.01-100, or whose
   CI spans more than a factor of 100 is not estimable: give that variable no
   effect and say why.
1. **Names.** Give each variable, outcome and intervention a short, meaningful
   id (letters, digits, `_`; lower case), a `label` in English, Spanish and
   Catalan (`{en: ..., es: ..., ca: ...}`) and `units` where they apply. Add an
   `i18n:` section, by language first, as in the schema:
   `i18n: {es: {names: {...}, levels: {...}}, ca: {...}}`. Record every
   renaming in `derivation: {renamed: {old_id: new_id}}`.
2. **Levels.** Put the reference category first (e.g. `never` before `former`
   and `current`; `no` before `yes`) and reorder `probs` and categorical effect
   values accordingly (the reference value is 1 for OR/HR/RR, 0 for beta; the
   others are each level against the new reference: divide ratios by the new
   reference's old value). **Exception:** the level order of a nominal or
   ordinal variable is its latent scale, and the derivation's latent
   correlations were computed with the order it has. Do not reorder such a
   variable if it has correlations of |r| ≥ 0.05. Instead, if its order does
   not follow the variables it correlates with (e.g. subtypes not sorted by the
   marker that defines them), ask the teacher to set the level order in jamovi
   and run *Derive* again.
3. **Causal structure.** For each latent correlation of the derivation with
   |r| ≥ 0.05, decide from domain knowledge whether one variable causes the
   other (e.g. age → blood pressure, smoking → lung function), both share a
   cause, or no direction is defensible. Write directed pairs under
   `structure:` as `[cause, effect, strength]`, with the strength equal to the
   latent correlation when the effect has a single parent; when it has several,
   reduce the strengths so that their combined explained variance stays below
   0.9 and the pairwise correlations stay close to the derivation (say so in
   the report). Never create cycles. Keep |strength| ≤ 0.9: a latent
   correlation of ±0.98 is *Derive*'s limit and means the data hit a boundary
   (e.g. the few carriers of a rare variant all in one group); use a moderate
   value (0.5-0.7) instead. Then list the **exogenous** variables (those with no
   incoming edge): `correlations:` may only pair two of them, so delete every
   other pair. The library check rejects a correlation that touches a variable
   with an incoming edge.
4. **Rules.** Add `p:`, `only_if:`/`otherwise:` or `formula:` where a
   combination is impossible or a value is structural (menopause and age,
   pregnancy and sex, cigarettes per day only for current smokers, BMI from
   weight and height, …). Rules may use only the operators and functions listed
   in `schema.md`. Strong exclusivity involving a rare category (two mutually
   exclusive mutations, one with p < 0.05) is better written as a rule than as
   a latent edge. An edge acts on the whole latent scale, so it also shifts
   people who do not carry the rare category, and it distorts the other
   correlations.
5. **Effects.** Keep the **adjusted** estimates of the derivation as the
   conditional effects (shrunk as in step 0 when needed), rounded to
   teaching-friendly values that stay inside their 95% CI. Choose a sensible `per` for continuous variables (per 10 years,
   per 5 kg/m2, …) and convert the value (a ratio per 1 unit to the power
   `per`; a beta times `per`). If a variable is a mediator in your structure,
   say in the teaching note that its adjusted effect is a direct effect. Do not
   invent effects for variables the derivation did not estimate. A variable
   whose estimate is close to the null with a CI that includes it may go to
   `null_variables:` with no effect, if that is plausible; say so.
6. **Outcome.** Keep the type and the calibration target (prevalence, mean and
   residual sd, rate, risk at a time, shape). Set `designs:` to the designs that
   make sense for this outcome (see the schema) and `window`/`followup` for
   events. *Derive* does not know the time units: times are in the data's
   units (often days). Set `time_units` and convert `risk: at`, `followup`,
   `window` and the design times (e.g. days / 365.25 → years).
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
  (quote distributions, and any label with a comma, inside `{ }`; quote
  `yes`/`no` keys);
- **a short report**: every structural decision and its reason, every
  rounding that moved an estimate by more than 10%, rules added, variables left
  undirected, and anything the teacher should verify.

If you can run R, validate before answering: with the studysim package
installed (or its source), run `ai/studysim-scenario/scripts/check_scenario.R
scenario.yaml` and fix every error; report its comparison of simulated and data
estimates. If you cannot run R, check by hand before answering:
`correlations:` pairs only exogenous variables; no |strength| above 0.9; every
effect names a variable or intervention of the scenario; `i18n:` has
languages (`es`, `ca`) as its top keys; and categorical effects have one value
per level, reference first. Tell the teacher to load the file in jamovi
(*Library* ▸ check) and to paste back any problem it lists.
