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
  single or multiple imputation) and checks it by simulation; an AI prompt and
  a Claude skill in `ai/` turn it into a teaching scenario.

## Scenarios

50 scenarios, most replicating a real study: their parameters were estimated
with the module's own *Derive* engine from open patient-level data (R
packages survival, MASS, datasets, HSAUR3, TH.data, pROC, medicaldata,
aplore3, NHANES), or taken from the published tables of the study; each was
then given a causal structure and a teaching note (English, Spanish, Catalan).
The files store parameters only, never data. The format is in
`inst/library/schema.md`. Effect sizes reproduce the source's main estimates
as teaching values, not meta-analytic estimates. The
[scenario catalogue](https://victor-moreno.github.io/jamovi-studysim/scenarios.html)
explains the method and filters the scenarios by domain, design and topic.

| domain | scenario | designs | parameters from |
|---|---|---|---|
| Cardiovascular | Antihypertensive drug vs placebo (`antihypertensive_trial`) | rct, crossover, cluster_rct, factorial, longitudinal, pre_post, method_comparison | published studies |
|  | Aspirin and beta-carotene in male physicians (`physicians_health`) | factorial, rct | published studies |
|  | Blood pressure in US adults (`nhanes_blood_pressure`) | cross_sectional, case_control, method_comparison | NHANESraw (R package NHANES) |
|  | Coronary heart disease incidence (`chd_incidence`) | cohort, case_control, matched_cc, nested_cc, case_cohort, ecological | published studies |
|  | High-sensitivity troponin for myocardial infarction (`troponin_mi`) | diagnostic, cross_sectional | published studies |
|  | Intensive blood-pressure control (SPRINT) (`sprint_blood_pressure`) | rct | published studies |
|  | Streptokinase and aspirin after myocardial infarction (`isis2`) | factorial, rct | published studies |
| Child Health | Vitamin A and child mortality (`vitamin_a_aceh`) | cluster_rct, rct | published studies |
| Environmental | Succimer for lead-exposed children (`lead_succimer`) | longitudinal, rct, method_comparison | published studies |
|  | Water hardness and mortality (`water_hardness`) | ecological, cohort | R dataset water (HSAUR3 package) |
| Gastroenterology | Rectal indomethacin to prevent post-ERCP pancreatitis (`indomethacin_ercp`) | rct, cluster_rct | indo_rct (R package medicaldata) |
| Haematology | Progression of MGUS (`mgus_progression`) | prognosis, cohort | R dataset mgus2 (survival package) |
|  | Serum free light chains and mortality (`free_light_chain`) | cohort, case_cohort, nested_cc | R dataset flchain (survival package) |
| Hepatology | Primary biliary cholangitis (`pbc_mayo`) | prognosis, rct | R dataset pbc (survival package) |
| Infectious | Dexamethasone in hospitalised COVID-19 (`recovery_dexamethasone`) | rct | published studies |
|  | Handwashing and childhood pneumonia (`handwashing_karachi`) | cluster_rct, rct | published studies |
|  | Salk polio vaccine field trial (`polio_vaccine`) | rct, cluster_rct | published studies |
|  | Streptomycin for pulmonary tuberculosis (`streptomycin_tb`) | rct | strep_tb (R package medicaldata) |
| Mental Health | Computerised CBT for depression (`beat_the_blues`) | longitudinal, rct, pre_post | R dataset BtheB (HSAUR3 package) |
| Metabolic | Diabetes in Pima women (`pima_diabetes`) | cross_sectional, diagnostic, case_control | R datasets Pima.tr and Pima.te (MASS package) |
|  | Metformin vs placebo on HbA1c (`metformin_hba1c`) | rct, cluster_rct, longitudinal, pre_post, method_comparison | published studies |
| Musculoskeletal | Fracture in older women (`glow_fracture`) | cohort, case_control | GLOW500 (R package aplore3) |
| Neurology | Biomarkers after subarachnoid haemorrhage (`subarachnoid_haemorrhage`) | diagnostic, cohort | R dataset aSAH (pROC package) |
|  | Progabide for epilepsy (`epilepsy_progabide`) | rct | R dataset epil (MASS package) |
| Odontology | Fluoride varnish and dental caries (`caries_fluoride_varnish`) | cluster_rct, rct, cohort | published studies |
|  | Periodontal treatment in pregnancy (`periodontal_pregnancy`) | rct, longitudinal | opt (R package medicaldata) |
|  | Periodontitis in adults: smoking and diabetes (`periodontitis_smoking`) | cross_sectional, case_control | published studies |
| Oncology | Adjuvant chemotherapy for colon cancer (`colon_adjuvant`) | rct, prognosis | R dataset colon (survival package) |
|  | Breast cancer prognosis (`breast_cancer_prognosis`) | prognosis, cohort | published studies |
|  | Breast cancer risk factors (`breast_cancer_risk`) | case_control, matched_cc, cohort, nested_cc, case_cohort, ecological | published studies |
|  | Breast fine-needle aspirates (`breast_fna`) | diagnostic | R dataset biopsy (MASS package) |
|  | Chemotherapy for advanced lung cancer (`veterans_lung`) | rct, prognosis | R dataset veteran (survival package) |
|  | Dietary fat and breast cancer (`dietary_fat_breast`) | ecological, case_control, cohort | published studies |
|  | Hormonal therapy in node-positive breast cancer (`gbsg2_breast`) | rct, prognosis | R dataset GBSG2 (TH.data package) |
|  | Lung cancer and smoking (`lung_cancer_smoking`) | case_control, matched_cc, cohort, nested_cc | Doll & Hill 1950 smoking tables |
|  | Oesophageal cancer: alcohol and tobacco (`esoph_cancer`) | case_control, matched_cc, cohort | R dataset esoph (datasets package) |
|  | PSA and prostate cancer (`prostate_cancer_psa`) | diagnostic, cross_sectional, case_control | published studies |
|  | Relapse of Wilms tumour (`wilms_tumour`) | cohort, case_cohort, nested_cc, case_control | R dataset nwtco (survival package) |
|  | Survival in advanced lung cancer (`lung_ncctg`) | prognosis | R dataset lung (survival package) |
| Ophthalmology | Glaucoma by laser scanning tomography (`glaucoma_hrt`) | diagnostic | R dataset GlaucomaM (TH.data package) |
|  | Myopia in children (`myopia`) | cohort, case_control | myopia (R package aplore3) |
| Perinatal | Low birth weight (`low_birth_weight`) | cross_sectional, case_control, cohort | R dataset birthwt (MASS package) |
| Pharmacology | Bioequivalence of two formulations (`bioequivalence`) | crossover, rct | published studies |
|  | Two hypnotics compared (`hypnotics_sleep`) | crossover, rct | R dataset sleep (datasets package) |
| Podiatry | Ankle- and toe-brachial index for peripheral arterial disease (`pad_ankle_brachial`) | diagnostic, cross_sectional | published studies |
|  | Foot orthoses for plantar fasciitis (`plantar_fasciitis_orthoses`) | rct, pre_post | published studies |
|  | Foot ulceration in diabetes (`diabetic_foot_ulcer`) | cohort, prognosis, case_control, nested_cc | published studies |
|  | Terbinafine vs itraconazole for toenail onychomycosis (`onychomycosis_terbinafine`) | rct | published studies |
| Reproductive | Secondary infertility after abortion (`infertility_abortion`) | matched_cc, case_control, cross_sectional | R dataset infert (datasets package) |
| Respiratory | Formoterol vs salbutamol in asthma (`asthma_formoterol`) | crossover, rct | published studies |

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
