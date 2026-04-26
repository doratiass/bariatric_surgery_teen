# Bariatric Surgery Outcomes in Adolescents: Longitudinal Clinical Data Pipeline

## Project overview
This repository appears to implement a longitudinal epidemiologic analysis pipeline for adolescents with severe obesity, comparing participants who underwent bariatric surgery with matched/eligible controls managed without surgery. The code defines an end-to-end workflow for importing clinical data, cleaning repeated anthropometric and laboratory measurements, constructing longitudinal analysis tables, and fitting mixed-effects models.

The analytical focus in the current scripts is on 5-year follow-up trajectories for anthropometric and nutritional/laboratory outcomes.

## Scientific background (inferred from code)
Based on the scripts, the analysis pipeline is designed to evaluate post-index changes in:
- Anthropometrics (height, weight, BMI, SDS-derived growth measures)
- Laboratory indicators (e.g., hemoglobin, folic acid, vitamin B12, vitamin D, TSH)

The code structure suggests a retrospective cohort design with repeated measurements per participant and longitudinal modeling to compare temporal trajectories between groups.

## Researcher / analyst role
This repository is documented from the perspective of the **data scientist / analyst** responsible for:
- Data ingestion and transformation
- Analytical dataset construction
- Statistical model implementation
- Computational reproducibility and reporting outputs

This role description does **not** imply Principal Investigator (PI) status.

## Repository structure
- `00 - funcs.R`  
  Shared helper functions for diagnostics, duplicate resolution, variable labeling, model coefficient extraction, and mixed-model visualization.
- `01 - import_data.R`  
  Data import and initial harmonization (names, dates, numeric conversion, recoding).
- `02 - clean_data.R`  
  Longitudinal BMI/lab cleaning, duplicate handling, SDS derivation, and construction of final long/wide analytical datasets.
- `03 - analysis.R`  
  Descriptive summaries, longitudinal plots, and mixed-effects model fitting/export.
- `04 - sex diff.R`  
  Sex-stratified mixed-model workflow and reporting exports.
- `data/` (expected, not versioned here)  
  Input CSV files and saved `.RData` objects.
- `export/`, `export_sex/` (generated)  
  Tables and figures exported by scripts.

## Data description (inferred from variable usage)
The pipeline appears to use variables from multiple domains:
- **Demographics / social**: age, sex, sector, socioeconomic status, periphery
- **Clinical comorbidities / treatment context**: diabetes, pre-diabetes, hypertension, hyperlipidemia, antidepressant exposure, repeat surgery indicator
- **Anthropometrics**: repeated height/weight/BMI and SDS/percentile derivations
- **Laboratory follow-up**: hemoglobin, folic acid, vitamin B12, vitamin D, TSH (baseline and yearly summaries)
- **Computational/modeling fields**: index year/date, follow-up year, time-from-index, grouped long-format test/value structure

## Outcome definitions implemented in code
In the current repository state, outcomes are longitudinal anthropometric and lab measurements over follow-up years. The code defines group comparison as `Case` vs `Control` and models repeated outcomes over time.

> Note: A centenarian vs non-centenarian classification is **not present** in the current scripts.

## High-level analytical workflow
1. **Data loading and cleaning** (`01 - import_data.R`)  
   Load raw CSVs, standardize names, parse dates, recode key variables.
2. **Feature preparation / preprocessing** (`02 - clean_data.R`)  
   Remove implausible outliers, reconcile duplicate measurements, reshape to analysis structures.
3. **Missing data handling** (`00 - funcs.R`, `02 - clean_data.R`)  
   Explicit NA filtering and height trajectory correction/imputation rules.
4. **Train/test split and cross-validation**  
   Not implemented in current scripts (workflow is longitudinal mixed-effects rather than ML train/test evaluation).
5. **Logistic regression**  
   Not implemented in current scripts.
6. **LASSO logistic regression**  
   Not implemented in current scripts.
7. **XGBoost classification**  
   Not implemented in current scripts.
8. **Model evaluation** (`03 - analysis.R`, `04 - sex diff.R`)  
   Descriptive tables, p-values, coefficient summaries, and visual diagnostics of model predictions.
9. **Saving model objects/results**  
   Save final datasets to `data/final_data.RData`; export tables/figures to `export/` and `export_sex/`.

## Packages used (grouped by purpose)
- **Data wrangling / utilities**: `tidyverse`, `janitor`, `lubridate`, `zoo`
- **Descriptive statistics / tables**: `gtsummary`, `gt`, `flextable`, `rstatix`
- **Modeling**: `lme4`, `lmerTest`, `rms`
- **Visualization / model effects**: `ggpubr`, `sjPlot`, `ggeffects`, `patchwork`
- **Specialized growth references**: `childsds`
- **Model tidying support**: `broom.mixed`

## How to run scripts (intended order)
Run from repository root in this order:
1. `source("01 - import_data.R")`
2. `source("02 - clean_data.R")`
3. `source("03 - analysis.R")`
4. `source("04 - sex diff.R")` (optional, sex-focused outputs)

## Expected inputs and outputs
### Inputs
- `data/data.csv`
- `data/bmi_data.csv`

### Outputs
- `data/final_data.RData`
- Tables: `export/*.docx`, `export_sex/*.docx`
- Figures: `export/*.jpeg`, `export_sex/*.pdf`

## Reproducibility notes
- The workflow depends on local file paths under `data/` and generated folders (`export`, `export_sex`).
- Intermediate and final objects are persisted via `.RData` files.
- Some analyses rely on deterministic transformations; explicit random seeds are not central in the current mixed-model scripts.
- If run on a new system, verify:
  - package versions,
  - locale/date parsing behavior,
  - and existence of expected directories before export.

## Limitations
- Raw patient-level data are not included for privacy reasons.
- Full numeric reproducibility requires access to the original source datasets.
- Several modeling/data-cleaning rules are domain-specific and should be interpreted with clinical oversight.
- Some variable mappings and thresholds are hard-coded and may require validation when adapting to new cohorts.

## Potential issues to review with PI/domain experts
- Clinical plausibility of outlier thresholds and imputation windows for anthropometric values.
- Whether all fitted models should be included in coefficient summary exports (e.g., folic acid is modeled but not included in one summary table in `03 - analysis.R`).
- Confirmation of baseline window definitions for laboratory values and subgroup assumptions.

## Contact / citation
If no formal citation guidance is available, add project-specific citation text here.

For repository questions, add preferred contact details (analyst/team) in this section.
