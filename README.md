# Bariatric Surgery Outcomes in Adolescents

*Long-term nutritional and anthropometric outcomes after adolescent bariatric surgery*

## About this repository
This repository contains the R analysis workflow for a retrospective cohort study of adolescents with severe obesity. The study compares:

- **Surgery group:** adolescents who underwent bariatric surgery before age 18.
- **Control group:** adolescents with severe obesity managed with structured lifestyle modification.

The analysis focuses on trajectories over a **5-year follow-up**, including anthropometric outcomes and laboratory biomarkers.

## Publication
This study has been published:

- **Obesity Surgery (2026)**  
  https://doi.org/10.1007/s11695-026-08521-8

## My role
I am the **analyst** for this study and repository.

## Study outcomes covered in the code
### Anthropometric outcomes
- Weight
- Body Mass Index (BMI)
- BMI Standard Deviation Score (BMI-SDS)
- Additional SDS measures for growth-related interpretation

### Nutritional / laboratory outcomes
- Hemoglobin
- Thyroid-stimulating hormone (TSH)
- Vitamin D
- Folic acid
- Vitamin B12

## Repository structure
- `00-func.R`  
  Utility/helper functions used across the project, including:
  - continuous-variable diagnostics (`check_cont`)
  - duplicate-measurement handling (`find_best_measurement`)
  - height correction/imputation (`fix_height`)
  - variable labeling helpers (`label_get`, `vars_label`, `var_get`)
  - model coefficient extraction (`extract_coefficients`)
  - mixed-model visualization (`visualize_lme_model`)

- `01-import_data.R`  
  Data import and initial cleaning:
  - standardization of column names
  - date parsing
  - recoding of key variables
  - preparation of derived laboratory differences

- `02-clean_data.R`  
  Data curation and transformation:
  - outlier filtering
  - duplicate resolution
  - BMI reshaping and derivation
  - SDS calculations
  - creation of analysis-ready long/wide datasets

- `03-analysis.R`  
  Statistical analysis and reporting outputs:
  - baseline descriptive summaries
  - distribution checks and diagnostics
  - longitudinal plotting
  - linear mixed-effects modeling
  - export of results tables/figures

## Statistical approach
- **Descriptive statistics** for baseline comparison and cohort characterization.
- **Longitudinal mixed-effects models** to account for repeated measurements within participants.
- **Interaction terms** (e.g., time × group) to evaluate differential trends between surgery and control groups.
- **Segmented follow-up windows** (e.g., 0–2 years and 2–5 years) where clinically/statistically appropriate.
- **Visualization-first reporting** to pair model-based trends with observed trajectories.

## Requirements
- **R:** 4.4.2 or newer
- **Core packages:**
  - `tidyverse`
  - `ggpubr`
  - `zoo`
  - `rstatix`
  - `lme4`
  - `sjPlot`
  - `ggeffects`
  - `patchwork`
  - `broom.mixed`
  - plus supporting packages such as `clipr`, `lubridate`, `janitor`, and `childsds`

## Usage notes
Because the underlying data include sensitive clinical information, raw data are not distributed with this repository.

You can use this project to:
- understand the end-to-end clinical analytics pipeline (import → clean → model → visualize),
- adapt the methods for similarly structured datasets,
- reproduce the analytical strategy (not the original patient-level results).

## Data privacy and reproducibility
- **Data sharing:** Not possible due to privacy and governance constraints.
- **Code sharing:** Full analysis logic is provided for transparency and methodological reuse.
- **Reproducibility scope:** Conceptual/methodological reproducibility, not direct numerical replication without source data.

## Contact
For scientific or analytic questions related to this repository, please contact:  
**Dor Atias** — [atias_dor@mac.org.il](mailto:atias_dor@mac.org.il)

---
Maintained by the Bariatric Surgery Outcomes Research Group.
