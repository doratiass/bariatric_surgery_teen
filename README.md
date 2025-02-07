# Bariatric Surgery Outcomes in Adolescents

*Exploring Long-Term Nutritional and Anthropometric Changes After Surgery*

------------------------------------------------------------------------

## Overview

This repository contains R scripts and functions developed as part of a retrospective cohort study examining the long-term outcomes of bariatric surgeries on adolescents. The study compares adolescents who underwent bariatric surgery before the age of 18 to a control group of similarly obese individuals who participated in lifestyle modification programs. Our primary focus is on nutritional and anthropometric measures over a 5-year follow-up period, including weight, BMI (and BMI Standard Deviation Scores [SDS]), and laboratory values (hemoglobin, TSH, vitamin D, folic acid, and vitamin B12).

> **Note:** Due to strict privacy regulations, the underlying data are not shared. The provided code is intended for educational purposes and to illustrate the statistical methods applied in the study—not for direct replication or execution on your own data.

------------------------------------------------------------------------

## Background

Obesity in adolescence is a major public health issue with profound long-term implications for both physical and mental health. The increasing global prevalence of obesity has prompted the medical and public health communities to search for effective treatment strategies. Bariatric surgery has emerged as a fast-acting intervention for severe obesity, particularly in cases where traditional lifestyle modifications have failed. However, surgery is not without risk—the long-term nutritional consequences (e.g., vitamin deficiencies) and potential need for revision surgeries underscore the importance of thorough follow-up and rigorous statistical evaluation.

In our study, we aimed to:

-   **Assess the impact of bariatric surgery** on weight loss and BMI reduction over a 5-year period.
-   **Examine changes in nutritional indices** such as hemoglobin, TSH, vitamin D, folic acid, and vitamin B12.
-   **Compare these outcomes** to a control group of adolescents with severe obesity who underwent lifestyle modifications.

------------------------------------------------------------------------

## Repository Contents

-   **00-func.R**\
    Contains utility functions for data analysis and model visualization. Functions include:
    -   `check_cont`: Generates plots and summary tables (density, QQ, box plots, and normality tests) for continuous variables (with optional grouping).
    -   `find_best_measurement`: Selects the best measurement for each unique date using either a “closest” or “average” method.
    -   `fix_height`: Corrects and imputes height measurements over time.
    -   Labeling functions (`label_get`, `vars_label`, `var_get`) using a dictionary of variable names.
    -   `extract_coefficients`: Extracts and formats coefficients from linear model summaries.
    -   `visualize_lme_model`: Visualizes linear mixed-effects model predictions and marginal effects by generating interaction plots and combining multiple effect plots.
-   **01-import_data.R**\
    Loads and cleans the main dataset (`df_raw`) and a BMI dataset (`bmi_raw`).
    -   The script cleans column names, converts date columns using `lubridate::dmy`, recodes categorical variables, and calculates difference variables for laboratory test results.
-   **02-clean_data.R**\
    Processes BMI data and laboratory follow-up data by:
    -   Removing outliers based on acceptable ranges.
    -   Identifying and cleaning duplicate measurements (using `find_best_measurement`).
    -   Reshaping BMI data from long to wide format.
    -   Calculating BMI, Standard Deviation Scores (SDS) for BMI, weight, and height.
    -   Merging the laboratory data with the BMI data to create the final long and wide datasets.
-   **03-analysis.R**\
    Performs the main statistical analyses and generates visualizations:
    -   Defines variables and checks distributions (using `check_cont`).
    -   Creates descriptive statistics tables (using `gtsummary`) for baseline demographics and continuous variables.
    -   Generates plots for BMI and laboratory measurements over time.
    -   Fits multiple linear mixed-effects models (LMMs) to evaluate trends over time, including interaction terms to compare case and control groups.
    -   Exports the model coefficients summary and visualizations as DOCX and JPEG files.

------------------------------------------------------------------------

## Statistical Methods Emphasized in the Code

-   **Descriptive Statistics:**
    -   Summaries for continuous variables (mean, SD, median, IQR) and categorical variables (counts and percentages).
    -   Use of non-parametric tests (e.g., Wilcoxon rank sum tests) and parametric tests (e.g., Welch Two Sample t-tests) to assess differences between groups.
-   **Longitudinal Analysis:**
    -   **Linear Mixed-Effects Models:**\
        These models accommodate repeated measurements per subject and allow for the analysis of time trends and group differences.
        -   Interaction terms (e.g., `year * group`) are included to assess whether the temporal trend differs between groups.
        -   Separate analyses were performed for different follow-up periods (0–2 years and 2–5 years) when necessary.
-   **Standard Deviation Scores (SDS):**
    -   Calculation of SDS values for anthropometric measures (BMI, weight, and height) to account for growth variability in adolescents.
-   **Visualization Techniques:**
    -   Custom plotting functions are used to generate publication-quality plots (using `ggpubr`, `sjPlot`, `ggeffects`, and `patchwork`) that display both individual trajectories and group-level trends.
    -   Diagnostic plots (density, QQ, box plots) help in evaluating the distribution and normality of continuous variables.

------------------------------------------------------------------------

## Requirements

-   **R Version:** 4.4.2 or later
-   **Key R Packages:**
    -   `tidyverse`
    -   `ggpubr`
    -   `zoo`
    -   `rstatix`
    -   `lme4`
    -   `sjPlot`
    -   `ggeffects`
    -   `patchwork`
    -   `broom.mixed`
    -   Additional packages such as `clipr`, `lubridate`, `janitor`, and `childsds` are used in specific scripts.

Ensure that these packages are installed before running any of the scripts.

------------------------------------------------------------------------

## Usage

Since the data are sensitive and cannot be shared, the code in this repository is intended for educational and illustrative purposes only. You can review the scripts to understand:

-   How clinical data are imported, cleaned, and transformed.
-   The methods used to remove outliers and duplicate measurements.
-   The calculation of BMI and SDS values.
-   The application of linear mixed-effects models to analyze longitudinal data.
-   The generation of visualizations to represent individual and group trends over time.

If you wish to adapt the code for your own dataset, please ensure your data align with the structure and variable names assumed in these scripts.

------------------------------------------------------------------------

## Disclaimer

**Important:**\
- **Data Privacy:** The underlying data cannot be shared due to strict privacy regulations.\
- **Purpose:** The code is provided for educational purposes to demonstrate the statistical methods and data processing techniques used in our study.\
- The scripts are not intended for cloning and running without modification to suit your own data.

------------------------------------------------------------------------

## Contact

For further information or questions regarding the code or study methods, please contact the project team: [Dor Atias](mailto:atias_dor@mac.org.il).

------------------------------------------------------------------------

*This repository is maintained by the Bariatric Surgery Outcomes Research Group.*\
*© [Year] [Your Institution]. All rights reserved.*
