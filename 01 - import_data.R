# ============================================================================ #
# Script: 01 - import_data.R
# Description: Import source data files and perform initial harmonization of
#              dates, numerics, and key demographic/clinical variables.
# Inputs: data/data.csv, data/bmi_data.csv.
# Outputs: In-memory cleaned objects: df_raw and bmi_raw.
# Notes: Variable recoding follows the original project definitions and keeps
#        downstream object names unchanged.
# ============================================================================ #

# Load required libraries
library(tidyverse)
library(lubridate) # for date conversion functions

# ---------------------------------------------------------------------------- #
# Load and Clean Main Dataset (df_raw) ---------------------------------------
# ---------------------------------------------------------------------------- #

df_raw <- read_csv("data/data.csv") %>%
  # Clean column names to snake_case
  janitor::clean_names() %>%

  # Convert all columns with "date" in their names to Date objects (day-month-year)
  mutate(across(contains("date"), dmy)) %>%

  # For columns containing "_result_", convert values to numeric and replace
  # non-positive values with NA
  mutate(across(
    contains("_result_"),
    ~ ifelse(as.numeric(.x) > 0, as.numeric(.x), NA)
  )) %>%

  # Convert specified binary columns to logical (TRUE when value equals 1)
  mutate(across(
    c(
      "repeated_surgery_after_index",
      "dm2",
      "hyperlipidemia_5",
      "pre_dm",
      "htn",
      "lipid",
      "anti_depress_drugs_before_index",
      "anti_depress_drugs_after_index"
    ),
    ~ .x == 1
  )) %>%

  # Recode and convert variables to appropriate types
  mutate(
    # Ensure age is numeric
    age = as.numeric(age),

    # Recode group from Hebrew to English labels and convert to factor
    group = factor(case_when(
      group == "ניסוי" ~ "Case",
      group == "בקרה" ~ "Control"
    )),

    # Recode sex from Hebrew abbreviations to English and convert to factor
    sex = factor(case_when(
      sex == "ז" ~ "Male",
      sex == "נ" ~ "Female"
    )),

    # Recode sector from Hebrew to English labels and set factor levels
    sector = factor(
      case_when(
        sector == "ללא" ~ "General",
        sector == "רוסי" ~ "General",
        sector == "ערבי" ~ "Arab",
        sector == "חרדי" ~ "Ultra-orthodox",
        sector == "דתי לאומי" ~ "General"
      ),
      levels = c("General", "Arab", "Ultra-orthodox")
    ),

    # Recode socio-economic status (ses) to categories based on numeric value
    ses = factor(
      case_when(
        ses <= 4 ~ "Low",
        ses <= 7 ~ "Medium",
        TRUE ~ "High"
      ),
      levels = c("Low", "Medium", "High")
    ),

    # Convert periphery indicator from Hebrew ("כן") to logical TRUE/FALSE
    periphery = ifelse(periphery == "כן", TRUE, FALSE)
  ) %>%

  # Extract the year from the index_date column and insert it immediately after index_date
  mutate(index_year = year(index_date), .after = index_date) %>%

  # Calculate differences for lab test results (5th year average - last pre-index value)
  mutate(
    folic_acid_diff = ifelse(
      is.na(folic_acid_last_sample_result_before_index) |
        is.na(avg_value_of_folic_acid_in_5th_year_from_index),
      NA,
      avg_value_of_folic_acid_in_5th_year_from_index -
        folic_acid_last_sample_result_before_index
    ),
    .after = folic_acid_last_sample_result_before_index
  ) %>%
  mutate(
    hemoglobin_diff = ifelse(
      is.na(hemoglobin_last_sample_result_before_index) |
        is.na(avg_value_of_hemoglobin_in_5th_year_from_index),
      NA,
      avg_value_of_hemoglobin_in_5th_year_from_index -
        hemoglobin_last_sample_result_before_index
    ),
    .after = hemoglobin_last_sample_result_before_index
  ) %>%
  mutate(
    tsh_diff = ifelse(
      is.na(tsh_last_sample_result_before_index) |
        is.na(avg_value_of_tsh_in_5th_year_from_index),
      NA,
      avg_value_of_tsh_in_5th_year_from_index -
        tsh_last_sample_result_before_index
    ),
    .after = tsh_last_sample_result_before_index
  ) %>%
  mutate(
    vitamin_b12_diff = ifelse(
      is.na(vitamin_b12_last_sample_result_before_index) |
        is.na(avg_value_of_vitamin_b12_in_5th_year_from_index),
      NA,
      avg_value_of_vitamin_b12_in_5th_year_from_index -
        vitamin_b12_last_sample_result_before_index
    ),
    .after = vitamin_b12_last_sample_result_before_index
  ) %>%
  mutate(
    vitamin_d_diff = ifelse(
      is.na(vitamin_d_last_sample_result_before_index) |
        is.na(avg_value_of_vitamin_d_in_5th_year_from_index),
      NA,
      avg_value_of_vitamin_d_in_5th_year_from_index -
        vitamin_d_last_sample_result_before_index
    ),
    .after = vitamin_d_last_sample_result_before_index
  ) %>%
  mutate(
    last_sample_date = pmax(
      folic_acid_last_sample_date_before_index,
      hemoglobin_last_sample_date_before_index,
      tsh_last_sample_date_before_index,
      vitamin_b12_last_sample_date_before_index,
      vitamin_d_last_sample_date_before_index,
      na.rm = TRUE
    ),
    .after = index_year
  )

# ---------------------------------------------------------------------------- #
# Load and Clean BMI Dataset (bmi_raw) ---------------------------------------
# ---------------------------------------------------------------------------- #

bmi_raw <- read_csv("data/bmi_data.csv") %>%
  # Clean column names to snake_case
  janitor::clean_names() %>%

  # Convert any column with "date" in its name to Date objects
  mutate(across(contains("date"), dmy)) %>%

  # Recode test variable from Hebrew labels to English and ensure observation_result is numeric
  mutate(
    test = factor(case_when(
      test == "גובה" ~ "height",
      test == "משקל" ~ "weight",
      TRUE ~ test
    )),
    observation_result = as.numeric(observation_result)
  ) %>%

  # Remove rows with any missing values
  drop_na()
