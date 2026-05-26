# ============================================================================ #
# Script: 02 - clean_data.R
# Description: Clean and transform BMI and laboratory follow-up data into
#              analysis-ready long and wide datasets for downstream modeling.
# Inputs: data.csv (loaded as df_raw), bmi_data.csv (loaded as bmi_raw),
#         utility functions in 00 - funcs.R.
# Outputs: data/final_data.RData (final_df_long, final_df_wide).
# Notes: Duplicate-handling methods ("closest" and "average") are both retained;
#        the main analysis dataset uses the "closest" method as in the
#        original workflow.
# ============================================================================ #

# Load required libraries and custom functions
library(tidyverse)
library(childsds)
source("00 - funcs.R")

# ============================================================================ #
# Process BMI Data: Remove Outliers and Impossible Values ---------------------#
# ============================================================================ #

## Remove outliers based on acceptable ranges for each measurement type.
## Reference links:
##   - https://www.cdc.gov/growthcharts/data/extended-bmi/BMI-Age-percentiles-GIRLS.pdf
##   - https://www.cdc.gov/growthcharts/data/set2/chart-04.pdf
##   - https://www.cdc.gov/growthcharts/data/set2clinical/cj41l071.pdf

# Plot boxplots to visualize outliers for measurements where observation_date is after index_date.
bmi_raw %>%
  filter(observation_date > index_date) %>%
  ggplot(aes(x = test, y = observation_result, color = test)) +
  geom_boxplot() +
  facet_wrap(. ~ test, scales = "free")

# Filter out outliers by setting acceptable value ranges for each test type.
bmi_data_outliers <- bmi_raw %>%
  mutate(
    observation_result = case_when(
      test == "BMI" & observation_result > 15 & observation_result < 200 ~
        observation_result,
      test == "height" & observation_result > 140 & observation_result < 220 ~
        observation_result,
      test == "weight" & observation_result > 30 & observation_result < 250 ~
        observation_result
    )
  ) %>%
  drop_na()

# Plot boxplots again for the outlier-removed data.
bmi_data_outliers %>%
  filter(observation_date > index_date) %>%
  ggplot(aes(x = test, y = observation_result, color = test)) +
  geom_boxplot() +
  facet_wrap(. ~ test, scales = "free")

# ============================================================================ #
# Remove Duplicate Measurements and Find Best Measurement ---------------------#
# ============================================================================ #

# Identify duplicate measurements based on fake_id, test, and observation_date.
bmi_data_outliers %>%
  group_by(fake_id, test, observation_date) %>%
  mutate(n = n(), .groups = "drop") %>%
  filter(n > 1L) %>%
  pull(fake_id) -> dup_id

# For duplicates, apply a custom function to find the best measurement.
bmi_data_dup_clean <- bmi_data_outliers %>%
  filter(fake_id %in% unique(dup_id)) %>%
  group_by(fake_id, index_date, test) %>%
  reframe(find_best_measurement(observation_date, observation_result))

# Also create a duplicate-cleaned dataset using the average method.
bmi_data_dup_clean_avg <- bmi_data_outliers %>%
  filter(fake_id %in% unique(dup_id)) %>%
  group_by(fake_id, index_date, test) %>%
  reframe(find_best_measurement(
    observation_date,
    observation_result,
    return_method = "average"
  ))

# ---------------------------------------------------------------------------- #
## Helper: Build cleaned BMI panel from a duplicate-resolution table ----------#
# ---------------------------------------------------------------------------- #
build_bmi_panel <- function(dup_clean_tbl) {
  bind_rows(
    bmi_data_outliers %>%
      filter(!(fake_id %in% dup_id)) %>%
      select(
        fake_id,
        index_date,
        dates = observation_date,
        test,
        values = observation_result
      ),
    dup_clean_tbl
  ) %>%
    filter(test != "BMI") %>% # Remove raw BMI values; BMI is recomputed
    group_by(fake_id, index_date, dates) %>%
    pivot_wider(names_from = test, values_from = values) %>%
    filter(!is.na(weight)) %>%
    arrange(fake_id, dates) %>%
    group_by(fake_id, index_date) %>%
    reframe(
      dates,
      height = fix_height(height, dates),
      weight = weight
    ) %>%
    mutate(
      BMI = weight / (height / 100)^2,
      time_from_index = as.numeric(difftime(dates, index_date, units = "days"))
    ) %>%
    drop_na()
}

# ============================================================================ #
# Clean and Reshape BMI Data --------------------------------------------------#
# ============================================================================ #

# Combine non-duplicate data with the cleaned duplicate measurements.
# Reshape the data from long to wide format, calculate BMI, and compute time differences.
bmi_data_clean <- build_bmi_panel(bmi_data_dup_clean) %>%
  left_join(df_raw %>% select(fake_id, sex, start_age = age)) %>%
  mutate(age = start_age + time_from_index / 365.25)

# Create an alternate version of the cleaned BMI data using the average method for duplicates.
bmi_data_clean_avg <- build_bmi_panel(bmi_data_dup_clean_avg)

# ============================================================================ #
# Summarize BMI Data Over Time ------------------------------------------------#
# ============================================================================ #

# Calculate standard deviation scores (SDS) for height, weight, and BMI.
# Summarize data by year (up to 5 years from index_date).
sum_bmi <- bmi_data_clean %>%
  filter(time_from_index > -366, time_from_index < 2191) %>%
  mutate(
    height_sds = sds(
      height,
      age = age,
      sex = sex,
      male = "Male",
      female = "Female",
      ref = cdc.ref,
      item = "height2_20",
      type = "SDS"
    ),
    weight_sds = sds(
      weight,
      age = age,
      sex = sex,
      male = "Male",
      female = "Female",
      ref = cdc.ref,
      item = "weight2_20",
      type = "SDS"
    ),
    bmi_sds = sds(
      BMI,
      age = age,
      sex = sex,
      male = "Male",
      female = "Female",
      ref = cdc.ref,
      item = "bmi",
      type = "SDS"
    ),
    bmi_perc = sds(
      BMI,
      age = age,
      sex = sex,
      male = "Male",
      female = "Female",
      ref = cdc.ref,
      item = "bmi",
      type = "perc"
    )
  ) %>%
  select(-c(age, start_age)) %>%
  mutate(year_from_index = ceiling(time_from_index / 365.25)) %>%
  group_by(fake_id, year = year_from_index) %>%
  summarise(
    last_test_date = max(dates, na.rm = TRUE),
    bmi = mean(BMI, na.rm = TRUE),
    weight = mean(weight, na.rm = TRUE),
    height = mean(height, na.rm = TRUE),
    bmi_sds = mean(bmi_sds, na.rm = TRUE),
    bmi_perc = mean(bmi_perc, na.rm = TRUE),
    weight_sds = mean(weight_sds, na.rm = TRUE),
    height_sds = mean(height_sds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(fake_id) %>%
  mutate(
    last_test_date = max(last_test_date, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  filter(year < 6) %>%
  pivot_longer(
    cols = c(bmi, weight, height, bmi_sds, bmi_perc, weight_sds, height_sds),
    names_to = "test",
    values_to = "value"
  )

# ============================================================================ #
# Process Labs Follow-Up Data -------------------------------------------------#
# ============================================================================ #

# Prepare lab follow-up data by reshaping pre-index lab data and merging with
# post-index average lab data. The pre-index lab data is filtered to a one-year
# window (0 to 365 days from index_date).

labs_raw <- right_join(
  df_raw %>%
    select(fake_id, index_date, last_sample_date, contains("before_index")) %>%
    select(-anti_depress_drugs_before_index) %>%
    pivot_longer(
      cols = -c(fake_id, index_date, last_sample_date), # Exclude fake_id and index_date from reshaping
      names_to = c("test_name", ".value"), # Split column names into test_name and .value
      names_sep = "_last_sample_" # Use the separator to split column names
    ) %>%
    mutate(diff = as.numeric(index_date - date_before_index)) %>%
    filter(
      !is.na(date_before_index),
      !is.na(result_before_index),
      diff >= 0,
      diff <= 365
    ) %>%
    select(
      fake_id,
      test_name,
      last_test_date = last_sample_date,
      result_before_index
    ) %>%
    pivot_wider(
      names_from = test_name,
      values_from = result_before_index,
      names_glue = "{test_name}_0"
    ) %>%
    select(fake_id, last_test_date, contains("_0")),
  df_raw %>%
    select(fake_id, contains("avg")) %>%
    select(-contains("_25")) %>%
    rename_with(
      ~ str_remove_all(., "avg_value_of_|_in_|_year_from_index") %>% # Remove unwanted parts
        gsub("1st", "_1", .) %>% # Convert "1st" to "1"
        gsub("2nd", "_2", .) %>% # Convert "2nd" to "2"
        gsub("3rd", "_3", .) %>% # Convert "3rd" to "3"
        gsub("4th", "_4", .) %>% # Convert "4th" to "4"
        gsub("5th", "_5", .), # Convert "5th" to "5"
      starts_with("avg_value_of_") # Apply only to relevant columns
    ),
  by = "fake_id"
)

# Reshape the labs data from wide to long format.
labs <- labs_raw %>%
  pivot_longer(
    cols = matches("_(0|1|2|3|4|5)$"), # Select columns ending with _0, _1, ..., _5
    names_to = c("test", "year"),
    names_pattern = "(.+)_([0-9]+)$", # Use regex to extract test name and year
    values_to = "value"
  ) %>%
  mutate(year = as.integer(year)) %>%
  drop_na(value)

# ============================================================================ #
# Create Final Datasets -------------------------------------------------------#
# ============================================================================ #

# Identify cohort IDs where baseline (year 0) BMI is at least 40.
cohort_ids <- sum_bmi %>%
  filter(year == 0, test == "bmi", value >= 40) %>%
  pull(fake_id)

# Create a final long-format dataset by merging demographic and clinical data with
# BMI summary and labs follow-up data.
final_df_long <- df_raw %>%
  select(
    index_year,
    index_date,
    fake_id,
    group,
    age,
    sex,
    ses,
    sector,
    periphery,
    dm2,
    pre_dm,
    htn,
    lipid,
    hyperlipidemia_5,
    anti_depress_drugs_before_index,
    anti_depress_drugs_after_index,
    repeated_surgery_after_index
    # Additional lab columns are commented out below
    # folic_acid_start = folic_acid_last_sample_result_before_index,
    # folic_acid_end = avg_value_of_folic_acid_in_5th_year_from_index,
    # folic_acid_diff,
    # hemoglobin_start = hemoglobin_last_sample_result_before_index,
    # hemoglobin_end = avg_value_of_hemoglobin_in_5th_year_from_index,
    # hemoglobin_diff,
    # tsh_start = tsh_last_sample_result_before_index,
    # tsh_end = avg_value_of_tsh_in_5th_year_from_index, tsh_diff,
    # vitamin_b12_start = vitamin_b12_last_sample_result_before_index,
    # vitamin_b12_end = avg_value_of_vitamin_b12_in_5th_year_from_index,
    # vitamin_b12_diff,
    # vitamin_d_start = vitamin_d_last_sample_result_before_index,
    # vitamin_d_end = avg_value_of_vitamin_d_in_5th_year_from_index,
    # vitamin_d_diff,
  ) %>%
  right_join(
    bind_rows(sum_bmi, labs),
    by = "fake_id"
  ) %>%
  filter(fake_id %in% cohort_ids) %>%
  mutate(
    follow_up_time = as.numeric(
      difftime(last_test_date, index_date, units = "days") / 365.25
    )
  ) %>%
  select(-last_test_date, -index_date) %>%
  group_by(fake_id) %>%
  mutate(
    follow_up_time = max(follow_up_time, na.rm = TRUE),
    year = abs(year)
  ) %>%
  ungroup()

# Convert the long-format dataset into a wide-format dataset.
final_df_wide <- final_df_long %>%
  pivot_wider(
    names_from = c(test, year),
    values_from = value
  )

save(
  bmi_data_clean,
  final_df_long,
  final_df_wide,
  file = "data/final_data.RData"
)
