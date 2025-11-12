# ---------------------------------------------------------------------------- #
# Script: Descriptive Statistics, Visualizations, and Predictive Models
# Description: This script generates descriptive statistics tables using gtsummary,
#              visualizes BMI and laboratory follow-up data over time, and fits
#              multiple linear mixed-effects models. The results are exported as
#              DOCX and JPEG files.
# ---------------------------------------------------------------------------- #

# Load required libraries and custom functions
library(tidyverse)
library(gtsummary)
library(flextable)
library(lmerTest)
library(performance)
source("00 - funcs.R")
load("data/final_data.RData")
# ---------------------------------------------------------------------------- #
# Variable Definitions -------------------------------------------------------
# ---------------------------------------------------------------------------- #

# Identify numeric columns in the final wide-format dataframe
num_cols <- colnames(final_df_wide)[sapply(final_df_wide, is.numeric)]

# Check the distribution of a continuous variable by group using a custom function
check_cont(final_df_wide, "folic_acid_0", "group")

# Define normally distributed variables and determine non-normal variables from numeric columns
norm_vars <- c("weight_0", "height_0", "hemoglobin_0", "vitamin_d_0")
non_norm_vars <- num_cols[!(num_cols %in% c(norm_vars, "fake_id"))]

# Specify variables to be included in Table 1 (baseline demographics and clinical characteristics)
tbl_1_vars <- c(
  "index_year",
  "age",
  "sex",
  "ses",
  "sector",
  "periphery",
  "dm2",
  "pre_dm",
  "htn",
  "hyperlipidemia_5"
)

# ---------------------------------------------------------------------------- #
# Descriptive Statistics -----------------------------------------------------
# ---------------------------------------------------------------------------- #
## Table 1: Baseline Characteristics by Group ---------------------------------
final_df_wide %>%
  select(group, all_of(tbl_1_vars)) %>%
  # Rename columns using a custom label-getting function
  rename_all(function(x) sapply(x, label_get, USE.NAMES = FALSE)) %>%
  tbl_summary(
    by = group,
    missing = "no",
    type = list(
      label_get("age") ~ 'continuous',
      all_continuous() ~ 'continuous'
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      c(label_get("index_year")) ~ "{median} ({p25}, {p75})"
    ),
    value = list(
      label_get("sex") ~ "Male"
    )
  ) %>%
  add_n(statistic = "{N_miss} ({p_miss})") %>%
  modify_header(n = "**Missing**") %>%
  # Optionally, add standardized mean differences (commented out)
  # add_difference(everything() ~ "smd") %>%
  # Optionally, hide confidence interval columns (commented out)
  # modify_column_hide(c(conf.low, conf.high)) %>%
  # modify_header(estimate = "**SMD**") %>%
  add_p(
    test = list(
      # Use Wilcoxon rank sum test for non-normal variables within tbl_1_vars
      all_of(vars_label(non_norm_vars[non_norm_vars %in% tbl_1_vars])) ~
        "wilcox.test"
    )
  ) -> tbl_1

# Map test names to more descriptive labels for Table 1 and display the mapping
tbl_1$table_body %>%
  select(variable, test_name) %>%
  distinct() %>%
  mutate(
    test_name = case_when(
      test_name == "t.test" ~ "Welch Two Sample t-test",
      test_name == "wilcox.test" ~ "Wilcoxon rank sum test",
      test_name == "wilcox.exact" ~ "Wilcoxon rank sum exact test",
      test_name == "chisq.test.no.correct" ~ "Pearson's Chi-squared test",
      test_name == "chisq.test" ~ "Pearson's Chi-squared test",
      test_name == "fisher.test" ~ "Fisher's exact test"
    )
  )

# Export Table 1 as a Word document
tbl_1 %>%
  as_flex_table() %>%
  save_as_docx(path = file.path("export", "tbl_1.docx"))

## Table 2: Additional Continuous Variables by Group --------------------------
final_df_wide %>%
  # Sort columns alphabetically, then remove baseline variables and unwanted columns
  select(sort(names(.))) %>%
  select(-c(all_of(tbl_1_vars), lipid, fake_id)) %>%
  rename_all(function(x) sapply(x, label_get, USE.NAMES = FALSE)) %>%
  tbl_summary(
    by = group,
    missing = "no",
    type = list(all_continuous() ~ 'continuous'),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    )
  ) %>%
  add_n(statistic = "{N_miss} ({p_miss})") %>%
  modify_header(n = "**Missing**") %>%
  # Optionally, add differences (commented out)
  # add_difference(everything() ~ "smd") %>%
  # Optionally, hide confidence interval columns (commented out)
  # modify_column_hide(c(conf.low, conf.high)) %>%
  # modify_header(estimate = "**SMD**") %>%
  add_p(
    test = list(
      # Apply t-test for normally distributed variables not in tbl_1_vars
      all_of(vars_label(norm_vars[!(norm_vars %in% tbl_1_vars)])) ~ "t.test",
      # Apply Wilcoxon rank sum test for non-normal variables not in tbl_1_vars
      all_of(vars_label(non_norm_vars[!(non_norm_vars %in% tbl_1_vars)])) ~
        "wilcox.test"
    )
  ) -> tbl_2

# Map test names to more descriptive labels for Table 2 and print the mapping
tbl_2$table_body %>%
  select(variable, test_name) %>%
  distinct() %>%
  mutate(
    test_name = case_when(
      test_name == "t.test" ~ "Welch Two Sample t-test",
      test_name == "wilcox.test" ~ "Wilcoxon rank sum test",
      test_name == "wilcox.exact" ~ "Wilcoxon rank sum exact test",
      test_name == "chisq.test.no.correct" ~ "Pearson's Chi-squared test",
      test_name == "chisq.test" ~ "Pearson's Chi-squared test",
      test_name == "fisher.test" ~ "Fisher's exact test"
    )
  ) %>%
  print(n = 70)

# Export Table 2 as a Word document
tbl_2 %>%
  as_flex_table() %>%
  save_as_docx(path = file.path("export", "tbl_2.docx"))

# ---------------------------------------------------------------------------- #
# Plots ----------------------------------------------------------------------
# ---------------------------------------------------------------------------- #

# Define color palette and custom theme for all plots
col_pal <- "Dark2"
plot_theme <- theme_minimal(base_size = 14) + # Base font size
  theme(
    panel.grid.major = element_line(
      size = 0.2,
      linetype = "dashed",
      color = "gray80"
    ),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 14, face = "bold"), # Larger facet labels
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )

## Visualize BMI Data Over Time ------------------------------------------------
bmi_plot_df <- final_df_long %>%
  filter(test %in% c("bmi_sds", "weight_sds", "bmi", "weight")) %>%
  mutate(
    test = factor(
      case_when(
        test == "bmi_sds" ~ "BMI (SDS)",
        test == "weight_sds" ~ "Weight (SDS)",
        test == "bmi" ~ "BMI (Kg/m²)",
        test == "weight" ~ "Weight (Kg)"
      ),
      levels = c("BMI (SDS)", "Weight (SDS)", "BMI (Kg/m²)", "Weight (Kg)")
    )
  )

# Create a plot showing the mean and 95% CI for BMI and weight, with individual trajectories
bmi_plot_df %>%
  group_by(year, group, test) %>%
  summarize(
    mean = mean(value, na.rm = TRUE),
    se = sd(value, na.rm = TRUE) / sqrt(n())
  ) %>%
  ggplot(aes(x = year, y = mean, color = group)) +
  geom_line() +
  geom_point() +
  geom_line(
    aes(x = year, y = value, color = group, group = fake_id),
    alpha = 0.1,
    data = bmi_plot_df
  ) +
  geom_errorbar(
    aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
    width = 0.2
  ) +
  scale_color_brewer(palette = col_pal) + # Use a colorblind-friendly palette
  labs(
    title = "Measurements over time",
    x = "Follow up Year",
    y = "Mean value",
    color = ""
  ) +
  facet_wrap(test ~ ., scales = "free") +
  plot_theme -> fig_bmi

# Save the BMI plot as a JPEG file
ggsave(
  file.path("export", "fig_bmi.jpeg"),
  fig_bmi,
  width = 30,
  height = 20,
  dpi = 300,
  background = "white",
  units = "cm"
)

## Visualize Laboratory Follow-Up Data -----------------------------------------
labs_plot_df <- final_df_long %>%
  filter(
    !(test %in%
      c(
        "bmi",
        "weight",
        "height",
        "folic_acid",
        "bmi_sds",
        "weight_sds",
        "height_sds"
      ))
  ) %>%
  mutate(
    test = factor(case_when(
      test == "hemoglobin" ~ "Hemoglobin",
      test == "tsh" ~ "TSH",
      test == "vitamin_b12" ~ "Vitamin B12",
      test == "vitamin_d" ~ "Vitamin D"
    ))
  ) %>%
  group_by(year, group, test) %>%
  summarize(
    mean = mean(value, na.rm = TRUE),
    se = sd(value, na.rm = TRUE) / sqrt(n())
  )

# Prepare p-value annotations from Table 2 for lab data plot
labs_plot_p <- tbl_2$table_body %>%
  filter(grepl("Hemoglobin|TSH|Vitamin|B12", variable)) %>%
  mutate(
    test = factor(case_when(
      grepl("Hemoglobin", variable) ~ "Hemoglobin",
      grepl("TSH", variable) ~ "TSH",
      grepl("B12", variable) ~ "Vitamin B12",
      grepl("D", variable) ~ "Vitamin D"
    )),
    year = case_when(
      grepl("before", variable) ~ 0,
      grepl("first", variable) ~ 1,
      grepl("second", variable) ~ 2,
      grepl("third", variable) ~ 3,
      grepl("fourth", variable) ~ 4,
      grepl("fifth", variable) ~ 5
    ),
    p_mark = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(test, year, p_val = p.value, p_mark)

# Annotate the lab plot with p-values at the highest mean value per group and test
labs_plot_annotated <- labs_plot_df %>%
  left_join(labs_plot_p, by = c("year", "test")) %>%
  group_by(year, test) %>%
  filter(mean == max(mean, na.rm = TRUE)) %>%
  distinct(year, test, .keep_all = TRUE) # Ensure only one row per year and test

# Create an enhanced lab data plot with error bars and p-value annotations
fig_labs <- labs_plot_df %>%
  ggplot(aes(x = year, y = mean, color = group)) +
  geom_line(size = 1.2) + # Thicker lines for visibility
  geom_point(size = 3) + # Larger points for clarity
  geom_errorbar(
    aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
    width = 0.2,
    alpha = 0.7
  ) + # Semi-transparent error bars
  geom_text(
    data = labs_plot_annotated,
    aes(label = p_mark, y = mean + 1.96 * se + 0.2),
    color = "black",
    size = 4,
    fontface = "bold"
  ) + # Bold annotations
  scale_color_brewer(palette = "Dark2") + # Use a colorblind-friendly palette
  labs(
    title = "Measurements Over Time",
    subtitle = "Mean ± 95% Confidence Interval",
    x = "Year",
    y = "Mean Value",
    color = "Group"
  ) +
  facet_wrap(test ~ ., scales = "free", ncol = 2) + # Adjust facet layout
  plot_theme

# Save the laboratory follow-up plot as a JPEG file
ggsave(
  file.path("export", "fig_labs.jpeg"),
  fig_labs,
  width = 30,
  height = 20,
  dpi = 300,
  background = "white",
  units = "cm"
)

# ---------------------------------------------------------------------------- #
# Predictive Models: Linear Mixed-Effects Models -----------------------------
# ---------------------------------------------------------------------------- #
## Height Model -------------------------------------------------------------
# Fit a linear mixed-effects model for height SDS
height_model <- lmer(
  value ~ year * group + sex + age + (1 | fake_id),
  data = final_df_long %>%
    mutate(group = relevel(group, "Control")) %>%
    filter(test == "height_sds")
)

# Extract coefficients for the height model
height_coefs <- extract_coefficients(height_model, "height_sds")

# Visualize the height model fit
visualize_lme_model(height_model, "Height SDS")

# Save the height model plot as a JPEG file
ggsave(
  file.path("export", "fig_lme_height.jpeg"),
  last_plot(),
  width = 30,
  height = 15,
  dpi = 300,
  background = "white",
  units = "cm"
)

## Weight Model (0-2 Years) -------------------------------------------------
# Fit a linear mixed-effects model for weight SDS for years <= 2
weight_0_2_model <- lmer(
  value ~ year * group + sex + age + (1 | fake_id),
  data = final_df_long %>%
    mutate(group = relevel(group, "Control")) %>%
    filter(test == "weight_sds", year <= 2)
)

# Extract coefficients for the weight 0-2 model
weight_0_2_coefs <- extract_coefficients(weight_0_2_model, "weight_sds")

# Visualize the weight 0-2 model fit
visualize_lme_model(weight_0_2_model, "Weight SDS 0-2", max_year = 2)

# Save the weight 0-2 model plot as a JPEG file
ggsave(
  file.path("export", "fig_lme_weight_0_2.jpeg"),
  last_plot(),
  width = 30,
  height = 15,
  dpi = 300,
  background = "white",
  units = "cm"
)

## Weight Model (2-5 Years) -------------------------------------------------
# Fit a linear mixed-effects model for weight SDS for years >= 2
weight_2_5_model <- lmer(
  value ~ year * group + sex + age + (1 | fake_id),
  data = final_df_long %>%
    mutate(group = relevel(group, "Control")) %>%
    filter(test == "weight_sds", year >= 2)
)

# Extract coefficients for the weight 2-5 model
weight_2_5_coefs <- extract_coefficients(weight_2_5_model, "weight_sds")

# Visualize the weight 2-5 model fit
visualize_lme_model(weight_2_5_model, "Weight SDS 2-5", min_year = 2)

# Save the weight 2-5 model plot as a JPEG file
ggsave(
  file.path("export", "fig_lme_weight_2_5.jpeg"),
  last_plot(),
  width = 30,
  height = 15,
  dpi = 300,
  background = "white",
  units = "cm"
)

## BMI Model (0-2 Years) ----------------------------------------------------
# Fit a linear mixed-effects model for BMI SDS for years <= 2
bmi_0_2_model <- lmer(
  value ~ year * group + sex + age + (1 | fake_id),
  data = final_df_long %>%
    mutate(group = relevel(group, "Control")) %>%
    filter(test == "bmi_sds", year <= 2)
)

# Extract coefficients for the BMI 0-2 model
bmi_0_2_coefs <- extract_coefficients(bmi_0_2_model, "bmi_sds")

# Visualize the BMI 0-2 model fit
visualize_lme_model(bmi_0_2_model, "BMI SDS", max_year = 2)

# Save the BMI 0-2 model plot as a JPEG file
ggsave(
  file.path("export", "fig_lme_bmi_0_2.jpeg"),
  last_plot(),
  width = 30,
  height = 15,
  dpi = 300,
  background = "white",
  units = "cm"
)

## BMI Model (2-5 Years) ----------------------------------------------------
# Fit a linear mixed-effects model for BMI SDS for years >= 2
bmi_2_5_model <- lmer(
  value ~ year * group + sex + age + (1 | fake_id),
  data = final_df_long %>%
    mutate(group = relevel(group, "Control")) %>%
    filter(test == "bmi_sds", year >= 2)
)

# Extract coefficients for the BMI 2-5 model
bmi_2_5_coefs <- extract_coefficients(bmi_2_5_model, "bmi_sds")

# Visualize the BMI 2-5 model fit
visualize_lme_model(bmi_2_5_model, "BMI SDS", min_year = 2)

# Save the BMI 2-5 model plot as a JPEG file
ggsave(
  file.path("export", "fig_lme_bmi_2_5.jpeg"),
  last_plot(),
  width = 30,
  height = 15,
  dpi = 300,
  background = "white",
  units = "cm"
)

## Hemoglobin Model ---------------------------------------------------------
# Fit a linear mixed-effects model for hemoglobin
hemoglobin_model <- lmer(
  value ~ year * group + sex + age + (1 | fake_id),
  data = final_df_long %>%
    mutate(group = relevel(group, "Control")) %>%
    filter(test == "hemoglobin")
)

# Extract coefficients for the hemoglobin model
hemoglobin_coefs <- extract_coefficients(hemoglobin_model, "hemoglobin")

# Visualize the hemoglobin model fit
visualize_lme_model(hemoglobin_model, "Hemoglobin")

# Save the hemoglobin model plot as a JPEG file
ggsave(
  file.path("export", "fig_lme_hem.jpeg"),
  last_plot(),
  width = 30,
  height = 15,
  dpi = 300,
  background = "white",
  units = "cm"
)

## TSH Model --------------------------------------------------------------
# Fit a linear mixed-effects model for TSH
tsh_model <- lmer(
  value ~ year * group + sex + age + (1 | fake_id),
  data = final_df_long %>%
    mutate(group = relevel(group, "Control")) %>%
    filter(test == "tsh")
)

# Extract coefficients for the TSH model
tsh_coefs <- extract_coefficients(tsh_model, "tsh")

# Visualize the TSH model fit
visualize_lme_model(tsh_model, "TSH")

# Save the TSH model plot as a JPEG file
ggsave(
  file.path("export", "fig_tsh.jpeg"),
  last_plot(),
  width = 30,
  height = 15,
  dpi = 300,
  background = "white",
  units = "cm"
)

## Vitamin B12 Model -------------------------------------------------------
# Fit a linear mixed-effects model for Vitamin B12
vitamin_b12_model <- lmer(
  value ~ year * group + sex + age + (1 | fake_id),
  data = final_df_long %>%
    mutate(group = relevel(group, "Control")) %>%
    filter(test == "vitamin_b12")
)

# Extract coefficients for the Vitamin B12 model
vitamin_b12_coefs <- extract_coefficients(vitamin_b12_model, "vitamin_b12")

# Visualize the Vitamin B12 model fit
visualize_lme_model(vitamin_b12_model, "Vitamin B12")

# Save the Vitamin B12 model plot as a JPEG file
ggsave(
  file.path("export", "fig_lme_b12.jpeg"),
  last_plot(),
  width = 30,
  height = 15,
  dpi = 300,
  background = "white",
  units = "cm"
)

## Folic Acid Model --------------------------------------------------------
# Fit a linear mixed-effects model for folic acid
folic_acid_model <- lmer(
  value ~ year * group + sex + age + (1 | fake_id),
  data = final_df_long %>%
    mutate(group = relevel(group, "Control")) %>%
    filter(test == "folic_acid")
)

# Extract coefficients for the folic acid model
folic_acid_coefs <- extract_coefficients(folic_acid_model, "folic_acid")

# Visualize the folic acid model fit
visualize_lme_model(folic_acid_model, "Folic Acid")

# Save the folic acid model plot as a JPEG file
ggsave(
  file.path("export", "fig_lme_folic.jpeg"),
  last_plot(),
  width = 30,
  height = 15,
  dpi = 300,
  background = "white",
  units = "cm"
)

## Vitamin D Model ---------------------------------------------------------
# Fit a linear mixed-effects model for Vitamin D
vitamin_d_model <- lmer(
  value ~ year * group + sex + age + (1 | fake_id),
  data = final_df_long %>%
    mutate(group = relevel(group, "Control")) %>%
    filter(test == "vitamin_d")
)

# Extract coefficients for the Vitamin D model
vitamin_d_coefs <- extract_coefficients(vitamin_d_model, "vitamin_d")

# Visualize the Vitamin D model fit
visualize_lme_model(vitamin_d_model, "Vitamin D")

# Save the Vitamin D model plot as a JPEG file
ggsave(
  file.path("export", "fig_lme_vit_d.jpeg"),
  last_plot(),
  width = 30,
  height = 15,
  dpi = 300,
  background = "white",
  units = "cm"
)

# ---------------------------------------------------------------------------- #
# Model Summary: Combine and Export Coefficients from All Models -------------
# ---------------------------------------------------------------------------- #

# List model names to summarize
model_names <- c(
  "height",
  "weight_0_2",
  "weight_2_5",
  "bmi_0_2",
  "bmi_2_5",
  "hemoglobin",
  "tsh",
  "vitamin_b12",
  "vitamin_d"
)

# Extract coefficients from each model into a list of tables
model_tables <- map(
  model_names,
  ~ extract_coefficients(get(paste0(., "_model")), .)
)

# Merge all model coefficient tables into one combined table
coef_table <- reduce(model_tables, left_join, by = "term") %>%
  as_tibble()

# Format the combined coefficient table using gt and add spanner labels
coef_table_gt <- coef_table %>%
  gt() %>%
  tab_header(
    title = "Summary of linear mixed models"
  ) %>%
  fmt_number(
    columns = paste0(model_names, "_est"),
    decimals = 2
  ) %>%
  cols_label(
    ends_with("est") ~ "Beta",
    ends_with("pval") ~ "p value"
  ) %>%
  tab_spanner(label = "Height", columns = starts_with("height")) %>%
  tab_spanner(label = "Weight 0-2", columns = starts_with("weight_0_2")) %>%
  tab_spanner(label = "Weight 2-5", columns = starts_with("weight_2_5")) %>%
  tab_spanner(label = "BMI 0-2", columns = starts_with("bmi_0_2")) %>%
  tab_spanner(label = "BMI 2-5", columns = starts_with("bmi_2_5")) %>%
  tab_spanner(label = "Hemoglobin", columns = starts_with("hemoglobin")) %>%
  tab_spanner(label = "TSH", columns = starts_with("tsh")) %>%
  tab_spanner(label = "Vitamin B12", columns = starts_with("vitamin_b12")) %>%
  tab_spanner(label = "Vitamin D", columns = starts_with("vitamin_d"))

# Save the coefficient summary table as a DOCX file
gtsave(
  coef_table_gt,
  file = file.path("export", "coef_table.docx"),
  to = "docx"
)

# Revision -------------------------------------------------------------------

new_pal <- c(
  "#0b5d44ff",
  "#E6AB02",
  #"#c76215ff",
  "#a00f0fff"
)

bmi_cat_df <- final_df_long %>%
  filter(test %in% c("bmi", "bmi_sds", "bmi_perc"), !is.na(value)) %>%
  select(fake_id, group, year, test, value) %>%
  pivot_wider(names_from = test, values_from = value) %>%
  mutate(
    bmi_cat = case_when(
      #bmi_sds < 0.05 ~ "Underweight",
      #bmi_sds < 0.85 ~ "Healthy weight",
      bmi_sds < 0.95 ~ "Healthy weight + Overweight",
      TRUE ~ "Obesity"
    ),
    # Optional refinement for Severe Obesity (if you have bmi_95th or bmi_perc etc.)
    bmi_cat = factor(
      if_else(
        bmi >= 35 | bmi_perc >= 0.95 & bmi >= 1.2 * (bmi), # placeholder; replace if you have actual BMI_95th
        "Severe obesity",
        bmi_cat
      ),
      levels = c(
        # "Healthy weight",
        # "Overweight",
        "Healthy weight + Overweight",
        "Obesity",
        "Severe obesity"
      )
    )
  )

bmi_cat_df %>%
  ggplot(aes(x = year, fill = bmi_cat)) +
  # geom_point(stat = "count") +
  # geom_line(stat = "count") +
  geom_bar(width = 0.5) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  #scale_fill_manual(values = new_pal) +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  facet_grid(group ~ .) +
  labs(
    x = "Year",
    y = "Count",
    fill = "BMI Category"
  ) +
  plot_theme

ggsave(
  file.path("export", "fig_bmi_cat.svg"),
  last_plot(),
  width = 30,
  height = 20,
  dpi = 300,
  #background = "white",
  units = "cm"
)

bmi_cat_df %>%
  group_by(year, group, bmi_cat) %>%
  summarize(n = n()) %>%
  write_csv(file.path("export", "bmi_cat_counts.csv"))
