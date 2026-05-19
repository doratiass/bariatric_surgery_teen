# ── Packages ──────────────────────────────────────────────────────────────
library(tidyverse)
library(lmerTest) # lmer() + Satterthwaite p-values
library(rms) # rcs()
library(sjPlot) # plot_model()
library(patchwork) # wrap_plots()
library(gtsummary)
library(flextable)
library(gt)
source("00 - funcs.R")
load("data/final_data.RData")

# Helper functions -----
base_formula <- value ~ rcs(year, 3) * sex + age + (1 | fake_id)

fit_lmer <- function(tst) {
  lmer(
    base_formula,
    data = final_df_long %>% filter(test == tst, group == "Case")
  )
}

plot_pred <- function(mod, ylab) {
  plot_model(
    mod,
    type = "pred",
    terms = c("year [all]", "sex"),
    jitter = 0.3,
    show.data = TRUE,
    show.legend = TRUE,
    show.p = TRUE,
  ) +
    scale_x_continuous(breaks = 0:5) +
    labs(x = "Year", y = ylab, title = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Fit the models --------------------------------------------------------------
## Prepare a “look-up” table of tests and y-axis labels ----------------------
tests_tbl <- tribble(
  ~test                 ,
  ~label                ,
  "height"              ,
  "Height"              ,
  "height_sds"          ,
  "Height SDS"          ,
  "weight"              ,
  "Weight"              ,
  "weight_sds"          ,
  "Weight SDS"          ,
  "bmi"                 ,
  "Body Mass Index"     ,
  "bmi_sds"             ,
  "Body Mass Index SDS" ,
  "hemoglobin"          ,
  "Hemoglobin"          ,
  "tsh"                 ,
  "TSH"                 ,
  "vitamin_b12"         ,
  "Vitamin B12"         ,
  "vitamin_d"           ,
  "Vitamin D"
)

## Fit the models -------------------------------------------------
models <- tests_tbl %>%
  mutate(
    model = map(test, fit_lmer),
    plot = map2(model, label, plot_pred),
    test = ifelse(
      test %in% c("height", "weight", "bmi"),
      paste0(test, "_nds"),
      test
    )
  )

# How many measurements were after the age of 19
bmi_data_clean %>%
  filter(fake_id %in% final_df_long$fake_id) %>%
  left_join(
    final_df_wide %>% select(fake_id, group) %>% distinct(),
    by = "fake_id"
  ) %>%
  filter(group == "Case") %>%
  mutate(
    time_from_index = as.numeric(difftime(dates, index_date, units = "days")) /
      365.25
  ) %>%
  filter(time_from_index <= 5) %>%
  nrow()

bmi_data_clean %>%
  filter(fake_id %in% final_df_long$fake_id) %>%
  left_join(
    final_df_wide %>% select(fake_id, group) %>% distinct(),
    by = "fake_id"
  ) %>%
  filter(group == "Case") %>%
  mutate(
    time_from_index = as.numeric(difftime(dates, index_date, units = "days")) /
      365.25
  ) %>%
  filter(age > 19, time_from_index <= 5) %>%
  distinct(fake_id) %>%
  nrow()

# Tables ----------------------------------------------------------------------
## Table 1 ----------------------------------------------------------------------
tbl_1_sex <- final_df_long %>%
  filter(group == "Case", year == 0, test %in% tests_tbl$test) %>%
  pivot_wider(names_from = test, values_from = value) %>%
  select(-fake_id, -group, -year, -lipid, -index_year) %>%

  rename_all(function(x) sapply(x, label_get, USE.NAMES = FALSE)) %>%
  tbl_summary(
    by = label_get("sex"),
    missing = "no",
    type = list(
      label_get("age") ~ 'continuous',
      all_continuous() ~ 'continuous'
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    )
  ) %>%
  add_n(statistic = "{N_miss} ({p_miss})") %>%
  modify_header(n = "**Missing**") %>%
  add_p()

# Export Table 1 as a Word document
tbl_1_sex %>%
  as_flex_table() %>%
  save_as_docx(path = file.path("export_sex", "tbl_1.docx"))

## Table 2 ----------------------------------------------------------------------
tbl_2_sex <- final_df_long %>%
  filter(group == "Case", test %in% tests_tbl$test) %>%
  group_by(fake_id) %>%
  filter(year == max(year) | year == min(year)) %>%
  group_by(fake_id, sex, test) %>%
  summarise(
    baseline = if (any(year == min(year))) {
      value[which(year == min(year))[1]]
    } else {
      NA_real_
    },
    last = if (any(year == max(year))) {
      value[which(year == max(year))[1]]
    } else {
      NA_real_
    },
    value_diff = ifelse(
      is.na(baseline) | is.na(last) | length(unique(year)) == 1,
      NA_real_,
      last - baseline
    ),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = test, values_from = value_diff) %>%
  select(-fake_id, -baseline, -last) %>%
  rename_all(function(x) sapply(x, label_get, USE.NAMES = FALSE)) %>%
  tbl_summary(
    by = label_get("sex"),
    missing = "no"
  ) %>%
  # add_n(statistic = "{N_miss} ({p_miss})") %>%
  # modify_header(n = "**Missing**") %>%
  add_p()

tbl_2_sex %>%
  as_flex_table() %>%
  save_as_docx(path = file.path("export_sex", "tbl_2.docx"))

## Supplementary Table 1 ------------------------------------------------
# ---------------------------------------------------------------------------- #
# Model Summary: Combine and Export Coefficients from All Models -------------
# ---------------------------------------------------------------------------- #

# List model names to summarize
model_names <- c(
  "height_nds",
  "weight_nds",
  "bmi_nds",
  "height_sds",
  "weight_sds",
  "bmi_sds",
  "hemoglobin",
  "tsh",
  "vitamin_b12",
  "vitamin_d"
)

# Extract coefficients from each model into a list of tables
model_tables <- map(
  model_names,
  ~ extract_coefficients(models[models$test == ., "model", drop = T][[1]], .)
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
  tab_spanner(label = "Height", columns = starts_with("height_nds")) %>%
  tab_spanner(label = "Weight", columns = starts_with("weight_nds")) %>%
  tab_spanner(label = "BMI", columns = starts_with("bmi_nds")) %>%
  tab_spanner(label = "Height SDS", columns = starts_with("height_sds")) %>%
  tab_spanner(label = "Weight SDS", columns = starts_with("weight_sds")) %>%
  tab_spanner(label = "BMI SDS", columns = starts_with("bmi_sds")) %>%
  tab_spanner(label = "Hemoglobin", columns = starts_with("hemoglobin")) %>%
  tab_spanner(label = "TSH", columns = starts_with("tsh")) %>%
  tab_spanner(label = "Vitamin B12", columns = starts_with("vitamin_b12")) %>%
  tab_spanner(label = "Vitamin D", columns = starts_with("vitamin_d"))

# Save the coefficient summary table as a DOCX file
gtsave(
  coef_table_gt,
  file = file.path("export_sex", "coef_table.docx"),
  to = "docx"
)

# Visualize all plots ---------------------------------------------------------
library(patchwork)

fig_1_a <- models %>%
  filter(test %in% c("height_sds", "weight_sds", "bmi_sds")) %>%
  pull(plot) %>% # extract the list of ggplots
  wrap_plots(ncol = 3, guides = "collect") & # “collect” = one shared legend
  theme(legend.position = "none")

fig_1_b <- models %>%
  filter(
    !(test %in%
      c(
        "height_sds",
        "weight_sds",
        "bmi_sds",
        "height_nds",
        "weight_nds",
        "bmi_nds"
      ))
  ) %>%
  pull(plot) %>% # extract the list of ggplots
  wrap_plots(ncol = 2, guides = "collect") & # “collect” = one shared legend
  theme(legend.position = "bottom")

fig_1 <- fig_1_a /
  fig_1_b +
  plot_layout(heights = c(1, 2)) + # fig_1_b gets 1.5x the height of fig_1_a
  plot_annotation(tag_levels = "A")

ggsave("export_sex/fig_1.png", fig_1, width = 15, height = 10, dpi = 900)

# create patchwork and set shared legend + theme via plot_annotation
fig_1_nds <- models %>%
  filter(test %in% c("height_nds", "weight_nds", "bmi_nds")) %>%
  pull(plot) %>%
  wrap_plots(ncol = 3) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A", theme = theme(legend.position = "bottom"))

ggsave(
  "export_sex/fig_1_nds.png",
  fig_1_nds,
  width = 30 / 1.2,
  height = 10 / 1.2,
  dpi = 900
)
