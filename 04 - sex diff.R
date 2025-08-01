# ── Packages ──────────────────────────────────────────────────────────────
library(tidyverse)
library(lmerTest) # lmer() + Satterthwaite p-values
library(rms) # rcs()
library(sjPlot) # plot_model()
library(patchwork) # wrap_plots()
load("data/final_data.RData")

# ── 1. Prepare a “look-up” table of tests and y-axis labels ───────────────
tests_tbl <- tribble(
  ~test,
  ~label,
  "height_sds",
  "Height SDS",
  "weight_sds",
  "Weight SDS",
  "bmi_sds",
  "Body Mass Index SDS",
  "hemoglobin",
  "Hemoglobin",
  "tsh",
  "TSH",
  "vitamin_b12",
  "Vitamin B12",
  "vitamin_d",
  "Vitamin D"
)

# ── 2. Reusable objects ───────────────────────────────────────────────────
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

# ── 3. Fit, plot, and combine in one pipe ────────────────────────────────
models <- tests_tbl %>%
  mutate(
    model = map(test, fit_lmer),
    plot = map2(model, label, plot_pred)
  )
# ── 3. Combine all plots in one pipe ────────────────────────────────

big_plot <-
  models %>%
  pull(plot) %>% # extract the list of ggplots
  wrap_plots(ncol = 2, guides = "collect") & # “collect” = one shared legend
  theme(legend.position = "bottom")

# ── 4. Draw ───────────────────────────────────────────────────────────────
big_plot
