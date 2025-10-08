# ------------------------------------------------------------------------------
# Script: Utility Functions for Data Analysis and Model Visualization
# Description: This script defines helper functions for:
#   - Plotting and summarizing continuous variables.
#   - Selecting the best measurement from multiple observations.
#   - Fixing inconsistent height measurements over time.
#   - Labeling variables using a dictionary.
#   - Extracting coefficients from linear models.
#   - Visualizing linear mixed-effects models with predictions and marginal effects.
# ------------------------------------------------------------------------------

# Load required libraries
library(tidyverse)
library(ggpubr)
library(zoo)
library(rstatix)
library(lme4)
library(sjPlot)
library(ggeffects)
library(patchwork)
library(broom.mixed)

# ------------------------------------------------------------------------------
# Function: check_cont
# Description: Generates a collection of plots and summary tables for a
#              continuous variable. If a grouping variable is provided, the plots
#              and tables are stratified by group.
# Arguments:
#   - df: Data frame containing the data.
#   - var: Name of the continuous variable (as a string).
#   - group: (Optional) Grouping variable (as a string).
# Returns:
#   - A combined plot grid with density, QQ, box plots, description statistics,
#     and Shapiro-Wilk normality test results.
# ------------------------------------------------------------------------------
check_cont <- function(df, var, group = NULL) {
  if (is.null(group)) {
    # Without grouping: create overall plots and tables
    dens_plot <- ggdensity(
      df,
      x = var,
      add = "mean",
      rug = TRUE,
      palette = "jco"
    ) +
      theme_minimal() +
      labs(title = paste("Density plot of", var))

    qq_plot <- ggqqplot(df, x = var, palette = "jco") +
      theme_minimal() +
      labs(title = paste("QQ plot of", var))

    box_plot <- ggboxplot(df, y = var, palette = "jco") +
      theme_minimal() +
      labs(title = paste("Box plot of", var))

    desc_plot <- ggtexttable(
      desc_statby(df, measure.var = var, grps = NULL)[, c(
        "min",
        "max",
        "median",
        "mean",
        "iqr",
        "sd"
      )],
      rows = NULL
    ) %>%
      tab_add_title(text = "Description statistics", face = "bold")

    shapiro_plot <- ggtexttable(
      df %>%
        shapiro_test(!!sym(var)) %>%
        mutate(p = round(p, 4)),
      rows = NULL
    ) %>%
      tab_add_title(text = "Shapiro-Wilk Normality Test", face = "bold")
  } else {
    # With grouping: create stratified plots and tables
    dens_plot <- ggdensity(
      df,
      x = var,
      fill = group,
      color = group,
      add = "mean",
      rug = TRUE,
      palette = "jco"
    ) +
      theme_minimal() +
      labs(title = paste("Density plot of", var))

    qq_plot <- ggqqplot(df, x = var, color = group, palette = "jco") +
      theme_minimal() +
      labs(title = paste("QQ plot of", var))

    box_plot <- ggboxplot(
      df,
      x = group,
      y = var,
      fill = group,
      palette = "jco"
    ) +
      theme_minimal() +
      labs(title = paste("Box plot of", var))

    desc_plot <- ggtexttable(
      desc_statby(df, measure.var = var, grps = group)[, c(
        "group",
        "min",
        "max",
        "median",
        "mean",
        "iqr",
        "sd"
      )],
      rows = NULL
    ) %>%
      tab_add_title(text = "Description statistics", face = "bold")

    shapiro_plot <- ggtexttable(
      df %>%
        group_by(group) %>%
        shapiro_test(!!sym(var)) %>%
        mutate(p = round(p, 4)),
      rows = NULL
    ) %>%
      tab_add_title(text = "Shapiro-Wilk Normality Test", face = "bold")
  }

  # Arrange and return all plots and tables in a 2x2 grid with a common legend
  ggarrange(
    dens_plot,
    qq_plot,
    box_plot,
    ggarrange(desc_plot, shapiro_plot, ncol = 1, nrow = 2),
    legend = "bottom",
    common.legend = TRUE,
    ncol = 2,
    nrow = 2
  )
}

# ------------------------------------------------------------------------------
# Function: find_best_measurement
# Description: Selects the best measurement for each unique date using either
#              the "closest" method (choosing the value closest to the previous
#              selected value) or the "average" method (computing the average).
# Arguments:
#   - dates: Vector of dates for the measurements.
#   - values: Vector of measurement values.
#   - return: Either "dates" or "values" (currently returns the entire tibble).
#   - return_method: "closest" to choose the closest measurement, "average" for the average.
# Returns:
#   - A tibble with the selected best measurement for each unique date.
# ------------------------------------------------------------------------------
find_best_measurement <- function(
  dates,
  values,
  return = "dates",
  return_method = "closest"
) {
  # Validate input lengths and parameter values
  if (length(dates) != length(values)) {
    stop("The lengths of 'dates' and 'values' must be equal.")
  }
  if (!return %in% c("dates", "values")) {
    stop("Invalid 'return' value. Choose either 'dates' or 'values'.")
  }
  if (!return_method %in% c("closest", "average")) {
    stop("Invalid 'return_method' value. Choose either 'closest' or 'average'.")
  }

  # Remove NA values and arrange data by dates and values
  df <- tibble(dates = dates, values = values) %>%
    drop_na() %>%
    arrange(dates, values)

  # Warn if no data remains after removing NA values
  if (nrow(df) == 0) {
    warning("No valid data after removing NA values.")
    return(NULL)
  }

  # Extract unique dates from the data
  u_dates <- unique(df$dates)

  # Initialize an empty tibble to hold the results
  result <- tibble()

  # Loop over each unique date to select the best measurement
  for (i in seq_along(u_dates)) {
    current_date <- u_dates[i]
    if (i == 1) {
      # For the first date, select the maximum value (or average if chosen)
      best_row <- df %>%
        filter(dates == current_date) %>%
        {
          if (return_method == "closest") {
            slice_max(., values, with_ties = FALSE)
          } else {
            summarise(., dates = current_date, values = mean(values))
          }
        }
    } else {
      # For subsequent dates, find the value closest to the previous selection or take average
      previous_value <- result$values[nrow(result)]

      best_row <- df %>%
        filter(dates == current_date) %>%
        {
          if (return_method == "closest") {
            mutate(., diff = abs(values - previous_value)) %>%
              slice_min(diff, with_ties = FALSE) %>%
              select(-diff)
          } else {
            summarise(., dates = current_date, values = mean(values))
          }
        }
    }
    # Append the selected row to the result tibble
    result <- bind_rows(result, best_row)
  }

  # Return the complete result tibble
  return(result)
  # Optionally, uncomment the following lines to return only dates or values:
  # if (return == "dates") {
  #   return(result$dates)
  # } else {
  #   return(result$values)
  # }
}

# ------------------------------------------------------------------------------
# Function: fix_height
# Description: Corrects height measurements over time to ensure that values are
#              non-decreasing and fills in missing values using interpolation
#              or nearest valid values within a specific time window.
# Arguments:
#   - height: Vector of height measurements.
#   - date: Vector of corresponding dates.
# Returns:
#   - A vector of corrected height measurements.
# ------------------------------------------------------------------------------
library(dplyr)
library(zoo) # for na.locf and na.approx

fix_height <- function(height, date) {
  # Create a tibble with dates and height values, and sort by date
  df <- tibble(date = date, height = height) %>%
    arrange(date, height)
  if (length(height) > 1) {
    # Fix starting point: If the first height is NA, replace it with the first valid value
    # within 186 days if available.
    if (is.na(df$height[1])) {
      right_index <- 2
      while (
        is.na(df$height[right_index]) &
          right_index < length(height)
      ) {
        right_index <- right_index + 1
      }
      if (difftime(df$date[right_index], df$date[1], units = "days") < 186) {
        df$height[1] <- df$height[right_index]
      }
    }
    # Ensure the height does not decrease over time by comparing to the last valid measurement
    for (i in 2:length(height)) {
      if (!is.na(df$height[i])) {
        left_index <- i - 1
        if (is.na(df$height[left_index])) {
          while (
            is.na(df$height[left_index]) &
              left_index > 1
          ) {
            left_index <- left_index - 1
          }
        }
        if (
          !is.na(df$height[left_index]) &
            df$height[i] < df$height[left_index]
        ) {
          df$height[i] <- df$height[left_index]
        }
      }
    }
    # Fix ending point: If the last height is NA, impute it using the last valid value within 186 days.
    if (is.na(df$height[length(height)])) {
      left_index <- length(height) - 1
      while (
        is.na(df$height[left_index]) &
          left_index > 1
      ) {
        left_index <- left_index - 1
      }
      if (
        difftime(df$date[length(height)], df$date[left_index], units = "days") <
          186
      ) {
        df$height[length(height)] <- df$height[left_index]
      }
    }
    # Fix missing values in the middle: Interpolate or average adjacent valid values if the difference is small.
    mis_n <- ifelse(length(height) == 2, 2, length(height) - 1)
    for (i in 2:mis_n) {
      left_index <- i - 1
      right_index <- i + 1
      if (is.na(df$height[i])) {
        while (
          is.na(df$height[right_index]) &
            right_index < length(height)
        ) {
          right_index <- right_index + 1
        }
        if (
          !is.na(df$height[right_index]) &
            !is.na(df$height[left_index])
        ) {
          if (abs(df$height[right_index] - df$height[left_index]) < 10) {
            df$height[i] <- (df$height[left_index] + df$height[right_index]) / 2
          } else if (
            difftime(
              df$date[right_index],
              df$date[left_index],
              units = "days"
            ) <
              186
          ) {
            df$height[i] <- df$height[right_index]
          } else if ((right_index - left_index) == 2) {
            df$height[i] <- (df$height[left_index] + df$height[right_index]) / 2
          }
        }
      }
    }
  }
  return(df$height)
}

# ------------------------------------------------------------------------------
# Variable Labeling Dictionary
# Description: Maps original variable names to more descriptive, human-readable labels.
# ------------------------------------------------------------------------------
vars_dict <- tibble(
  "index_year" = "Year of index date",
  "age" = "Age",
  "sex" = "Sex (M)",
  "ses" = "Socioeconomic status",
  "sector" = "Sector",
  "periphery" = "Reside in the periphery",
  "repeated_surgery_after_index" = "Re-surgery after index",
  "dm2" = "Diabetes",
  "pre_dm" = "Pre-diabetes",
  "htn" = "Hypertension",
  "lipid" = "Hyperlipidemia",
  "hyperlipidemia_5" = "Hyperlipidemia",
  "anti_depress_drugs_before_index" = "Anti-depressant drugs before index",
  "anti_depress_drugs_after_index" = "Anti-depressant drugs after index",
  "bmi_sds" = "BMI SDS",
  "bmi_0" = "BMI before index",
  "bmi_1" = "BMI at the first year after index",
  "bmi_2" = "BMI at the second year after index",
  "bmi_3" = "BMI at the third year after index",
  "bmi_4" = "BMI at the fourth year after index",
  "bmi_5" = "BMI at the fifth year after index",
  "weight_sds" = "Weight SDS",
  "weight_0" = "Weight before index",
  "weight_1" = "Weight at the first year after index",
  "weight_2" = "Weight at the second year after index",
  "weight_3" = "Weight at the third year after index",
  "weight_4" = "Weight at the fourth year after index",
  "weight_5" = "Weight at the fifth year after index",
  "height_sds" = "Height SDS",
  "height_0" = "Height before index",
  "height_1" = "Height at the first year after index",
  "height_2" = "Height at the second year after index",
  "height_3" = "Height at the third year after index",
  "height_4" = "Height at the fourth year after index",
  "height_5" = "Height at the fifth year after index",
  "hemoglobin" = "Hemoglobin level",
  "hemoglobin_0" = "Hemoglobin level before index",
  "hemoglobin_1" = "Hemoglobin level at the first year after index",
  "hemoglobin_2" = "Hemoglobin level at the second year after index",
  "hemoglobin_3" = "Hemoglobin level at the third year after index",
  "hemoglobin_4" = "Hemoglobin level at the fourth year after index",
  "hemoglobin_5" = "Hemoglobin level at the fifth year after index",
  "folic_acid_0" = "Folic acid level before index",
  "folic_acid_1" = "Folic acid level at the first year after index",
  "folic_acid_2" = "Folic acid level at the second year after index",
  "folic_acid_3" = "Folic acid level at the third year after index",
  "folic_acid_4" = "Folic acid level at the fourth year after index",
  "folic_acid_5" = "Folic acid level at the fifth year after index",
  "vitamin_b12" = "B12 level",
  "vitamin_b12_0" = "B12 level before index",
  "vitamin_b12_1" = "B12 level at the first year after index",
  "vitamin_b12_2" = "B12 level at the second year after index",
  "vitamin_b12_3" = "B12 level at the third year after index",
  "vitamin_b12_4" = "B12 level at the fourth year after index",
  "vitamin_b12_5" = "B12 level at the fifth year after index",
  "tsh" = "TSH level",
  "tsh_0" = "TSH level before index",
  "tsh_1" = "TSH level at the first year after index",
  "tsh_2" = "TSH level at the second year after index",
  "tsh_3" = "TSH level at the third year after index",
  "tsh_4" = "TSH level at the fourth year after index",
  "tsh_5" = "TSH level at the fifth year after index",
  "vitamin_d" = "Vitamin D level",
  "vitamin_d_0" = "Vitamin D level before index",
  "vitamin_d_1" = "Vitamin D level at the first year after index",
  "vitamin_d_2" = "Vitamin D level at the second year after index",
  "vitamin_d_3" = "Vitamin D level at the third year after index",
  "vitamin_d_4" = "Vitamin D level at the fourth year after index",
  "vitamin_d_5" = "Vitamin D level at the fifth year after index"
) %>%
  pivot_longer(
    cols = 1:ncol(.),
    names_to = "var",
    values_to = "name"
  )

# ------------------------------------------------------------------------------
# Function: label_get
# Description: Returns the descriptive label for a variable if it exists in the
#              vars_dict; otherwise, returns the original variable name.
# ------------------------------------------------------------------------------
label_get <- function(x) {
  ifelse(
    x %in% vars_dict$var,
    vars_dict[vars_dict$var == x, "name", drop = TRUE],
    x
  )
}

# ------------------------------------------------------------------------------
# Function: vars_label
# Description: Applies label_get to each element of a vector of variable names.
# ------------------------------------------------------------------------------
vars_label <- function(x) {
  sapply(x, label_get, USE.NAMES = FALSE)
}

# ------------------------------------------------------------------------------
# Function: var_get
# Description: Retrieves the original variable name given a descriptive label.
# ------------------------------------------------------------------------------
var_get <- function(x) {
  ifelse(
    x %in% vars_dict$name,
    vars_dict[vars_dict$name == x, "var", drop = TRUE],
    x
  )
}

# ------------------------------------------------------------------------------
# Function: extract_coefficients
# Description: Extracts coefficients from a linear model summary, formats the
#              estimates and p-values, and renames columns with the provided
#              variable name.
# Arguments:
#   - model: Fitted linear model object.
#   - variable_name: A string used to prefix the column names.
# Returns:
#   - A tibble with the terms, formatted estimates, and p-values.
# ------------------------------------------------------------------------------
extract_coefficients <- function(model, variable_name) {
  summary(model)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    select(term, estimate = Estimate, pval = `Pr(>|t|)`) %>%
    mutate(
      term = case_when(
        term == "(Intercept)" ~ "Intercept",
        term == "year" ~ "Year",
        term == "rcs(year, 3)year" ~ "Year (rcs)",
        term == "rcs(year, 3)year'" ~ "Year (rcs)'",
        term == "groupCase" ~ "Group (Case)",
        term == "sexMale" ~ "Male",
        term == "age" ~ "Age",
        term == "year:groupCase" ~ "Year: Group",
        term == "rcs(year, 3)year:sexMale" ~ "Year (rcs):Sex",
        term == "rcs(year, 3)year':sexMale" ~ "Year (rcs)':Sex",
        TRUE ~ term
      ),
      estimate = formatC(estimate, format = "f", digits = 2),
      pval = formatC(pval, format = "f", digits = 2)
    ) %>%
    rename_with(~ paste0(variable_name, "_est"), estimate) %>%
    rename_with(~ paste0(variable_name, "_pval"), pval)
}

# ------------------------------------------------------------------------------
# Function: visualize_lme_model
# Description: Visualizes the predictions and marginal effects of a linear
#              mixed-effects model. Generates an interaction plot and individual
#              marginal effect plots for year, group, sex, and age, then combines
#              them into one plot.
# Arguments:
#   - model: Fitted linear mixed-effects model.
#   - name: Descriptive name for the model (used in titles).
#   - data: Data frame used for predictions (default: final_df_long).
#   - min_year: Minimum year value for predictions (default: minimum in data).
#   - max_year: Maximum year value for predictions (default: maximum in data).
#   - default_sex: Default sex value for prediction (default: "Female").
#   - default_age: Default age for prediction (default: mean age from data).
# Returns:
#   - A combined ggplot object displaying the interaction and marginal effects.
# ------------------------------------------------------------------------------
visualize_lme_model <- function(
  model,
  name,
  data = final_df_long,
  min_year = NULL,
  max_year = NULL,
  default_sex = "Female",
  default_age = NULL
) {
  # Set default age to the mean age if not provided
  if (is.null(default_age)) {
    default_age <- mean(data$age, na.rm = TRUE)
  }

  # Determine the range of years for prediction if not provided
  if (is.null(min_year)) {
    min_year <- min(data$year, na.rm = TRUE)
  }

  if (is.null(max_year)) {
    max_year <- max(data$year, na.rm = TRUE)
  }

  # Extract fixed-effects p-values using broom.mixed::tidy
  fixed_effects <- tidy(model, effects = "fixed")
  sig_levels <- fixed_effects$p.value
  names(sig_levels) <- fixed_effects$term

  # Helper function to format significance annotations for a given term
  annotate_significance <- function(term) {
    p <- sig_levels[term]
    if (is.na(p)) {
      return("P: NA")
    }
    sprintf("P: %.3f", p)
  }

  # Generate a data frame for model predictions across the specified year range and groups
  predicted <- data.frame(
    year = seq(min_year, max_year, length.out = 100),
    group = rep(c("Control", "Case"), each = 100),
    sex = default_sex,
    age = default_age
  )
  predicted$value <- predict(model, newdata = predicted, re.form = NA)

  # Extract the p-value for the interaction term "year:groupCase"
  interaction_p <- sig_levels["year:groupCase"]

  # Create an interaction plot using sjPlot::plot_model
  interaction_plot <- plot_model(model, type = "int") +
    labs(
      title = paste(
        "Predicted",
        name,
        "Over Time (P:",
        ifelse(is.na(interaction_p), "NA", sprintf("%.3f", interaction_p)),
        ")"
      ),
      x = "Year",
      y = paste("Predicted", name),
      color = ""
    ) +
    theme_minimal()

  # Generate marginal effects for each predictor using ggpredict
  effects_year <- ggpredict(model, terms = "year")
  effects_group <- ggpredict(model, terms = "group")
  effects_sex <- ggpredict(model, terms = "sex")
  effects_age <- ggpredict(model, terms = "age")

  # Create individual marginal effect plots with significance annotations
  year_effect_plot <- plot(effects_year) +
    annotate(
      "text",
      x = max(effects_year$x),
      y = max(effects_year$predicted),
      label = annotate_significance("year"),
      size = 4,
      hjust = 1.1
    ) +
    labs(
      title = paste("Predictors' Marginal Effect on", name, "SDS"),
      y = paste("Predicted", name)
    )

  group_effect_plot <- plot(effects_group) +
    annotate(
      "text",
      x = 2,
      y = max(effects_group$predicted),
      label = annotate_significance("groupCase"),
      size = 4,
      hjust = 1.1
    ) +
    labs(title = "", y = "")

  sex_effect_plot <- plot(effects_sex) +
    annotate(
      "text",
      x = 2,
      y = max(effects_sex$predicted),
      label = annotate_significance("sexMale"),
      size = 4,
      hjust = 1.1
    ) +
    labs(title = "", y = "")

  age_effect_plot <- plot(effects_age) +
    annotate(
      "text",
      x = max(effects_age$x),
      y = max(effects_age$predicted),
      label = annotate_significance("age"),
      size = 4,
      hjust = 1.1
    ) +
    labs(title = "", y = "")

  # Combine all marginal effect plots using patchwork
  marginal_effects_plots <- (year_effect_plot |
    group_effect_plot |
    sex_effect_plot |
    age_effect_plot)

  # Combine the interaction plot with the marginal effects plots and add an overall title
  combined_plot <- (interaction_plot / marginal_effects_plots) +
    plot_annotation(title = "Linear Mixed Effects Model Visualization")

  return(combined_plot)
}
