# Load necessary libraries
library(dplyr)
library(purrr)
library(broom)
library(Metrics)
library(DT) # For datatable function

# Generalized function to run the regression and calculate statistics
run_regression_stats <- function(df, pollutant, fleet_pollutant) {
  formula <- as.formula(paste(pollutant, "~", fleet_pollutant))
  model <- lm(formula, data = df)
  model_summary <- summary(model)
  predictions <- predict(model, df)
  
  n <- nrow(df)
  slope <- model_summary$coefficients[2, 1]
  intercept <- model_summary$coefficients[1, 1]
  rmse_val <- rmse(df[[pollutant]], predictions)
  ame_val <- mae(df[[pollutant]], predictions)
  
  tibble(
    monitor = unique(df$monitor),
    N = n,
    slope = slope,
    intercept = intercept,
    RMSE = rmse_val,
    AME = ame_val
  )
}

# Function to apply the regression for a specified pollutant
apply_regression <- function(data, pollutant, fleet_pollutant) {
  results <- data %>%
    group_by(monitor) %>%
    nest() %>%
    mutate(stats = map(data, ~ run_regression_stats(.x, pollutant, fleet_pollutant))) %>%
    unnest(stats) %>%
    select(-data) %>%
    mutate(across(c(slope, intercept, RMSE, AME), ~ round(.x, 2)))
  
  return(results)
}
