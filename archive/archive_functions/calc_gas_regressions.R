# Generalized function to run the regression and calculate statistics
run_regression_stats_gas <- function(df, pollutant, reference_pollutant) {
  # Remove rows with NA values in the relevant columns
  df <- df %>%
    filter(!is.na(!!sym(pollutant)) & !is.na(!!sym(reference_pollutant)))
  
  # Check if there are enough non-NA cases to run the regression
  if (nrow(df) < 2) {
    return(tibble(
      monitor = unique(df$monitor),
      N = nrow(df),
      slope = NA,
      intercept = NA,
      RMSE = NA,
      AME = NA
    ))
  }
  
  # Use backticks for the reference pollutant
  formula <- as.formula(paste("`", pollutant, "` ~ `", reference_pollutant, "`", sep = ""))
  
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
apply_regression_gas <- function(data, pollutant, reference_pollutant) {
  results <- data %>%
    group_by(monitor) %>%
    nest() %>%
    mutate(stats = map(data, ~ run_regression_stats_gas(.x, pollutant, reference_pollutant))) %>%
    unnest(stats) %>%
    select(-data) %>%
    mutate(across(c(slope, intercept, RMSE, AME), ~ round(.x, 2)))
  
  return(results)
}
