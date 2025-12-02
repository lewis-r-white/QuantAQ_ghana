# Script: build_gas_calibration.R

# Purpose: Build gas calibration slopes/intercepts using the colocation period only. Run ONLY when:
#  - a new golden monitor is chosen
#  - the colocation window changes
#  - new monitors are added to the gas fleet

# Output:
#   data/calibration/gas_calibration_table_coefs.csv


library(tidyverse)
library(here)

source(here("src", "merge_sd_data_gas.R"))
source(here("src", "load_pollution_datasets.R"))


### Pull data from golden monitor 

# This section extracts hourly gas concentrations from the monitor chosen as the “golden monitor” (here, MOD-00397) during the colocation period.

# We join this reference time series back onto the colocation dataset so each monitor can be regressed against the same reference measurements.

# If the golden monitor choice has already been validated (via correlations or prior work), you can update the monitor ID here directly without re-running the diagnostic plots.

get_reference_data <- function(colocation_data, monitor_id, gases) {
  ref_cols <- setNames(gases, paste0("reference_", gases))
  
  colocation_data %>%
    filter(monitor == monitor_id) %>%
    select(timestamp, !!!setNames(syms(gases), names(ref_cols)))
}



## Fit Calibration Regressions (Determine the correction equation)

# For each gas and for each monitor in the colocation period, we estimate a simple linear calibration model:
#  - Monitor Gas = 𝛼 + 𝛽 × Reference Gas

# These slopes and intercepts are then used to correct all community-period gas values.

get_gas_regression_results <- function(colocation_with_ref, gases, reference_gases) {
  results_list <- list()
  
  for (monitor in unique(colocation_with_ref$monitor)) {
    monitor_data <- colocation_with_ref %>% filter(monitor == !!monitor)
    
    for (i in seq_along(gases)) {
      gas <- gases[i]
      reference_gas <- reference_gases[i]
      
      valid_data <- monitor_data %>% filter(!is.na(.data[[gas]]), !is.na(.data[[reference_gas]]))
      
      if (nrow(valid_data) >= 2) {
        model <- lm(as.formula(paste(gas, "~", reference_gas)), data = valid_data)
        results_list[[length(results_list) + 1]] <- tibble(
          monitor = monitor,
          gas = gas,
          slope = coef(model)[[2]],
          intercept = coef(model)[[1]],
          R2 = summary(model)$r.squared
        )
      } else {
        results_list[[length(results_list) + 1]] <- tibble(
          monitor = monitor, gas = gas, slope = NA, intercept = NA, R2 = NA
        )
      }
    }
  }
  
  bind_rows(results_list)
}



# Load colocation gas
colocation_gas <- read_rds(here("data", "gas", "colocation", "colocation_gas_20230815-20231025.rds"))

gases <- c("co", "no", "no2", "o3")

reference_data <- get_reference_data(colocation_gas, "MOD-00397", gases)

colocation_with_ref <- left_join(colocation_gas, reference_data, by = "timestamp")

regression_results <- get_gas_regression_results(
  colocation_with_ref,
  gases,
  paste0("reference_", gases)
)


write_csv(
  regression_results,
  here("data", "calibration", "gas_calibration_table_coefs.csv")
)