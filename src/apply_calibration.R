# apply_calibration.R
#
# Purpose:
# Applies calibration coefficients (slope + intercept) to merged
# pollution datasets for a given pollutant. For each monitor:
# - Joins the appropriate calibration coefficients.
# - Computes corrected values where valid calibration exists.
# - Keeps raw values if the monitor has no valid calibration.
# - Flags whether calibration was applied.
# - Replaces implausibly low values (default < 1) with NA.
# - Prints a short summary of how many values were filtered.

# Dependencies: none, but requires data to be calibrated and equation table

# Used by:
#  The final PM processing workflow after calibration tables are built.
# Called separately for pm1, pm25, and pm10 in the R Markdown pipeline.

# Notes:
# Expects `cal_tbl` to contain one row per monitor with slope/intercept.
# `min_valid_value` is used to drop negative/implausibly small readings.
# The function returns a clean tibble ready for completeness summarization.


library(tidyverse)
library(purrr)

# function to apply the calibration
apply_calibration <- function(df, cal_tbl, pollutant,
                              add_raw_col = TRUE,
                              calibrate_flag_col = paste0("calibrated_", pollutant),
                              min_valid_value = 1) {
  
  coef_tbl <- cal_tbl %>%
    filter(pollutant == !!pollutant) %>%
    distinct(monitor, .keep_all = TRUE) %>%
    select(monitor, slope, intercept)
  
  raw_col  <- paste0(pollutant, "_raw")
  
  # --- Apply calibration ---
  df2 <- df %>%
    left_join(coef_tbl, by = "monitor") %>%
    mutate(
      !!raw_col := .data[[pollutant]],
      valid_coef = !is.na(slope) & is.finite(slope) & slope != 0,
      corrected  = if_else(valid_coef,
                           (.data[[pollutant]] - intercept) / slope,
                           .data[[pollutant]])
    )
  
  # --- Count values below cutoff BEFORE replacing with NA ---
  n_low_corrected <- sum(df2$corrected < min_valid_value, na.rm = TRUE)
  n_low_raw       <- sum(df2[[raw_col]] < min_valid_value, na.rm = TRUE)
  
  # --- Replace low/negative with NA ---
  df2 <- df2 %>%
    mutate(
      corrected = if_else(corrected < min_valid_value, NA_real_, corrected),
      !!raw_col := if_else(.data[[raw_col]] < min_valid_value, NA_real_, .data[[raw_col]]),
      !!pollutant := corrected,
      !!calibrate_flag_col := valid_coef & !is.na(.data[[raw_col]])
    ) %>%
    select(monitor, timestamp, date, hour, source,
           !!calibrate_flag_col, dplyr::any_of(raw_col), !!pollutant)
  
  # --- Print summary message ---
  message("For ", pollutant, ": ",
          "\n  → Replaced ", n_low_corrected, " corrected values < ", min_valid_value,
          "\n  → Replaced ", n_low_raw,       " raw values < ", min_valid_value)
  
  df2
}




