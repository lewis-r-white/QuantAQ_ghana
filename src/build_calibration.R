# build_calibration.R

# Purpose:
# Provides a dispatcher that constructs calibration coefficient tables for a single pollutant and a single colocation window.
# Automatically selects the appropriate calibration method:
# *Fleet-average regression when no reference monitor is defined.
# *Reference-monitor–based regression when a reference is provided.

# Dependencies:
#  calibration_methods.R (calibrate_window_fleet, calibrate_window_vs_ref)
#  calibration_helpers.R (get_window)

# Used by:
#   The main calibration script or pipeline that loops across all pollutants and colocation windows to assemble the full calibration table.

# Notes:
#  Requires `merged_full` (merged cloud + SD dataset).
#  Windows and thresholds must be defined in the YAML-style config.
#  For vs-ref windows, `baseline_cal_tbl` must include prior coefficients for the reference monitor.

library(tidyverse)
library(purrr)
library(rlang)


### FUNCTION TO IMPLEMENT THE REGRESSION FINDING FUNCTION ----
# Dispatcher: reads the window and thresholds from config.
# If reference_monitor is NULL → run fleet mode.
# If reference_monitor is set → run vs-ref mode (and require baseline_cal_tbl).
# Returns the finished calibration tibble for that window/pollutant.

build_calibration_table_for <- function(merged_full,
                                        pollutant,
                                        config,
                                        window_name,
                                        baseline_cal_tbl = NULL) {
  w  <- get_window(config, window_name)
  th <- config$thresholds
  
  if (is.null(w$reference_monitor)) {
    calibrate_window_fleet(merged_full, pollutant, w, th)
  } else {
    if (is.null(baseline_cal_tbl)) {
      stop("baseline_cal_tbl required for vs-ref windows (", window_name, ").")
    }
    calibrate_window_vs_ref(merged_full, pollutant, w, th, baseline_cal_tbl)
  }
}



# build_all_calibrations <- function(merged_results, pollutants, config, cal_w1) {
#   purrr::map(pollutants, function(p) {
#     build_calibration_table_for(
#       merged_full      = merged_results[[p]],
#       pollutant        = p,
#       config           = config,
#       window_name      = "window2_new_monitors",
#       baseline_cal_tbl = cal_w1[[p]]
#     )
#   }) %>% rlang::set_names(pollutants)
# }



