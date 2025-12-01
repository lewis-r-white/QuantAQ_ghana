# load_baseline_calibration.R
#
# Purpose:
# Loads previously saved calibration coefficient tables for each pollutant (pm1, pm25, pm10) from the /data/calibration directory.
# These baseline tables are required when computing new calibration windows that use a reference monitor — the pipeline needs access to the most recent prior calibration for that reference monitor.

# Dependencies:

# Used by:
# The main calibration script that loops over colocation windows.
# Needed specifically for any window that defines a reference_monitor.

# Notes:
# If a calibration file does not exist, the function returns NULL for that pollutant. This is expected for first-time calibration runs or new pollutants/fleets.
# Returns a named list such as: list(pm1 = tibble/NULL, pm25 = tibble/NULL, pm10 = tibble/NULL)

library(tidyverse)
library(here)

load_baseline_calibration <- function(pollutants) {
  out <- list()
  
  for (p in pollutants) {
    fpath <- here::here("data", "calibration",
                        paste0("calibration_", p, ".csv"))
    
    if (file.exists(fpath)) {
      message("Loading baseline calibration for ", p, ": ", fpath)
      out[[p]] <- readr::read_csv(fpath, show_col_types = FALSE)
    } else {
      message("No baseline calibration found for ", p, ". Setting to NULL.")
      out[[p]] <- NULL
    }
  }
  
  out
}