#  calibration_methods.R

# Purpose:
# Implements the two core calibration strategies for each colocation  window: 
# (1) regression against fleet-average PM  
# (2) regression against a corrected reference monitor. 
# These functions produce the per-window calibration coefficient tables used downstream.

# Dependencies:
# calibration_helpers.R   (for get_window, get_monitor_ids, compute_fleet_avg, fit_by_monitor)

# Used by:
# build_calibration.R   (loops across windows and pollutants and calls these functions to build calibration tables)

# Notes:
# These functions assume the merged cloud/SD dataset (`data_full`) with columns monitor, timestamp, date, hour, pollutant values.
# No file I/O occurs here — this module only performs computations.

library(tidyverse)
library(purrr)
library(rlang)

### FUNCTION TO FIND REGRESSION COEFFICIENTS WHEN USING FLEET AVERAGE ----
# Slice the window dates; optionally restrict to a monitor group (e.g., original/new fleet).
# Join each monitor’s series by timestamp to get pairs (y = monitor_raw, x = fleet_avg).
# Run fit_by_monitor() and filter by min_points and min_r2.

calibrate_window_fleet <- function(data_full, pollutant, window, thresholds) {
  dfw <- dplyr::filter(data_full,
                       date >= as.Date(window$start),
                       date <= as.Date(window$end))
  if (nrow(dfw) == 0L) return(tibble::tibble())
  
  restrict_ids <- get_monitor_ids(window$monitors)
  
  fleet <- compute_fleet_avg(dfw, pollutant,
                             thresholds$min_active_monitors,
                             restrict_ids)
  if (nrow(fleet) == 0L) return(tibble::tibble())
  
  
  target_ids <- get_monitor_ids(window$monitors)
  
  targets <- if (is.null(target_ids)) {
    dfw
  } else {
    dfw %>% dplyr::filter(monitor %in% target_ids)
  }
  
  reg_data <- targets %>%
    dplyr::select(monitor, timestamp, y = !!rlang::sym(pollutant)) %>%
    dplyr::inner_join(fleet, by = "timestamp") %>%
    dplyr::rename(x = fleet_avg)
  
  if (nrow(reg_data) == 0L) return(tibble::tibble())
  
  fit_by_monitor(reg_data, "y", "x") %>%
    dplyr::filter(n_points >= thresholds$min_points,
                  r2 >= thresholds$min_r2) %>%
    dplyr::mutate(
      pollutant = pollutant,
      colocation_start = as.POSIXct(window$start, tz = "UTC"),
      colocation_end   = as.POSIXct(window$end,   tz = "UTC"),
      fitted_on        = Sys.Date(),
      window_name      = window$name,
      method = "fleet_avg",
      reference_monitor = NA_character_
    ) %>%
    dplyr::select(monitor, pollutant, slope, intercept, r2, rmse, n_points,
                  method, reference_monitor, colocation_start, colocation_end,
                  fitted_on, window_name)
}


### FUNCTION TO FIND REGRESSION COEFFICIENTS WHEN USING REFERENCE MONITOR ----

#Grab the reference_monitor from the window.
# Select the most recent prior coefficients for that reference monitor and pollutant from baseline_cal_tbl
# Slice the window; build reference_corrected within the window:
# Restrict to target monitors (e.g., new_fleet) and join on timestamp to get pairs (y = monitor_raw, x = ref_corr).
# Run fit_by_monitor(), apply quality filters, and add metadata (method = "coloc_vs_corrected_ref", plus the reference ID and window dates).

calibrate_window_vs_ref <- function(data_full, pollutant, window, thresholds, baseline_cal_tbl) {
  ref_id <- window$reference_monitor
  if (is.null(ref_id) || is.na(ref_id)) {
    stop("reference_monitor required for vs-ref method.")
  }
  
  # --- pick the MOST RECENT prior coef for the reference monitor ---
  # We prefer `fitted_on` (a Date). If missing, fall back to `colocation_end`.
  prior <- baseline_cal_tbl %>%
    dplyr::filter(monitor == ref_id, pollutant == !!pollutant) %>%
    dplyr::mutate(
      .recency = dplyr::coalesce(as.Date(fitted_on), as.Date(colocation_end))
    ) %>%
    dplyr::arrange(dplyr::desc(.recency), dplyr::desc(n_points), dplyr::desc(r2)) %>%
    dplyr::slice(1)
  
  if (nrow(prior) != 1 || !is.finite(prior$slope) || prior$slope == 0) {
    stop("Valid baseline coef not found for ", ref_id, " (", pollutant, ").")
  }
  
  # --- slice the window ---
  dfw <- dplyr::filter(
    data_full,
    date >= as.Date(window$start),
    date <= as.Date(window$end)
  )
  if (nrow(dfw) == 0L) return(tibble::tibble())
  
  # --- build corrected reference within the window using prior coef ---
  ref_corr <- dfw %>%
    dplyr::filter(monitor == ref_id) %>%
    dplyr::mutate(ref_corr = (.data[[pollutant]] - prior$intercept) / prior$slope) %>%
    dplyr::select(timestamp, ref_corr)
  
  if (nrow(ref_corr) == 0) {
    stop("Reference ", ref_id, " has no data in window for ", pollutant, ".")
  }
  
  # --- limit targets to the specified group (if provided) ---
  target_ids <- get_monitor_ids(window$monitors)
  targets <- if (is.null(target_ids)) dfw else dplyr::filter(dfw, monitor %in% target_ids)
  
  # --- assemble regression data (each target vs corrected reference) ---
  reg_data <- targets %>%
    dplyr::select(monitor, timestamp, y = !!rlang::sym(pollutant)) %>%
    dplyr::inner_join(ref_corr, by = "timestamp") %>%
    dplyr::rename(x = ref_corr)
  
  if (nrow(reg_data) == 0L) return(tibble::tibble())
  
  # --- fit per-monitor, filter for quality, and annotate metadata ---
  fit_by_monitor(reg_data, "y", "x") %>%
    dplyr::filter(n_points >= thresholds$min_points, r2 >= thresholds$min_r2) %>%
    dplyr::mutate(
      pollutant         = pollutant,
      method            = "coloc_vs_corrected_ref",
      reference_monitor = ref_id,
      colocation_start  = as.POSIXct(window$start, tz = "UTC"),
      colocation_end    = as.POSIXct(window$end,   tz = "UTC"),
      fitted_on         = Sys.Date(),
      window_name       = window$name
    ) %>%
    dplyr::select(
      monitor, pollutant, slope, intercept, r2, rmse, n_points,
      method, reference_monitor,
      colocation_start, colocation_end, fitted_on, window_name
    )
}