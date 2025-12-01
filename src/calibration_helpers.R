# calibration_helpers.R

# Purpose:
# Utility functions used throughout the calibration workflow. 
# These helpers retrieve monitor groups, extract window metadata, compute fleet averages, fit regressions per monitor, pick the most recent calibration coefficients, and provide small operators such as %||%.

# Dependencies:
#   none (just helper functions)

# Used by:
#   - calibration_methods.R (fleet-average and reference-monitor modes)
#   - build_calibration.R   (loops across windows and pollutants)
#   - apply_calibration.R   (pick_most_recent)

# Notes:
# These functions contain no hard-coded pollutant names. they are general utilities that support all PM types.

library(tidyverse)
library(purrr)
library(rlang)

## to pull correct ids
get_monitor_ids <- function(group_name) {
  if (is.null(group_name) || is.na(group_name)) return(NULL)
  path <- here::here("data", paste0(group_name, ".csv"))
  if (!file.exists(path)) return(NULL)
  readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::pull(monitor) %>%
    unique()
}


# Looks up a window (dates, monitor group, optional reference_monitor).
get_window <- function(config, window_name) {
  ws <- config$colocation_windows
  ix <- which(vapply(ws, function(w) identical(w$name, window_name), logical(1)))
  if (length(ix) != 1) stop("Window '", window_name, "' not found or ambiguous.")
  ws[[ix]]
}




# For each minute (timestamp), compute the mean across active colocated monitors only if at least min_active_monitors have non-missing values.
# Returns timestamp, n_active, fleet_avg (NA rows removed).
compute_fleet_avg <- function(df, pollutant, min_active_monitors, restrict_ids = NULL) {
  d <- if (is.null(restrict_ids)) df else dplyr::filter(df, monitor %in% restrict_ids)
  d %>%
    dplyr::group_by(timestamp) %>%
    dplyr::summarise(
      n_active = sum(!is.na(.data[[pollutant]])),
      fleet_avg = ifelse(n_active >= min_active_monitors,
                         mean(.data[[pollutant]], na.rm = TRUE),
                         NA_real_),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(fleet_avg))
}


# For each monitor, fit y \~ x (linear regression) and report slope, intercept, r2, rmse, and n_points (rows used in the fit).
# Works for either mode because you pass in the appropriate x (fleet average or corrected reference) and y (monitor raw).
fit_by_monitor <- function(df, y_var, x_var) {
  df %>%
    dplyr::filter(!is.na(.data[[y_var]]), !is.na(.data[[x_var]])) %>%
    dplyr::group_by(monitor) %>%
    dplyr::reframe({
      fit <- lm(stats::reformulate(x_var, y_var), data = dplyr::cur_data())
      tibble::tibble(
        n_points  = nrow(dplyr::cur_data()),
        slope     = unname(coef(fit)[[2]]),
        intercept = unname(coef(fit)[[1]]),
        r2        = summary(fit)$r.squared,
        rmse      = sqrt(mean(stats::residuals(fit)^2))
      )
    }, .groups = "drop")
}

# helper: NULL-coalescing operator
# Return first value if it’s not NULL, otherwise return fallback value. In our case, empty calibration tibble so things don't break. 
`%||%` <- function(a, b) if (!is.null(a)) a else b 



# function to pick the most recent coefficients from calibration 
pick_most_recent <- function(cal_tbl, pollutant) {
  cal_tbl %>%
    dplyr::filter(.data$pollutant == pollutant) %>%
    dplyr::mutate(
      .recency = dplyr::coalesce(as.Date(fitted_on), as.Date(colocation_end))
    ) %>%
    dplyr::arrange(monitor, dplyr::desc(.recency), dplyr::desc(n_points), dplyr::desc(r2)) %>%
    dplyr::distinct(monitor, .keep_all = TRUE) %>%
    dplyr::select(-.recency)
}





