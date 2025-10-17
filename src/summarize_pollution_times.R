library(dplyr)

summarize_pollution_times <- function(data, pollutant,
                                      min_minutes_per_hour = 45,
                                      min_hours_per_day   = 18,
                                      min_active_monitors = 10,   # NEW: fleet avg only if ≥ this many monitors
                                      require_calibrated  = FALSE,
                                      calibrated_col      = "calibrated") {
  # Ensure time fields exist/are consistent
  data <- data %>%
    dplyr::mutate(
      timestamp = as.POSIXct(timestamp, tz = "UTC"),
      date      = as.Date(timestamp),
      hour      = lubridate::hour(timestamp)
    )
  
  # Optionally keep only rows that were actually corrected
  if (require_calibrated && calibrated_col %in% names(data)) {
    data <- data %>% dplyr::filter(.data[[calibrated_col]] %in% TRUE)
  }
  
  mean_col  <- paste0("mean_",  pollutant)
  fleet_col <- paste0("fleet_average_", pollutant)
  
  # ---- Hourly (per monitor) with completeness rule ----
  hourly_summary <- data %>%
    dplyr::group_by(monitor, date, hour) %>%
    dplyr::summarise(
      n_minute_obs = sum(!is.na(.data[[pollutant]])),
      !!mean_col := dplyr::if_else(
        n_minute_obs >= min_minutes_per_hour,
        mean(.data[[pollutant]], na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    )
  
  # ---- Daily (per monitor) from hourly with completeness rule ----
  daily_summary <- hourly_summary %>%
    dplyr::group_by(monitor, date) %>%
    dplyr::summarise(
      n_complete_hours = sum(!is.na(.data[[mean_col]])),
      !!mean_col := dplyr::if_else(
        n_complete_hours >= min_hours_per_day,
        mean(.data[[mean_col]], na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    )
  
  # ---- Fleet hourly with ≥ min_active_monitors rule ----
  fleet_hourly <- hourly_summary %>%
    dplyr::group_by(date, hour) %>%
    dplyr::summarise(
      n_active = sum(!is.na(.data[[mean_col]])),
      !!fleet_col := dplyr::if_else(
        n_active >= min_active_monitors,
        mean(.data[[mean_col]], na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(!!fleet_col := dplyr::na_if(.data[[fleet_col]], NaN))
  
  # ---- Fleet daily with ≥ min_active_monitors rule ----
  fleet_daily <- daily_summary %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      n_active = sum(!is.na(.data[[mean_col]])),
      !!fleet_col := dplyr::if_else(
        n_active >= min_active_monitors,
        mean(.data[[mean_col]], na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(!!fleet_col := dplyr::na_if(.data[[fleet_col]], NaN))
  
  # ---- Attach fleet averages back to per-monitor summaries ----
  hourly_full <- dplyr::left_join(hourly_summary, fleet_hourly, by = c("date", "hour"))
  daily_full  <- dplyr::left_join(daily_summary,  fleet_daily,  by = "date")
  
  list(hourly = hourly_full, daily = daily_full)
}


## version until oct 2025 ----

# summarize_pollution_times <- function(data, pollutant) {
#   mean_col <- paste0("mean_", pollutant)
#   fleet_col <- paste0("fleet_average_", pollutant)
#   
#   # Hourly summary with completeness check (45 valid minutely observations)
#   hourly_summary <- data %>%
#     group_by(monitor, date, hour) %>%
#     summarise(
#       n_minute_obs = sum(!is.na(!!sym(pollutant))),  # Count non-NA minute-level observations
#       !!mean_col := ifelse(n_minute_obs >= 45, mean(!!sym(pollutant), na.rm = TRUE), NA_real_),  # Only calculate if 75% of minutes are non-NA
#       .groups = 'drop'
#     )
#   
#   # Daily summary with completeness check (18 completed hours)
#   daily_summary <- hourly_summary %>%
#     group_by(monitor, date) %>%
#     summarise(
#       n_complete_hours = sum(!is.na(!!sym(mean_col))),  # Count non-NA complete hours
#       !!mean_col := ifelse(n_complete_hours >= 18, mean(!!sym(mean_col), na.rm = TRUE), NA_real_),  # Only calculate if 75% of hours are complete
#       .groups = 'drop'
#     )
#   
#   # Fleet summaries
#   fleet_hourly <- hourly_summary %>%
#     group_by(date, hour) %>%
#     summarise(
#       !!fleet_col := mean(!!sym(mean_col), na.rm = TRUE),
#       .groups = 'drop'
#     )
#   
#   fleet_daily <- daily_summary %>%
#     group_by(date) %>%
#     summarise(
#       !!fleet_col := mean(!!sym(mean_col), na.rm = TRUE),
#       .groups = 'drop'
#     )
#   
#   # Join fleet averages with individual summaries
#   hourly_full <- left_join(hourly_summary, fleet_hourly, by = c("date", "hour"))
#   daily_full <- left_join(daily_summary, fleet_daily, by = "date")
#   
#   list(hourly = hourly_full, daily = daily_full)
# }






# initial version -----

# library(dplyr)
# 
# summarize_pollution_times <- function(data, pollutant) {
#   mean_col <- paste0("mean_", pollutant)
#   fleet_col <- paste0("fleet_average_", pollutant)
#   
#   hourly_summary <- data %>%
#     group_by(monitor, date, hour) %>%
#     summarise(!!mean_col := mean(!!sym(pollutant), na.rm = TRUE), .groups = 'drop')
#   
#   daily_summary <- data %>%
#     group_by(monitor, date) %>%
#     summarise(!!mean_col := mean(!!sym(pollutant), na.rm = TRUE), .groups = 'drop')
#   
#   fleet_hourly <- hourly_summary %>%
#     group_by(date, hour) %>%
#     summarise(!!fleet_col := mean(!!sym(mean_col), na.rm = TRUE), .groups = 'drop')
#   
#   fleet_daily <- daily_summary %>%
#     group_by(date) %>%
#     summarise(!!fleet_col := mean(!!sym(mean_col), na.rm = TRUE), .groups = 'drop')
#   
#   hourly_full <- left_join(hourly_summary, fleet_hourly, by = c("date", "hour"))
#   daily_full <- left_join(daily_summary, fleet_daily, by = "date")
#   
#   list(hourly = hourly_full, daily = daily_full)
# }

