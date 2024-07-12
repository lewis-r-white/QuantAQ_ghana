library(dplyr)

summarize_pollution_times <- function(data, pollutant) {
  mean_col <- paste0("mean_", pollutant)
  fleet_col <- paste0("fleet_average_", pollutant)
  
  hourly_summary <- data %>%
    group_by(monitor, date, hour) %>%
    summarise(!!mean_col := mean(!!sym(pollutant), na.rm = TRUE), .groups = 'drop')
  
  daily_summary <- data %>%
    group_by(monitor, date) %>%
    summarise(!!mean_col := mean(!!sym(pollutant), na.rm = TRUE), .groups = 'drop')
  
  fleet_hourly <- hourly_summary %>%
    group_by(date, hour) %>%
    summarise(!!fleet_col := mean(!!sym(mean_col), na.rm = TRUE), .groups = 'drop')
  
  fleet_daily <- daily_summary %>%
    group_by(date) %>%
    summarise(!!fleet_col := mean(!!sym(mean_col), na.rm = TRUE), .groups = 'drop')
  
  hourly_full <- left_join(hourly_summary, fleet_hourly, by = c("date", "hour"))
  daily_full <- left_join(daily_summary, fleet_daily, by = "date")
  
  list(hourly = hourly_full, daily = daily_full)
}
