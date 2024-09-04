library(dplyr)
library(ggplot2)

compare_fleet_avg_monitor <- function(data, pollutant, period = "hourly", ncol = 5) {
  mean_col <- paste0("mean_", pollutant)
  fleet_col <- paste0("fleet_average_", pollutant)
  
  if (period == "hourly") {
    # Calculate fleet average for hourly data
    fleet_avg <- data %>%
      group_by(date, hour) %>%
      summarise(!!fleet_col := mean(!!sym(mean_col), na.rm = TRUE), .groups = 'drop')
    
    # Remove existing fleet average column if it exists
    if (fleet_col %in% names(data)) {
      data <- data %>% select(-!!sym(fleet_col))
    }
    
    # Join fleet average to individual monitor data
    data_full <- left_join(data, fleet_avg, by = c("date", "hour"))
    
  } else if (period == "daily") {
    # Calculate fleet average for daily data
    fleet_avg <- data %>%
      group_by(date) %>%
      summarise(!!fleet_col := mean(!!sym(mean_col), na.rm = TRUE), .groups = 'drop')
    
    # Remove existing fleet average column if it exists
    if (fleet_col %in% names(data)) {
      data <- data %>% select(-!!sym(fleet_col))
    }
    
    # Join fleet average to individual monitor data
    data_full <- left_join(data, fleet_avg, by = "date")
  } else {
    stop("Invalid period specified. Please choose either 'hourly' or 'daily'.")
  }
  
  # Plot comparison
  ggplot(data_full, aes(x = !!sym(fleet_col), y = !!sym(mean_col))) +
    geom_point(alpha = 0.3) +
    facet_wrap(~monitor, ncol = ncol) +
    theme_bw() +
    labs(x = paste(str_to_title(period), toupper(pollutant), "Fleet Average"),
         y = paste(str_to_title(period), "Monitor", toupper(pollutant), "Average"),
         title = paste("Comparing", str_to_title(period), toupper(pollutant), "Average for Individual Monitor to", str_to_title(period), "\nFleet Average")) +
    theme(
      axis.title = element_text(size = 14),   # Increase axis titles size
      axis.text = element_text(size = 11),    # Increase axis labels size
      strip.text = element_text(size = 11)  # Increase facet group text size
    )
}
