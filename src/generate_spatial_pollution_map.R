library(dplyr)
library(ggplot2)
library(sf)
library(viridis)

generate_spatial_pollution_map <- function(data, pollutant, location_data) {
  mean_col <- paste0("mean_", pollutant)
  
  # Calculate summary statistics
  summary_stats <- data %>%
    group_by(monitor) %>%
    summarize(
      mean_value = mean(!!sym(pollutant), na.rm = TRUE),
      median_value = median(!!sym(pollutant), na.rm = TRUE),
      sd_value = sd(!!sym(pollutant), na.rm = TRUE)
    )
  
  monitor_data <- location_data %>%
    left_join(summary_stats, by = "monitor")
  
  # Plot spatial distribution
  plot <- ggplot(monitor_data) +
    geom_sf(aes(geometry = geometry, color = mean_value), size = 5) +
    scale_color_viridis_c() +
    labs(title = paste("Spatial Distribution of Mean", toupper(pollutant), "Levels"),
         color = paste("Mean", toupper(pollutant))) +
    theme_minimal()
  
  return(plot)
}