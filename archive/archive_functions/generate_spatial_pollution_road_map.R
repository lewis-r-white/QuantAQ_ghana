library(ggplot2)
library(viridis)

generate_spatial_pollution_road_map <- function(monitor_data, roads_data, pollutant) {
  ggplot() +
    geom_sf(data = roads_data, color = "gray", size = 0.3) +  # Add roads data
    geom_sf(data = monitor_data, aes(geometry = geometry, color = mean_value), size = 5, alpha = 0.6) +  # Add monitor data
    scale_color_viridis_c() +
    labs(title = paste("Spatial Distribution of Mean", toupper(pollutant), "Levels with Roads"),
         color = paste("Mean", toupper(pollutant))) +
    theme_minimal()
}


