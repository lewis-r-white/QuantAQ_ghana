# library(dplyr)
# library(sf)
# 
# prep_monitor_road_data <- function(data, pollutant, location_data, roads_data) {
#   # Calculate summary statistics
#   summary_stats <- data %>%
#     group_by(monitor) %>%
#     summarize(
#       mean_value = mean(!!sym(pollutant), na.rm = TRUE),
#       median_value = median(!!sym(pollutant), na.rm = TRUE),
#       sd_value = sd(!!sym(pollutant), na.rm = TRUE)
#     )
#   
#   # Merge with monitor location data
#   monitor_data <- location_data %>%
#     left_join(summary_stats, by = "monitor")
#   
#   # Convert monitor data to spatial object using sf
#   monitor_sf <- st_as_sf(monitor_data)
#   
#   # Calculate distance to nearest road
#   nearest_roads <- st_nearest_feature(monitor_sf, roads_data)
#   distances <- st_distance(monitor_sf, roads_data[nearest_roads, ], by_element = TRUE)
#   
#   # Add distances to the monitor data and convert to numeric
#   monitor_sf <- monitor_sf %>%
#     mutate(distance_to_road = as.numeric(distances))
#   
#   return(monitor_sf)
# }

library(dplyr)
library(sf)

prep_monitor_road_data <- function(data, pollutant, location_data, roads_data) {
  # Calculate summary statistics
  summary_stats <- data %>%
    group_by(monitor) %>%
    summarize(
      mean_value = mean(!!sym(pollutant), na.rm = TRUE),
      median_value = median(!!sym(pollutant), na.rm = TRUE),
      sd_value = sd(!!sym(pollutant), na.rm = TRUE)
    )
  
  # Merge with monitor location data
  monitor_data <- location_data %>%
    left_join(summary_stats, by = "monitor")
  
  # Convert monitor data to spatial object using sf
  monitor_sf <- st_as_sf(monitor_data)
  
  # Calculate distance to nearest road and get the nearest road index
  nearest_roads <- st_nearest_feature(monitor_sf, roads_data)
  distances <- st_distance(monitor_sf, roads_data[nearest_roads, ], by_element = TRUE)
  
  # Add distances and nearest road type to the monitor data
  monitor_sf <- monitor_sf %>%
    mutate(
      distance_to_road = as.numeric(distances),
      road_type = roads_data$highway[nearest_roads]
    )
  
  return(monitor_sf)
}
