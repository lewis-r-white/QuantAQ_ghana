library(spdep)
library(kableExtra)

calculate_moran_i <- function(data, pollutant, location_data, k = 5) {
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
  
  # Convert monitor data to spatial object using sf
  monitor_sf <- st_as_sf(monitor_data)
  
  # Calculate distance-based spatial weights
  coords <- st_coordinates(monitor_sf)
  nb <- knn2nb(knearneigh(coords, k = k)) # k-nearest neighbors
  listw <- nb2listw(nb, style = "W")
  
  # Calculate Moran's I
  moran_result <- moran.test(monitor_sf$mean_value, listw)
  
  # Extract relevant values directly
  observed_value <- as.numeric(moran_result$estimate["Moran I statistic"])
  expected_value <- as.numeric(moran_result$estimate["Expectation"])
  variance <- as.numeric(moran_result$estimate["Variance"])
  z_score <- as.numeric(moran_result$statistic)
  p_value <- as.numeric(moran_result$p.value)
  
  # Create the Moran's I results table
  moran_table <- data.frame(
    Test = "Moran's I Statistic",
    `Observed Value` = observed_value,
    `Expected Value` = expected_value,
    `Variance` = variance,
    `Z-score` = z_score,
    `p-value` = p_value
  )
  
  return(moran_table)
}

