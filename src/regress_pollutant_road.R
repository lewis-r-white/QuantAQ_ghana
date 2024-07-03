library(ggplot2)
library(modelsummary)

regress_polutant_road <- function(data, pollutant) {
  regression_model <- lm(mean_value ~ distance_to_road, data = data)
  
  plot <- ggplot(data) +
    geom_point(aes(x = distance_to_road, y = mean_value)) +
    geom_smooth(aes(x = distance_to_road, y = mean_value), method = "lm", se = FALSE) +
    labs(title = paste("Relationship between Distance to Road and Mean", toupper(pollutant), "Levels"),
         x = "Distance to Road (meters)",
         y = paste("Average", toupper(pollutant), "Reading")) +
    theme_minimal()
  
  summary_table <- modelsummary(regression_model)
  
  list(plot = plot, summary_table = summary_table)
}

