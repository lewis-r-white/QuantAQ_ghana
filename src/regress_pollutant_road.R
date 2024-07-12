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
  
  model_name <- paste("Mean", toupper(pollutant), "Regressed on Distance to Road")
  model_list <- setNames(list(regression_model), model_name)
  
  summary_table <- modelsummary(
    model_list, 
    coef_rename = c("distance_to_road" = "Distance to Road"),
    gof_omit = "AIC|BIC|Log.Lik",
    estimate = "{estimate} ({std.error}){stars}",
    statistic = "p.value"
  )
  
  list(plot = plot, summary_table = summary_table)
}

