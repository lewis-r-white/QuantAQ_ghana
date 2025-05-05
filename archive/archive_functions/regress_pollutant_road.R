# library(ggplot2)
# library(modelsummary)
# 
# regress_polutant_road <- function(data, pollutant) {
#   regression_model <- lm(mean_value ~ distance_to_road, data = data)
#   
#   plot <- ggplot(data) +
#     geom_point(aes(x = distance_to_road, y = mean_value)) +
#     geom_smooth(aes(x = distance_to_road, y = mean_value), method = "lm", se = FALSE) +
#     labs(title = paste("Relationship between Distance to Road and Mean", toupper(pollutant), "Levels"),
#          x = "Distance to Road (meters)",
#          y = paste("Average", toupper(pollutant), "Reading")) +
#     theme_minimal()
#   
#   model_name <- paste("Mean", toupper(pollutant), "Regressed on Distance to Road")
#   model_list <- setNames(list(regression_model), model_name)
#   
#   summary_table <- modelsummary(
#     model_list, 
#     coef_rename = c("distance_to_road" = "Distance to Road"),
#     gof_omit = "AIC|BIC|Log.Lik",
#     estimate = "{estimate} ({std.error}){stars}",
#     statistic = "p.value"
#   )
#   
#   list(plot = plot, summary_table = summary_table)
# }
# 

regress_polutant_road <- function(data, pollutant) {

  
  # Regression model without considering road type
  regression_model_no_type <- lm(log(mean_value) ~ distance_to_road + households, data = data)
  

  # Define custom colors
  custom_colors <- c("City Connection" = "#1f78b4", "Residential" = "goldenrod", "Other" = "darkorchid")
  
  # Extract slope and p-value for plot without road type
  coef_no_type <- summary(regression_model_no_type)$coefficients
  slope_no_type <- coef_no_type["distance_to_road", "Estimate"]
  p_value_no_type <- coef_no_type["distance_to_road", "Pr(>|t|)"]
  
  # Plot without considering road type
  plot_no_type <- ggplot(data) +
    geom_point(aes(x = distance_to_road, y = mean_value)) +
    geom_smooth(aes(x = distance_to_road, y = mean_value), method = "lm", se = FALSE) +
    labs(x = "Distance to Road (meters)",
         y = paste("Average", toupper(pollutant), "Reading")) +
    annotate("text", x = Inf, y = Inf, label = paste("Slope:", round(slope_no_type, 2), "\nP-value:", round(p_value_no_type, 4)),
             hjust = 1.5, vjust = 7, size = 4, color = "black") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 11), # Increase axis text size
      axis.ticks = element_line(size = 0.7), # Increase tick mark size
      legend.text = element_text(size = 10), # Increase legend text size
      legend.title = element_text(size = 12), # Increase legend title size
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
    )
  
  # Plot considering road type
  plot_with_type <- ggplot(data) +
    geom_point(aes(x = distance_to_road, y = mean_value, color = road_type)) +
    geom_smooth(aes(x = distance_to_road, y = mean_value, color = road_type), method = "lm", se = FALSE) +
    scale_color_manual(values = custom_colors) +  # Apply custom colors
    labs(x = "Distance to Road (meters)",
         y = paste("Average", toupper(pollutant), "Reading"),
         color = "Type of Road") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 11), # Increase axis text size
      axis.ticks = element_line(size = 0.7), # Increase tick mark size
      legend.text = element_text(size = 10), # Increase legend text size
      legend.title = element_text(size = 12), # Increase legend title size
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
    )
  
  plot_total_length <- ggplot(data) +
    geom_point(aes(x = total_length, y = mean_value)) +
    geom_smooth(aes(x = total_length, y = mean_value), method = "lm", se = FALSE) +
    labs(x = "Total Road Length Within 100 Meter Buffer (m)",
         y = paste("Average", toupper(pollutant), "Reading")) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 11), # Increase axis text size
      axis.ticks = element_line(size = 0.7), # Increase tick mark size
      legend.text = element_text(size = 10), # Increase legend text size
      legend.title = element_text(size = 12), # Increase legend title size
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
    )
  
  
  
  # Model summaries
  model_name_no_type <- paste("Mean", toupper(pollutant), "Regressed on Distance to Road")
  
  model_list <- setNames(list(regression_model_no_type), 
                         c(model_name_no_type))
  
  summary_table <- modelsummary(
    model_list, 
    coef_rename = c("distance_to_road" = "Distance to Road", "total_length" = "Road Length within 100m Buffer"),
    gof_omit = "AIC|BIC|Log.Lik",
    estimate = "{estimate} ({std.error}){stars}",
    statistic = "p.value"
  )
  
  list(plot_with_type = plot_with_type, plot_no_type = plot_no_type, plot_total_length = plot_total_length, summary_table = summary_table)
}
