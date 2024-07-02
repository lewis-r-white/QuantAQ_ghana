library(dplyr)
library(ggplot2)
library(lubridate)
library(viridis)

generate_heatmap <- function(data, pollutant) {
  mean_col <- paste0("mean_", pollutant)
  
  # Ensure the date column is in date format using lubridate
  data <- data %>%
    mutate(date = ymd(date))  # Use lubridate to ensure date conversion
  
  heat_map_data <- data %>%
    group_by(date, hour) %>%
    summarize(!!mean_col := mean(!!sym(mean_col), na.rm = TRUE), .groups = 'drop') %>%
    ungroup() %>%
    mutate(day = day(date),
           month = factor(format(date, "%b"), levels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")),
           year = year(date))
  
  ggplot(heat_map_data, aes(day, hour, fill = !!sym(mean_col))) +
    geom_tile(color = "white", size = 0.1) + 
    scale_fill_viridis(name = paste("Average", toupper(pollutant), "Reading"), option = "F") + 
    facet_grid(~month) +
    scale_y_continuous(trans = "reverse", breaks = unique(heat_map_data$hour)) +
    scale_x_continuous(breaks = c(1, 10, 20, 31)) +
    theme_minimal(base_size = 8) +
    labs(title = paste("Hourly", toupper(pollutant), "Reading Aggregated Across Monitors"),
         x = "Day",
         y = "Hour Commencing") + 
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 6)) +
    theme(strip.background = element_rect(colour = "white")) +
    theme(plot.title = element_text(hjust = 0)) +
    theme(axis.ticks = element_blank()) +
    theme(axis.text = element_text(size = 7)) +
    theme(legend.title = element_text(size = 8)) +
    theme(legend.text = element_text(size = 6)) +
    theme(panel.background = element_blank())
}


