generate_reference_data <- function(data, monitor_id, pollutant, time_frame) {
  mean_col <- sym(paste0("mean_", pollutant))
  monitor_col <- sym(paste0("monitor_", monitor_id))
  
  if (time_frame == "hourly") {
    reference_data <- data %>%
      filter(monitor == monitor_id) %>%
      mutate(!!monitor_col := !!mean_col) %>%
      select(!!monitor_col, date, hour)
  } else if (time_frame == "daily") {
    reference_data <- data %>%
      filter(monitor == monitor_id) %>%
      mutate(!!monitor_col := !!mean_col) %>%
      select(!!monitor_col, date)
  } else {
    stop("Invalid time frame specified. Please choose either 'hourly' or 'daily'.")
  }
  
  return(reference_data)
}

join_reference_data <- function(data, reference_data, time_frame) {
  if (time_frame == "hourly") {
    data_full <- data %>%
      left_join(reference_data, by = c("date", "hour"))
  } else if (time_frame == "daily") {
    data_full <- data %>%
      left_join(reference_data, by = "date")
  }
  
  return(data_full)
}

plot_comparison <- function(data, monitor_id, pollutant, time_frame, title) {
  mean_col <- sym(paste0("mean_", pollutant))
  monitor_col <- sym(paste0("monitor_", monitor_id))
  
  data %>%
    filter(!is.na(!!monitor_col) & !is.na(!!mean_col)) %>%
    ggplot(aes(x = !!monitor_col, y = !!mean_col)) +
    geom_point(alpha = 0.4) +
    facet_wrap(~monitor) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 0, hjust = 1)) +
    labs(x = paste(monitor_id, str_to_title(time_frame), toupper(pollutant)),
         y = paste("Monitor", str_to_title(time_frame), toupper(pollutant)),
         title = title)
}

compare_pollutant_to_reference <- function(data, monitor_id, pollutant, time_frame = "hourly", title = NULL) {
  # Generate reference data
  reference_data <- generate_reference_data(data, monitor_id, pollutant, time_frame)
  
  # Join reference data with full dataset
  data_full <- join_reference_data(data, reference_data, time_frame)
  
  # Plot comparison
  plot_comparison(data_full, monitor_id, pollutant, time_frame, title)
}
