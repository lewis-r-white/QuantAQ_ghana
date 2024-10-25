load_pollution_datasets <- function(pollutant) {
  columns <- c("monitor", "timestamp", "date", "hour", pollutant)
  
  data <- fread("/Users/lewiswhite/CHAP_columbia/QuantAQ/data/cloud/ghana_AQ_parent_full_20240925.csv", 
                select = columns, showProgress = TRUE)
  
  # Filter out rows with pm10 values above 1500 if the pollutant is pm10. Suggested data error entry. 
  if (pollutant == "pm10") {
    data <- data %>% filter(!!sym(pollutant) <= 1500)
  }
  
  # Add source column only where pollutant is not NA
  data <- data %>% 
    mutate(source = if_else(!is.na(!!sym(pollutant)), "cloud", NA_character_))
  
  colocation_data <- data %>% filter(date >= as.Date("2023-08-16") & date <= as.Date("2023-09-20"))
  
  community_data <- data %>% filter(date >= as.Date("2023-09-26"))
  
  assign(paste0(pollutant, "_raw"), data, envir = .GlobalEnv)
  assign(paste0(pollutant, "_colocation"), colocation_data, envir = .GlobalEnv)
  assign(paste0(pollutant, "_community"), community_data, envir = .GlobalEnv)
}


