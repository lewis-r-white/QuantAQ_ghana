library(data.table)
library(dplyr)

load_pollution_datasets <- function(pollutant) {
  columns <- c("monitor", "timestamp", "date", "hour", pollutant)
  
  data <- fread("/Users/lewiswhite/CHAP_columbia/QuantAQ/ghana_AQ_parent_full.csv", 
                select = columns, showProgress = TRUE)
  
  colocation_data <- data %>% filter(date >= as.Date("2023-08-16") & date <= as.Date("2023-09-20"))
  
  community_data <- data %>% filter(date >= as.Date("2023-09-26"))
  
  assign(paste0(pollutant, "_raw"), data, envir = .GlobalEnv)
  assign(paste0(pollutant, "_colocation"), colocation_data, envir = .GlobalEnv)
  assign(paste0(pollutant, "_community"), community_data, envir = .GlobalEnv)
}
