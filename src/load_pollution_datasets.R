# REPLACE your current load_pollution_datasets() with this:
load_pollution_datasets <- function(pollutant, file_path, file_type = "csv") {
  cols <- c("monitor", "timestamp", "date", "hour", pollutant)
  data <- switch(
    file_type,
    "csv" = data.table::fread(file_path, select = cols, showProgress = TRUE),
    "rds" = {
      tmp <- readRDS(file_path)
      dplyr::select(tmp, dplyr::any_of(cols))
    },
    stop("Unsupported file type. Use 'csv' or 'rds'.")
  )
  
  if (pollutant == "pm10") {
    data <- dplyr::filter(data, !!rlang::sym(pollutant) <= 1500)
  }
  
  data <- dplyr::mutate(
    data,
    source = dplyr::if_else(!is.na(!!rlang::sym(pollutant)), "cloud", NA_character_)
  )
  
  list(raw_cloud = data)
}




## VERSION PRE OCT 2025
# load_pollution_datasets <- function(pollutant, file_path, file_type = "csv") {
#   columns <- c("monitor", "timestamp", "date", "hour", pollutant)
#   
#   # Load data based on file type
#   data <- switch(file_type,
#                  "csv" = fread(file_path, select = columns, showProgress = TRUE),
#                  "rds" = {
#                    temp <- readRDS(file_path)
#                    temp %>% select(any_of(columns))
#                  },
#                  stop("Unsupported file type. Use 'csv' or 'rds'.")
#   )
#   
#   # Filter out extreme values for PM10
#   if (pollutant == "pm10") {
#     data <- data %>% filter(!!sym(pollutant) <= 1500)
#   }
#   
#   # Add source label
#   data <- data %>%
#     mutate(source = if_else(!is.na(!!sym(pollutant)), "cloud", NA_character_))
#   
#   # Split by time periods
#   colocation_data <- data %>% filter(date >= as.Date("2023-08-16") & date <= as.Date("2023-09-20"))
#   community_data <- data %>% filter(date >= as.Date("2023-09-26"))
#   
#   # Return named list
#   list(
#     raw_cloud = data,
#     colocation_raw_cloud = colocation_data,
#     community_raw_cloud = community_data
#   )
# }




## FIRST VERSION
# load_pollution_datasets <- function(pollutant, file_path, file_type = "csv") {
#   columns <- c("monitor", "timestamp", "date", "hour", pollutant)
#   
#   # Load data based on the specified file type
#   data <- switch(file_type,
#                  "csv" = fread(file_path, select = columns, showProgress = TRUE),
#                  "rds" = {
#                    data <- readRDS(file_path)
#                    data %>% select(any_of(columns))  # Select columns if they exist
#                  },
#                  stop("Unsupported file type. Use 'csv' or 'rds'.")
#   )
#   
#   # Apply specific filtering if the pollutant is pm10
#   if (pollutant == "pm10") {
#     data <- data %>% filter(!!sym(pollutant) <= 1500)
#   }
#   
#   # Add source column only where pollutant is not NA
#   data <- data %>% 
#     mutate(source = if_else(!is.na(!!sym(pollutant)), "cloud", NA_character_))
#   
#   # Split data into colocation and community datasets
#   colocation_data <- data %>% filter(date >= as.Date("2023-08-16") & date <= as.Date("2023-09-20"))
#   community_data <- data %>% filter(date >= as.Date("2023-09-26"))
#   
#   # Assign data to global environment with dynamic names
#   assign(paste0(pollutant, "_raw_cloud"), data, envir = .GlobalEnv)
#   assign(paste0(pollutant, "_colocation_raw_cloud"), colocation_data, envir = .GlobalEnv)
#   assign(paste0(pollutant, "_community_raw_cloud"), community_data, envir = .GlobalEnv)
# }
