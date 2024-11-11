load_pollution_datasets <- function(pollutant, file_path, file_type = "csv") {
  columns <- c("monitor", "timestamp", "date", "hour", pollutant)
  
  # Load data based on the specified file type
  data <- switch(file_type,
                 "csv" = fread(file_path, select = columns, showProgress = TRUE),
                 "rds" = {
                   data <- readRDS(file_path)
                   data %>% select(any_of(columns))  # Select columns if they exist
                 },
                 stop("Unsupported file type. Use 'csv' or 'rds'.")
  )
  
  # Apply specific filtering if the pollutant is pm10
  if (pollutant == "pm10") {
    data <- data %>% filter(!!sym(pollutant) <= 1500)
  }
  
  # Add source column only where pollutant is not NA
  data <- data %>% 
    mutate(source = if_else(!is.na(!!sym(pollutant)), "cloud", NA_character_))
  
  # Split data into colocation and community datasets
  colocation_data <- data %>% filter(date >= as.Date("2023-08-16") & date <= as.Date("2023-09-20"))
  community_data <- data %>% filter(date >= as.Date("2023-09-26"))
  
  # Assign data to global environment with dynamic names
  assign(paste0(pollutant, "_raw"), data, envir = .GlobalEnv)
  assign(paste0(pollutant, "_colocation"), colocation_data, envir = .GlobalEnv)
  assign(paste0(pollutant, "_community"), community_data, envir = .GlobalEnv)
}
