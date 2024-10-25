library(httr)
library(jsonlite)
library(boxr)
library(tidyverse)
library(here)

source(here("src", "box_api_client_info.R")) #where I store my client id and secret token

# authenticate 
box_auth(client_id = client_id, client_secret = client_secret)



# Step 1: List all files in the folder
folder_id <- "286055258347" 
file_list <- as.data.frame(box_ls(folder_id))

# Step 2: Filter for CSV files only
csv_files <- file_list[grep(".csv$", file_list$name), ]

# Step 3: Initialize an empty list to store the data from each file
data_list <- list()

tictoc::tic()

# Step 4: Loop through each CSV file and read it with the specified options
for (i in 1:nrow(csv_files)) {
  file_id <- csv_files$id[i]
  file_name <- csv_files$name[i]
  
  # Read the CSV with the specified options (skip, fill, comment.char)
  full_data <- box_read_csv(file_id, skip = 3, fill = TRUE, comment.char = "")
  
  # Ensure timestamp is read as character
  full_data$timestamp_iso <- as.character(full_data$timestamp_iso)
  
  # Select only the desired columns
  selected_columns <- c("timestamp_iso")
  data_list[[file_name]] <- full_data[, selected_columns, drop = FALSE]
}

tictoc::toc()

# Step 5: Combine all data frames in the list into one data frame
combined_data <- do.call(rbind, data_list)

# Custom function to safely convert to POSIXct, returning NA if the conversion fails
safe_as_posixct <- function(x) {
  tryCatch({
    as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }, error = function(e) {
    NA  # Return NA if conversion fails
  })
}

# Step 1: Apply the safe conversion to handle invalid timestamps
combined_data_clean <- combined_data %>%
  mutate(timestamp_iso = sapply(timestamp_iso, safe_as_posixct)) %>%  # Convert or set to NA
  filter(!is.na(timestamp_iso)) %>%  # Remove rows where timestamp conversion failed 
  mutate(timestamp_iso = as.POSIXct(timestamp_iso, origin = "1970-01-01", tz = "UTC"))

# Step 2: Round the valid timestamps to the nearest hour and count observations
MOD_00397_collapsed <- combined_data_clean %>%
  mutate(date_hour = floor_date(timestamp_iso, unit = "hour")) %>%
  group_by(date_hour) %>%
  summarise(observation_count = n()) %>%
  ungroup() %>%
  filter(date_hour > as.Date("2023-08-15") & date_hour < as.Date("2024-12-31")) %>%
  mutate(monitor = "MOD-00397")








library(boxr)
library(dplyr)
library(lubridate)
library(tictoc)

# Custom function to safely convert to POSIXct, returning NA if the conversion fails
safe_as_posixct <- function(x) {
  tryCatch({
    as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }, error = function(e) {
    NA  # Return NA if conversion fails
  })
}

# Define a function to process CSV files for a given folder_id and monitor_name
# Updated function to handle cases where 'timestamp_iso' column is missing or empty
process_monitor_data <- function(folder_id, monitor_name) {
  
  # Step 1: List all files in the folder
  file_list <- as.data.frame(box_ls(folder_id))
  
  # Step 2: Filter for CSV files only
  csv_files <- file_list[grep(".csv$", file_list$name), ]
  
  # Step 3: Initialize an empty list to store the data from each file
  data_list <- list()
  
  tictoc::tic("Data Processing Time")  # Start timing
  
  # Step 4: Loop through each CSV file and read it with the specified options
  for (i in 1:nrow(csv_files)) {
    file_id <- csv_files$id[i]
    file_name <- csv_files$name[i]
    
    # Read the CSV with the specified options (skip, fill, comment.char), using UTF-8 encoding to handle multibyte strings
    full_data <- box_read_csv(file_id, skip = 3, fill = TRUE, comment.char = "", encoding = "UTF-8")
    
    # Check if 'timestamp_iso' column exists and has data
    if ("timestamp_iso" %in% names(full_data) && nrow(full_data) > 0) {
      # Clean invalid multibyte characters from 'timestamp_iso'
      full_data$timestamp_iso <- iconv(full_data$timestamp_iso, from = "UTF-8", to = "ASCII", sub = "")
      
      # Only select rows where 'timestamp_iso' is not empty after cleaning
      if (any(nchar(full_data$timestamp_iso) > 0)) {
        selected_columns <- c("timestamp_iso")
        data_list[[file_name]] <- full_data[, selected_columns, drop = FALSE]
      } else {
        warning(paste("File", file_name, "has no valid 'timestamp_iso' values after cleaning. Skipping."))
      }
    } else {
      warning(paste("File", file_name, "does not contain 'timestamp_iso' column. Skipping."))
    }
  }
  
  tictoc::toc()  # End timing
  
  # Step 5: Combine all data frames in the list into one data frame
  if (length(data_list) > 0) {
    combined_data <- do.call(rbind, data_list)
    
    # Step 6: Apply the safe conversion to handle invalid timestamps
    combined_data_clean <- combined_data %>%
      mutate(timestamp_iso = sapply(timestamp_iso, safe_as_posixct)) %>%  # Convert or set to NA
      filter(!is.na(timestamp_iso)) %>%  # Remove rows where timestamp conversion failed 
      mutate(timestamp_iso = as.POSIXct(timestamp_iso, origin = "1970-01-01", tz = "UTC"))
    
    # Step 7: Round the valid timestamps to the nearest hour and count observations
    collapsed_data <- combined_data_clean %>%
      mutate(date_hour = floor_date(timestamp_iso, unit = "hour")) %>%
      group_by(date_hour) %>%
      summarise(observation_count = n()) %>%
      ungroup() %>%
      filter(date_hour > as.Date("2023-08-15") & date_hour < as.Date("2024-12-31")) %>%
      mutate(monitor = monitor_name)
    
    return(collapsed_data)
    
  } else {
    warning("No valid data found for monitor", monitor_name)
    return(NULL)
  }
}

# Example usage:
MOD_00397_collapsed <- process_monitor_data("286055258347", "MOD-00397")

MOD_00398_collapsed <- process_monitor_data("286054480353", "MOD-00398")

MOD_00399_collapsed <- process_monitor_data("286056347695", "MOD-00399")






#### TRYING TO MAKE THIS INTO A FUNCTION 

# library(future)
# library(future.apply)
# library(boxr)
# library(tictoc)
# 
# # Define the function to process data for a specific folder and monitor
# process_monitor_data <- function(folder_id, monitor) {
#   
#   # Perform Box authentication in each worker session
#   box_auth(client_id = "qd345xgqheynof6a9ezdncrd7vmyik45", client_secret = "p1XaClk35TwV3sGaphqQHP8Heo0irpIT")
#   
#   # Step 1: List all files in the folder
#   file_list <- as.data.frame(box_ls(folder_id))
#   
#   # Step 2: Filter for CSV files only
#   csv_files <- file_list[grep(".csv$", file_list$name), ]
#   
#   # Step 3: Initialize an empty list to store the data from each file
#   data_list <- list()
#   
#   tictoc::tic()
#   
#   # Step 4: Loop through each CSV file and read it with the specified options
#   for (i in 1:nrow(csv_files)) {
#     file_id <- csv_files$id[i]
#     file_name <- csv_files$name[i]
#     
#     # Read the CSV with the specified options (skip, fill, comment.char)
#     full_data <- box_read_csv(file_id, skip = 3, fill = TRUE, comment.char = "")
#     
#     # Skip if the file is empty or does not have a 'timestamp_iso' column
#     if (nrow(full_data) == 0 || !"timestamp_iso" %in% colnames(full_data)) {
#       warning(paste("Skipping file:", file_name, "as it contains no data or no timestamp column."))
#       next
#     }
#     
#     # Ensure timestamp is read as character
#     full_data$timestamp_iso <- as.character(full_data$timestamp_iso)
#     
#     # Select only the desired columns
#     selected_columns <- c("timestamp_iso")
#     data_list[[file_name]] <- full_data[, selected_columns, drop = FALSE]
#   }
#   
#   tictoc::toc()
#   
#   # Step 5: Combine all data frames in the list into one data frame
#   if (length(data_list) == 0) {
#     warning(paste("No valid data for monitor:", monitor))
#     return(NULL)
#   }
#   
#   combined_data <- do.call(rbind, data_list)
#   
#   # Custom function to safely convert to POSIXct, returning NA if the conversion fails
#   safe_as_posixct <- function(x) {
#     tryCatch({
#       as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
#     }, error = function(e) {
#       NA  # Return NA if conversion fails
#     })
#   }
#   
#   # Step 6: Apply the safe conversion to handle invalid timestamps
#   combined_data_clean <- combined_data %>%
#     mutate(timestamp_iso = sapply(timestamp_iso, safe_as_posixct)) %>%  # Convert or set to NA
#     filter(!is.na(timestamp_iso)) %>%  # Remove rows where timestamp conversion failed 
#     mutate(timestamp_iso = as.POSIXct(timestamp_iso, origin = "1970-01-01", tz = "UTC"))
#   
#   # Step 7: Round the valid timestamps to the nearest hour and count observations
#   combined_data_collapsed <- combined_data_clean %>%
#     mutate(date_hour = floor_date(timestamp_iso, unit = "hour")) %>%
#     group_by(date_hour) %>%
#     summarise(observation_count = n()) %>%
#     ungroup() %>%
#     filter(date_hour > as.Date("2023-08-15") & date_hour < as.Date("2024-12-31")) %>%
#     mutate(monitor = monitor)
#   
#   return(combined_data_collapsed)
# }
# 
# # Set up parallel backend using all available cores
# plan(multisession, workers = parallel::detectCores() - 3)  # Reserve 3 cores for other tasks
# 
# # List of folder_id and monitor pairs
# monitor_folders <- data.frame(
#   folder_id = c("286055258347", "286054480353", "286056347695", "286059109542", "286059283016", "286056419205", 
#                 "286056878207", "286059111942", "286059273455", "286059590386", "286055533245", "286055166294", 
#                 "286055478931", "286059280460", "286058131111", "286054650966", "286055126176", "286059796600", 
#                 "286058315580", "286054088525", "286059786192", "286059868598", "286054271668", "286055121223", 
#                 "286055670566", "286056337896", "286059481208", "286057830214", "286054879087", "286059735853", 
#                 "286056832345", "286057184988", "286055003395", "286054854193", "286057388636", "286054904571", 
#                 "286054326991", "286054465340", "286059033497"),
#   monitor = c("MOD-00397", "MOD-00398", "MOD-00399", "MOD-00400", "MOD-00401", "MOD-PM-00876", "MOD-PM-00877", 
#               "MOD-PM-00878", "MOD-PM-00879", "MOD-PM-00880", "MOD-PM-00881", "MOD-PM-00882", "MOD-PM-00883", 
#               "MOD-PM-00884", "MOD-PM-00885", "MOD-PM-00886", "MOD-PM-00888", "MOD-PM-00889", "MOD-PM-00890", 
#               "MOD-PM-00891", "MOD-PM-00892", "MOD-PM-00893", "MOD-PM-00894", "MOD-PM-00895", "MOD-PM-00896", 
#               "MOD-PM-00897", "MOD-PM-00898", "MOD-PM-00899", "MOD-PM-00900", "MOD-PM-01051", "MOD-PM-01052", 
#               "MOD-PM-01053", "MOD-PM-01054", "MOD-PM-01055", "MOD-PM-01056", "MOD-PM-01057", "MOD-PM-01058", 
#               "MOD-PM-01059", "MOD-PM-01060"))
# 
# monitor_folders <- data.frame(
#   folder_id = c("286055258347", "286054480353", "286056347695"),
#   monitor = c("MOD-00397", "MOD-00398", "MOD-00399"))
# 
# # Use future_lapply to parallelize processing of folder_id and monitor pairs
# all_data_list <- future_lapply(1:nrow(monitor_folders), function(i) {
#   folder_id <- monitor_folders$folder_id[i]
#   monitor <- monitor_folders$monitor[i]
#   
#   # Process each monitor data in parallel
#   process_monitor_data(folder_id, monitor)
# })
# 
# # Combine all the collapsed data frames into one
# final_combined_data <- do.call(rbind, all_data_list)
# 
# # View the combined result
# head(final_combined_data)
