# Load necessary libraries
library(lubridate)
library(tidyverse)
library(data.table)
library(here)


# Function to load and process all CSV files from one folder ----
process_folder <- function(folder_path, parent_folder_name) {
  # List all CSV files in the folder
  csv_files <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
  
  # Load only the `timestamp_iso` column from each CSV
  data_list <- lapply(seq_along(csv_files), function(i) {
    
    file <- csv_files[i]
    
    # Print progress message
    message(paste("Processing file", i, "of", length(csv_files), "in folder", parent_folder_name))
    
    # Extract monitor name from the file name
    monitor_name <- sub("\\.final\\.csv$", "", basename(file))
    
    # Try to load the data and handle potential errors
    tryCatch({
      data <- fread(file, select = "timestamp_iso", fill = TRUE, quote = "")
      
      # Add the monitor name as a new column
      data[, monitor := monitor_name]
      
      # Attempt to parse timestamp_iso; invalid formats will become NA
      data[, timestamp_iso := ymd_hms(timestamp_iso, quiet = TRUE)]
      
      return(data)
    }, error = function(e) {
      message(paste("Error in file:", file, " - skipping"))
      return(NULL)
    })
  })
  
  # Combine the data
  combined_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  # Create floor date/hour variable
  combined_data[, floor_hour := floor_date(timestamp_iso, unit = "hour")]
  
  # Count observations per hour
  hourly_count <- combined_data[, .N, by = .(floor_hour, monitor)]
  
  return(hourly_count)
}


# Main folder path
parent_folder_path <- "/Users/lewiswhite/CHAP_columbia/QuantAQ/data/SD_data"

# List all folders within the parent folder (excluding R scripts)
folders_to_process <- list.dirs(parent_folder_path, recursive = FALSE)

# Process all the folders and combine all data
all_data <- rbindlist(lapply(folders_to_process, function(folder) {
  parent_folder_name <- basename(folder)
  process_folder(folder, parent_folder_name)
}))




# saveRDS(all_data, file = "sd_time_data.rds")

# READ IN DATA IF ALREADY GENERATED ----
all_data <- readRDS(here("data", "SD_data", "sd_time_data_20241023.rds")) %>%
  filter(!is.na(floor_hour))

# remove times outside of study period, specify that hour is complete if at least 100 obs in the hour
all_sd_clean <- all_data %>%
  rename(obs_in_hour_sd = N) %>%
  filter(!is.na(floor_hour)) %>%
  filter(floor_hour > as.Date("2023-08-15")) %>%
  mutate(hour_complete_sd = ifelse(obs_in_hour_sd > 45, 1, 0)) %>%
  mutate(date = lubridate::as_date(floor_hour)) 

# calculate how many hours represented in each day, specify that day is complete if over 12 hours of data, add week
all_sd_date <- all_sd_clean %>% 
  group_by(date, monitor) %>%
  summarize(hours_with_data = sum(hour_complete_sd, na.rm = TRUE)) %>%
  mutate(day_complete_enough = ifelse(hours_with_data > 18, 1, 0)) %>%
  mutate(week = floor_date(date, "week")) %>%
  filter(date > as.Date("2023-07-31") & date < as.Date("2024-10-31"))

# calculate how many days of data within week
all_sd_week <- all_sd_date %>%
  group_by(monitor, week) %>%
  summarise(days_with_data = sum(day_complete_enough, na.rm = TRUE)) %>%
  ungroup()


all_weeks <- data.frame(week = seq(as.Date("2023-08-13"), as.Date("2024-08-15"), by = "week"))

all_monitor_weeks <- expand.grid(monitor = unique(all_sd_week$monitor),
                                 week = all_weeks$week)

sd_weekly_filled <- all_monitor_weeks %>%
  left_join(all_sd_week, by = c("monitor", "week")) %>%
  mutate(days_with_data = ifelse(is.na(days_with_data), 0, days_with_data))




# Plot the heatmap of the week
ggplot(sd_weekly_filled, aes(x = week, y = monitor, fill = days_with_data)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("white", "lightblue", "darkblue"), 
                       values = scales::rescale(c(0, 1, max(sd_weekly_filled$days_with_data))), 
                       na.value = "white") +
  labs(title = "SD Card Data: Availability per Monitor per Week",
       x = "Week", 
       y = "Monitor", 
       fill = "Days with Data") +
  scale_x_date(date_breaks = "4 week", date_labels = "%Y-%m-%d") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11), # Increase x-axis text size
        axis.text.y = element_text(size = 11),                         # Increase y-axis text size
        axis.title.x = element_text(size = 14),                        # Increase x-axis title size
        axis.title.y = element_text(size = 14),                        # Increase y-axis title size
        plot.title = element_text(size = 16, face = "bold"),           # Increase plot title size
        legend.title = element_text(size = 12),                        # Increase legend title size
        legend.text = element_text(size = 10))    





### ADDING CLOUD DATA ----

# load in the full cloud dataset 
final_result_df <- readRDS(here("data" , "cloud", "processed_monitor_data.rds"))

# to decrease file size, select columns of interest and remove rows where pm25 is NA
cloud_df <- final_result_df %>% 
  select(monitor, timestamp, pm25) %>%
  filter(!is.na(pm25))

# count obs in each hour (minutely now) and count hour as complete if there are 10 obs 
cloud_df_hourly <- cloud_df %>%
  mutate(hour = floor_date(timestamp, unit = "hour")) %>%  # Floor timestamps to the hour
  group_by(monitor, hour) %>%
  summarise(obs_in_hour_cloud = n()) %>%  # Count observations in each hour
  ungroup() %>%
  mutate(hour_complete_cloud = ifelse(obs_in_hour_cloud >= 45, 1, 0))  # Flag complete hours

# summarize number of complete hours per day, mark day as complete if 12 hours represented 
cloud_df_daily <- cloud_df_hourly %>%
  mutate(date = as.Date(hour)) %>%  # Extract date from the hourly timestamp
  group_by(monitor, date) %>%
  summarise(complete_hours = sum(hour_complete_cloud, na.rm = TRUE)) %>%  # Count complete hours in each day
  ungroup() %>%
  mutate(day_complete_enough = ifelse(complete_hours > 12, 1, 0))  # Flag complete days


# add a week column, count number of complete days per week
cloud_df_weekly <- cloud_df_daily %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(monitor, week) %>%
  summarise(days_with_data = sum(day_complete_enough, na.rm = TRUE)) %>%
  ungroup()


cloud_weekly_filled <- all_monitor_weeks %>%
  left_join(cloud_df_weekly, by = c("monitor", "week")) %>%
  mutate(days_with_data = ifelse(is.na(days_with_data), 0, days_with_data))



ggplot(cloud_weekly_filled, aes(x = week, y = monitor, fill = days_with_data)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("white", "lightblue", "darkblue"), 
                       values = scales::rescale(c(0, 1, max(cloud_weekly_filled$days_with_data))), 
                       na.value = "white") +
  labs(title = "Cloud Data: Availability per Monitor per Week",
       x = "Week", 
       y = "Monitor", 
       fill = "Days with Data") +
  scale_x_date(date_breaks = "4 week", date_labels = "%Y-%m-%d") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11), # Increase x-axis text size
        axis.text.y = element_text(size = 11),                         # Increase y-axis text size
        axis.title.x = element_text(size = 14),                        # Increase x-axis title size
        axis.title.y = element_text(size = 14),                        # Increase y-axis title size
        plot.title = element_text(size = 16, face = "bold"),           # Increase plot title size
        legend.title = element_text(size = 12),                        # Increase legend title size
        legend.text = element_text(size = 10))                         # Increase legend text size





### IDENTIFY GAPS BETWEEN BOTH

all_sd_clean_filtered <- all_sd_clean %>%
  filter(date > as.Date("2023-07-31") & date < as.Date("2024-10-31"))

cloud_df_hourly_filtered <- cloud_df_hourly %>%
  filter(as.Date(hour) > as.Date("2023-07-31") & as.Date(hour) < as.Date("2024-10-31"))

sd_cloud_merged_hourly <- full_join(all_sd_clean_filtered, cloud_df_hourly_filtered, 
                                    by = c("floor_hour" = "hour", "monitor" = "monitor")) %>%
  mutate(date = as.Date(floor_hour))


sd_cloud_daily <- sd_cloud_merged_hourly %>%
  mutate(hour_complete = ifelse(hour_complete_sd == 1 | hour_complete_cloud == 1, 1, 0)) %>%
  group_by(monitor, date) %>%
  summarise(complete_hours = sum(hour_complete, na.rm = TRUE)) %>%  # Count complete hours in each day
  ungroup() %>%
  mutate(day_complete_enough = ifelse(complete_hours > 18, 1, 0))  # Flag complete days


# add a week column, count number of complete days per week
sd_cloud_weekly <- sd_cloud_daily %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(monitor, week) %>%
  summarise(days_with_data = sum(day_complete_enough, na.rm = TRUE)) %>%
  ungroup()

sd_cloud_weekly_filled <- all_monitor_weeks %>%
  left_join(sd_cloud_weekly, by = c("monitor", "week")) %>%
  mutate(days_with_data = ifelse(is.na(days_with_data), 0, days_with_data))


ggplot(sd_cloud_weekly_filled, aes(x = week, y = monitor, fill = days_with_data)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("white", "lightblue", "darkblue"), 
                       values = scales::rescale(c(0, 1, max(sd_cloud_weekly_filled$days_with_data))), 
                       na.value = "white") +
  labs(title = "SD Card and Cloud Data Combined: Availability per Monitor per Week",
       x = "Week", 
       y = "Monitor", 
       fill = "Days with Data") +
  scale_x_date(date_breaks = "4 week", date_labels = "%Y-%m-%d") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11), # Increase x-axis text size
        axis.text.y = element_text(size = 11),                         # Increase y-axis text size
        axis.title.x = element_text(size = 14),                        # Increase x-axis title size
        axis.title.y = element_text(size = 14),                        # Increase y-axis title size
        plot.title = element_text(size = 16, face = "bold"),           # Increase plot title size
        legend.title = element_text(size = 12),                        # Increase legend title size
        legend.text = element_text(size = 10))                         # Increase legend text size







ggplot(sd_cloud_weekly_filled, aes(x = week, y = monitor, fill = days_with_data)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("#bf6565", "white", "white"), 
                       values = scales::rescale(c(0, 1, max(sd_cloud_weekly_filled$days_with_data))), 
                       na.value = "#bf6565") +
  labs(title = "Periods of Missing Data Across SD and Cloud Data",
       x = "Week", 
       y = "Monitor", 
       fill = "Days with Data") +
  scale_x_date(date_breaks = "4 week", date_labels = "%Y-%m-%d") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11), # Increase x-axis text size
        axis.text.y = element_text(size = 11),                         # Increase y-axis text size
        axis.title.x = element_text(size = 14),                        # Increase x-axis title size
        axis.title.y = element_text(size = 14),                        # Increase y-axis title size
        plot.title = element_text(size = 16, face = "bold"))           # Increase plot title size








### SUMMARY TABLES

# Define the start and end date for the period
start_date <- as.Date("2023-09-25")
end_date <- as.Date("2024-08-10")

# Create the combined summary table for both SD and Cloud data
combined_summary_table <- sd_cloud_merged_hourly %>%
  filter(date >= start_date & date <= end_date) %>%
  group_by(monitor) %>%
  summarise(
    # SD Card data statistics
    sd_earliest_timestamp = min(floor_hour[!is.na(hour_complete_sd)]),  # Earliest timestamp for SD card data
    sd_latest_timestamp = max(floor_hour[!is.na(hour_complete_sd)]),    # Latest timestamp for SD card data
    sd_total_hours_represented = sum(hour_complete_sd, na.rm = TRUE),   # Total hours represented in SD card data
    sd_total_days_represented = n_distinct(date[!is.na(hour_complete_sd)]),  # Total days represented in SD card data
    sd_percent_data_available = round((sd_total_hours_represented / (as.numeric(difftime(end_date, start_date, units = "days")) * 24)) * 100, 2),  # % SD card data available
    
    # Cloud data statistics
    cloud_earliest_timestamp = min(floor_hour[!is.na(hour_complete_cloud)]),  # Earliest timestamp for cloud data
    cloud_latest_timestamp = max(floor_hour[!is.na(hour_complete_cloud)]),    # Latest timestamp for cloud data
    cloud_total_hours_represented = sum(hour_complete_cloud, na.rm = TRUE),   # Total hours represented in cloud data
    cloud_total_days_represented = n_distinct(date[!is.na(hour_complete_cloud)]),  # Total days represented in cloud data
    cloud_percent_data_available = round((cloud_total_hours_represented / (as.numeric(difftime(end_date, start_date, units = "days")) * 24)) * 100, 2),  # % cloud data available
    
    # Combined SD and Cloud data statistics
    combined_total_hours_represented = sum(pmax(hour_complete_sd, hour_complete_cloud, na.rm = TRUE), na.rm = TRUE),  # Take the max for each hour between SD and Cloud
    combined_total_days_represented = n_distinct(date[hour_complete_sd == 1 | hour_complete_cloud == 1]),  # Count days where either SD or Cloud has complete data
    combined_percent_data_available = round((combined_total_hours_represented / (as.numeric(difftime(end_date, start_date, units = "days")) * 24)) * 100, 2),  # % combined data available
    
    # Missing data statistics
    sd_percent_missing_data = round(100 - sd_percent_data_available, 2),  # % SD card data missing
    cloud_percent_missing_data = round(100 - cloud_percent_data_available, 2),  # % cloud data missing
    combined_percent_missing_data = round(100 - combined_percent_data_available, 2)  # % combined data missing
  )




write_csv(combined_summary_table, "sd_cloud_completeness_summary_table.csv")






# CODE GRAVEYARD
# 
# 
# # Step 1: Create a week column
# cloud_df_weekly <- cloud_df_daily %>%
#   mutate(week = floor_date(date, "week"))
# 
# # Step 2: Summarize the number of complete days per week for each monitor
# summary_table <- cloud_df_weekly %>%
#   group_by(monitor, week) %>%
#   summarise(days_with_data = sum(day_complete_enough)) %>%
#   ungroup()
# 
# # Step 3: Create a long format summary for plotting (optional, not necessary in this case)
# # In this case, it's already in a suitable format for plotting
# 
# # Step 4: Plot the heatmap
# ggplot(summary_table, aes(x = week, y = monitor, fill = days_with_data)) +
#   geom_tile() +
#   scale_fill_gradient(low = "lightblue", high = "darkblue") +
#   labs(title = "Cloud Data Availability per Monitor per Week",
#        x = "Week", 
#        y = "Monitor", 
#        fill = "Days with Data") +
#   scale_x_date(date_breaks = "4 week", date_labels = "%Y-%m-%d") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11), # Increase x-axis text size
#         axis.text.y = element_text(size = 11),                         # Increase y-axis text size
#         axis.title.x = element_text(size = 14),                        # Increase x-axis title size
#         axis.title.y = element_text(size = 14),                        # Increase y-axis title size
#         plot.title = element_text(size = 16, face = "bold"),           # Increase plot title size
#         legend.title = element_text(size = 12),                        # Increase legend title size
#         legend.text = element_text(size = 10))                         # Increase legend text size
# 
