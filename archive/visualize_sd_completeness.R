# Load necessary libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

# Define the base path to your monitors
base_path <- "/Users/lewiswhite/CHAP_columbia/QuantAQ_ghana/data/Modulair_Parent_Folder"

# Get a list of all monitor folders (MOD- folders)
mod_folders <- list.dirs(base_path, recursive = FALSE, full.names = TRUE)

# Initialize an empty data frame to store the results
monitor_data <- data.frame()

# Loop through each monitor folder
for (mod_folder in mod_folders) {
  # Get all .csv files in the folder
  csv_files <- list.files(mod_folder, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  
  # Remove duplicate file names (keep unique ones)
  unique_files <- unique(csv_files)
  
  # Proceed only if there are .csv files in the folder
  if (length(unique_files) > 0) {
    # Extract the monitor ID (folder name)
    monitor_id <- basename(mod_folder)
    
    # Extract the dates from the filenames
    file_dates <- as.Date(sub("DATA_(\\d{8})\\.csv$", "\\1", basename(unique_files)), format = "%Y%m%d")
    
    # Add monitor ID and dates to the data frame
    monitor_data <- rbind(monitor_data, data.frame(monitor = monitor_id, date = file_dates))
  }
}


# Get a list of all monitor IDs from the folder names
all_monitors <- basename(mod_folders)

# Get the monitors that are already in the monitor_data data frame
monitors_with_data <- unique(monitor_data$monitor)

# Identify missing monitors
missing_monitors <- setdiff(all_monitors, monitors_with_data)

# Create a data frame for missing monitors with NA dates
missing_data <- data.frame(monitor = missing_monitors, date = NA)

# Add the missing monitors to monitor_data
monitor_data <- full_join(monitor_data, missing_data) 



# Create a week column to group data by week
monitor_data_filtered <- monitor_data %>%
  mutate(week = floor_date(date, "week")) %>%
  filter(date > as.Date("2023-07-31") & date < as.Date("2024-10-31")) %>%
  full_join(missing_data)

# Create a summary table of the number of CSVs per monitor per week
summary_table <- monitor_data_filtered %>%
  group_by(monitor, week) %>%
  summarise(days_with_data = n()) %>%
  spread(week, days_with_data, fill = 0)

long_summary <- summary_table %>%
  pivot_longer(cols = -monitor, 
               names_to = "week", 
               values_to = "days_with_data")

long_summary$week <- as.Date(long_summary$week, format = "%Y-%m-%d")

# Create the heatmap
ggplot(long_summary, aes(x = week, y = monitor, fill = days_with_data)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "SD Card Availability per Monitor per Week",
       x = "Week", 
       y = "Monitor", 
       fill = "Days with Data") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(long_summary, aes(x = week, y = monitor, fill = days_with_data)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "SD Card Availability per Monitor per Week",
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


