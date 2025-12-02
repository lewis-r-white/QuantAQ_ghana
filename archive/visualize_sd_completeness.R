# Load necessary libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)


ghana_devices <- c("MOD-00077", "MOD-00397", "MOD-00398", "MOD-00399", "MOD-00400",
                   "MOD-00401", "MOD-PM-00871", "MOD-PM-00872", "MOD-PM-00876",
                   "MOD-PM-00877", "MOD-PM-00878", "MOD-PM-00879", "MOD-PM-00880",
                   "MOD-PM-00881", "MOD-PM-00882", "MOD-PM-00883", "MOD-PM-00884",
                   "MOD-PM-00885", "MOD-PM-00886", "MOD-PM-00887", "MOD-PM-00888",
                   "MOD-PM-00889", "MOD-PM-00890", "MOD-PM-00891", "MOD-PM-00892",
                   "MOD-PM-00893", "MOD-PM-00894", "MOD-PM-00895", "MOD-PM-00896",
                   "MOD-PM-00897", "MOD-PM-00898", "MOD-PM-00899", "MOD-PM-00900",
                   "MOD-PM-01051", "MOD-PM-01052", "MOD-PM-01053", "MOD-PM-01054",
                   "MOD-PM-01055", "MOD-PM-01056", "MOD-PM-01057", "MOD-PM-01058",
                   "MOD-PM-01059", "MOD-PM-01060", "MOD-PM-01071", "MOD-PM-01072",
                   "MOD-PM-01073", "MOD-PM-01074", "MOD-PM-01075", "MOD-PM-01076",
                   "MOD-PM-01077", "MOD-PM-01078", "MOD-PM-01079", "MOD-PM-01080",
                   "MOD-PM-01081", "MOD-PM-01082", "MOD-PM-01083", "MOD-PM-01084",
                   "MOD-PM-01085", "MOD-PM-01086", "MOD-PM-01087", "MOD-PM-01088",
                   "MOD-PM-01089", "MOD-PM-01090", "MOD-PM-01091", "MOD-PM-01092",
                   "MOD-PM-01093")

# Define the base path to your monitors
base_path <- "/Users/lewiswhite/CHAP_columbia/QuantAQ_ghana/data/all_measurements/sd/raw"

# Get a list of all monitor folders (MOD- folders)
mod_folders <- list.dirs(base_path, recursive = FALSE, full.names = TRUE)


## OLD VERSION 2024 SD Card Data 
# # Initialize an empty data frame to store the results
# monitor_data <- data.frame()
# 
# # Loop through each monitor folder
# for (mod_folder in mod_folders) {
#   # Get all .csv files in the folder
#   csv_files <- list.files(mod_folder, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
#   
#   # Remove duplicate file names (keep unique ones)
#   unique_files <- unique(csv_files)
#   
#   # Proceed only if there are .csv files in the folder
#   if (length(unique_files) > 0) {
#     # Extract the monitor ID (folder name)
#     monitor_id <- basename(mod_folder)
#     
#     # Extract the dates from the filenames
#     file_dates <- as.Date(sub("DATA_(\\d{8})\\.csv$", "\\1", basename(unique_files)), format = "%Y%m%d")
#     
#     # Add monitor ID and dates to the data frame
#     monitor_data <- rbind(monitor_data, data.frame(monitor = monitor_id, date = file_dates))
#   }
# }

# Get a list of all monitor IDs from the folder names
#all_monitors <- basename(mod_folders)



# ---------------------------------------------------------------------------

## CODE TO CHECK IF CSVS ARE IN THE RIGHT LOCATION
library(data.table)

# --- helpers ---
extract_monitor_code <- function(x) {
  code <- stringr::str_extract(basename(x), "(MOD(?:_PM)?_\\d{4,5})")
  if (!is.na(code)) code <- gsub("_", "-", code)  # MOD_PM_00890 -> MOD-PM-00890
  code
}

# Read only the metadata lines; pull `deviceSN` from the key-value block
read_device_sn_meta <- function(file, max_lines = 80) {
  # Read just the top of file; fast & memory-light
  lines <- tryCatch(readLines(file, n = max_lines, warn = FALSE), error = function(e) character())
  if (!length(lines)) return(NA_character_)
  
  # Look for a line like: deviceSN,<value>  OR  deviceSN\t<value>  OR  deviceSN;<value>
  # Case-insensitive, tolerates spaces and quotes around the value
  idx <- which(grepl("^\\s*deviceSN\\s*([,;\t])", lines, ignore.case = TRUE))
  if (!length(idx)) return(NA_character_)
  
  line <- lines[idx[1]]
  # Split on first comma/semicolon/tab
  val  <- sub("^\\s*deviceSN\\s*[,;\t]\\s*", "", line, ignore.case = TRUE)
  val  <- gsub('^"|"$', "", trimws(val))     # drop surrounding quotes if any
  val  <- gsub("_", "-", val)                # normalize underscores -> dashes
  val
}

# --- scan folders ---
base_path <- "/Users/lewiswhite/CHAP_columbia/QuantAQ_ghana/data/all_measurements/sd/raw"
mod_folders <- list.dirs(base_path, recursive = FALSE, full.names = TRUE)

scan_results <- purrr::map_dfr(mod_folders, function(dir_path) {
  expected_sn <- extract_monitor_code(dir_path)   # e.g. MOD-PM-00890
  
  csv_files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  csv_files <- csv_files[!grepl("/logs/", csv_files)]
  
  if (!length(csv_files)) {
    return(tibble(
      folder = basename(dir_path),
      expected_sn = expected_sn,
      n_csv = 0L,
      unique_deviceSNs = list(character()),
      n_unique_deviceSN = 0L
    ))
  }
  
  # Pull deviceSN from metadata header only (no data load)
  sn_all <- csv_files %>%
    purrr::map_chr(~ read_device_sn_meta(.x)) %>%
    unique() %>%
    sort()
  
  tibble(
    folder = basename(dir_path),
    expected_sn = expected_sn,
    n_csv = length(csv_files),
    unique_deviceSNs = list(sn_all[!is.na(sn_all) & nzchar(sn_all)]),
    n_unique_deviceSN = sum(!is.na(sn_all) & nzchar(sn_all))
  )
})

# --- flags & quick views ---
results_flagged <- scan_results %>%
  mutate(
    has_multiple_SNs   = n_unique_deviceSN > 1,
    single_sn_mismatch = n_unique_deviceSN == 1 &
      !is.na(expected_sn) &
      purrr::map_chr(unique_deviceSNs, ~ .x[1]) != expected_sn,
    missing_deviceSN   = n_unique_deviceSN == 0
  )

mixed_folders <- results_flagged %>%
  filter(has_multiple_SNs) %>%
  mutate(deviceSNs = purrr::map_chr(unique_deviceSNs, ~ paste(.x, collapse = ", "))) %>%
  select(folder, expected_sn, n_csv, n_unique_deviceSN, deviceSNs)

mismatch_folders <- results_flagged %>%
  filter(single_sn_mismatch) %>%
  mutate(found_sn = purrr::map_chr(unique_deviceSNs, ~ .x[1])) %>%
  select(folder, expected_sn, found_sn, n_csv)

missing_meta <- results_flagged %>%
  filter(missing_deviceSN) %>%
  select(folder, expected_sn, n_csv)

# Inspect
mixed_folders %>% arrange(desc(n_unique_deviceSN)) %>% print(n = 50)
mismatch_folders %>% arrange(folder) %>% print(n = 50)
missing_meta %>% arrange(folder) %>% print(n = 50)


# ---------------------------------------------------------------------------

  






## NEW VERSION 2025 SD Card data 
extract_monitor_code <- function(x) {
  stringr::str_extract(basename(x), "(MOD(?:_PM)?_\\d{4,5})")
}

monitor_data <- purrr::map_dfr(mod_folders, function(dir_path) {
  monitor_code <- extract_monitor_code(dir_path)
  
  # Skip folders that don't contain a recognizable monitor code
  if (is.na(monitor_code)) return(NULL)
  
  # Find all CSVs (ignore logs subfolders if present)
  csv_files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  csv_files <- csv_files[!grepl("/logs/", csv_files)]
  if (!length(csv_files)) return(NULL)
  
  # Pull dates like DATA_20250317@FW24.csv  ->  20250317
  dates_chr <- stringr::str_extract(basename(csv_files), "(?<=^DATA_)\\d{8}(?=@)")
  dates <- suppressWarnings(as.Date(dates_chr, format = "%Y%m%d"))
  
  tibble(
    monitor = monitor_code,
    folder  = basename(dir_path),
    file    = basename(csv_files),
    date    = dates
  ) %>%
    filter(!is.na(date)) %>%
    distinct(monitor, date, .keep_all = FALSE)
})

# replace _ with - in monitor names for consitency 
monitor_data <- monitor_data %>%
  mutate(monitor = gsub("_", "-", monitor))




# Get the monitors that are already in the monitor_data data frame
monitors_with_data <- unique(monitor_data$monitor)

# Identify missing monitors
missing_monitors <- setdiff(ghana_devices, monitors_with_data)

# Create a data frame for missing monitors with NA dates
missing_data <- data.frame(monitor = missing_monitors, date = NA)

# Add the missing monitors to monitor_data
monitor_data <- full_join(monitor_data, missing_data) 


# Create a week column to group data by week
monitor_data_filtered <- monitor_data %>%
  mutate(week = floor_date(date, "week")) %>%
  filter(date > as.Date("2025-02-28") & date < as.Date("2025-09-10")) %>%
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
  scale_fill_gradientn(colors = c("white", "lightblue", "darkblue"))+
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








### ADDING CLOUD DATA ----
recent_readings <- final_result_df %>%
  filter(!is.na(pm25)) %>%
  group_by(monitor) %>%
  summarize(recent_reading = max(timestamp))

copy_for_report <- recent_readings %>%
  mutate(first_date = as.Date("2025-09-01"),
         reading_date = date(recent_reading),
         time_since_reading = first_date - reading_date)

write_csv(copy_for_report, "/Users/lewiswhite/Downloads/device.csv")

copy_for_report %>% pull(monitor)

setdiff(ghana_devices, copy_for_report %>% pull(monitor))






# load in the full cloud dataset 
final_result_df <- readRDS(here("data" , "all_measurements", "cloud", "processed_monitor_data_20250301_20250901.rds"))

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
  mutate(day_complete_enough = ifelse(complete_hours > 18, 1, 0))  # Flag complete days



max(cloud_df_daily$date) - min(cloud_df_daily$date)

daily_completeness <- cloud_df_daily %>%
  group_by(monitor) %>%
  summarize(days_complete = sum(day_complete_enough)) %>%
  ungroup() %>%
  mutate(percent_complete = round(days_complete /185 * 100, 1))

write_csv(daily_completeness, "/Users/lewiswhite/Downloads/daily_completeness.csv")



# add a week column, count number of complete days per week
cloud_df_weekly <- cloud_df_daily %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(monitor, week) %>%
  summarise(days_with_data = sum(day_complete_enough, na.rm = TRUE)) %>%
  ungroup()


all_monitor_weeks <- expand.grid(monitor = unique(monitor_data_filtered$monitor), # all monitors are listed in SD 
                                 week = cloud_df_weekly$week) # take range for cloud data

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



## combine based on available day 
sd_daily <- monitor_data_filtered %>%
  distinct(monitor, date) %>%
  mutate(sd_complete = TRUE)

cloud_daily <- cloud_df_daily %>%
  transmute(
    monitor,
    date,
    cloud_complete = day_complete_enough == 1
  )

date_rng <- range(c(sd_daily$date, cloud_daily$date), na.rm = TRUE)

template <- expand_grid(
  monitor = union(sd_daily$monitor, cloud_daily$monitor),
  date    = seq(date_rng[1], date_rng[2], by = "day")
)

combined_daily <- template %>%
  left_join(sd_daily,   by = c("monitor","date")) %>%
  left_join(cloud_daily, by = c("monitor","date")) %>%
  mutate(
    sd_complete    = coalesce(sd_complete, FALSE),
    cloud_complete = coalesce(cloud_complete, FALSE),
    any_complete   = sd_complete | cloud_complete,        # <-- overall (union)
    both_complete  = sd_complete & cloud_complete,        # for QC
    source_status  = case_when(
      both_complete        ~ "both",
      sd_complete          ~ "sd_only",
      cloud_complete       ~ "cloud_only",
      TRUE                 ~ "none"
    )
  )

combined_week <- combined_daily %>%
  mutate(week = floor_date(date, "week", week_start = 1)) %>%
  group_by(monitor, week) %>%
  summarise(
    days_any        = sum(any_complete),      # 0..7 (use this for overall heatmap)
    days_sd_only    = sum(source_status == "sd_only"),
    days_cloud_only = sum(source_status == "cloud_only"),
    days_both       = sum(source_status == "both"),
    .groups = "drop"
  )


ggplot(combined_week, aes(x = week, y = monitor, fill = days_any)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = c("white", "lightblue", "darkblue"),
    na.value = "white",
    name = "Days with Data"
  ) +
  labs(
    title = "Overall Availability per Monitor per Week (Cloud + SD)",
    x = "Week",
    y = "Monitor"
  ) +
  scale_x_date(date_breaks = "4 weeks", date_labels = "%Y-%m-%d") +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 90, hjust = 1, size = 11),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title   = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 10)
  )
