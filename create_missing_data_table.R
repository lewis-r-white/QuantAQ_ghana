library(QuantAQAPIClient)
library(ggplot2)
library(lubridate)
library(tictoc)
library(DT)

# Function to process data for a specific device
process_device <- function(device_name) {
  
  # Fetch data
  device_data <- as.data.frame(get_data(device_name, limit = 20000))
  
  # Process timestamp and create full dataset
  device_full <- device_data %>%
    mutate(timestamp = as.POSIXct(timestamp)) %>%
    mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M")) %>%
    mutate(timestamp2 = as.POSIXct(timestamp)) %>%
    complete(timestamp2 = seq.POSIXt(min(timestamp2), max(timestamp2), by = "min")) %>%
    mutate(date = as.Date(timestamp2)) %>%
    mutate(hour = hour(ymd_hms(timestamp2)))
  
  # Calculate missing hours based on the absence of timestamp
  result <- device_full %>%
    group_by(date, hour) %>%
    summarise(no_data = sum(is.na(pm25))) %>%
    mutate(missing_hour = ifelse(no_data > 30, 1, 0)) %>%
    ungroup() %>%
    group_by(date) %>%
    summarise(hours_missing_day = sum(missing_hour)) %>%
    pivot_wider(names_from = date, values_from = hours_missing_day) %>%
    mutate(device = device_name) %>%
    select(device, everything())
  
  return(result)
}

# List of device names
device_names <- c("MOD-PM-00826", "MOD-PM-00847")

# Apply the function to each device and combine the results
result_list <- lapply(device_names, process_device)
final_result <- bind_rows(result_list)

final_result %>% datatable()

