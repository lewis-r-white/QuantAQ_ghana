library(QuantAQAPIClient)
library(ggplot2)
library(lubridate)
library(tictoc)
library(DT)

setup_client() #input API in order to access the data

whoami() #obtain account info

# get_devices() obtain a list of all devices I have access to (team devices + public devices )

#checking out public device in Kintampo Ghana 
kintampo_air <- as.data.frame(get_data("MOD-PM-00895"))



# LOAD UGANDA DEVICE DATA

uganda_devices <- list() #initialize an empty list 

uganda_sn <- c(
  "MOD-PM-00826", "MOD-PM-00847", "MOD-PM-00846", "MOD-PM-00832", 
  "MOD-PM-00844", "MOD-PM-00845", "MOD-PM-00838", "MOD-PM-00828", 
  "MOD-PM-00836", "MOD-PM-00831", "MOD-00117"
)

# Loop through each serial number and retrieve the data
for (sn in uganda_sn) {
  uganda_devices[[sn]] <- as.data.frame(get_data(sn))
}


# LOAD KENYA DEVICE DATA 


kenya_devices <- list() #initialize list

# Serial numbers for devices in Kenya
kenya_sn <- c(
  "MOD-PM-00829", "MOD-PM-00830", "MOD-PM-00840", "MOD-PM-00834", 
  "MOD-PM-00843", "MOD-PM-00841", "MOD-PM-00848", "MOD-PM-00849", 
  "MOD-PM-00850", "MOD-PM-00852", "MOD-00116"
)

# Loop through each serial number and retrieve the data
for (sn in kenya_sn) {
  kenya_devices[[sn]] <- as.data.frame(get_data(sn))
}


# LOAD ETHIOPIA DEVICE DATA


ethiopia_devices <- list() #initialize list

ethiopia_sn <- c(
  "MOD-PM-00855", "MOD-PM-00842", "MOD-PM-00833", "MOD-PM-00853", "MOD-PM-00837",
  "MOD-PM-00835", "MOD-PM-00827", "MOD-PM-00854", "MOD-PM-00839", "MOD-PM-00851", "MOD-00118"
)

# Loop through each serial number and retrieve the data
for (sn in ethiopia_sn) {
  ethiopia_devices[[sn]] <- as.data.frame(get_data(sn))
}



# ANALYSIS 


# basic time series of pm2.5 for each device in Uganda ---- 

# Loop through each device in the list
for (sn in names(uganda_devices)) {
  # Access the data frame for the current device
  current_data <- uganda_devices[[sn]]
  
  # Create a simple graph for PM2.5 data (you can customize this as needed)
  plot <- ggplot(current_data, aes(x = timestamp, y = pm25)) +
    geom_line() +
    labs(title = paste("PM2.5 for Device SN:", sn)) + 
    theme_minimal()
  
  # Print the plot to display it
  print(plot)
}

# Daily pm 2.5 for device "MOD-PM-00826" in Uganda 

tic() #start timer
Uganda_00826 <- as.data.frame(get_data("MOD-PM-00826", limit = 20000)) #97.843 sec elapsed
toc() #stop timer

Uganda_00826 %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  filter(date >= as.Date("2023-10-01") & date <= as.Date("2023-11-30")) %>%
  summarise(mean_pm25 = mean(pm25, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = mean_pm25)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Date",
       y = "Mean Daily PM 2.5",
       title = "Mean Daily PM 2.5 for device MOD-PM-00826 in Uganda")




# creating table of NA values for each day
Uganda_00826_full <- Uganda_00826 %>%
  mutate(date = as.Date(timestamp),
         hour = hour(ymd_hms(Uganda_00826$timestamp)))


# table version
Uganda_00826_full %>%
  group_by(date, hour) %>%
  summarise(no_data = sum(is.na(pm25))) %>%
  mutate(missing_hour = ifelse(no_data > 30, 1, 0)) %>%
  ungroup() %>%
  group_by(date) %>%
  summarise(hours_missing_day = sum(missing_hour)) %>%
  pivot_wider(names_from = date, values_from = hours_missing_day) 





## Trying getting data by date

x <- as.data.frame(get_data_by_date(sn = "MOD-PM-00847", date = "2023-11-04"))




### getting data for date range for one device ----

# Define start_date and end_date
start_date <- as.Date("2023-10-15")
end_date <- as.Date("2023-11-12")
device = "MOD-PM-00836" #offline uganda device
device = "MOD-PM-01054" #ghana device



# Function to get data by date, handling errors
get_data_safe <- possibly(get_data_by_date, otherwise = NULL) #possibly is used to create a version of the get_data_by_date function that returns 

# Use map to get data for each date, handling errors
result_list <- map(seq(start_date, end_date, by = "days"), function(date) {
  formatted_date <- format(date, "%Y-%m-%d")
  get_data_safe(sn = device, date = formatted_date)
})

# Filter out NULL elements (empty lists)
result_list <- purrr::discard(result_list, ~ is.null(.x) || length(.x) == 0)

# Combine the list of data frames into a single data frame
result_df <- do.call(rbind, lapply(result_list, as.data.frame)) %>%
  mutate(monitor = device) %>%
  select(monitor, everything()) %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>% 
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M")) %>% 
  mutate(timestamp = lubridate::ymd_hm(timestamp))


minutely_df <- data.frame(timestamp = seq.POSIXt(
  as.POSIXct(start_date, tz = "GMT"),
  as.POSIXct(end_date, tz = "GMT"),
  by = "min"
))

result_df_full <- full_join(result_df, minutely_df) %>% 
  arrange(timestamp) %>%
  mutate(date = as.Date(timestamp)) %>%  
  mutate(hour = hour(ymd_hms(timestamp))) %>%
  mutate(monitor = device) %>%
  select(monitor, timestamp, date, hour, everything())

result_df_full %>%
  group_by(date, hour, monitor) %>%
  summarise(no_data = sum(is.na(pm25))) %>%
  mutate(missing_hour = ifelse(no_data > 30, 1, 0)) %>%
  ungroup() %>%
  group_by(date, monitor) %>%
  summarise(hours_missing_day = sum(missing_hour)) %>%
  pivot_wider(names_from = date, values_from = hours_missing_day) %>%
  datatable()



### Getting data by date range for list of devices using for loop

# Define start_date and end_date
start_date <- as.Date("2023-11-15")
end_date <- as.Date("2023-12-01")

# List of device serial numbers
device_list <- c("MOD-PM-00826", "MOD-PM-00838", "MOD-00117", "MOD-PM-00836")

# Initialize an empty list to store results for each device
result_combined <- list()

# Loop through each device in the device list
for (device in device_list) {
  # Function to get data by date, handling errors
  get_data_safe <- possibly(get_data_by_date, otherwise = NULL)
  
  # Use map to get data for each date, handling errors
  result_list <- map(seq(start_date, end_date, by = "days"), function(date) {
    formatted_date <- format(date, "%Y-%m-%d")
    get_data_safe(sn = device, date = formatted_date)
  })
  
  # Filter out NULL elements (empty lists)
  result_list <- purrr::discard(result_list, ~ is.null(.x) || length(.x) == 0)
  
  if (!is_empty(result_list)) {
  # Combine the list of data frames into a single data frame
  result_df <- do.call(rbind, lapply(result_list, as.data.frame)) %>%
    mutate(monitor = device) %>%
    select(monitor, everything()) %>%
    mutate(timestamp = as.POSIXct(timestamp)) %>% 
    mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M")) %>% 
    mutate(timestamp = lubridate::ymd_hm(timestamp))
  
  minutely_df <- data.frame(timestamp = seq.POSIXt(
    as.POSIXct(start_date, tz = "UTC"),
    as.POSIXct(end_date + 1, tz = "UTC"),
    by = "min"
  ))
  
  result_df_full <- full_join(result_df, minutely_df) %>% 
    arrange(timestamp) %>%
    mutate(date = as.Date(timestamp)) %>%  
    mutate(hour = hour(ymd_hms(timestamp))) %>%
    mutate(monitor = device) %>%
    select(monitor, timestamp, date, hour, everything()) 
  
  # Store the result for the current device in the combined list
  result_combined[[device]] <- result_df_full
  } else {
    # If there's no data for this device, create an empty dataframe
    minutely_df_empty <- data.frame(timestamp = seq.POSIXt(
      as.POSIXct(start_date, tz = "UTC"),
      as.POSIXct(end_date + 1, tz = "UTC"),
      by = "min"
    ))
    
    minutely_df_empty <- minutely_df_empty %>% mutate(date = as.Date(timestamp)) %>%  
      mutate(hour = hour(ymd_hms(timestamp))) %>%
      mutate(monitor = device) %>%
      select(monitor, timestamp, date, hour) 
      
    result_combined[[device]] <- minutely_df_empty
  }
}

# Combine data for all devices into a single data frame
final_result_df <- bind_rows(result_combined)

final_result_df %>%
  filter(date <= as.Date(end_date)) %>%
  group_by(date, hour, monitor) %>%
  summarise(no_data = sum(is.na(pm25))) %>%
  mutate(missing_hour = ifelse(no_data > 30, 1, 0)) %>%
  ungroup() %>%
  group_by(date, monitor) %>%
  summarise(hours_missing_day = sum(missing_hour)) %>%
  pivot_wider(names_from = date, values_from = hours_missing_day) %>%
  datatable()





### using lubridate for one device

# Define start_date and end_date
start_date <- as.Date("2023-10-15 00:00")
end_date <- as.Date("2023-11-12 23:59")

device = "MOD-PM-00836" #offline uganda device
device = "MOD-PM-01054" #ghana device


start_date <- as.POSIXct("2023-11-15 00:01", tz = 'Africa/Kampala')
end_date <- as.POSIXct("2023-12-01 23:59", tz = 'Africa/Kampala')
device = "MOD-PM-00845" #online uganda device


# Function to get data by date, handling errors
get_data_safe <- possibly(get_data_by_date, otherwise = NULL) #possibly is used to create a version of the get_data_by_date function that returns 

# Use map to get data for each date, handling errors
result_list <- map(seq(start_date, end_date, by = "days"), function(date) {
  formatted_date <- format(date, "%Y-%m-%d")
  get_data_safe(sn = device, date = formatted_date)
})

# Filter out NULL elements (empty lists)
result_list <- purrr::discard(result_list, ~ is.null(.x) || length(.x) == 0)

# Combine the list of data frames into a single data frame
result_df <- do.call(rbind, lapply(result_list, as.data.frame)) %>%
  mutate(monitor = device) %>%
  select(monitor, everything()) %>%
  mutate(timestamp = as_datetime(timestamp)) %>% 
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M")) %>%
  mutate(timestamp = ymd_hm(timestamp)) %>%
  mutate(timestamp_local = as_datetime(timestamp_local),
         timestamp_local = format(timestamp_local, "%Y-%m-%d %H:%M"),
         timestamp_local = ymd_hm(timestamp_local))


minutely_df <- data.frame(timestamp = seq.POSIXt(
  as.POSIXct(start_date, tz = "UTC"),
  as.POSIXct(end_date + 1, tz = "UTC"),
  by = "min"
))

result_df_full <- full_join(result_df, minutely_df, by = c("timestamp_local" = "timestamp")) %>% 
  arrange(timestamp) %>%
  mutate(date = as.Date(timestamp)) %>%  
  mutate(hour = hour(ymd_hms(timestamp))) %>%
  mutate(monitor = device) %>%
  select(monitor, timestamp, date, hour, everything())

result_df_full %>%
  group_by(date, hour, monitor) %>%
  summarise(no_data = sum(is.na(pm25))) %>%
  mutate(missing_hour = ifelse(no_data > 30, 1, 0)) %>%
  ungroup() %>%
  group_by(date, monitor) %>%
  summarise(hours_missing_day = sum(missing_hour)) %>%
  pivot_wider(names_from = date, values_from = hours_missing_day) %>%
  datatable()

