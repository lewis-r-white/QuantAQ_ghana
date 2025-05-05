# load packages ----
library(QuantAQAPIClient)
library(here) 
library(lubridate) 
library(tictoc)
library(DT)
library(purrr)
library(tidyverse)
library(sf)
library(terra)

# Connect to QuantAQ API (necessary to obtain data) ----
setup_client() #log in to QuantAQ account and click developer in the left menu to grab API key


# List devices ----

ghana_devices <- c("MOD-PM-01054", "MOD-PM-00900", "MOD-PM-00876", "MOD-PM-00882", "MOD-PM-00896",
                   "MOD-PM-00897", "MOD-PM-00892", "MOD-PM-00877", "MOD-PM-01060", "MOD-PM-01055",
                   "MOD-PM-00884", "MOD-PM-01056", "MOD-PM-01051", "MOD-PM-01059", "MOD-PM-00881",
                   "MOD-PM-00891", "MOD-PM-00898", "MOD-PM-01052", "MOD-00400", "MOD-PM-00894",
                   "MOD-PM-01053", "MOD-PM-00887", "MOD-PM-00886", "MOD-PM-00879", "MOD-PM-00890",
                   "MOD-PM-00889", "MOD-PM-00899", "MOD-PM-00883", "MOD-PM-00895", "MOD-PM-01057",
                   "MOD-PM-01058", "MOD-PM-00893", "MOD-PM-00878", "MOD-PM-00888", "MOD-PM-00885",
                   "MOD-00398", "MOD-00401", "MOD-00399", "MOD-00397", "MOD-PM-00880")


Sys.setenv(TZ = 'Africa/Accra') #GMT for Ghana


# Define start_date and end_date
start_date <- as.Date("2024-05-25") ## DUE TO TIMEZONES, START DATE NEEDS TO BE 1 DAY BEFORE THE ACTUAL START DATE OF INTEREST

end_date <- as.Date("2024-06-10")

country = "Ghana"

# List of device serial numbers
device_list <- ghana_devices

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
    result_df <- do.call(bind_rows, lapply(result_list, as.data.frame)) %>%
      mutate(monitor = device) %>%
      select(monitor, everything()) %>%
      mutate(timestamp = as.POSIXct(timestamp)) %>% 
      mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M")) %>% 
      mutate(timestamp = lubridate::ymd_hm(timestamp)) %>%
      mutate(local_timestamp = timestamp) #local timestamp is the same as the timestamp variable for Ghana (GMT) 
    
    minutely_df <- data.frame(timestamp = seq.POSIXt(
      as.POSIXct(start_date, tz = "UTC"),
      as.POSIXct(end_date + 1, tz = "UTC"),
      by = "min"
    )) %>%
      mutate(local_timestamp = timestamp)
    
    result_df_full <- full_join(result_df, minutely_df) %>% 
      arrange(timestamp) %>%
      mutate(date = as.Date(local_timestamp)) %>%  
      mutate(hour = hour(ymd_hms(local_timestamp))) %>%
      mutate(monitor = device) %>%
      select(monitor, timestamp, local_timestamp, date, hour, everything()) 
    
    # Store the result for the current device in the combined list
    result_combined[[device]] <- result_df_full
  } else {
    # If there's no data for this device, create an empty dataframe
    minutely_df_empty <- data.frame(timestamp = seq.POSIXt(
      as.POSIXct(start_date, tz = "UTC"),
      as.POSIXct(end_date + 1, tz = "UTC"),
      by = "min"
    )) %>%
      mutate(local_timestamp = timestamp)
    
    minutely_df_empty <- minutely_df_empty %>% mutate(date = as.Date(local_timestamp)) %>%  
      mutate(hour = hour(ymd_hms(local_timestamp))) %>%
      mutate(monitor = device) %>%
      select(monitor, timestamp, local_timestamp, date, hour) 
    
    result_combined[[device]] <- minutely_df_empty
  }
}

# Combine data for all devices into a single data frame
final_result_df <- bind_rows(result_combined)


final_result_df %>%
  filter(date <= as.Date(end_date),
         date > as.Date(start_date)) %>%
  group_by(date, hour, monitor) %>%
  summarise(no_data = sum(is.na(pm25))) %>%
  mutate(missing_hour = ifelse(no_data > 30, 1, 0)) %>% #missing hour classified if more than 30 observations are missing in an hour
  ungroup() %>%
  group_by(date, monitor) %>%
  summarise(hours_missing_day = sum(missing_hour)) %>%
  pivot_wider(names_from = date, values_from = hours_missing_day) %>%
  mutate(country = country) %>%
  select(monitor, country, everything()) %>%
  datatable()




# IDENTIFY UNIQUE COORDINATES AND PREP MAP DATA

final_result_observations <- final_result_df[complete.cases(final_result_df$geo_lat, final_result_df$geo_lon), ]

# Get unique combinations for each device
unique_lat_lon <- final_result_observations %>%
  group_by(monitor) %>%
  distinct(geo_lat, geo_lon) %>%
  
  # ADJUSTMENTS (ASK DJ TO CONFIRM) DUE TO ERRORS IN LONGITUDE
  mutate(geo_lon = case_when(geo_lon == -173058.0000 ~ -1.73058,
                             geo_lon == 1.5990 ~ -1.5990,
                             TRUE ~ geo_lon)) %>%
  filter(geo_lat != 8.05630) %>%
  
  # Convert latitude and longitude to spatial points
  mutate(monitor_points = st_as_sf(., coords = c("geo_lon", "geo_lat"), crs = st_crs(4326)))


# CREATE THE DEVICES POINT MAP DATA

monitor_points <- st_as_sf(unique_lat_lon, coords = c("geo_lon", "geo_lat"), crs = st_crs(4326))


# LOAD IN THE COUNTRY AND REGION MAP DATA

country <- st_read(here("gha_admbnda_gss_20210308_SHP", "gha_admbnda_adm0_gss_20210308.shp"))

regions <- st_read(here("gha_admbnda_gss_20210308_SHP", "gha_admbnda_adm1_gss_20210308.shp")) %>%
  rename(region = ADM1_EN)

bono_east <- regions %>% filter(region == "Bono East")

# just the lat/lon for kintampo
kintampo_sf <- st_as_sf(data.frame(
  location = "Kintampo",
  geo_lon = -1.7296,
  geo_lat = 8.0593
), coords = c("geo_lon", "geo_lat"), crs = st_crs(4326))


# OPTION 1: NO BOUNDING BOX, HIGHLIGHT BONO EAST AND THEN ZOOM IN ON BONO EAST REGION


# Plot the map
ggplot() +
  # Add Ghana regions
  geom_sf(data = regions, fill = ifelse(regions$region == "Bono East", "#f5e493", "lightblue"), color = "black", alpha = 0.5) +
  # Add monitor points with transparency
  geom_sf(data = monitor_points, aes(geometry = geometry), color = "red", alpha = 0.5, size = 2) +
  # Add Ghana country boundary
  geom_sf(data = country, fill = NA, color = "black", size = 1) +
  labs(title = "Monitor Locations in Ghana") +
  theme_bw()



# Plot the map
ggplot() +
  # Add Bono East regions
  geom_sf(data = bono_east, fill = "#f5e493", color = "black", alpha = 0.5) +
  # Add monitor points with transparency
  geom_sf(data = monitor_points, aes(geometry = geometry), color = "red", alpha = 0.3, size = 3) +
  # Add Kintampo marker
  geom_sf(data = kintampo_sf, aes(geometry = geometry), shape = 2, size = 5, color = "#5024b5") +
  # Add Ghana country boundary
  labs(title = "Monitor Locations in Bono East") +
  theme_bw()
  

# OPTION 2: INCLUDE BOUNDING BOX AND ZOOM IN ON MORE SPECIFIC AREAS OF DEPLOYMENT 

# Define the bounding box coordinates
bbox_coords <- matrix(c(-2.2, 7.6, -2.2, 8.8, -1.3, 8.8, -1.3, 7.6, -2.2, 7.6), ncol = 2, byrow = TRUE)
bbox_polygon <- st_polygon(list(bbox_coords))
bbox_sf <- st_sfc(bbox_polygon, crs = st_crs(4326))

# Plot the map with the bounding box
ggplot() +
  # Add Ghana regions
  geom_sf(data = regions, fill = ifelse(regions$region == "Bono East", "#f5e493", "lightblue"), color = "black", alpha = 0.5) +
  # Add monitor points with transparency
  geom_sf(data = monitor_points, aes(geometry = geometry), color = "red", alpha = 0.5, size = 2) +
  # Add bounding box
  geom_sf(data = bbox_sf, fill = NA, color = "blue", lwd = 0.5, linetype = "solid") +
  # Add Ghana country boundary
  geom_sf(data = country, fill = NA, color = "black", size = 1) +
  labs(title = "Monitor Locations in Ghana") +
  theme_bw()



# Plot the zoomed-in map
ggplot() +
  # Add Ghana regions within the bounding box
  geom_sf(data = regions, fill = ifelse(regions$region == "Bono East", "#f5e493", "lightblue"), color = "black", alpha = 0.5) +
  # Add monitor points with transparency within the bounding box
  geom_sf(data = monitor_points, aes(geometry = geometry), color = "red", alpha = 0.5, size = 5) +
  # Add Kintampo marker
  geom_sf(data = kintampo_sf, aes(geometry = geometry), shape = 20, size = 3, color = "black") +
  # Add annotation text
  annotate("text", x = -1.73, y = 8.09, label = "KHRC", color = "black", size = 5) +
  # Zoom in to the bounding box
  coord_sf(xlim = c(-2.2, -1.3), ylim = c(7.6, 8.8), expand = FALSE) +
  labs(title = "Monitor Locations (Zoomed-In)",
       x = "", 
       y = "") +
  theme_bw()


