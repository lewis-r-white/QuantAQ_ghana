library(tidyverse)
library(here)
library(lubridate)
library(scales)
# Load the SD card data

sd1 <- read_rds("/Users/lewiswhite/CHAP_columbia/QuantAQ_ghana/data/all_measurements/sd/full_sd_card_2023-04-24_to_2024-09-11.rds")
sd2 <- read_rds("/Users/lewiswhite/CHAP_columbia/QuantAQ_ghana/data/all_measurements/sd/full_sd_card_2024-08-20_to_2025-01-29.rds")
sd3 <- read_rds("/Users/lewiswhite/CHAP_columbia/QuantAQ_ghana/data/all_measurements/sd/full_sd_card_2025-03-01_to_2025-09-01.rds")

sd_full <- rbind(sd1, sd2, sd3) %>% 
  select(timestamp_iso, pm25, monitor)


original_fleet <- read_csv("/Users/lewiswhite/CHAP_columbia/QuantAQ_ghana/data/original_fleet.csv")
new_fleet <- read_csv("/Users/lewiswhite/CHAP_columbia/QuantAQ_ghana/data/new_fleet.csv")


fleet_window <- sd_full %>%
  semi_join(original_fleet, by = "monitor") %>%
  mutate(date_hour = floor_date(timestamp_iso, "hour")) %>%
  summarise(
    start = min(date_hour, na.rm = TRUE),
    end   = max(date_hour, na.rm = TRUE)
  )


sd_hour_orig <- sd_full %>%
  semi_join(original_fleet, by = "monitor") %>%
  mutate(
    date_hour   = floor_date(timestamp_iso, "hour"),
    pm_measured = as.integer(!is.na(pm25))
  ) %>%
  group_by(monitor, date_hour) %>%
  summarise(minutes_in_hour = sum(pm_measured), .groups = "drop") %>%
  group_by(monitor) %>%
  complete(
    date_hour = seq(fleet_window$start, fleet_window$end, by = "hour"),
    fill = list(minutes_in_hour = 0)
  ) %>%
  ungroup() %>%
  mutate(
    hour = hour(date_hour),
    hour_complete = minutes_in_hour >= 45
  )


hod_orig <- sd_hour_orig %>%
  group_by(monitor, hour) %>%
  summarise(
    percent_complete = mean(hour_complete),
    .groups = "drop"
  )



ggplot(hod_orig, aes(x = hour, y = monitor, fill = percent_complete)) +
  geom_tile() +
  scale_x_continuous(breaks = 0:23) +
  scale_fill_viridis_c(
    option = "magma",
    limits = c(0, .65),
    oob = squish,
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "SD card completeness by hour of day (Original fleet)",
    subtitle = "Hour considered complete if ≥45 minutes of PM2.5 data",
    x = "Hour of day",
    y = "Monitor",
    fill = "Completeness"
  ) +
  theme_minimal(base_size = 12)


hod_fleet <- hod_orig %>%
  group_by(hour) %>%
  summarise(
    fleet_complete = mean(percent_complete),
    .groups = "drop"
  )

ggplot(hod_fleet, aes(x = hour, y = fleet_complete)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Fleet-average SD card completeness by hour of day (Original Fleet)",
    x = "Hour of day",
    y = "Completeness"
  ) +
  theme_minimal()



## SD card data gaps

# Heat map of each monitor daily completeness. Or weekly # days complete. 

# heat map of each monitor and hour of the day. how often that hour is complete. 

head(sd_full) 

start <- ymd_hms("2023-10-01 00:00:00", tz = "UTC")  
end   <- ymd_hms("2025-10-01 00:00:00", tz = "UTC")   

sd_hour <- sd_full %>%
  mutate(
    date_hour   = floor_date(timestamp_iso, unit = "hour"),
    pm_measured = as.integer(!is.na(pm25))
  ) %>%
  filter(date_hour >= start, date_hour < end) %>%
  group_by(monitor, date_hour) %>%
  summarise(minutes_in_hour = sum(pm_measured), .groups = "drop") %>%
  group_by(monitor) %>%
  complete(
    date_hour = seq(start, end - hours(1), by = "hour"),
    fill = list(minutes_in_hour = 0)
  ) %>%
  ungroup() %>%
  mutate(hour = hour(date_hour),
         hour_complete = ifelse(minutes_in_hour >= 45, 1,0))


hod <- sd_hour %>%
  group_by(monitor, hour) %>%
  summarise(
    n_possible = n(),
    n_complete = sum(hour_complete),
    p_complete = mean(hour_complete),
    .groups = "drop"
  )


ggplot(hod, aes(x = hour, y = monitor, fill = p_complete)) +
  geom_tile() +
  scale_x_continuous(breaks = 0:23) +
  scale_fill_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Hour of day",
    y = "Monitor",
    fill = "Hour complete",
    title = "SD card completeness by hour of day",
    subtitle = "Hour complete if ≥45 minutes of PM2.5 data"
  ) +
  theme_minimal(base_size = 12)
