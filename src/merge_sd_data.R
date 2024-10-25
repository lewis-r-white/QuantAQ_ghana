library(dplyr)

# Function to merge data for a given pollutant
merge_pollutant_data <- function(pm_raw, sd_card_data, pollutant) {
  
  # Ensure that the timestamp columns are in POSIXct format
  pm_raw <- pm_raw %>%
    mutate(timestamp = as.POSIXct(timestamp, format="%Y-%m-%d %H:%M:%S"))
  
  sd_card_data <- sd_card_data %>%
    mutate(timestamp_iso = as.POSIXct(timestamp_iso, format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(source = "sd_card")
  
  # Join the datasets
  merged_data <- pm_raw %>%
    left_join(sd_card_data, by = c("timestamp" = "timestamp_iso", "monitor" = "monitor"), suffix = c(".raw", ".sd"))
  
  # Use coalesce to fill NA values in the pollutant from sd_card_data
  merged_data <- merged_data %>%
    mutate(!!pollutant := coalesce(!!sym(paste0(pollutant, ".raw")), !!sym(paste0(pollutant, ".sd")))) %>%
    select(monitor, timestamp, date, hour, all_of(pollutant), source.raw, source.sd) %>%
    mutate(source = coalesce(source.raw, source.sd)) %>%  # Coalesce the source column
    select(-source.raw, -source.sd)  # Drop the intermediate source columns
  
  return(merged_data)
}

