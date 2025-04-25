library(tidyverse)
library(here)
library(here) # file path org
library(lubridate)# working with dates
library(tictoc) # timing
library(DT) # datatables
library(purrr) # applying functions across df
library(tidyverse) # data cleaning and plotting
library(data.table) 
library(sf) # spatial data 
library(knitr)
library(modelsummary) # table of regressions
library(spdep)
library(gstat)
library(units) 
library(broom)


# source functions to merge in the SD card data for cases when server data is missing 
source(here("src", "load_pollution_datasets.R"))
source(here("src", "merge_sd_data.R"))
source(here("src", "process_multiple_pollutants.R"))


# CLOUD ----

# List of pollutants you want to process
pollutants <- c("ws", "wd")

raw_data <- list()

for (pollutant in pollutants) {
  load_pollution_datasets(pollutant, file_path = "/Users/lewiswhite/CHAP_columbia/QuantAQ/data/cloud/ghana_AQ_parent_full_20240925.csv", file_type = "csv")
  # Store the raw data
  raw_data[[paste0(pollutant, "_raw")]] <- get(paste0(pollutant, "_raw"))
}



#SD CARD ----

# List all MOD files with full path
mod_files <- list.files(
  path = "/Users/lewiswhite/CHAP_columbia/QuantAQ/data/SD_data/MOD", 
  full.names = TRUE, 
  recursive = FALSE, 
  pattern = "MOD-00.*\\.csv"
)

# Function to read, select columns, and add monitor name for MOD files
read_and_select_mod <- function(file) {
  # Extract the monitor name from the file name
  monitor_name <- str_extract(basename(file), "MOD-\\d+")
  
  read_csv(file) %>%
    select(timestamp_iso, ws, wd) %>%
    mutate(monitor = monitor_name)
}

# Read all MOD files and combine them
MOD_sd_card <- mod_files %>%
  map_dfr(read_and_select_mod)

mod_sd_full <- MOD_sd_card %>%
  filter(timestamp_iso > as.Date("2023-08-15")) %>%
  mutate(source = "sd_card")





## MERGE THE DATA ----


## MERGE THE SD DATA WITH THE SERVER DATA ---- 

pollutants <- c("ws", "wd") 

# Process the pollutants
wind_results <- process_multiple_pollutants(pollutants, raw_data, mod_sd_full)


ws_merged <- wind_results$ws$merged

wd_merged <- wind_results$wd$merged


wind_full <- full_join(ws_merged, wd_merged) %>%
  select(monitor, timestamp, date, hour, ws, wd, source)

write_rds(wind_full, here("data", "wind", "wind_full_20241118.rds"))

colocation_wind <- wind_full %>%
  filter(timestamp >= as.Date("2023-08-16") & timestamp <= as.Date("2023-09-21"))

write_rds(colocation_wind, here("data", "wind", "colocation_wind.rds"))

