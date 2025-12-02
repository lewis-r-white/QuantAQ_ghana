# load_and_merge_gas_data.R

# PURPOSE:
#   Load gas data (CO, NO, NO2, O3) from: 1) Cloud parent dataset, and 2) SD card MOD files
#   Harmonize columns and merge sources using:
#   - Prefer cloud values
#   - Fill missing cloud values using SD card data

# OUTPUT:
# A named list of tibbles (one per gas) plus a combined gas_full table.

# DEPENDS ON:
# - load_pollution_datasets.R   (cloud loader)
# - load_sd_gas_data.R          (SD loader)

# USED BY:
#   - gas workflow Rmd

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)

# ------------------------------------------------------------
# Core function
# ------------------------------------------------------------

load_and_merge_gas_data <- function(pollutants,
                                    raw_data,
                                    sd_rds_path = NULL) {
  
  # ----------------------------------------------------------
  # 1. Safe SD loading
  # ----------------------------------------------------------
  if (!is.null(sd_rds_path) && file.exists(sd_rds_path)) {
    message("✔ SD card gas data found — merging cloud + SD")
    
    sd_gas <- readr::read_rds(sd_rds_path) %>%
      mutate(
        timestamp = lubridate::ymd_hms(timestamp_iso, tz = "UTC"),
        date      = as.Date(timestamp),
        hour      = lubridate::hour(timestamp),
        source    = "sd_card"
      ) %>%
      select(-timestamp_iso)  # remove old column so nothing breaks later
    
  } else {
    message("✖ No SD card gas data found — using cloud-only workflow")
    sd_gas <- NULL
  }
  
  # ----------------------------------------------------------
  # 2. Merge cloud + SD for each pollutant
  #    Priority: cloud value, fallback to SD
  # ----------------------------------------------------------
  
  merge_cloud_and_sd <- function(cloud_df, sd_df, gas) {
    
    # cloud only
    if (is.null(sd_df)) {
      return(
        cloud_df %>% mutate(source = "cloud")
      )
    }
    
    # keep relevant SD columns
    sd_small <- sd_df %>%
      select(monitor, timestamp, date, hour, all_of(gas), source)
    
    # full join → allow cloud fallback
    merged <- full_join(
      cloud_df,
      sd_small,
      by = c("monitor", "timestamp", "date", "hour"),
      suffix = c("_cloud", "_sd")
    )
    
    # cloud preferred, SD fallback
    merged <- merged %>%
      mutate(
        !!gas := coalesce(.data[[paste0(gas, "_cloud")]],
                          .data[[paste0(gas, "_sd")]]),
        source = case_when(
          !is.na(.data[[paste0(gas, "_cloud")]]) ~ "cloud",
          !is.na(.data[[paste0(gas, "_sd")]]) ~ "sd_card",
          TRUE ~ NA_character_
        )
      ) %>%
      select(monitor, timestamp, date, hour, all_of(gas), source)
    
    return(merged)
  }
  
  # ----------------------------------------------------------
  # 3. Apply merging to each pollutant
  # raw_data[[p]]$raw_cloud is assumed structure from your PM loader
  # ----------------------------------------------------------
  merged_list <- purrr::map(pollutants, function(p) {
    cloud_df <- raw_data[[p]]$raw_cloud
    merge_cloud_and_sd(cloud_df, sd_gas, p)
  })
  names(merged_list) <- pollutants
  
  # ----------------------------------------------------------
  # 4. Combine into gas_full
  # ----------------------------------------------------------
  gas_full <- merged_list %>%
    imap(function(df, gas) {
      df %>% rename(!!gas := !!sym(gas))
    }) %>%
    reduce(full_join, by = c("monitor", "timestamp", "date", "hour", "source"))
  
  # ----------------------------------------------------------
  # Return
  # ----------------------------------------------------------
  list(
    merged_list = merged_list,
    gas_full = gas_full
  )
}
