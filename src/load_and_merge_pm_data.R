# Provides functions to load SD card PM data, load cloud PM data (via load_pollution_datasets), and merge them into unified PM time series
# per pollutant. Handles missing SD files and keeps source labels.

# Dependencies: load_pollution_datasets()

library(tidyverse)
library(purrr)

## Empty SD card data set so merge function still works if no SD card data

empty_sd_card_template <- function() {
  tibble::tibble(
    timestamp_iso = as.POSIXct(character()),
    monitor       = character(),
    pm1           = double(),
    pm25          = double(),
    pm10          = double(),
    co            = double(),
    no            = double(),
    no2           = double(),
    o3            = double(),
    source        = character()
  )
}


# LOAD SD CARD DATA -----
load_sd_card_data <- function(sd_paths) {
  if (length(sd_paths) == 0) {
    return(empty_sd_card_template())
  }
  
  sd_list <- lapply(sd_paths, function(p) {
    if (!file.exists(p)) {
      message("SD card file not found: ", p, " — skipping.")
      return(NULL)
    }
    
    tryCatch({
      readr::read_csv(p, show_col_types = FALSE) %>%
        mutate(source = "sd_card")
    }, error = function(e) {
      message("Failed to read SD card file: ", p, " — skipping.")
      NULL
    })
  })
  
  # Remove any NULLs
  sd_list <- sd_list[!vapply(sd_list, is.null, logical(1))]
  
  # If EVERYTHING failed → return empty template
  if (length(sd_list) == 0) {
    return(empty_sd_card_template())
  }
  
  bind_rows(sd_list)
}


# LOAD CLOUD DATA -------
# load_cloud_data <- function(pollutants, cloud_path) {
#   raw <- lapply(pollutants, function(p) {
#     load_pollution_datasets(
#       pollutant = p,
#       file_path = cloud_path,
#       file_type = "csv"
#     )
#   })
#   names(raw) <- pollutants
#   raw
# }

load_cloud_data <- function(pollutants, cloud_path) {
  raw <- lapply(pollutants, function(p) {
    out <- load_pollution_datasets(
      pollutant = p,
      file_path = cloud_path,
      file_type = "csv"
    )
    
    out$raw_cloud <- out$raw_cloud %>%
      mutate(source = "cloud")
    
    out
  })
  
  names(raw) <- pollutants
  raw
}



# MERGE POLLUTANTS: per-pollutant merge
merge_sd_data_one <- function(pollutant_data, sd_card_data, pollutant) {
  
  # If sd_card_data is empty → use only cloud data
  if (nrow(sd_card_data) == 0) {
    return(
      pollutant_data %>%
        mutate(
          source = if ("source" %in% names(.)) source else "cloud"
        ) %>%
        select(monitor, timestamp, date, hour, !!rlang::sym(pollutant), source)
    )
  }
  
  # Ensure each has a source column
  if (!"source" %in% names(pollutant_data))
    pollutant_data <- pollutant_data %>% mutate(source = NA_character_)
  
  if (!"source" %in% names(sd_card_data))
    sd_card_data <- sd_card_data %>% mutate(source = "sd_card")
  
  # joined <- left_join(
  #   pollutant_data,
  #   sd_card_data,
  #   by = c("timestamp" = "timestamp_iso", "monitor" = "monitor"),
  #   suffix = c(".cloud", ".sd")
  
  joined <- full_join(
    pollutant_data,
    sd_card_data,
    by = c("timestamp" = "timestamp_iso", "monitor" = "monitor"),
    suffix = c(".cloud", ".sd")
  )
  
  pol_cloud <- paste0(pollutant, ".cloud")
  pol_sd    <- paste0(pollutant, ".sd")
  
  # joined %>%
  #   mutate(
  #     !!rlang::sym(pollutant) := coalesce(.data[[pol_cloud]], .data[[pol_sd]]),
  #     source = coalesce(
  #       .data[["source.cloud"]],
  #       .data[["source.sd"]],
  #       .data[["source"]]
  #     )
  #   ) %>%
  #   select(monitor, timestamp, date, hour, !!rlang::sym(pollutant), source)
  
  joined %>%
    mutate(
      !!rlang::sym(pollutant) := coalesce(.data[[pol_cloud]], .data[[pol_sd]]),
      source = coalesce(
        if ("source.cloud" %in% names(.)) .data[["source.cloud"]] else NA_character_,
        if ("source.sd" %in% names(.))    .data[["source.sd"]]    else NA_character_,
        if ("source" %in% names(.))       .data[["source"]]       else NA_character_
      )
    ) %>%
    select(monitor, timestamp, date, hour, !!rlang::sym(pollutant), source)
  
}


# MERGE ALL POLLUTANTS ----
merge_sd_cloud_all <- function(pollutants, raw_data, sd_card_data) {
  setNames(lapply(pollutants, function(p) {
    merge_sd_data_one(raw_data[[p]]$raw_cloud, sd_card_data, p)
  }), pollutants)
}


## CONVENIENCE WRAPPER TO LOAD AND MERGE ALL IN ONE LINE
load_and_merge_all_pm <- function(sd_paths, cloud_path, pollutants) {
  sd <- load_sd_card_data(sd_paths)
  cloud <- load_cloud_data(pollutants, cloud_path)
  merged <- merge_sd_cloud_all(pollutants, cloud, sd)
  
  list(
    sd = sd,
    cloud = cloud,
    merged = merged
  )
}
