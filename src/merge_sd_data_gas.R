# merge_sd_data_gas.R

# PURPOSE:
#   Merge cloud and SD card gas data for a single pollutant.
#   Cloud values take priority; SD fills gaps.

# FUNCTIONS PROVIDED:
#   - merge_sd_data_gas_one()
#   - merge_cloud_sd_gas()

# USED BY:
#   - gas_calibration.Rmd workflow

library(dplyr)
library(rlang)


# Merge a single pollutant
merge_sd_data_gas_one <- function(cloud_df, sd_df, pollutant) {
  
  merged <- cloud_df %>%
    left_join(
      sd_df,
      by = c("timestamp" = "timestamp_iso", "monitor"),
      suffix = c(".cloud", ".sd")
    ) %>%
    mutate(
      # cloud preferred, SD fallback
      !!pollutant := coalesce(
        .data[[paste0(pollutant, ".cloud")]],
        .data[[paste0(pollutant, ".sd")]]
      ),
      source = coalesce(source.cloud, source.sd)
    ) %>%
    select(monitor, timestamp, date, hour, all_of(pollutant), source)
  
  return(merged)
}


# Merge all gases in one call
merge_cloud_sd_gas <- function(pollutants, raw_data, sd_df) {
  
  setNames(
    lapply(pollutants, function(p) {
      cloud_full <- raw_data[[p]]$raw_cloud
      merge_sd_data_gas_one(cloud_full, sd_df, p)
    }),
    pollutants
  )
}
