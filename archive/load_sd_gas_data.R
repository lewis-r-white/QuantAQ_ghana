# load_sd_gas_data.R

# Load SD card gas data for MOD devices.

# This function:
# Accepts a vector of file paths to SD card CSVs
# Reads only gas-relevant columns (timestamp, co, no, no2, o3, monitor)
# Adds `source = "sd_card"` to the output
# Returns an EMPTY tibble with the correct structure if:
# - no paths are provided
# - files do not exist
# - all reads fail

# This ensures downstream merging functions NEVER break due to missing SD card data.
# Used in both co-location and community processing.

load_sd_gas_data <- function(sd_paths) {
  if (length(sd_paths) == 0) {
    return(tibble(
      timestamp = as.POSIXct(character()),
      monitor   = character(),
      co = double(),
      no = double(),
      no2 = double(),
      o3 = double(),
      date = as.Date(character()),
      hour = integer(),
      source = character()
    ))
  }
  
  sd_list <- lapply(sd_paths, function(file) {
    monitor_name <- stringr::str_extract(basename(file), "MOD-\\d+")
    
    tryCatch({
      readr::read_csv(file, show_col_types = FALSE) %>%
        select(timestamp_iso, co, no, no2, o3) %>%
        rename(timestamp = timestamp_iso) %>%
        mutate(
          monitor = monitor_name,
          date    = as.Date(timestamp),
          hour    = lubridate::hour(timestamp),
          source  = "sd_card"
        )
    }, error = function(e) NULL)
  })
  
  sd_list <- sd_list[!vapply(sd_list, is.null, logical(1))]
  dplyr::bind_rows(sd_list)
}