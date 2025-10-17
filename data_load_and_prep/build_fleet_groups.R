library(readr)
library(dplyr)
library(stringr)
library(here)


full_sd_card1 <- read_csv(here("data", "all_measurements", "sd", "full_sd_card_2024-08-20_to_2025-01-29.csv")) %>%
  mutate(source = "sd_card")

full_sd_card2 <- read_csv(here("data", "all_measurements", "sd", "full_sd_card_2025-03-01_to_2025-09-01.csv")) %>%
  mutate(source = "sd_card")

full_sd_card <- bind_rows(full_sd_card1, full_sd_card2)

# LOAD CLOUD DATA ---- 

# List of pollutants
pollutants <- c("pm1", "pm25", "pm10")

file_path <- "/Users/lewiswhite/CHAP_columbia/QuantAQ_ghana/data/all_measurements/cloud/ghana_AQ_parent_full_20240816_20250901.csv"

# Load all cloud data in a structured way
raw_data <- lapply(pollutants, function(pollutant) {
  load_pollution_datasets(pollutant, file_path = file_path, file_type = "csv")
})

# Name the list by pollutant
names(raw_data) <- pollutants





merged_results <- merge_cloud_sd_colocation_and_community(pollutants, raw_data, full_sd_card)










# ---- inputs ----
monitor_info_path <- here("data", "monitor_community_info.csv")

seed_pollutant <- "pm25"
mf <- merged_results[[seed_pollutant]]$merged_full
w1 <- config$colocation_windows[[1]]
w2 <- config$colocation_windows[[2]]

# ---- helpers ----
monitors_in_window <- function(df, start, end) {
  df %>%
    filter(date >= as.Date(start), date <= as.Date(end)) %>%
    distinct(monitor) %>%
    arrange(monitor) %>%
    pull(monitor)
}

# ---- original fleet from your community info table ----
mon_info <- readr::read_csv(monitor_info_path, show_col_types = FALSE) %>%
  mutate(monitor = str_trim(monitor)) %>%
  filter(!is.na(monitor), monitor != "")

original_ids_all <- mon_info %>% distinct(monitor) %>% pull(monitor)

# ---- new fleet = seen in window 2 but NOT in original fleet ----
present_w2 <- monitors_in_window(mf, w2$start, w2$end)
new_ids <- setdiff(present_w2, original_ids_all)

# ---- sanity checks & messages ----
if (length(original_ids_all) == 0) warning("No original_fleet monitors found in window1; check dates/IDs.")
if (length(new_ids) == 0) message("No new_fleet monitors detected in window2 (relative to original_fleet).")



# Ensure the reference monitor is in original_fleet
if (!is.null(w2$reference_monitor) && !is.na(w2$reference_monitor)) {
  if (!(w2$reference_monitor %in% original_ids_all)) {
    warning(paste("Reference monitor", w2$reference_monitor, "not found in monitor_community_info.csv"))
  }
}

# ---- write CSVs the rest of your pipeline expects ----
write_csv(tibble(monitor = original_ids_all), here("data", "original_fleet.csv"))
write_csv(tibble(monitor = new_ids), here("data", "new_fleet.csv"))

