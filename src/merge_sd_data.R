# Function to merge sd_card data when cloud data is missing
merge_sd_data <- function(pollutant_data, sd_card_data, pollutant) {
  # Left join to keep all cloud data and only add sd_card data where cloud is missing
  merged_data <- pollutant_data %>%
    left_join(sd_card_data, by = c("timestamp" = "timestamp_iso", "monitor" = "monitor"), suffix = c(".raw", ".sd")) %>%
    mutate(
      !!pollutant := coalesce(!!sym(paste0(pollutant, ".raw")), !!sym(paste0(pollutant, ".sd"))),
      source = coalesce(source.raw, source.sd)
    ) %>%
    select(monitor, timestamp, date, hour, all_of(pollutant), source)  # Only keep necessary columns
  
  return(merged_data)
}
