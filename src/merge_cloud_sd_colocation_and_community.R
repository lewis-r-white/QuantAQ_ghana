source(here("src", "merge_sd_data.R"))

merge_cloud_sd_colocation_and_community <- function(pollutants, raw_data, sd_card_data) {
  results <- list()
  
  for (pollutant in pollutants) {
    cloud_full     <- raw_data[[pollutant]]$raw_cloud
    colocation_raw <- raw_data[[pollutant]]$colocation_raw_cloud
    community_raw  <- raw_data[[pollutant]]$community_raw_cloud
    
    merged_full      <- merge_sd_data(cloud_full,     sd_card_data, pollutant)
    merged_coloc     <- merge_sd_data(colocation_raw, sd_card_data, pollutant)
    merged_community <- merge_sd_data(community_raw,  sd_card_data, pollutant)
    
    results[[pollutant]] <- list(
      merged_full      = merged_full,
      merged_colocation = merged_coloc,
      merged_community  = merged_community
    )
  }
  
  return(results)
}
