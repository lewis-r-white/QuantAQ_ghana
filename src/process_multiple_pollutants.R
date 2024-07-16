# Function to process multiple pollutants
process_multiple_pollutants <- function(pollutants, raw_data, sd_card_data) {
  results <- list()
  
  for (pollutant in pollutants) {
    # Merge the data for the current pollutant
    merged_data <- merge_pollutant_data(raw_data[[paste0(pollutant, "_raw")]], sd_card_data, pollutant)
    
    # Summarize the data for the current pollutant
    result <- summarize_pollution_times(merged_data, pollutant)
    
    # Store the results in the list
    results[[pollutant]] <- result
  }
  
  return(results)
}