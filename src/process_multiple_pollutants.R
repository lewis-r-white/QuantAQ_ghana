# source(here("src", "merge_sd_data.R"))
# 
# 
# process_multiple_pollutants <- function(pollutants, raw_data, sd_card_data) {
#   results <- list()
#   
#   for (pollutant in pollutants) {
#     # Pull out the raw cloud data for this pollutant
#     cloud_data <- raw_data[[pollutant]]$raw_cloud
#     
#     # Merge with SD card data
#     merged_data <- merge_sd_data(cloud_data, sd_card_data, pollutant)
#     results[[pollutant]] <- merged_data
#   }
#   
#   return(results)
# }




# # Function to process multiple pollutants
# process_multiple_pollutants <- function(pollutants, raw_data, sd_card_data) {
#   results <- list()
#   
#   for (pollutant in pollutants) {
#     # Access the appropriate raw data element by name
#     pollutant_data <- raw_data[[paste0(pollutant, "_raw")]]
#     
#     # Merge the data for the current pollutant
#     merged_data <- merge_sd_data(pollutant_data, sd_card_data, pollutant)
#     
#     # Summarize the data for the current pollutant
#     summarized_data <- summarize_pollution_times(merged_data, pollutant)
#     
#     # Store the merged and summarized data in the results list
#     results[[pollutant]] <- list(merged = merged_data, summarized = summarized_data)
#   }
#   
#   return(results)
# }

# # Function to merge cloud and SD data for multiple pollutants
# process_multiple_pollutants <- function(pollutants, raw_data, sd_card_data) {
#   results <- list()
#   
#   for (pollutant in pollutants) {
#     pollutant_data <- raw_data[[paste0(pollutant, "_raw")]]
#     
#     # Merge cloud and SD card data
#     merged_data <- merge_sd_data(pollutant_data, sd_card_data, pollutant)
#     
#     # Store only the merged data
#     results[[pollutant]] <- merged_data
#   }
#   
#   return(results)
# }
