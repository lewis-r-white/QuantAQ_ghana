# Wrap the function `get_data_by_date` within `possibly` to handle potential errors
get_data_safe <- possibly(get_data_by_date, otherwise = NULL)


# Define a function `get_full_data_test` that takes start date, end date, and device names as inputs
get_full_data_test <- function(start_date, end_date, device_names) {
  
  # Convert start_date and end_date to POSIXt objects for date manipulation
  start_date <- as.POSIXct(start_date)
  end_date <- as.POSIXct(end_date)
  
  # Use `map_df` to iterate over each device in `device_names` and retrieve data for each day
  result_df <- map_df(device_names, function(device) {
    
    # Use `map` to iterate through each day between `start_date` and `end_date`
    result_list <- map(seq(start_date, end_date, by = "days"), function(date) {
      formatted_date <- format(date, "%Y-%m-%d") #format date
      get_data_safe(sn = device, date = formatted_date) # Call the `get_data_safe` function with device serial number (`sn`) and formatted date
    }) %>%
      
      # Filter out NULL elements (empty lists) or lists with length 0 (no data)
      discard(~ is.null(.x) || length(.x) == 0) %>%
      # Convert each element to a data frame
      map(as.data.frame)
    # Combine the list of data frames into a single data frame for each device 
    result_df_device <- do.call(rbind, result_list) %>%
      
      # Code to fill in the missing time gaps
      mutate(timestamp = as.POSIXct(timestamp)) %>% #reformat for time manipulation
      mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M")) %>% #remove seconds from timestamp
      mutate(timestamp2 = as.POSIXct(timestamp)) %>% #reformat for time manipulation
      complete(timestamp2 = seq.POSIXt(as.POSIXct(start_date), as.POSIXct(end_date), by = "min")) %>% #timestamp2 contains every minute intervals between the specified start date and end date
      mutate(date = as.Date(timestamp2)) %>% #extract date from timestamp2
      mutate(hour = hour(ymd_hms(timestamp2))) %>% #get the hour number too
      mutate(monitor = device) %>% #add the monitor name to the data frame
      select(monitor, everything()) #reorder the dataframe so the monitor is first (for table aesthetic)
    
    return(result_df_device)
  })
  
  return(result_df)
}

# # Example usage
# start_date <- as.Date("2023-10-15")
# end_date <- as.Date("2023-11-12")
# device_names <- c("MOD-PM-00847", "other_device_name")
# result <- get_data_for_devices(start_date, end_date, device_names)