library(tidyverse)
library(here)
library(lubridate)
library(bitops)
library(gt)

#### Description of cleaning

# The GRIMM device stores each environmental sensor reading (temperature, RH, pressure, input3) as a 10-bit ADC value split across two places:
# - Ue1–Ue4 contain the high 8 bits
# - UeL contains the low 2 bits for all four sensors packed together

# To reconstruct the full 10-bit value (0–1023):
# - Extract the low 2 bits for each sensor from UeL
# - shift the high 8 bits left by 2 positions (equivalent to multiplying by 4)
# - Add the low bits back in

# Once the 10-bit ADC value is reconstructed, convert it to voltage:
# - voltage = ADC × (10 / 1024)

# Finally, apply the instrument-specific calibration parameters from the \$(1..4) and \*(1..4) lines in the file:
# 1. **Temp:** factor = 20; offset = 2.000
# 2. **RH:** factor = 20; offset = 0
# 3. **Pressure:** factor = 176.21145; offset = 0.275

# This produces properly calibrated °C, %RH, and hPa values.


# factors and offsets pulled from device metadata (NEED TO UPDATE IF DEVICE METADATA IS DIFFERENT)
temp_factor = 20
temp_offset = 2

RH_factor = 20
RH_offset = 0

pressure_factor = 176.21145
pressure_offset = 0.275


## Create function to loop through raw GRIMM files 
load_grimm_file <- function(file_path) {
  
  # --- read entire file ---
  lines <- read_lines(file_path)
  
  # --- extract P and N lines ---
  p_lines <- lines[grepl("^\\s*P", lines)]
  n_lines <- lines[grepl("^\\s*N_", lines)]
  
  p_lines_clean <- str_remove(str_trim(p_lines), "^P\\s+") 
  n_lines_clean <- str_remove(str_trim(n_lines), "^N_\\s+")
  
  # --- parse P lines ---
  p_data <- read_table(p_lines_clean,
                       col_names = FALSE,
                       col_types = cols(.default = "d"),
                       skip_empty_rows = TRUE)
  
  # --- name columns ---
  names(p_data)[1:5]  <- c("year", "month", "day", "hour", "minute")
  names(p_data)[6:10] <- c("location", "gravimetric_factor", "error_code", 
                           "battery_voltage", "valve_current")
  names(p_data)[11:16] <- c("UeL","Ue4","Ue3","Ue2","Ue1","lv")
  
  # --- parse N lines ---
  n_data <- read_table(n_lines_clean,
                       col_names = c("pm10","pm25"),
                       skip_empty_rows = TRUE)
  
  # --- match P rows to N rows safely ---
  line_types <- tibble(
    line = lines,
    type = case_when(
      grepl("^\\s*P", line)  ~ "P",
      grepl("^\\s*N_", line) ~ "N",
      TRUE                   ~ "OTHER"
    )
  )
  
  p_idx <- which(line_types$type == "P")
  n_idx <- which(line_types$type == "N")
  
  # match only the P rows that have N rows
  valid_p_idx <- p_idx[seq_along(n_idx)]
  
  p_data <- p_data %>% 
    dplyr::slice(match(valid_p_idx, p_idx))
  
  # ---------- BIT RECONSTRUCTION + CALIBRATION ----------
  temp_factor = 20;      temp_offset = 2.000
  RH_factor   = 20;      RH_offset   = 0
  pressure_factor = 176.21145; pressure_offset = 0.275
  
  p_data_clean <- p_data %>%
    mutate(
      # LOW 2 BITS PER SENSOR
      lo1 = bitAnd(UeL, 3L),
      lo2 = bitAnd(bitShiftR(UeL, 2L), 3L),
      lo3 = bitAnd(bitShiftR(UeL, 4L), 3L),
      lo4 = bitAnd(bitShiftR(UeL, 6L), 3L),
      
      # RECONSTRUCT 10-BIT ADC VALUES
      adc1 = Ue1 * 4L + lo1,
      adc2 = Ue2 * 4L + lo2,
      adc3 = Ue3 * 4L + lo3,
      adc4 = Ue4 * 4L + lo4,
      
      # ADC → VOLTS
      volt1 = adc1 * 10 / 1024,
      volt2 = adc2 * 10 / 1024,
      volt4 = adc4 * 10 / 1024,
      
      # APPLY CALIBRATION
      temp_c       = (volt1 - temp_offset) * temp_factor,
      rh_percent   = (volt2 - RH_offset) * RH_factor,
      pressure_hpa = (volt4 - pressure_offset) * pressure_factor
    )
  
  # --- combine calibrated P with PM rows ---
  combined <- bind_cols(p_data_clean, n_data) %>%
    mutate(datetime = make_datetime(year + 2000, month, day, hour, minute),
           year_hour = floor_date(datetime, unit = "hours"),
           date = as.Date(year_hour),
           file = basename(file_path))
  
  return(combined)
}


folder <- "/Users/lewiswhite/CHAP_columbia/QuantAQ_ghana/data/FEMs/GRIMM/raw/GRIMM_20260123_20260126"

files <- list.files(folder, pattern = "\\.TXT$", full.names = TRUE)


grimm_all <- map_dfr(files, load_grimm_file) 

## put in place 45 minute requirements even though should all be complete
grimm_hourly <- grimm_all %>%
  group_by(year_hour, date) %>%
  count(!is.na(pm25) | !is.na(temp_c)) %>%
  left_join(grimm_all, by = c("year_hour", "date")) %>%
  filter(n >= 45) %>%
  summarize(grimm_temp = mean(temp_c, na.rm = TRUE),
            grimm_humidity = mean(rh_percent, na.rm = TRUE),
            grimm_pressure = mean(pressure_hpa, na.rm = TRUE),
            grimm_pm25 = mean(pm25, na.rm = TRUE),
            grimm_pm10 = mean(pm10, na.rm = TRUE))

write_csv(grimm_hourly, here("data", "FEMs", "GRIMM", "clean", "grimm_hourly_20250811-20251008.csv"))


grimm_daily <- grimm_hourly  %>%
  group_by(date) %>%
  count(!is.na(grimm_temp) | !is.na(grimm_pm25)) %>%
  left_join(grimm_hourly, by = "date") %>%
  filter(n >= 18) %>%
  summarize(grimm_temp = mean(grimm_temp, na.rm = TRUE),
            grimm_humidity = mean(grimm_humidity, na.rm = TRUE),
            grimm_pressure = mean(grimm_pressure, na.rm = TRUE),
            grimm_pm25 = mean(grimm_pm25, na.rm = TRUE),
            grimm_pm10 = mean(grimm_pm10, na.rm = TRUE))
  


write_csv(grimm_daily, here("data", "FEMs", "GRIMM", "clean", "grimm_daily_20250811-20251008.csv"))



