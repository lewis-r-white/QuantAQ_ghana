###############################################################################
## data preparation in response to OpenAQ request
## Format datetime into ISO 8601 format

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(here)
library(data.table)

### 2017 data
BAM_2017 <- read_csv(here("ethiopia_work", "BAM_Apr16th_30th_2017.csv"))

print(BAM_2017$datetime)

## Timestamps are already in Addis local time, so time values should remain the same, and only the timezone metadata needs updating:

BAM_2017_corrected_metadata <- BAM_2017 %>%
  mutate(
    # Ensure datetime is correctly parsed
    datetime_parsed = mdy_hm(datetime),
    
    # Assign the correct timezone (without shifting time)
    datetime_addis_corrected = force_tz(datetime_parsed, tzone = "Africa/Addis_Ababa"),
    
    # Format as ISO-8601 with explicit UTC offset (+03:00)
    datetime_ISO = sub("(\\d{2})(\\d{2})$", "\\1:\\2", format(datetime_addis_corrected, "%Y-%m-%dT%H:%M:%S%z"))
  )

print(head(BAM_2017_corrected_metadata$datetime_ISO))

# Write csv output dataset
#write_csv(BAM_corrected_metadata, "outputfilepath/outputfile.csv")


### 2023 data
BAM_2023 <- read_csv(here("ethiopia_work", "BAM_Aug1st_15th_2023.csv"))

BAM_2023_corrected_metadata <- BAM_2023 %>%
  mutate(
    # Ensure datetime is correctly parsed
    datetime_parsed = mdy_hm(datetime),
    
    # Assign the correct timezone (without shifting time)
    datetime_addis_corrected = force_tz(datetime_parsed, tzone = "Africa/Addis_Ababa"),
    
    # Format as ISO-8601 with explicit UTC offset (+03:00)
    datetime_ISO = sub("(\\d{2})(\\d{2})$", "\\1:\\2", format(datetime_addis_corrected, "%Y-%m-%dT%H:%M:%S%z"))
  )

print(head(BAM_2023_corrected_metadata$datetime_ISO))

# Write csv output dataset
#write_csv(BAM_2023_corrected_metadata, "outputfilepath/outputfile.csv")


