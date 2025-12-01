# CLEANS AIR QUALITY DATA FROM BAM MONITOR, A FEDERAL EQUIVALENCY METHOD 

library(tidyverse)
library(here)

bam_raw = read_csv("/Users/lewiswhite/CHAP_columbia/QuantAQ_ghana/archive/data_archive/BAM/bam_raw.csv", skip = 4)

bam_clean = bam_raw %>% 
  filter(Status == "00000") %>%
  rename(time = Time,
         pm25 = `ConcHR(ug/m3)`,
         temp = `AT(C)`,
         rh = `RH(%)`) %>%
  mutate(date_hour = lubridate::floor_date(time, unit = "hours")) %>%
  mutate(rh = as.numeric(rh)) %>%
  group_by(date_hour) %>%
  summarize(mean_pm25 = mean(pm25),
            mean_temp = mean(temp),
            mean_rh = mean(rh)) %>%
  ungroup() %>%
  filter(mean_pm25 < 500,
         mean_pm25 > 0) %>%
  select(date_hour, mean_pm25, mean_temp, mean_rh) 
  


