library(here) # file path org
library(lubridate)# working with dates
library(tictoc) # timing
library(DT) # datatables
library(purrr) # applying functions across df
library(tidyverse) # data cleaning and plotting
library(data.table) 
library(sf) # spatial data 
library(readxl)
library(units) 
library(gridExtra)
library(broom)
library(Metrics) 
library(kableExtra) # table creation


# LOAD LOCATION DATA FROM MONITORS ----

# identify the location columns
location_columns <- c("monitor", "geo_lat", "geo_lon", "date")

# read in the monitor location data using fread to handle large dataset
location_data <- fread("/Users/lewiswhite/CHAP_columbia/QuantAQ/data/all_measurements/cloud/ghana_AQ_parent_full_20230815-20240925.csv", 
                       select = location_columns, 
                       showProgress = TRUE)

# remove missing values and filter to recent monitor locations 
location_complete <- location_data[complete.cases(location_data$geo_lat, location_data$geo_lon), ] %>%
  filter(date > as.Date("2024-03-01"))

# Get unique combinations for each device
monitor_locations <- location_complete %>%
  group_by(monitor) %>%
  distinct(geo_lat, geo_lon) %>%
  
  # ADJUSTMENTS (ASK DJ TO CONFIRM) DUE TO ERRORS IN LONGITUDE
  mutate(geo_lon = case_when(geo_lon == -173058.0000 ~ -1.73058,
                             geo_lon == 1.5990 ~ -1.5990,
                             TRUE ~ geo_lon)) %>%
  filter(geo_lat != 8.05630)

# create spatial feature dataset of monitor points
monitor_points <- st_as_sf(monitor_locations, coords = c("geo_lon", "geo_lat"), crs = st_crs(4326)) %>%
  mutate(mod_pm = str_detect(monitor, "MOD-PM")) %>%
  mutate(monitor_type = case_when(mod_pm == TRUE ~ "MOD-PM",
                                  mod_pm == FALSE ~ "MODULAIR"))






# LOAD POPULATION DATA FROM KHRC

prisma_populations <- readxl::read_xlsx(here("archive", "data_archive", "modulair_communities.xlsx")) 

prisma_populations <- prisma_populations %>%
  mutate(village = sub(" \\(.*$", "", village)) %>%
  mutate(village = str_to_title(village)) %>%
  group_by(village) %>%
  summarise(households = sum(households),
            population = sum(population)) %>%
  mutate(
    village = str_to_title(sub(" \\(.*$", "", village)),
    village = case_when(
      village == "Asantekwa" ~ "Asantekwaa",
      village == "Korawura Akura" ~ "Kurawura Akura",
      village == "Soronuase" ~ "Sorunuase",
      TRUE ~ village
    )
  )


monitor_names <- tibble::tribble(
  ~monitor, ~description, ~location,
  "MOD-PM-00887", "Babator", "Babatokuma GH",
  "MOD-PM-00881", "Mo-line", "Kintampo GH",
  "MOD-PM-01052", "Kokuma", "Kokuma GH",
  "MOD-PM-01055", "Krabonso", "Krabonso GH",
  "MOD-PM-00893", "Jato Akura", "Jato Akura GH",
  "MOD-PM-01060", "Kintampo Central College Areas", "Kintampo GH",
  "MOD-PM-00899", "Nante", "Nante GH",
  "MOD-00399", "Dwenewoho", "Dwenewoho GH",
  "MOD-PM-01057", "Sorunuase", "Sorunuase GH",
  "MOD-PM-00877", "Pamdu", "Pamdu GH",
  "MOD-PM-00898", "Anokyekrom", "Anokyekrom GH",
  "MOD-PM-00900", "Kawampe", "Kawampe GH",
  "MOD-PM-00883", "Portor", "Portor GH",
  "MOD-00398", "Kurawura Akura", "Kurawura Akura GH",
  "MOD-PM-00884", "KHRC Residency", "Kintampo GH",
  "MOD-PM-01056", "Gulumpe", "Gulumpe GH",
  "MOD-PM-00879", "Dawadawa", "Dawadawa GH",
  "MOD-PM-00897", "Pramposo", "Pramposo GH",
  "MOD-PM-01053", "Anyima", "Anyima GH",
  "MOD-PM-00880", "Kadelso", "Kadelso GH",
  "MOD-PM-01051", "Weila", "Weila GH",
  "MOD-PM-01058", "Krutakyi", "Krutakyi GH",
  "MOD-PM-00895", "Ntankro", "Kintampo GH",
  "MOD-PM-00889", "Beposo", "Beposo GH",
  "MOD-PM-00882", "Apaaso", "Kintampo GH",
  "MOD-PM-01059", "Paninamisa", "Paninamisa GH",
  "MOD-00397", "Apesika", "Apesika GH",
  "MOD-PM-00878", "Jema Pentecost", "Jema GH",
  "MOD-00401", "New Longoro", "New Longoro GH",
  "MOD-PM-00891", "Habitat", "Kintampo GH",
  "MOD-PM-00885", "Kwabia", "Kwabia GH",
  "MOD-PM-00890", "Akora", "Akora GH",
  "MOD-PM-00876", "Amoma", "Amoma GH",
  "MOD-PM-00888", "Nante Zongo", "Nante Zongo GH",
  "MOD-PM-00894", "Alhassan Akura", "Alhassan Akura GH",
  "MOD-00400", "Kintampo-PTC Area (Magazine)", "Kintampo GH",
  "MOD-PM-01054", "Ampoma", "Ampoma GH",
  "MOD-PM-00896", "Asantekwaa", "Asantekwaa GH",
  "MOD-PM-00886", "Jema Zongo", "Jema GH",
  "MOD-PM-00892", "Atta Akura", "Atta Akura GH"
)

monitor_names <- monitor_names %>%
  mutate(location = gsub(" GH", "", location)) %>%
  mutate(locations = if_else(grepl("Kintampo", location),
                             paste(description, location, sep = ", "),
                             location))


monitor_locations_full <- left_join(monitor_names, monitor_locations)

monitor_community_populations <- full_join(monitor_names, prisma_populations, by = c("location" = "village"))


full_monitor_info <- monitor_locations_full %>% 
  select(description, geo_lat, geo_lon) %>%
  full_join(monitor_community_populations, by = "description") %>%
  mutate(monitor_type = ifelse(str_detect(monitor, "MOD-PM"), "MOD-PM", "MOD")) %>%
  select(monitor, monitor_type, description, location, locations, geo_lat, geo_lon, households, population)

#write_csv(full_monitor_info, here("data", "monitor_community_info.csv"))
