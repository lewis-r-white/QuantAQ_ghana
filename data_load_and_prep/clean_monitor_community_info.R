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



location_columns <- c("monitor", "geo_lat", "geo_lon")

files <- c(
  "/Users/lewiswhite/CHAP_columbia/QuantAQ_ghana/data/all_measurements/cloud/ghana_AQ_parent_full_20230815-20240925.csv",
  "/Users/lewiswhite/CHAP_columbia/QuantAQ_ghana/data/all_measurements/cloud/ghana_AQ_parent_full_20240816_20250901.csv",
  "/Users/lewiswhite/CHAP_columbia/QuantAQ_ghana/data/all_measurements/cloud/ghana_AQ_parent_full_20250831_20260315.csv"
)

summarize_location_file <- function(file) {
  dt <- fread(file, select = location_columns, showProgress = TRUE)
  
  dt <- dt[!is.na(geo_lat) & !is.na(geo_lon)]
  
  # manual adjustments
  dt[, geo_lon := fifelse(geo_lon == -173058.0000, -1.73058, geo_lon)]
  dt[, geo_lon := fifelse(geo_lon == 1.5990, -1.5990, geo_lon)]
  
  # optional: round to reduce tiny GPS jitter
  dt[, geo_lat := round(geo_lat, 4)]
  dt[, geo_lon := round(geo_lon, 4)]
  
  # restrict to plausible Ghana bounds
  dt <- dt[geo_lat >= 4 & geo_lat <= 12 & geo_lon >= -4 & geo_lon <= 2]
  
  out <- dt[, .N, by = .(monitor, geo_lat, geo_lon)]
  
  rm(dt)
  gc()
  
  out
}

location_counts_list <- lapply(files, summarize_location_file)
location_counts <- rbindlist(location_counts_list)

location_counts_total <- location_counts[
  , .(n = sum(N)),
  by = .(monitor, geo_lat, geo_lon)
]

setorder(location_counts_total, monitor, -n)

best_location_per_monitor <- location_counts_total[
  , .SD[1],
  by = monitor
]

monitor_points <- st_as_sf(
  best_location_per_monitor,
  coords = c("geo_lon", "geo_lat"),
  crs = st_crs(4326)
) %>%
  mutate(
    mod_pm = str_detect(monitor, "MOD-PM"),
    monitor_type = case_when(
      mod_pm ~ "MOD-PM",
      TRUE ~ "MODULAIR"
    )
  )

prisma_populations <- readxl::read_xlsx(
  here("archive", "data_archive", "modulair_communities.xlsx")
) %>%
  mutate(
    village = sub(" \\(.*$", "", village),
    village = str_to_title(village),
    village = case_when(
      village == "Asantekwa" ~ "Asantekwaa",
      village == "Korawura Akura" ~ "Kurawura Akura",
      village == "Soronuase" ~ "Sorunuase",
      TRUE ~ village
    )
  ) %>%
  group_by(village) %>%
  summarise(
    households = sum(households, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  )

monitor_names_full <- bind_rows(monitor_names, monitor_names_new) %>%
  mutate(
    location = gsub(" GH", "", location),
    locations = if_else(
      grepl("Kintampo", location),
      paste(description, location, sep = ", "),
      location
    )
  )

monitor_locations_full <- left_join(
  monitor_names_full,
  best_location_per_monitor %>% select(-n),
  by = "monitor"
)

monitor_community_populations <- left_join(
  monitor_names_full,
  prisma_populations,
  by = c("description" = "village")
)

full_monitor_info <- monitor_locations_full %>%
  select(monitor, geo_lat, geo_lon) %>%
  left_join(monitor_community_populations, by = "monitor") %>%
  mutate(
    monitor_type = ifelse(str_detect(monitor, "MOD-PM"), "MOD-PM", "MOD"),
    coord_source = case_when(
      !is.na(geo_lat) & !is.na(geo_lon) ~ "monitor_data",
      TRUE ~ NA_character_
    )
  ) %>%
  select(monitor, monitor_type, description, location, locations, geo_lat, geo_lon, households, population, coord_source)



## Making some manual adjustments 

replacement_map <- tibble::tribble(
  ~description, ~base_description,
  "Kadelso (new)", "Kadelso",
  "Portor (new)", "Portor",
  "Kwabia (new)", "Kwabia",
  "Asantekwaa (new)", "Asantekwaa",
  "Apesika (new)", "Apesika",
  "Apesika (old)", "Apesika",
  "Dwenewoho (new)", "Dwenewoho",
  "Pramposo (new)", "Pramposo",
  "Gulumpe (new)", "Gulumpe",
  "Kawampe (new)", "Kawampe",
  "New Longoro (new)", "New Longoro",
  "Ntankro (new)", "Ntankro",
  "Alhassan Akura (new)", "Alhassan Akura",
  "Atta Akura (replacement)", "Atta Akura",
  "Replaced Habitat sensor", "Habitat",
  "Replaced Babatokuma sensor", "Babator",
  "Sorunuase downwind", "Sorunuase"
)

site_lookup <- full_monitor_info %>%
  distinct(description, .keep_all = TRUE) %>%
  select(
    description,
    ref_geo_lat = geo_lat,
    ref_geo_lon = geo_lon,
    ref_households = households,
    ref_population = population
  )


full_monitor_info <- full_monitor_info %>%
  left_join(replacement_map, by = "description") %>%
  mutate(base_description = coalesce(base_description, description)) %>%
  left_join(site_lookup, by = c("base_description" = "description")) %>%
  mutate(
    households = coalesce(households, ref_households),
    population = coalesce(population, ref_population),
    geo_lat = coalesce(geo_lat, ref_geo_lat),
    geo_lon = coalesce(geo_lon, ref_geo_lon),
    coord_source = case_when(
      is.na(coord_source) & !is.na(geo_lat) & !is.na(geo_lon) &
        description != base_description ~ "matched_to_same_site",
      TRUE ~ coord_source
    )
  ) %>%
  select(-base_description, -starts_with("ref_"))

full_monitor_info <- full_monitor_info %>%
  mutate(
    geo_lat = case_when(
      monitor == "MOD-PM-01072" & is.na(geo_lat) ~ 8.06,
      monitor %in% c("MOD-PM-01081", "MOD-PM-01085") & is.na(geo_lat) ~ 8.06,
      TRUE ~ geo_lat
    ),
    geo_lon = case_when(
      monitor == "MOD-PM-01072" & is.na(geo_lon) ~ -1.73,
      monitor %in% c("MOD-PM-01081", "MOD-PM-01085") & is.na(geo_lon) ~ -1.73,
      TRUE ~ geo_lon
    ),
    coord_source = case_when(
      monitor == "MOD-PM-01072" & is.na(coord_source) ~ "manual_match_to_01081",
      monitor %in% c("MOD-PM-01081", "MOD-PM-01085") & is.na(coord_source) ~ "manual_kintampo_fallback",
      TRUE ~ coord_source
    )
  )

# write_csv(full_monitor_info, here("data", "monitor_community_info_20260324.csv"))

