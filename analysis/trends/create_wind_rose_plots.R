# TO CREATE WIND ROSE PLOTS

library(tidyverse)
library(here)
library(openair)
library(data.table)
library(gridExtra)
library(gridGraphics)
library(ggplotify)
library(patchwork)



# LOAD STATION DATA ----

metall <- read_rds(here("data", "weather", "external", "metall_2017-2025.rds")) %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "GMT"))

zentra <- read_rds(here("data", "weather", "external", "zentra_2024.rds"))


## LOAD MOD DATA ----

mod_00077 <- read_rds(here("data", "all_measurements", "cloud", "mod_00077_20240901-20241118.rds")) %>%
  select(monitor, timestamp, date, hour, met_ws, met_wd) %>%
  rename(ws = met_ws,
         wd = met_wd) # NOTE: does not appear to have wind speed data


mod_wind <- read_rds(here("data", "weather", "merged", "wind_full_20230815-20240820.rds"))

wind <- mod_wind %>%
  filter(!str_detect(monitor, "MOD-PM")) 

mod_wind_clean <- wind %>%
  filter(date > as.Date("2024-01-30"))

# CHECK MISSINGNESS OF DATA 

wind_completeness_data <- mod_wind_clean %>%
  mutate(week = floor_date(date, unit = "week")) %>%
  group_by(monitor, week) %>%
  summarize(complete_ws = sum(!is.na(ws)),
            complete_wd = sum(!is.na(wd))) %>%
  ungroup() %>%
  mutate(mins_in_week = 10080) %>%
  mutate(rate_ws_complete = complete_ws/mins_in_week,
         rate_wd_complete = complete_wd/mins_in_week)

wind_completeness_data %>%
  ggplot(aes(x = week, y = rate_ws_complete)) +
  geom_col() +
  facet_wrap(~monitor) +
  theme_minimal() +
  labs(x = "Week of Data",
       y = "Rate of Data Completeness",
       title = "Monitor Completeness Rate of Wind Data")

## CLEAN THE DATA 

metall_clean <- metall %>%
  filter(DateTime > as.POSIXct("2024-01-30", tz = "GMT"),
         DateTime < as.POSIXct("2024-08-17", tz = "GMT"),
         )

zentra_clean <- zentra %>%
  filter(DateTime > as.POSIXct("2024-01-30", tz = "GMT"),
         DateTime > as.POSIXct("2024-08-17", tz = "GMT"))



## PLOT THE WIND ROSES ----

# WEATHER DATA 
metall_clean %>%
  filter(WindSpeed_ms > 0, WindSpeed_ms < 40) %>% # filter wind speeds over 30 m/s as this shouldn't exist in GH
  windRose(ws = "WindSpeed_ms", 
           wd = "WindDirection_A", 
           angle = 18,
           paddle = F, 
           key.position = "right",
           main = "Wind Rose for Metall")

zentra_clean %>%
  filter(m.s.Wind.Speed > 0, m.s.Wind.Speed < 40) %>%
  windRose(ws = "m.s.Wind.Speed", 
           wd = "degrees.Wind.Direction", 
           angle = 18,
           paddle = F, 
           key.position = "right",
           main = "Wind Rose for Zentra")


# MOD DEVICE DATA 
mod_wind_clean %>% 
  filter(monitor == "MOD-00397") %>%
  filter(ws > 0, ws < 40) %>%
  windRose(ws = "ws", 
           wd = "wd", 
           angle = 18,
           paddle = F, 
           key.position = "right",
           main = "Wind Rose for MOD-00397")

mod_wind_clean %>% 
  filter(monitor == "MOD-00398") %>%
  filter(ws > 0, ws < 40) %>%
  windRose(ws = "ws", 
           wd = "wd", 
           angle = 18,
           paddle = F, 
           key.position = "right",
           main = "Wind Rose for MOD-00398")

mod_wind_clean %>% 
  filter(monitor == "MOD-00399") %>%
  filter(ws > 0, ws < 40) %>%
  windRose(ws = "ws", 
           wd = "wd", 
           angle = 18,
           paddle = F, 
           key.position = "right",
           main = "Wind Rose for MOD-00399")

mod_wind_clean %>% 
  filter(monitor == "MOD-00400") %>%
  filter(wd >= 0, wd <= 360) %>%
  filter(ws > 0, ws < 40) %>%
  windRose(ws = "ws", 
           wd = "wd", 
           angle = 18,
           paddle = F, 
           key.position = "right",
           main = "Wind Rose for MOD-00400")

mod_wind_clean %>% 
  filter(monitor == "MOD-00401") %>%
  filter(ws > 0, ws < 40) %>%
  windRose(ws = "ws", 
           wd = "wd", 
           angle = 18,
           paddle = F, 
           key.position = "right",
           main = "Wind Rose for MOD-00401")




### MAKE PLOTS BASED ON OVERLAPPING TIME ---- 

generate_wind_rose <- function(monitor_name, start_date, end_date, mod_data, zentra_data) {
  # Filter mod_wind_clean data
  mod_filtered <- mod_data %>%
    filter(monitor_name == monitor) %>%
    filter(date > as.Date(start_date),
           date < as.Date(end_date))
  
  # Filter zentra data
  zentra_filtered <- zentra_data %>%
    filter(DateTime > as.POSIXct(start_date, tz = "GMT"),
           DateTime < as.POSIXct(end_date, tz = "GMT"))
  
  # Filter invalid wind directions
  mod_filtered <- mod_filtered %>% filter(wd >= 0 & wd <= 360)
  zentra_filtered <- zentra_filtered %>% filter(degrees.Wind.Direction >= 0 & degrees.Wind.Direction <= 360)
  
  # Create wind rose for mod data and capture as grob
  mod_grob <- grid.grabExpr({
    mod_filtered %>%
      filter(ws > 0, ws < 40) %>%
      windRose(ws = "ws", 
               wd = "wd", 
               angle = 18,
               paddle = FALSE, 
               key.position = "right",
               main = paste("Wind Rose for", monitor_name))
  }, wrap.grobs = TRUE)
  
  # Create wind rose for zentra data and capture as grob
  zentra_grob <- grid.grabExpr({
    zentra_filtered %>%
      filter(m.s.Wind.Speed > 0, m.s.Wind.Speed < 40) %>%
      windRose(ws = "m.s.Wind.Speed", 
               wd = "degrees.Wind.Direction", 
               angle = 18,
               paddle = FALSE, 
               key.position = "right",
               main = paste("Wind Rose for Zentra"))
  }, wrap.grobs = TRUE)
  
  # Combine the two grobs using patchwork
  patchwork_plot <- (ggplotify::as.ggplot(mod_grob) | ggplotify::as.ggplot(zentra_grob))
  
  # Print the combined plot
  print(patchwork_plot)
}


monitors <- list(
  list(monitor = "MOD-00397", start_date = "2024-03-03", end_date = "2024-06-02"),
  list(monitor = "MOD-00398", start_date = "2024-06-02", end_date = "2024-08-05"),
  list(monitor = "MOD-00399", start_date = "2024-03-03", end_date = "2024-06-02"),
  list(monitor = "MOD-00400", start_date = "2024-03-03", end_date = "2024-05-19"),
  list(monitor = "MOD-00401", start_date = "2024-03-24", end_date = "2024-05-26")
)


for (params in monitors) {
  generate_wind_rose(
    monitor = params$monitor,
    start_date = params$start_date,
    end_date = params$end_date,
    mod_data = mod_wind_clean,
    zentra_data = zentra
  )
}



## Checking that code above works by doing it manually 

MOD00397_wind <- mod_wind_clean %>% 
  filter(monitor == "MOD-00397") %>%
  filter(date > as.Date("2024-03-03"),
         date < as.Date("2024-06-02"))

zentra_clean_00397 <- zentra %>%
  filter(DateTime > as.POSIXct("2024-03-03", tz = "GMT"),
         DateTime < as.POSIXct("2024-06-02", tz = "GMT"))

MOD00397_wind %>% 
  filter(ws > 0, ws < 40) %>%
  windRose(ws = "ws", 
           wd = "wd", 
           angle = 18,
           paddle = F, 
           key.position = "right",
           main = "Wind Rose for MOD-00397")

zentra_clean_00397 %>%
  filter(m.s.Wind.Speed > 0, m.s.Wind.Speed < 40) %>%
  windRose(ws = "m.s.Wind.Speed", 
           wd = "degrees.Wind.Direction", 
           angle = 18,
           paddle = F, 
           key.position = "right",
           main = "Wind Rose for Zentra")




