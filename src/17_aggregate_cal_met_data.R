## The goal of this script is to
##
## 1. Clean disaggregated  Weatherhawk data
## 2. Aggregate Castle Lake meteorlogical data from WeatherHawk

# 1. Load libraries--------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(here)

# 2. Import weather data---------------------------------------------

weatherhawk <- list.files(
  path = here('data/met_data/weatherhawk'),
  pattern = '*.csv',
  full.names = TRUE
) %>% 
  map_dfr(~read_csv(., col_types = cols(.default = "c")))

weatherhawk <- weatherhawk %>% 
  clean_names() %>% 
  select(-1) %>% 
  mutate(
    date_time = ymd_hms(date_time),
    air_temp_avg = as.numeric(air_temp_avg),
    air_temp_min = as.numeric(air_temp_min),
    air_temp_min_time = ymd_hms(air_temp_min_time),
    air_temp_max = as.numeric(air_temp_max),
    air_temp_max_time = ymd_hms(air_temp_max_time),
    humidity = as.numeric(humidity),
    barometer = as.numeric(barometer),
    battery = as.numeric(battery),
    battery_min = as.numeric(battery_min),
    e_to = as.numeric(e_to),
    rain_yearly = as.numeric(rain_yearly),
    solar_avg = as.numeric(solar_avg),
    wind_speed_avg_15 = as.numeric(wind_speed_avg_15),
    wind_speed_max = as.numeric(wind_speed_avg_15),
    wind_speed_max_time = ymd_hms(wind_speed_max_time),
    wind_speed_avg_18 = as.numeric(wind_speed_avg_18),
    wind_direction = as.numeric(wind_direction)
  )
  
  
  
  
  
  
  
  
  

