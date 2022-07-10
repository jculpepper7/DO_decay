## The goal of this script is to
##
## 1. Clean disaggregated  Weatherhawk data
## 2. Aggregate Castle Lake meteorlogical data from WeatherHawk

# 1. Load libraries--------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(padr)
library(plotly)

# 2. Import weather data---------------------------------------------

# 2a. Import raw weatherhawk data----
weatherhawk_raw <- list.files(
  path = here('data/met_data/weatherhawk'),
  pattern = '*.csv',
  full.names = TRUE
) %>% 
  map_dfr(~read_csv(., col_types = cols(.default = "c")))

#NOTE: upon inspection, the weatherhawk data changes date_time style 
#      (from ymd_hms to dmy_hms) which causes a failure to parse all
#      date_time data. Therefore, below I extract the varied date_time
#      data and correct it. Then I recombine the data into a single tibble.

# 2b. Extract problem rows (9398:16598) and amend data----
weatherhawk_raw_1 <- weatherhawk_raw %>% 
  slice(c(9398:16598)) %>% 
  clean_names() %>% 
  select(-1) %>% 
  mutate(
    date_time = mdy_hm(date_time),
    air_temp_avg = as.numeric(air_temp_avg),
    air_temp_min = as.numeric(air_temp_min),
    air_temp_min_time = mdy_hm(air_temp_min_time),
    air_temp_max = as.numeric(air_temp_max),
    air_temp_max_time = mdy_hm(air_temp_max_time),
    humidity = as.numeric(humidity),
    barometer = as.numeric(barometer),
    battery = as.numeric(battery),
    min_battery = as.numeric(min_battery),
    e_to = as.numeric(e_to),
    rain_yearly = as.numeric(rain_yearly),
    solar_avg = as.numeric(solar_avg),
    wind_speed_avg = as.numeric(wind_speed_avg),
    wind_speed_max = as.numeric(wind_speed_max),
    wind_speed_max_time = mdy_hm(wind_speed_max_time),
    wind_speed_avg_1 = as.numeric(wind_speed_avg_1),
    wind_direction = as.numeric(wind_direction)
  ) %>% 
  select(-wind_speed_avg_1)

#Take a look
str(weatherhawk_raw_1)  

# 2c. Remove problem rows and amend data----
weatherhawk_raw_2 <- weatherhawk_raw %>% 
  slice(-c(9398:16598)) %>% 
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
    min_battery = as.numeric(min_battery),
    e_to = as.numeric(e_to),
    rain_yearly = as.numeric(rain_yearly),
    solar_avg = as.numeric(solar_avg),
    wind_speed_avg = as.numeric(wind_speed_avg),
    wind_speed_max = as.numeric(wind_speed_max),
    wind_speed_max_time = ymd_hms(wind_speed_max_time),
    wind_speed_avg_1 = as.numeric(wind_speed_avg_1),
    wind_direction = as.numeric(wind_direction)
  ) %>% 
  select(-wind_speed_avg_1)

#Take a look
str(weatherhawk_raw_2)  

# 2d. Access 2019 air temp data----
#NOTE: Issues with 2019 raw data from Castle Lake data repo. 
#      Obtained air temperature and solar radiation data for missing segments

weatherhawk_2019 <- read_csv(here('data/met_data/weatherhawk_new/cal_weatherhawk_2019.csv')) %>% 
  clean_names() %>% 
  select(-record_id) %>% 
  mutate(
    date_time = mdy_hm(date_time)
  )

# 3. Combine and clean raw weatherhawk data-------------------------------------

# 3a. Combine data----
weatherhawk <- bind_rows(weatherhawk_raw_1, weatherhawk_raw_2, weatherhawk_2019)

#write_csv(weatherhawk, here('data/met_data/weatherhawk_new/cal_weatherhawk_2020.csv'))

# 3b. Take daily averages for the important variables----
weatherhawk_avg <- weatherhawk %>% 
  select(1,2,3,5,7,8,12,13,14) %>%
  mutate(
    year = year(date_time),
    month = month(date_time),
    day = day(date_time)
  ) %>% 
  group_by(year, month, day) %>% 
  summarise(
    air_temp_avg = mean(air_temp_avg),
    air_min_avg = mean(air_temp_min),
    air_max_avg = mean(air_temp_max),
    humidity_avg = mean(humidity),
    barometer_avg = mean(barometer),
    rain_yearly = mean(rain_yearly),
    solar_avg = mean(solar_avg),
    wind_speed_avg = mean(wind_speed_avg)
  ) %>% 
  mutate(
    date_time = make_date(year = year, month = month, day = day)
  ) %>% 
  ungroup() %>% 
  select(12, 4:11) %>% 
  pad()

#write_csv(weatherhawk_avg, here('data/met_data/weatherhawk/weatherhawk_avg_2017_2022.csv'))

ggplot(data = weatherhawk_avg) +
  #geom_line(aes(x = date_time, y = air_temp_avg), size = 1.5)+
  geom_line(aes(x = date_time, y = solar_avg))
  #geom_line(aes(x = date_time, y = humidity_avg))
  

  
  




  
  
  

