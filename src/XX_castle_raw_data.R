## 1. The goal of this script is to aggregate the disaggregated Castle Lake data
## 2. Clean the aggregated data (remove time periods recording air temp/DO)
## 3. Write a clean, aggregated file for Castle Lake 30m data

#1. Load libraries--------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(forecast)

#2. Import raw data-------------------------------------------------------------

#Import all raw TXT files, eliminating the miniDOT header (first 9 rows)
#Then name columns
df <- list.files(path = here('data/castle_raw'), full.names = TRUE) %>% 
  map_dfr(read_delim, 
          delim = ',', 
          skip = 9,
          col_names = c('unix', 'utc', 'pst', 'battery', 'temperature', 'do', 'do_sat', 'q')
          )

#Create a new df ('cal_raw') that corrects the structure of the df (see: str(df))
#then add lake name and depth
#then reorder columns and eliminate 'chr' columns
cal_raw <- df %>% 
  mutate(
    unix = as.numeric(unix),
    utc = ymd_hms(utc),
    pst = ymd_hms(pst),
    battery_volt = as.numeric(battery),
    temp_c = as.numeric(temperature),
    do_mg_l = as.numeric(do),
    do_sat = as.numeric(do_sat),
    q = as.numeric(q)
  ) %>% 
  mutate(
    lakename = 'castle',
    depth = '30m'
  ) %>% 
  select(lakename, depth, unix, utc, pst, temp_c, do_mg_l, do_sat, q, battery_volt, -c(temperature, do, battery))

head(cal_raw)
str(cal_raw)

ggplot(data = cal_raw)+
  geom_line(aes(x = pst, y = do_mg_l))

#3. Remove outliers in time series----------------------------------------------
#NOTE: Outliers occur when the sensor is removed from the water, but has not
#      yet been turned off. It may also be turned on before being placed in
#      the water.

tsoutliers(cal_raw$do_mg_l)

cal_ts_remove <- cal_raw %>% 
  mutate(
    do_mg_l = tsclean(do_mg_l)
  )

ggplot(data = cal_ts_remove)+
  geom_line(aes(x = pst, y = do_mg_l))
