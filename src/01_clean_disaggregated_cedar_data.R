## The purpose of this script is to clean dissagregated data from Cedar Lake
##
## 1. Load libraries
## 2. Read in data
## 3. Remove "bad" data using field notes
## 4. Write cleaned data to processed folder

# 1. Load libraries-------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(plotly)

# 2.Read data-------------------------------------------------------------------

#reading in HOBO U22 logger data 

cedar_1 <- read_csv(here('data/raw/cedar/cedar_hobo_2m_2021.08.28_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'cedar',
         depth = '2m') %>%
  select(lake, date_time, temp_c, depth)

cedar_2 <- read_csv(here('data/raw/cedar/cedar_hobo_2m_2021.10.14_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'cedar',
         depth = '2m') %>%
  select(lake, date_time, temp_c, depth)

cedar_3 <- read_csv(here('data/raw/cedar/cedar_hobo_2m_2022.06.11_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'cedar',
         depth = '2m') %>%
  select(lake, date_time, temp_c, depth)
  
cedar_4 <- read_csv(here('data/raw/cedar/cedar_hobo_sediment_2021.08.28_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'cedar',
         depth = 'sediment') %>%
  select(lake, date_time, temp_c, depth)

cedar_5 <- read_csv(here('data/raw/cedar/cedar_hobo_sediment_2021.10.14_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'cedar',
         depth = 'sediment') %>%
  select(lake, date_time, temp_c, depth)

cedar_6 <- read_csv(here('data/raw/cedar/cedar_hobo_sediment_2022.06.11_raw.csv'), skip = 1) %>% 
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'cedar',
         depth = 'sediment') %>%
  select(lake, date_time, temp_c, depth)  

# Reading raw miniDOT data

cedar_7 <- read_delim(here('data/raw/cedar/cedar_DOT_2021.08.28_raw.txt'), delim = ',', skip = 7) %>%
  clean_names() %>%#clean names from miniDOT software default
  slice(-c(1)) %>%
  select(1,2,3,5,6,7) %>%
  rename(
    unix = unix_timestamp, 
    utc = utc_date_time,
    pst = pacific_standard_time,
    temp_c = temperature,
    do_mg_l = dissolved_oxygen,
    do_sat = dissolved_oxygen_saturation
  ) %>%
  mutate(
    unix = as.numeric(unix),
    utc = ymd_hms(utc),
    pst = ymd_hms(pst),
    temp_c = as.numeric(temp_c),
    do_mg_l = as.numeric(do_mg_l),
    do_sat = as.numeric(do_sat)
  )

cedar_8 <- read_delim(here('data/raw/cedar/cedar_DOT_2021.10.14_raw.txt'), delim = ',', skip = 7) %>%
  clean_names() %>%#clean names from miniDOT software default
  slice(-c(1)) %>%
  select(1,2,3,5,6,7) %>%
  rename(
    unix = unix_timestamp, 
    utc = utc_date_time,
    pst = pacific_standard_time,
    temp_c = temperature,
    do_mg_l = dissolved_oxygen,
    do_sat = dissolved_oxygen_saturation
  ) %>%
  mutate(
    unix = as.numeric(unix),
    utc = ymd_hms(utc),
    pst = ymd_hms(pst),
    temp_c = as.numeric(temp_c),
    do_mg_l = as.numeric(do_mg_l),
    do_sat = as.numeric(do_sat)
  )

cedar_9 <- read_delim(here('data/raw/cedar/cedar_DOT_2022.06.12_raw.txt'), delim = ',', skip = 7) %>%
  clean_names() %>%#clean names from miniDOT software default
  slice(-c(1)) %>%
  select(1,2,3,5,6,7) %>%
  rename(
    unix = unix_timestamp, 
    utc = utc_date_time,
    pst = pacific_standard_time,
    temp_c = temperature,
    do_mg_l = dissolved_oxygen,
    do_sat = dissolved_oxygen_saturation
  ) %>%
  mutate(
    unix = as.numeric(unix),
    utc = ymd_hms(utc),
    pst = ymd_hms(pst),
    temp_c = as.numeric(temp_c),
    do_mg_l = as.numeric(do_mg_l),
    do_sat = as.numeric(do_sat)
  )

# Reading light pendant data

cedar_10 <- read_csv(here('data/raw/cedar/cedar_light_pendant_2022.06.11_raw.csv'), skip = 1) %>% 
  mutate(
    lake = c('cedar'),
    depth = c('2.5m')
  ) %>% 
  select(lake, date_time = 2, temp_c = 3, light_intensity_lux = 4, depth) %>% 
  mutate(
    date_time = mdy_hms(date_time)
  )

cedar_11 <- read_csv(here('data/raw/cedar/cedar_light_pendant_80cm_2022.06.11_raw.csv'), skip = 1) %>% 
  mutate(
    lake = c('lake'),
    depth = c('0.8m')
  ) %>% 
  select(lake, date_time = 2, temp_c = 3, light_intensity_lux = 4, depth) %>% 
  mutate(
    date_time = mdy_hms(date_time)
  )

cedar_12 <- read_csv(here('data/raw/cedar/cedar_light_pendant_150cm_2022.06.11_raw.csv'), skip = 1) %>% 
  mutate(
    lake = c('lake'),
    depth = c('1.5m')
  ) %>% 
  select(lake, date_time = 2, temp_c = 3, light_intensity_lux = 4, depth) %>% 
  mutate(
    date_time = mdy_hms(date_time)
  )

# 3. Clean inappropriate data using field notes----------------------------------------

#This code removes data that recorded when the sensors were out of the water
#e.g. taken out for calibration and to check equipment function
#also removed one hour from the miniDOTs when they are placed back into the 
#water as they cool down in the water (see README_cedar.TXT)

cedar_1_cleaned <- cedar_1 %>%
  slice(-c(1:41)) %>%
  na.omit()
#this eliminates all times before 11:00am on 2021/06/15
#HOBO was turned off before the recording at 13:00 on 2021/08/28, so no action
#was necessary to filter data after retrieval. 

cedar_2_cleaned <- cedar_2 %>%
  slice(-c(1:2)) %>% 
  slice(-c(1125:1157)) 
#eliminates times when sensor was turned on, but not yet in water
#eliminates times when sensor was removed from water for download, but was recording air temps and oxygen
#large storm on 6/14 prevented the redeployment of the cedar thermistor chain

cedar_3_cleaned <- cedar_3 %>% 
  slice(-c(5689:5765)) %>% 
  na.omit()
#eliminates times after 2022.06.11 15:00:00, as the sensor was removed from the water
#but not shut off for download.

cedar_4_cleaned <- cedar_4 %>%
  slice(-c(1:41)) %>%
  na.omit()
#this eliminates all times before 11:00am on 2021/06/15
#HOBO was turned off before the recording at 13:00 on 2021/08/28, so no action
#was necessary to filter data after retrieval. 

cedar_5_cleaned <- cedar_5 %>%
  slice(-c(1)) %>%
  slice(-c(1126:1158))
#eliminates times when sensor was turned on, but not yet in water
#eliminates times when sensor was removed from water for download, but was recording air temps and oxygen
#large storm on 6/14 prevented the redeployment of the cedar thermistor chain

cedar_6_cleaned <- cedar_6 %>% 
  slice(-c(5689:5765)) %>% 
  na.omit()
#eliminates times after 2022.06.11 15:00:00, as the sensor was removed from the water
#but not shut off for download.

cedar_7_cleaned <- cedar_7 %>%
  slice(-c(1:312)) %>%
  slice(-c(10670:10672))
#this eliminates all times before 11:00am on 2021/06/15
#HOBO was turned off before the recording at 13:00 on 2021/08/28, so no action
#was necessary to filter data after retrieval. 

cedar_8_cleaned <- cedar_8 %>%
  slice(-c(1:13)) %>%
  slice(-c(6740:6859))
#eliminates times when sensor was turned on, but not yet in water
#eliminates times when sensor was removed from water for download, but was recording air temps and oxygen
#large storm on 6/14 prevented the redeployment of the cedar thermistor chain, but sensor remained on

cedar_9_cleaned <- cedar_9 %>% 
  slice(-c(1:57)) %>% 
  slice(-c(34070:34281))

cedar_10_cleaned <- cedar_10 %>% 
  slice(-c(5689:5765)) %>% 
  na.omit()
#eliminates times after 2022.06.11 15:00:00, as the sensor was removed from the water
#but not shut off for download.

cedar_11_cleaned <- cedar_11 %>% 
  slice(-c(5689:5765)) %>% 
  slice(-c(1:19)) %>% 
  na.omit()
#eliminates the first 19 observations before sensor was submerged
#eliminates times after 2022.06.11 15:00:00, as the sensor was removed from the water
#but not shut off for download.

cedar_12_cleaned <- cedar_12 %>% 
  slice(-c(5689:5765)) %>% 
  slice(-c(1:19)) %>% 
  na.omit()
#eliminates the first 19 observations before sensor was submerged
#eliminates times after 2022.06.11 15:00:00, as the sensor was removed from the water
#but not shut off for download.

# 4. Write cleaned data to processed folder-------------------------------------

write_csv(cedar_1_cleaned, here('data/processed/cedar/cedar_hobo_2m_2021.08.28_cleaned.csv'))
write_csv(cedar_2_cleaned, here('data/processed/cedar/cedar_hobo_2m_2021.10.14_cleaned.csv'))
write_csv(cedar_3_cleaned, here('data/processed/cedar/cedar_hobo_2m_2022.06.11_cleaned.csv'))
write_csv(cedar_4_cleaned, here('data/processed/cedar/cedar_hobo_sediment_2021.08.28_cleaned.csv'))
write_csv(cedar_5_cleaned, here('data/processed/cedar/cedar_hobo_sediment_2021.10.14_cleaned.csv'))
write_csv(cedar_6_cleaned, here('data/processed/cedar/cedar_hobo_sediment_2022.06.11_cleaned.csv'))
write_csv(cedar_7_cleaned, here('data/processed/cedar/cedar_DOT_2021.08.28_cleaned.csv'))
write_csv(cedar_8_cleaned, here('data/processed/cedar/cedar_DOT_2021.10.14_cleaned.csv'))
write_csv(cedar_9_cleaned, here('data/processed/cedar/cedar_DOT_2022.06.11_cleaned.csv'))
write_csv(cedar_10_cleaned, here('data/processed/cedar/cedar_light_pendant_2022.06.11_cleaned.csv'))
write_csv(cedar_11_cleaned, here('data/processed/cedar/cedar_light_pendant_80cm_2022.06.11_cleaned.csv'))
write_csv(cedar_12_cleaned, here('data/processed/cedar/cedar_light_pendant_150cm_2022.06.11_cleaned.csv'))
  
  
  
  





