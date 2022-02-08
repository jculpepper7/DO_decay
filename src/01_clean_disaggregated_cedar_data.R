## The purpose of this script is to clean dissagregated data from Cedar Lake
##
## 1. Load libraries
## 2. Read in data
## 3. Remove "bad" data using field notes

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

cedar_3 <- read_csv(here('data/raw/cedar/cedar_hobo_sediment_2021.08.28_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'cedar',
         depth = 'sediment') %>%
  select(lake, date_time, temp_c, depth)

cedar_4 <- read_csv(here('data/raw/cedar/cedar_hobo_sediment_2021.10.14_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'cedar',
         depth = 'sediment') %>%
  select(lake, date_time, temp_c, depth)

# Reading raw miniDOT data

cedar_5 <- read_delim(here('data/raw/cedar/cedar_DOT_2021.08.28_raw.txt'), delim = ',', skip = 7) %>%
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

cedar_6 <- read_delim(here('data/raw/cedar/cedar_DOT_2021.10.14_raw.txt'), delim = ',', skip = 7) %>%
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
  
# 3. Remove "bad" data using field notes----------------------------------------

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
#-------------

cedar_2_cleaned <- cedar_2 %>%
  slice(-c(1:2)) %>% 
  slice(-c(1125:1157)) 
#eliminates times when sensor was turned on, but not yet in water
#eliminates times when sensor was removed from water for download, but was recording air temps and oxygen
#large storm on 6/14 prevented the redeployment of the cedar thermistor chain
#-------------

cedar_3_cleaned <- cedar_3 %>%
  slice(-c(1:41)) %>%
  na.omit()
#this eliminates all times before 11:00am on 2021/06/15
#HOBO was turned off before the recording at 13:00 on 2021/08/28, so no action
#was necessary to filter data after retrieval. 
#-------------

cedar_4_cleaned <- cedar_4 %>%
  slice(-c(1)) %>%
  slice(-c(1126:1158))
#eliminates times when sensor was turned on, but not yet in water
#eliminates times when sensor was removed from water for download, but was recording air temps and oxygen
#large storm on 6/14 prevented the redeployment of the cedar thermistor chain
#-------------

cedar_5_cleaned <- cedar_5 %>%
  slice(-c(1:312)) %>%
  slice(-c(10670:10672))
#this eliminates all times before 11:00am on 2021/06/15
#HOBO was turned off before the recording at 13:00 on 2021/08/28, so no action
#was necessary to filter data after retrieval. 
#-------------

cedar_6_cleaned <- cedar_6 %>%
  slice(-c(1:13)) %>%
  slice(-c(6740:6859))
#eliminates times when sensor was turned on, but not yet in water
#eliminates times when sensor was removed from water for download, but was recording air temps and oxygen
#large storm on 6/14 prevented the redeployment of the cedar thermistor chain
#-------------


# cedar_plt <- ggplot(data = cedar_6_cleaned) +
#   geom_line(aes(x = pst, y = temp_c), size = 1.5)+
#   theme_classic()
# ggplotly(cedar_plt)
#  
  
  
  
  
  





