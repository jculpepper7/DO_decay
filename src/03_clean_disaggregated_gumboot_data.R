## The purpose of this script is to clean dissagregated data from gumboot Lake
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

gumboot_1 <- read_csv(here('data/raw/gumboot/gumboot_hobo_2m_2021.10.14_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'gumboot',
         depth = '2m') %>%
  select(lake, date_time, temp_c, depth) %>%
  na.omit()

gumboot_2 <- read_csv(here('data/raw/gumboot/gumboot_hobo_sediment_2021.10.14_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'gumboot',
         depth = 'sediment') %>%
  select(lake, date_time, temp_c, depth) %>%
  na.omit()

# Reading raw miniDOT data

gumboot_3 <- read_delim(here('data/raw/gumboot/gumboot_miniDOT_2021.10.14_raw.txt'), delim = ',', skip = 7) %>%
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
#water as they cool down in the water (see README_gumboot.TXT)

gumboot_1_cleaned <- gumboot_1 %>%
  slice(-c(1:41)) %>%
  na.omit()
#this eliminates all times before 11:00am on 2021/06/15
#HOBO was turned off before the recording at 13:00 on 2021/08/28, so no action
#was necessary to filter data after retrieval. 
#-------------

gumboot_2_cleaned <- gumboot_2 %>%
  slice(-c(1:2)) %>% 
  slice(-c(1125:1157)) 
#eliminates times when sensor was turned on, but not yet in water
#eliminates times when sensor was removed from water for download, but was recording air temps and oxygen
#large storm on 6/14 prevented the redeployment of the gumboot thermistor chain
#-------------

gumboot_3_cleaned <- gumboot_3 %>%
  slice(-c(1:41)) %>%
  na.omit()
#this eliminates all times before 11:00am on 2021/06/15
#HOBO was turned off before the recording at 13:00 on 2021/08/28, so no action
#was necessary to filter data after retrieval. 
#-------------

# 4. Write cleaned data to processed folder-------------------------------------

write_csv(gumboot_1_cleaned, here('data/processed/gumboot/gumboot_hobo_2m_2021.08.28_cleaned.csv'))
write_csv(gumboot_2_cleaned, here('data/processed/gumboot/gumboot_hobo_2m_2021.10.14_cleaned.csv'))
write_csv(gumboot_3_cleaned, here('data/processed/gumboot/gumboot_hobo_sediment_2021.08.28_cleaned.csv'))











