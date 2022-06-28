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

gumboot_2 <- read_csv(here('data/raw/gumboot/gumboot_hobo_2m_2022.06.11_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'gumboot',
         depth = '2m') %>%
  select(lake, date_time, temp_c, depth) %>%
  na.omit()

gumboot_3 <- read_csv(here('data/raw/gumboot/gumboot_hobo_sediment_2021.10.14_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'gumboot',
         depth = 'sediment') %>%
  select(lake, date_time, temp_c, depth) %>%
  na.omit()

gumboot_4 <- read_csv(here('data/raw/gumboot/gumboot_hobo_sediment_2022.06.11_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'gumboot',
         depth = 'sediment') %>%
  select(lake, date_time, temp_c, depth) %>%
  na.omit()

# Reading raw miniDOT data

gumboot_5 <- read_delim(here('data/raw/gumboot/gumboot_miniDOT_2021.10.14_raw.txt'), delim = ',', skip = 7) %>%
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

gumboot_6 <- read_delim(here('data/raw/gumboot/gumboot_miniDOT_2022.06.12_raw.txt'), delim = ',', skip = 7) %>%
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

#Read light pendant data

gumboot_7 <- read_csv(here('data/raw/gumboot/gumboot_light_pendant_90cm_2022.06.11_raw.csv'), skip = 1) %>% 
  mutate(
    lake = c('lake'),
    depth = c('0.9m')
  ) %>% 
  select(lake, date_time = 2, temp_c = 3, light_intensity_lux = 4, depth) %>% 
  mutate(
    date_time = mdy_hms(date_time)
  )

gumboot_8 <- read_csv(here('data/raw/gumboot/gumboot_light_pendant_160cm_2022.06.11_raw.csv'), skip = 1) %>% 
  mutate(
    lake = c('lake'),
    depth = c('1.6m')
  ) %>% 
  select(lake, date_time = 2, temp_c = 3, light_intensity_lux = 4, depth) %>% 
  mutate(
    date_time = mdy_hms(date_time)
  )

# 3. Remove "bad" data using field notes----------------------------------------

#This code removes data that recorded when the sensors were out of the water
#e.g. taken out for calibration and to check equipment function
#also removed one hour from the miniDOTs when they are placed back into the 
#water as they cool down in the water (see README_gumboot.TXT)

gumboot_1_cleaned <- gumboot_1 %>%
  slice(-c(2951:2978)) %>%
  na.omit()
#removed all recordings after 15:00 on 10/14/2021 when the sensor was removed 
#from the water, but not yet shut off
#-------------

gumboot_2_cleaned <- gumboot_2 %>%
  slice(-c(2951:2978)) 
#removed all recordings after 15:00 on 10/14/2021 when the sensor was removed 
#from the water, but not yet shut off
#-------------

gumboot_3_cleaned <- gumboot_3 %>%
  slice(-c(1:66)) %>%
  slice(-c(17699:17805)) %>%
  na.omit()
#Buoy in water at approx. 17:30, so removed all times before 18:00 on 6/13/2021
#Buoy out of water at approx. 15:45; removed all times after 15:31 on 10/14/2021
#-------------

gumboot_4_cleaned <- gumboot_4 %>% 
  slice(-c(5687:5763)) %>% 
  na.omit()
#Removed all times after 12:00pm on 6/11/2022

gumboot_5_cleaned <- gumboot_5 %>% 
  na.omit()

gumboot_6_cleaned <- gumboot_6 %>% 
  slice(-c(1:58)) %>% 
  slice(-c(34058:34284))
#Removed all times after 12:00pm on 6/11/2022

gumboot_7_cleaned <- gumboot_7 %>% 
  slice(-c(5061:5139))
#Removed all times after 12:00pm on 6/11/2022

gumboot_8_cleaned <- gumboot_8 %>% 
  slice(-c(5061:51340))
#Removed all times after 12:00pm on 6/11/2022

# 4. Write cleaned data to processed folder-------------------------------------

write_csv(gumboot_1_cleaned, here('data/processed/gumboot/gumboot_hobo_2m_2021.10.14_cleaned.csv'))
write_csv(gumboot_2_cleaned, here('data/processed/gumboot/gumboot_hobo_2m_2022.06.11_cleaned.csv'))
write_csv(gumboot_3_cleaned, here('data/processed/gumboot/gumboot_hobo_sediment_2021.10.14_cleaned.csv'))
write_csv(gumboot_4_cleaned, here('data/processed/gumboot/gumboot_hobo_sediment_2022.06.11_cleaned.csv'))
write_csv(gumboot_5_cleaned, here('data/processed/gumboot/gumboot_DOT_2021.10.14_cleaned.csv'))
write_csv(gumboot_6_cleaned, here('data/processed/gumboot/gumboot_DOT_2022.06.11_cleaned.csv'))
write_csv(gumboot_7_cleaned, here('data/processed/gumboot/gumboot_light_pendant_90cm_2022.06.11_cleaned.csv'))
write_csv(gumboot_8_cleaned, here('data/processed/gumboot/gumboot_light_pendant_160cm_2022.06.11_cleaned.csv'))
