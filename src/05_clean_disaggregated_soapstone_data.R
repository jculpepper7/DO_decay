## The purpose of this script is to clean dissagregated data from soapstone Lake
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

soapstone_1 <- read_csv(here('data/raw/soapstone/soapstone_hobo_sediment_2021.08.28_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'soapstone',
         depth = 'sediment') %>%
  select(lake, date_time, temp_c, depth) %>%
  na.omit()

soapstone_2 <- read_csv(here('data/raw/soapstone/soapstone_hobo_sediment_2021.10.14_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'soapstone',
         depth = 'sediment') %>%
  select(lake, date_time, temp_c, depth) %>%
  na.omit()

soapstone_3 <- read_csv(here('data/raw/soapstone/soapstone_hobo_sediment_2022.06.11_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'soapstone',
         depth = 'sediment') %>%
  select(lake, date_time, temp_c, depth) %>%
  na.omit()

soapstone_4 <- read_csv(here('data/raw/soapstone/soapstone_light_pendant_2021.08.28_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3,4) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2, light_intensity_lux = 3) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'soapstone',
         depth = '1m') %>%
  select(lake, date_time, temp_c, light_intensity_lux, depth) %>%
  na.omit()

soapstone_5 <- read_csv(here('data/raw/soapstone/soapstone_light_pendant_2021.10.16_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3,4) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2, light_intensity_lux = 3) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'soapstone',
         depth = '1m') %>%
  select(lake, date_time, temp_c, light_intensity_lux, depth) %>%
  na.omit()

soapstone_6 <- read_csv(here('data/raw/soapstone/soapstone_light_pendant_2022.06.11_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3,4) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2, light_intensity_lux = 3) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(date_time = mdy_hms(date_time),
         lake = 'soapstone',
         depth = '1m') %>%
  select(lake, date_time, temp_c, light_intensity_lux, depth) %>%
  na.omit()

# Reading raw miniDOT data

soapstone_7 <- read_delim(here('data/raw/soapstone/soapstone_miniDOT_2021.08.28_raw.txt'), delim = ',', skip = 7) %>%
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

soapstone_8 <- read_delim(here('data/raw/soapstone/soapstone_miniDOT_2021.10.14_raw.txt'), delim = ',', skip = 7) %>%
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

soapstone_9 <- read_delim(here('data/raw/soapstone/soapstone_miniDOT_2022.06.12_raw.txt'), delim = ',', skip = 7) %>%
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
#water as they cool down in the water (see README_soapstone.TXT)

soapstone_1_cleaned <- soapstone_1 %>%
  na.omit()
#not necessary to remove any date_time, bc the sensor was removed from the water
#and recorded prior to a record outside of the water
#-------------

soapstone_2_cleaned <- soapstone_2 %>%
  slice(-c(1152:1161)) 
#removed all recordings after 10:00 on 10/14/2021 when the sensor was removed 
#from the water, but not yet shut off
#-------------

soapstone_3_cleaned <- soapstone_3 %>%
  slice(-c(5685:5763)) %>% 
  na.omit()
#not necessary to remove any date_time, bc the sensor was removed from the water
#and recorded prior to a record outside of the water
#-------------

soapstone_4_cleaned <- soapstone_4 %>%
  na.omit()
#measurements halted at 07/03/2021, so no removed date_times. 
#NOTE: Sensor went bad. Ordered a replacement, but redeployed this when possible (soapstone 5). Replaced at soapstone_6.
#-------------

soapstone_5_cleaned <- soapstone_5 %>% 
  na.omit()
#measurements halted at 10/02/2021, so no removed date_times
#-------------
  
soapstone_6_cleaned <- soapstone_6 %>% 
  slice(-c(5686:5763)) %>% 
  na.omit()
#This is the replacement light pendant. Pulled sensor at 11:30 6/11/2022
#------------

soapstone_7_cleaned <- soapstone_7 %>%
  slice(-c(1:48))
#Buoy in water at 14:00 on 6/13/2021, so removed all values prior to 15:00
#removed from water and turned off prior to reading at 9:30 on 8/28/2021
#so I did not remove any additional values at the end of the record.
#-------------  

soapstone_8_cleaned <- soapstone_8 %>%
  slice(-c(1:5)) %>%
  slice(-c(6762:6888))
#Buoy in water at 11:05 on 8/28/2021, so removed all values prior to 12:03
#Buoy removed from water at 10:45 on 10/14/2021, so removed all values after
#10:43 10/14/2021
#-------------  

soapstone_9_cleaned <- soapstone_9 %>%
  slice(-c(1:19)) %>%
  slice(-c(34133:34313))

#Buoy in water at 10:45 on 10/17/2021, so removed all values prior to 10:51
#Buoy removed from water at 11:30 on 6/11/2022, so removed all values after
#11:21 6/11/2022
#-------------  

# ss_plt <- ggplot(data = soapstone_6_cleaned)+
#   geom_line(aes(x = date_time, y = do_mg_l))+
#   theme_bw()
# ggplotly(ss_plt)

# 4. Write cleaned data to processed folder-------------------------------------

write_csv(soapstone_1_cleaned, here('data/processed/soapstone/soapstone_hobo_sediment_2021.08.28_cleaned.csv'))
write_csv(soapstone_2_cleaned, here('data/processed/soapstone/soapstone_hobo_sediment_2021.10.14_cleaned.csv'))
write_csv(soapstone_3_cleaned, here('data/processed/soapstone/soapstone_hobo_sediment_2022.06.11_cleaned.csv'))
write_csv(soapstone_4_cleaned, here('data/processed/soapstone/soapstone_light_pendant_2021.08.28_cleaned.csv'))
write_csv(soapstone_5_cleaned, here('data/processed/soapstone/soapstone_light_pendant_2021.10.16_cleaned.csv'))
write_csv(soapstone_6_cleaned, here('data/processed/soapstone/soapstone_light_pendant_2022.06.11_cleaned.csv'))
write_csv(soapstone_7_cleaned, here('data/processed/soapstone/soapstone_miniDOT_2021.08.28_cleaned.csv'))
write_csv(soapstone_8_cleaned, here('data/processed/soapstone/soapstone_miniDOT_2021.10.14_cleaned.csv'))
write_csv(soapstone_9_cleaned, here('data/processed/soapstone/soapstone_miniDOT_2022.06.11_cleaned.csv'))



