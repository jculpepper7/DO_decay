## Fix the structure of the cleaned datasets, to adhere with the datasets
## created in scripts 01, 03, 05. This data will then permit the aggregation 
## scripts to behave as previously written with a few minor changes

#libraries----------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(janitor)

# 1. fix soapstone processed data---------------------------------------------------

#soapstone miniDOT data-------------------------------------------------------------

soapstone_1 <- read_csv(here('data/processed/soapstone/soapstone_DOT_2020.06.17_cleaned.csv'))

soapstone_1_fixed <- soapstone_1 %>%
  clean_names() %>%
  rename(
    unix = unix_timestamp,
    utc = utc_date_time,
    pst = pacific_standard_time,
    temp_c = temperature,
    do_mg_l = dissolved_oxygen,
    do_sat = dissolved_oxygen_saturation
  ) %>%
  na.omit() 

write_csv(soapstone_1_fixed, here('data/processed/soapstone/soapstone_DOT_2020.06.17_cleaned.csv'))
#-----------------------------------

soapstone_2 <- read_csv(here('data/processed/soapstone/soapstone_DOT_2020.10.11_cleaned.csv'))

soapstone_2_fixed <- soapstone_2 %>%
  clean_names() %>%
  rename(
    unix = unix_timestamp,
    utc = utc_date_time,
    pst = pacific_standard_time,
    temp_c = temperature,
    do_mg_l = dissolved_oxygen,
    do_sat = dissolved_oxygen_saturation
  ) %>%
  na.omit() 

write_csv(soapstone_2_fixed, here('data/processed/soapstone/soapstone_DOT_2020.10.11_cleaned.csv'))
#-----------------------------------

soapstone_3 <- read_csv(here('data/processed/soapstone/soapstone_DOT_2021.06.11_cleaned.csv'))

soapstone_3_fixed <- soapstone_3 %>%
  clean_names() %>%
  rename(
    unix = unix_timestamp,
    utc = utc_date_time,
    pst = pacific_standard_time,
    temp_c = temperature,
    do_mg_l = dissolved_oxygen,
    do_sat = dissolved_oxygen_saturation
  ) %>%
  na.omit() 

write_csv(soapstone_3_fixed, here('data/processed/soapstone/soapstone_DOT_2021.06.11_cleaned.csv'))
#-----------------------------------

#soapstone HOBO sediment data---------------------------------------------------

soapstone_5 <- read_csv(here('data/processed/soapstone/soapstone_hobo_sediment_2020.06.18_cleaned.csv'))

soapstone_5_fixed <- soapstone_5 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('soapstone'),
    depth = c('sediment')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(soapstone_5_fixed, here('data/processed/soapstone/soapstone_hobo_sediment_2020.06.18_cleaned.csv'))
#-----------------------------------

soapstone_7 <- read_csv(here('data/processed/soapstone/soapstone_hobo_sediment_2020.10.10_cleaned.csv'))

soapstone_7_fixed <- soapstone_7 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('soapstone'),
    depth = c('sediment')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(soapstone_7_fixed, here('data/processed/soapstone/soapstone_hobo_sediment_2020.10.10_cleaned.csv'))
#-----------------------------------

soapstone_8 <- read_csv(here('data/processed/soapstone/soapstone_hobo_sediment_2021.06.12_cleaned.csv'))

soapstone_8_fixed <- soapstone_8 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('soapstone'),
    depth = c('sediment')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(soapstone_8_fixed, here('data/processed/soapstone/soapstone_hobo_sediment_2021.06.12_cleaned.csv'))
#-----------------------------------

#soapstone light pendant data-------------------------------------------------------

soapstone_1 <- read_csv(here('data/processed/soapstone/soapstone_light_pendant_2020.06.18_cleaned.csv'))

soapstone_1_fixed <- soapstone_1 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('soapstone'),
    depth = c('1m')
  ) %>%
  rename(
    temp_c = temp_C,
    light_intensity_lux = intensity_lux
  ) %>%
  select(lake, date_time, temp_c, light_intensity_lux, depth)

write_csv(soapstone_1_fixed, here('data/processed/soapstone/soapstone_light_pendant_2020.06.18_cleaned.csv'))
#-----------------------------------

soapstone_2 <- read_csv(here('data/processed/soapstone/soapstone_light_pendant_2020.10.11_cleaned.csv'))

soapstone_2_fixed <- soapstone_2 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('soapstone'),
    depth = c('1m')
  ) %>%
  rename(
    temp_c = temp_C,
    light_intensity_lux = intensity_lux
  ) %>%
  select(lake, date_time, temp_c, light_intensity_lux, depth)

write_csv(soapstone_2_fixed, here('data/processed/soapstone/soapstone_light_pendant_2020.10.11_cleaned.csv'))
#-----------------------------------

soapstone_3 <- read_csv(here('data/processed/soapstone/soapstone_light_pendant_2021.06.12_cleaned.csv'))

soapstone_3_fixed <- soapstone_3 %>%
  clean_names() %>%
  rename(
    date_time = 1,
    temp_c = 2,
    light_intensity_lux = 3
  ) %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('soapstone'),
    depth = c('1m')
  ) %>%
  select(lake, date_time, temp_c, light_intensity_lux, depth)

write_csv(soapstone_3_fixed, here('data/processed/soapstone/soapstone_light_pendant_2021.06.12_cleaned.csv'))
#-----------------------------------