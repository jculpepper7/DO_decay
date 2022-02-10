## Fix the structure of the cleaned datasets, to adhere with the datasets
## created in scripts 01, 03, 05. This data will then permit the aggregation 
## scripts to behave as previously written with a few minor changes

#libraries----------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(janitor)

# 1. fix cedar processed data---------------------------------------------------

#cedar miniDOT data-------------------------------------------------------------

cedar_1 <- read_csv(here('data/processed/cedar/cedar_DOT_2020.06.17_cleaned.csv'))

cedar_1_fixed <- cedar_1 %>%
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

write_csv(cedar_1_fixed, here('data/processed/cedar/cedar_DOT_2020.06.17_cleaned.csv'))
#-----------------------------------

cedar_2 <- read_csv(here('data/processed/cedar/cedar_DOT_2020.07.15_cleaned.csv'))

cedar_2_fixed <- cedar_2 %>%
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

write_csv(cedar_2_fixed, here('data/processed/cedar/cedar_DOT_2020.07.15_cleaned.csv'))
#-----------------------------------

cedar_3 <- read_csv(here('data/processed/cedar/cedar_DOT_2020.10.10_cleaned.csv'))

cedar_3_fixed <- cedar_3 %>%
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

write_csv(cedar_3_fixed, here('data/processed/cedar/cedar_DOT_2020.10.10_cleaned.csv'))
#-----------------------------------

cedar_4 <- read_csv(here('data/processed/cedar/cedar_DOT_2021.06.11_cleaned.csv'))

cedar_4_fixed <- cedar_4 %>%
  clean_names() %>%
  rename(
    unix = unix_timestamp,
    utc = utc_date_time,
    pst = pacific_standard_time,
    temp_c = temperature,
    do_mg_l = dissolved_oxygen,
    do_sat = dissolved_oxygen_saturation
  ) %>%
  # na.omit() %>% #not reading NAs from some reason
  # slice(-c(1:95)) %>%
  mutate(
    temp_c = as.numeric(temp_c),
    do_mg_l = as.numeric(do_mg_l),
    do_sat = as.numeric(do_sat)
  ) %>%
  na.omit()

write_csv(cedar_4_fixed, here('data/processed/cedar/cedar_DOT_2021.06.11_cleaned.csv'))
#-----------------------------------

#cedar HOBO 2m data-------------------------------------------------------------

#cedar <- read_csv(here('data/processed/cedar/cedar_hobo_2m_2021.10.14_cleaned.csv'))


cedar_5 <- read_csv(here('data/processed/cedar/cedar_hobo_2m_2020.06.18_cleaned.csv'))

cedar_5_fixed <- cedar_5 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('cedar'),
    depth = c('2m')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(cedar_5_fixed, here('data/processed/cedar/cedar_hobo_2m_2020.06.18_cleaned.csv'))
#-----------------------------------

cedar_6 <- read_csv(here('data/processed/cedar/cedar_hobo_2m_2020.07.16_cleaned.csv'))

cedar_6_fixed <- cedar_6 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('cedar'),
    depth = c('2m')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(cedar_6_fixed, here('data/processed/cedar/cedar_hobo_2m_2020.07.16_cleaned.csv'))
#-----------------------------------

cedar_7 <- read_csv(here('data/processed/cedar/cedar_hobo_2m_2020.10.10_cleaned.csv'))

cedar_7_fixed <- cedar_7 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('cedar'),
    depth = c('2m')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(cedar_7_fixed, here('data/processed/cedar/cedar_hobo_2m_2020.10.10_cleaned.csv'))
#-----------------------------------

cedar_8 <- read_csv(here('data/processed/cedar/cedar_hobo_2m_2021.06.12_cleaned.csv'))

cedar_8_fixed <- cedar_8 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('cedar'),
    depth = c('2m')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(cedar_8_fixed, here('data/processed/cedar/cedar_hobo_2m_2021.06.12_cleaned.csv'))
#-----------------------------------

#cedar HOBO sediment data-------------------------------------------------------

cedar_5 <- read_csv(here('data/processed/cedar/cedar_hobo_sediment_2020.06.18_cleaned.csv'))

cedar_5_fixed <- cedar_5 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('cedar'),
    depth = c('sediment')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(cedar_5_fixed, here('data/processed/cedar/cedar_hobo_sediment_2020.06.18_cleaned.csv'))
#-----------------------------------

cedar_6 <- read_csv(here('data/processed/cedar/cedar_hobo_sediment_2020.07.16_cleaned.csv'))

cedar_6_fixed <- cedar_6 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('cedar'),
    depth = c('sediment')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(cedar_6_fixed, here('data/processed/cedar/cedar_hobo_sediment_2020.07.16_cleaned.csv'))
#-----------------------------------

cedar_7 <- read_csv(here('data/processed/cedar/cedar_hobo_sediment_2020.10.10_cleaned.csv'))

cedar_7_fixed <- cedar_7 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('cedar'),
    depth = c('sediment')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(cedar_7_fixed, here('data/processed/cedar/cedar_hobo_sediment_2020.10.10_cleaned.csv'))
#-----------------------------------

cedar_8 <- read_csv(here('data/processed/cedar/cedar_hobo_sediment_2021.06.12_cleaned.csv'))

cedar_8_fixed <- cedar_8 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('cedar'),
    depth = c('sediment')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(cedar_8_fixed, here('data/processed/cedar/cedar_hobo_sediment_2021.06.12_cleaned.csv'))
#-----------------------------------

#cedar light pendant data-------------------------------------------------------

cedar_1 <- read_csv(here('data/processed/cedar/cedar_light_pendant_2020.06.18_cleaned.csv'))

cedar_1_fixed <- cedar_1 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('cedar'),
    depth = c('2.5m')
  ) %>%
  rename(
    temp_c = temp_C,
    light_intensity_lux = intensity_lux
  ) %>%
  select(lake, date_time, temp_c, light_intensity_lux, depth)

write_csv(cedar_1_fixed, here('data/processed/cedar/cedar_light_pendant_2020.06.18_cleaned.csv'))
#-----------------------------------

cedar_2 <- read_csv(here('data/raw/cedar/cedar_light_pendant_2020.07.16_raw.csv'), skip = 1)

cedar_2_fixed <- cedar_2 %>%
  clean_names() %>%
  select(2,3,4) %>%
  rename(date_time = 1,
         temp_c = 2, 
         light_intensity_lux = 3) %>%
  na.omit() %>%
  slice(-c(1:20)) %>%
  slice(-c(646:649)) %>%
  mutate(
    date_time = mdy_hms(date_time),
    lake = c('cedar'),
    depth = c('2.5m')
  ) %>%
  select(lake, date_time, temp_c, light_intensity_lux, depth)

write_csv(cedar_2_fixed, here('data/processed/cedar/cedar_light_pendant_2020.07.16_cleaned.csv'))
#-----------------------------------

cedar_3 <- read_csv(here('data/processed/cedar/cedar_light_pendant_2020.10.10_cleaned.csv'))

cedar_3_fixed <- cedar_3 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('cedar'),
    depth = c('2.5m')
  ) %>%
  rename(
    temp_c = temp_C,
    light_intensity_lux = intensity_lux
  ) %>%
  select(lake, date_time, temp_c, light_intensity_lux, depth)

write_csv(cedar_3_fixed, here('data/processed/cedar/cedar_light_pendant_2020.10.10_cleaned.csv'))
#-----------------------------------