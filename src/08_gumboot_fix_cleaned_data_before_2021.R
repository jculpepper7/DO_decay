## Fix the structure of the cleaned datasets, to adhere with the datasets
## created in scripts 01, 03, 05. This data will then permit the aggregation 
## scripts to behave as previously written with a few minor changes

#libraries----------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(janitor)

# 1. fix gumboot processed data---------------------------------------------------

#gumboot miniDOT data-------------------------------------------------------------

gumboot_1 <- read_csv(here('data/processed/gumboot/gumboot_DOT_2020.06.17_cleaned.csv'))

gumboot_1_fixed <- gumboot_1 %>%
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

write_csv(gumboot_1_fixed, here('data/processed/gumboot/gumboot_DOT_2020.06.17_cleaned.csv'))
#-----------------------------------

gumboot_2 <- read_csv(here('data/processed/gumboot/gumboot_DOT_2020.07.15_cleaned.csv'))

gumboot_2_fixed <- gumboot_2 %>%
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

write_csv(gumboot_2_fixed, here('data/processed/gumboot/gumboot_DOT_2020.07.15_cleaned.csv'))
#-----------------------------------

gumboot_3 <- read_csv(here('data/processed/gumboot/gumboot_DOT_2020.10.10_cleaned.csv'))

gumboot_3_fixed <- gumboot_3 %>%
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

write_csv(gumboot_3_fixed, here('data/processed/gumboot/gumboot_DOT_2020.10.10_cleaned.csv'))
#-----------------------------------

gumboot_4 <- read_csv(here('data/processed/gumboot/gumboot_DOT_2021.06.11_cleaned.csv'))

gumboot_4_fixed <- gumboot_4 %>%
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

write_csv(gumboot_4_fixed, here('data/processed/gumboot/gumboot_DOT_2021.06.11_cleaned.csv'))
#-----------------------------------

#gumboot HOBO 2m data-------------------------------------------------------------

#gumboot <- read_csv(here('data/processed/gumboot/gumboot_hobo_2m_2021.10.14_cleaned.csv'))


gumboot_5 <- read_csv(here('data/processed/gumboot/gumboot_hobo_2m_2020.06.18_cleaned.csv'))

gumboot_5_fixed <- gumboot_5 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('gumboot'),
    depth = c('2m')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(gumboot_5_fixed, here('data/processed/gumboot/gumboot_hobo_2m_2020.06.18_cleaned.csv'))
#-----------------------------------

gumboot_6 <- read_csv(here('data/processed/gumboot/gumboot_hobo_2m_2020.07.16_cleaned.csv'))

gumboot_6_fixed <- gumboot_6 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('gumboot'),
    depth = c('2m')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(gumboot_6_fixed, here('data/processed/gumboot/gumboot_hobo_2m_2020.07.16_cleaned.csv'))
#-----------------------------------

gumboot_7 <- read_csv(here('data/processed/gumboot/gumboot_hobo_2m_2020.10.10_cleaned.csv'))

gumboot_7_fixed <- gumboot_7 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('gumboot'),
    depth = c('2m')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(gumboot_7_fixed, here('data/processed/gumboot/gumboot_hobo_2m_2020.10.10_cleaned.csv'))
#-----------------------------------

gumboot_8 <- read_csv(here('data/processed/gumboot/gumboot_hobo_2m_2021.06.12_cleaned.csv'))

gumboot_8_fixed <- gumboot_8 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('gumboot'),
    depth = c('2m')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(gumboot_8_fixed, here('data/processed/gumboot/gumboot_hobo_2m_2021.06.12_cleaned.csv'))
#-----------------------------------

#gumboot HOBO sediment data-------------------------------------------------------

gumboot_5 <- read_csv(here('data/processed/gumboot/gumboot_hobo_sediment_2020.06.18_cleaned.csv'))

gumboot_5_fixed <- gumboot_5 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('gumboot'),
    depth = c('sediment')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(gumboot_5_fixed, here('data/processed/gumboot/gumboot_hobo_sediment_2020.06.18_cleaned.csv'))
#-----------------------------------

gumboot_6 <- read_csv(here('data/processed/gumboot/gumboot_hobo_sediment_2020.07.16_cleaned.csv'))

gumboot_6_fixed <- gumboot_6 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('gumboot'),
    depth = c('sediment')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(gumboot_6_fixed, here('data/processed/gumboot/gumboot_hobo_sediment_2020.07.16_cleaned.csv'))
#-----------------------------------

gumboot_7 <- read_csv(here('data/processed/gumboot/gumboot_hobo_sediment_2020.10.10_cleaned.csv'))

gumboot_7_fixed <- gumboot_7 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('gumboot'),
    depth = c('sediment')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(gumboot_7_fixed, here('data/processed/gumboot/gumboot_hobo_sediment_2020.10.10_cleaned.csv'))
#-----------------------------------

gumboot_8 <- read_csv(here('data/processed/gumboot/gumboot_hobo_sediment_2021.06.12_cleaned.csv'))

gumboot_8_fixed <- gumboot_8 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('gumboot'),
    depth = c('sediment')
  ) %>%
  rename(
    temp_c = temp_C
  ) %>%
  select(lake, date_time, temp_c, depth)

write_csv(gumboot_8_fixed, here('data/processed/gumboot/gumboot_hobo_sediment_2021.06.12_cleaned.csv'))
#-----------------------------------

#gumboot light pendant data-------------------------------------------------------

gumboot_1 <- read_csv(here('data/processed/gumboot/gumboot_light_pendant_2020.06.18_cleaned.csv'))

gumboot_1_fixed <- gumboot_1 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('gumboot'),
    depth = c('2.5m')
  ) %>%
  rename(
    temp_c = temp_C,
    light_intensity_lux = intensity_lux
  ) %>%
  select(lake, date_time, temp_c, light_intensity_lux, depth)

write_csv(gumboot_1_fixed, here('data/processed/gumboot/gumboot_light_pendant_2020.06.18_cleaned.csv'))
#-----------------------------------

gumboot_2 <- read_csv(here('data/processed/gumboot/gumboot_light_pendant_2020.07.16_cleaned.csv'))

gumboot_2_fixed <- gumboot_2 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('gumboot'),
    depth = c('2.5m')
  ) %>%
  rename(
    temp_c = temp_C,
    light_intensity_lux = intensity_lux
  ) %>%
  select(lake, date_time, temp_c, light_intensity_lux, depth)

write_csv(gumboot_1_fixed, here('data/processed/gumboot/gumboot_light_pendant_2020.07.16_cleaned.csv'))
#-----------------------------------

gumboot_3 <- read_csv(here('data/processed/gumboot/gumboot_light_pendant_2020.10.10_cleaned.csv'))

gumboot_3_fixed <- gumboot_3 %>%
  na.omit() %>%
  mutate(
    date_time = mdy_hm(date_time),
    lake = c('gumboot'),
    depth = c('2.5m')
  ) %>%
  rename(
    temp_c = temp_C,
    light_intensity_lux = intensity_lux
  ) %>%
  select(lake, date_time, temp_c, light_intensity_lux, depth)

write_csv(gumboot_3_fixed, here('data/processed/gumboot/gumboot_light_pendant_2020.10.10_cleaned.csv'))
#-----------------------------------