## The goal of this script is 1) to detect hypoxia in all time series and 
## 2) to extract the duration of each of those periods of hypoxia and
## 3) detect anoxia in all time series and
## 4) to exctract the duration of each of those periods of anoxia

# 1. Load libraries------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(janitor)

# 2. Import data---------------------------------------------------------

#Castle lake aggregated data and isolate DO
cal_do <- read_csv(here('data/processed/castle/castle_clean_agg_data_daily.csv')) %>% 
  mutate(
    lake = 'castle'
  ) %>% 
  select(lake, date, do_mg_l)

#Cedar lake aggregated data and isolate DO
cdr_do <- read_csv(here('data/processed/cedar/cedar_clean_agg_data_2022.csv'), guess_max = 100000) %>% 
  select(lake, pst, do_mg_l) %>% 
  na.omit() %>% 
  mutate(
    water_year = ifelse(month(pst) >= 10, year(pst)+1, year(pst)),
    day = day(pst),
    month = month(pst),
    year = year(pst)
  ) %>%
  select(-pst) %>% 
  group_by(year, month, day) %>%
  summarise(
    do_mg_l = mean(do_mg_l)
  ) %>% 
  ungroup() %>% 
  mutate(
    date = make_date(year = year, month = month, day = day),
    lake = c('cedar')
  ) %>% 
  select(lake, date, do_mg_l)

#Cliff lake aggregated data and isolate DO
clf_do <- read_csv(here('data/processed/cliff/cliff_clean_agg_data_daily.csv')) %>% 
  mutate(
    lake = 'cliff'
  ) %>% 
  select(lake, date, do_mg_l)

#Gumboot lake aggregated data and isolate DO
gb_do <- read_csv(here('data/processed/gumboot/gumboot_clean_agg_data_2022.csv'), guess_max = 100000) %>% 
  select(lake, pst, do_mg_l) %>% 
  na.omit() %>% 
  mutate(
    water_year = ifelse(month(pst) >= 10, year(pst)+1, year(pst)),
    day = day(pst),
    month = month(pst),
    year = year(pst)
  ) %>% 
  select(-pst) %>% 
  group_by(year, month, day) %>%
  summarise(
    do_mg_l = mean(do_mg_l)
  ) %>% 
  ungroup() %>% 
  mutate(
    date = make_date(year = year, month = month, day = day),
    lake = c('gumboot')
  ) %>% 
  select(lake, date, do_mg_l)  

#Soapstone Pond aggregated data and isolate DO
ss_do <- read_csv(here('data/processed/soapstone/soapstone_clean_agg_data_2022.csv'), guess_max = 50000) %>% 
  select(lake, pst, do_mg_l) %>% 
  na.omit() %>% 
  mutate(
    water_year = ifelse(month(pst) >= 10, year(pst)+1, year(pst)),
    day = day(pst),
    month = month(pst),
    year = year(pst)
  ) %>% 
  select(-pst) %>% 
  group_by(year, month, day) %>%
  summarise(
    do_mg_l = mean(do_mg_l)
  ) %>% 
  ungroup() %>% 
  mutate(
    date = make_date(year = year, month = month, day = day),
    lake = c('soapstone')
  ) %>% 
  select(lake, date, do_mg_l)    

#3. Join data------------------------------------------------------------

all_do <- bind_rows(cal_do, cdr_do, clf_do, gb_do, ss_do) %>% 
  mutate(
    water_year = if_else(month(date)>=10, year(date)+1, year(date))
  )

#3. Isolate hypoxia periods

#Count the total number of days when oxygen was <=2 mg/L
hypox_total <- all_do %>%
  group_by(lake, water_year) %>% 
  filter(do_mg_l <= 2 & do_mg_l > 1) %>% 
  summarise(
    hypox_total = n()
  )

#Count the total number of days when oxygen was <1 mg/L
anoxia_total <- all_do %>%
  group_by(lake, water_year) %>% 
  filter(do_mg_l <= 1) %>% 
  summarise(
    anoxia_total = n()
  )

#Count the longest period of days when oxygen was <=2 mg/L
hypox_duration <- all_do %>%
  group_by(lake, water_year) %>% 
  filter(do_mg_l <= 2) %>% 
  mutate(
    diff = date - lag(date)
  ) %>% 
  filter(diff == 1) %>% 
  summarise(
    hypox_duration = last(date) - first(date)
  )  

#Count the longest period of days when oxygen was <1 mg/L
anoxia_duration <- all_do %>%
  group_by(lake, water_year) %>% 
  filter(do_mg_l < 1) %>% 
  mutate(
    diff = date - lag(date)
  ) %>% 
  filter(diff == 1) %>% 
  summarise(
    anoxic_duration = last(date) - first(date)
  )  

#Combine hypoxia and anoxia tibbles
low_do <- hypox_total %>% 
  full_join(anoxia_total)

#write_csv(low_do, here('output/hypoxia_results/hypoxia_anoxia_total.csv'))
