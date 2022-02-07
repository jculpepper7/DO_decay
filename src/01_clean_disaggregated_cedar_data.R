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

# 2.Read data-------------------------------------------------------------------

cedar_1 <- read_csv(here('data/raw/cedar/cedar_hobo_2m_2021.08.28_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(lake = 'cedar',
         depth = '2m') %>%
  select(lake, date_time, temp_C, depth)

cedar_2 <- read_csv(here('data/raw/cedar/cedar_hobo_2m_2021.10.14_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_c = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(lake = 'cedar',
         depth = '2m') %>%
  select(lake, date_time, temp_c, depth)

cedar_3 <- read_csv(here('data/raw/cedar/cedar_hobo_sediment_2021.08.28_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_C = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(lake = 'cedar',
         depth = 'sediment') %>%
  select(lake, date_time, temp_c, depth)

cedar_4 <- read_csv(here('data/raw/cedar/cedar_hobo_sediment_2021.10.14_raw.csv'), skip = 1) %>%
  clean_names() %>% #clean names from HOBO software default
  select(2,3) %>% #Only columns 2 and 3 have data
  rename(date_time = 1, temp_C = 2) %>% #rename columns to more intuitive names from HOBO software defaults
  mutate(lake = 'cedar',
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

  
  
  
  
  
  





