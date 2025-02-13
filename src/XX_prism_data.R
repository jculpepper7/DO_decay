#The goal of this script is to calculate freezing and thawing degree days
#prior to each lakes thaw and melt.


# 1. Load libraries -------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)

# 2. Load data ------------------------------------------------------------

#Mean data
prism_2018_mean <- read_csv(here('data/met_data/prism/mean/cal_temp_mean_wy_2018.csv')) %>% 
  select(
    year = 1,
    temp_mean = 2
  )
prism_2019_mean <- read_csv(here('data/met_data/prism/mean/cal_temp_mean_wy_2019.csv'))%>% 
  select(
    year = 1,
    temp_mean = 2
  )
prism_2020_mean <- read_csv(here('data/met_data/prism/mean/cal_temp_mean_wy_2020.csv'))%>% 
  select(
    year = 1,
    temp_mean = 2
  )
prism_2021_mean <- read_csv(here('data/met_data/prism/mean/cal_temp_mean_wy_2021.csv'))%>% 
  select(
    year = 1,
    temp_mean = 2
  )
prism_2022_mean <- read_csv(here('data/met_data/prism/mean/cal_temp_mean_wy_2022.csv'))%>% 
  select(
    year = 1,
    temp_mean = 2
  )

#Max data
prism_2018_max <- read_csv(here('data/met_data/prism/max/cal_temp_max_wy_2018.csv')) %>% 
  select(
    year = 1,
    temp_max = 2
  )
prism_2019_max <- read_csv(here('data/met_data/prism/max/cal_temp_max_wy_2019.csv')) %>% 
  select(
    year = 1,
    temp_max = 2
  )
prism_2020_max <- read_csv(here('data/met_data/prism/max/cal_temp_max_wy_2020.csv')) %>% 
  select(
    year = 1,
    temp_max = 2
  )
prism_2021_max <- read_csv(here('data/met_data/prism/max/cal_temp_max_wy_2021.csv')) %>% 
  select(
    year = 1,
    temp_max = 2
  )
prism_2022_max <- read_csv(here('data/met_data/prism/max/cal_temp_max_wy_2022.csv'))%>% 
  select(
    year = 1,
    temp_max = 2
  )

#Min data
prism_2018_min <- read_csv(here('data/met_data/prism/min/cal_temp_min_wy_2018.csv')) %>% 
  select(
    year = 1,
    temp_min = 2
  )
prism_2019_min <- read_csv(here('data/met_data/prism/min/cal_temp_min_wy_2019.csv')) %>% 
  select(
    year = 1,
    temp_min = 2
  )
prism_2020_min <- read_csv(here('data/met_data/prism/min/cal_temp_min_wy_2020.csv')) %>% 
  select(
    year = 1,
    temp_min = 2
  )
prism_2021_min <- read_csv(here('data/met_data/prism/min/cal_temp_min_wy_2021.csv'))%>% 
  select(
    year = 1,
    temp_min = 2
  )
prism_2022_min <- read_csv(here('data/met_data/prism/min/cal_temp_min_wy_2022.csv'))%>% 
  select(
    year = 1,
    temp_min = 2
  )


# 3. Combine data ---------------------------------------------------------

#Mean df
prism_mean <- prism_2018_mean %>% 
  bind_rows(
    prism_2019_mean,
    prism_2020_mean,
    prism_2021_mean,
    prism_2022_mean
  )

#Max df
prism_max <- prism_2018_max %>% 
  bind_rows(
    prism_2019_max,
    prism_2020_max,
    prism_2021_max,
    prism_2022_max
  )

#Min df
prism_min <- prism_2018_min %>% 
  bind_rows(
    prism_2019_min,
    prism_2020_min,
    prism_2021_min,
    prism_2022_min
  )

#Combine all
prism_all <- prism_mean %>% 
  bind_cols(
    prism_max,
    prism_min
  ) %>% 
  select(
    year = 1,
    2,4,6
  )


# 4. Calculate FDD --------------------------------------------------------

fdd <- prism_mean
