## The goal of this script is to 
## 1. Aggregate ARIMA results from script 15
##

# 1. Load libraries-------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(lubridate)

# 2. Import data----------------------------------------------------------------

# 2a. Castle 2018 1----
load(here('output/ARIMA_results/castle_2018_arima_output.Rdata'))

cal_results_2018_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2018_1.1')
  ) %>% 
  add_row(segment = 'castle_2018_1.2', drift = output.list[[8]][["coef"]][["drift"]])

# 2b. Castle 2018 2----
load(here('output/ARIMA_results/castle_2018_2_arima_output.Rdata'))

cal_results_2018_2 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2018_2.1')
  ) %>% 
  add_row(segment = 'castle_2018_2.2', drift = output.list[[8]][["coef"]][["drift"]])

# 2c. Castle 2018 3----
load(here('output/ARIMA_results/castle_2018_3_arima_output.Rdata'))

cal_results_2018_3 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2018_3.1')
  ) %>% 
  add_row(segment = 'castle_2018_3.2', drift = output.list[[8]][["coef"]][["drift"]])

# 2d. Castle 2018 4----
load(here('output/ARIMA_results/castle_2018_4_arima_output.Rdata'))

cal_results_2018_4 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2018_4.1')
  ) %>% 
  add_row(segment = 'castle_2018_4.2', drift = output.list[[8]][["coef"]][["drift"]])

# 2e. Castle 2019 1----
load(here('output/ARIMA_results/castle_2019_arima_output.Rdata'))

cal_results_2019_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2019_1.1')
  ) %>% 
  add_row(segment = 'castle_2019_1.2', drift = output.list[[8]][["coef"]][["drift"]])

# 2f. Castle 2019 2----
load(here('output/ARIMA_results/castle_2019_2_arima_output.Rdata'))

cal_results_2019_2 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2019_2.1')
  ) 

# 2g. Castle 2020 1----
load(here('output/ARIMA_results/castle_2020_arima_output.Rdata'))

cal_results_2020_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2020_1.1')
  ) %>% 
  add_row(segment = 'castle_2020_1.2', drift = output.list[[8]][["coef"]][["drift"]])

# 2h. Castle 2020 2----
load(here('output/ARIMA_results/castle_2020_2_arima_output.Rdata'))

cal_results_2020_2 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2020_2.1')
  ) 

# 2i. Castle 2020 3----
load(here('output/ARIMA_results/castle_2020_3_arima_output.Rdata'))

cal_results_2020_3 <- tibble(output.list[[9]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2020_3.1')
  ) %>% 
  add_row(segment = 'castle_2020_3.2', drift = output.list[[10]][["coef"]][["drift"]]) %>% 
  add_row(segment = 'castle_2020_3.3', drift = output.list[[11]][["coef"]][["drift"]])

# 2j. Castle 2020 4----
load(here('output/ARIMA_results/castle_2020_4_arima_output.Rdata'))

cal_results_2020_4 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2020_4.1')
  ) %>% 
  add_row(segment = 'castle_2020_4.2', drift = output.list[[8]][["coef"]][["drift"]]) 

# 2k. Castle 2021 1----
load(here('output/ARIMA_results/castle_2021_arima_output.Rdata'))

cal_results_2021_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2021_1.1')
  ) %>% 
  add_row(segment = 'castle_2021_1.2', drift = output.list[[8]][["coef"]][["drift"]]) 

# 2l. Castle 2021 2----
load(here('output/ARIMA_results/castle_2021_2_arima_output.Rdata'))

cal_results_2021_2 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2021_2.1')
  ) 

# 2m. Castle 2021 3----
load(here('output/ARIMA_results/castle_2021_3_arima_output.Rdata'))

cal_results_2021_3 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2021_3.1')
  )

# 2n. Castle 2022 1----
load(here('output/ARIMA_results/castle_2022_arima_output.Rdata'))

cal_results_2022_1 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2022_1.1')
  ) 

# 2o. Castle 2022 2----
load(here('output/ARIMA_results/castle_2022_2_arima_output.Rdata'))

cal_results_2022_2 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2022_2.1')
  ) 

# 2p. Cedar 2020 1----
load(here('output/ARIMA_results/cedar_2020_arima_output.Rdata'))

cdr_results_2020 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cedar_2020_1.1')
  ) %>% 
  add_row(segment = 'cedar_2020_1.2', drift = output.list[[8]][["coef"]][["drift"]])



