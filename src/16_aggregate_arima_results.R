## The goal of this script is to 
## 1. Aggregate ARIMA results from script 15
##

# 1. Load libraries-------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(lubridate)

# 2. Import Castle data----------------------------------------------------------------

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

# 3. Import Cedar data-----------------------------------------------

# 3a. Cedar 2020 1----
load(here('output/ARIMA_results/cedar_2020_arima_output.Rdata'))

cdr_results_2020 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cedar_2020_1.1')
  ) %>% 
  add_row(segment = 'cedar_2020_1.2', drift = output.list[[8]][["coef"]][["drift"]])

# 3b. Cedar 2021 1----
load(here('output/ARIMA_results/cedar_2021_arima_output.Rdata'))

cdr_results_2021 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cedar_2021_1.1')
  ) %>% 
  add_row(segment = 'cedar_2021_1.2', drift = output.list[[8]][["coef"]][["drift"]])

# 3c. Cedar 2022 1----
load(here('output/ARIMA_results/cedar_2022_arima_output.Rdata'))

cdr_results_2022 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cedar_2022_1.1')
  ) 

# 4. Import Cliff data-----------------------------------------------

# 4a. Cliff 2020 1----
load(here('output/ARIMA_results/cliff_2020_arima_output.Rdata'))

clf_results_2020 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cliff_2020_1.1')
  ) %>% 
  add_row(segment = 'cliff_2020_1.2', drift = output.list[[8]][["coef"]][["drift"]])

# 4b. Cliff 2021 1----
load(here('output/ARIMA_results/cliff_2021_arima_output.Rdata'))

clf_results_2021 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cliff_2021_1.1')
  ) 

# 4b. Cliff 2021 2----
#NOTE: Mislabels in ARIMA code as 2021, but these ARIMA results 
#      are for the summer period DO decay during the summer of 2020
load(here('output/ARIMA_results/cliff_2021_2_arima_output.Rdata'))

clf_results_2020_2 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cliff_2020_2.1')
  ) %>% 
  add_row(segment = 'cliff_2020_2.2', drift = output.list[[8]][["coef"]][["drift"]])


# 5. Import Gumboot data---------------------------------------------

# 5a. Gumboot 2020 1----
load(here('output/ARIMA_results/gumboot_2020_arima_output.Rdata'))

gb_results_2020_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('gumboot_2020_1.1')
  ) %>% 
  add_row(segment = 'gumboot_2020_1.2', drift = output.list[[8]][["coef"]][["drift"]])

# 5b. Gumboot 2020 2----
load(here('output/ARIMA_results/gumboot_2020_2_arima_output.Rdata'))

gb_results_2020_2 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('gumboot_2020_2.1')
  )

# 5c. Gumboot 2021 1----
load(here('output/ARIMA_results/gumboot_2021_arima_output.Rdata'))

gb_results_2021_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('gumboot_2021_1.1')
  ) %>% 
  add_row(segment = 'gumboot_2021_1.2', drift = output.list[[8]][["coef"]][["drift"]])

# 5d. Gumboot 2022 1----
load(here('output/ARIMA_results/gumboot_2022_arima_output.Rdata'))

gb_results_2022_1 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('gumboot_2022_1.1')
  ) 

# 6. Import Soapstone data-------------------------------------------

# 6a. Soapstone 2020 1----
load(here('output/ARIMA_results/soapstone_2020_arima_output.Rdata'))

ss_results_2020_1 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('soapstone_2020_1.1')
  )

# 6b. Soapstone 2020 2----
load(here('output/ARIMA_results/soapstone_2020_2_arima_output.Rdata'))

ss_results_2020_2 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('soapstone_2020_2.1')
  ) %>% 
  add_row(segment = 'soapstone_2020_2.2', drift = output.list[[8]][["coef"]][["drift"]])

# 6c. Soapstone 2021 1----
load(here('output/ARIMA_results/soapstone_2021_arima_output.Rdata'))

ss_results_2021_1 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('soapstone_2021_1.1')
  ) 

# 6d. Soapstone 2021 2----
load(here('output/ARIMA_results/soapstone_2021_2_arima_output.Rdata'))

ss_results_2021_2 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('soapstone_2021_2.1')
  )

# 6e. Soapstone 2022 1----
load(here('output/ARIMA_results/soapstone_2022_arima_output.Rdata'))

ss_results_2022_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('soapstone_2022_1.1')
  ) %>% 
  add_row(segment = 'soapstone_2022_1.2', drift = output.list[[8]][["coef"]][["drift"]])

# 7. Aggregate all lake DO decay rates-------------------------------------

# Bind the above data

do_decay <- bind_rows(
  cal_results_2018_1, cal_results_2018_2, cal_results_2018_3, cal_results_2018_4,
  cal_results_2019_1, cal_results_2019_2,
  cal_results_2020_1, cal_results_2020_2, cal_results_2020_3, cal_results_2020_4,
  cal_results_2021_1, cal_results_2021_2, cal_results_2021_3,
  cal_results_2022_1, cal_results_2022_2,
  cdr_results_2020, cdr_results_2021, cdr_results_2022,
  clf_results_2020, clf_results_2020_2, clf_results_2021, 
  gb_results_2020_1, gb_results_2020_2,
  gb_results_2021_1, 
  gb_results_2022_1,
  ss_results_2020_1, ss_results_2020_2,
  ss_results_2021_1, ss_results_2021_2,
  ss_results_2022_1
) %>% 
  separate(segment, c('lake', 'water_year', 'segment', 'changepoint'))

#write_csv(do_decay, here('output/ARIMA_results/do_decay_aggregated.csv'))

# 8. Summary statistics-----------------------------------------------

do_decay <- read_csv(here('output/ARIMA_results/do_decay_aggregated.csv')) %>% 
  mutate(
    lake = as.factor(lake),
    segment = as.factor(segment),
    changepoint = as.factor(changepoint),
    water_year = as.factor(water_year)
  )

do_decay_summary <- do_decay %>% 
  group_by(lake, water_year) %>% 
  summarise(
    mean = mean(drift),
    median = median(drift),
    max = max(drift),
    min = min(drift)
  )

do_decay_summary_lake <- do_decay %>% 
  group_by(lake) %>% 
  summarise(
    mean = mean(drift),
    median = median(drift),
    max = max(drift),
    min = min(drift)
  )

# 9. Data visualization-----------------------------------------------

ggplot(data = do_decay)+
  geom_point(aes(x = water_year, y = drift, color = lake, shape = changepoint), stroke = 1, size = 2)+
  theme_classic()+
  facet_wrap(~segment, scales = 'free')

ggplot(data = do_decay)+
  geom_boxplot(aes(x = lake, y = drift))+
  geom_jitter(aes(x = lake, y = drift, fill = water_year), pch = 21, width = 0.2, size = 3, stroke = 1.2)+
  theme_classic()+
  facet_wrap(~lake, scales = 'free')

#ggsave(here('output/ARIMA_results/do_decay_boxplot_update_2022.07.18.jpeg'), dpi = 300)

