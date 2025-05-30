## The goal of this script is to 
## 1. Aggregate ARIMA results from script 15
##

# 1. Load libraries-------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(lubridate)

# 2. Import Castle data----------------------------------------------------------------

# **2a. Castle 2018 1----
load(here('output/ARIMA_results/castle_2018_arima_output.Rdata'))

cal_results_2018_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2018_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'castle_2018_1.2', 
          drift = output.list[[8]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']]))

# **2b. Castle 2018 2----
load(here('output/ARIMA_results/castle_2018_2_arima_output.Rdata'))

cal_results_2018_2 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2018_2.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'castle_2018_2.2', 
          drift = output.list[[8]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']]))

# **2c. Castle 2018 3----
load(here('output/ARIMA_results/castle_2018_3_arima_output.Rdata'))

cal_results_2018_3 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2018_3.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'castle_2018_3.2', 
          drift = output.list[[8]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']]))

# **2d. Castle 2018 4----
load(here('output/ARIMA_results/castle_2018_4_arima_output.Rdata'))

cal_results_2018_4 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2018_4.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'castle_2018_4.2', 
          drift = output.list[[8]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']]))

# **2e. Castle 2019 1----
load(here('output/ARIMA_results/castle_2019_arima_output.Rdata'))

cal_results_2019_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2019_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'castle_2019_1.2', 
          drift = output.list[[8]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']]))

# **2f. Castle 2019 2----
load(here('output/ARIMA_results/castle_2019_2_arima_output.Rdata'))

cal_results_2019_2 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2019_2.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) 

# **2g. Castle 2020 1----
load(here('output/ARIMA_results/castle_2020_arima_output.Rdata'))

cal_results_2020_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2020_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'castle_2020_1.2', 
          drift = output.list[[8]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']]))

# **2h. Castle 2020 2----
load(here('output/ARIMA_results/castle_2020_2_arima_output.Rdata'))

cal_results_2020_2 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2020_2.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) 

# **2i. Castle 2020 3----
load(here('output/ARIMA_results/castle_2020_3_arima_output.Rdata'))

cal_results_2020_3 <- tibble(output.list[[9]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2020_3.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'castle_2020_3.2', 
          drift = output.list[[10]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']])) %>% 
  add_row(segment = 'castle_2020_3.3', 
          drift = output.list[[11]][["coef"]][["drift"]],
          start = first(output.list[[5]][['date_time']]),
          end = last(output.list[[5]][['date_time']]))

# **2j. Castle 2020 4----
load(here('output/ARIMA_results/castle_2020_4_arima_output.Rdata'))

cal_results_2020_4 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2020_4.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'castle_2020_4.2', 
          drift = output.list[[8]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']])) 

# **2k. Castle 2021 1----
load(here('output/ARIMA_results/castle_2021_arima_output.Rdata'))

cal_results_2021_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2021_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'castle_2021_1.2', 
          drift = output.list[[8]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']])) 

# **2l. Castle 2021 2----
load(here('output/ARIMA_results/castle_2021_2_arima_output.Rdata'))

cal_results_2021_2 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2021_2.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) 

# **2m. Castle 2021 3----
load(here('output/ARIMA_results/castle_2021_3_arima_output.Rdata'))

cal_results_2021_3 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2021_3.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  )

# **2n. Castle 2022 1----
load(here('output/ARIMA_results/castle_2022_arima_output.Rdata'))

cal_results_2022_1 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2022_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) 

# **2o. Castle 2022 2----
load(here('output/ARIMA_results/castle_2022_2_arima_output.Rdata'))

cal_results_2022_2 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('castle_2022_2.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) 

# 3. Import Cedar data-----------------------------------------------

# **3a. Cedar 2020 1----
load(here('output/ARIMA_results/cedar_2020_arima_output.Rdata'))

cdr_results_2020_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cedar_2020_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'cedar_2020_1.2', 
          drift = output.list[[8]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']]))

# **3b. Cedar 2021 1----
load(here('output/ARIMA_results/cedar_2021_arima_output.Rdata'))

cdr_results_2021_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cedar_2021_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'cedar_2021_1.2', 
          drift = output.list[[8]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']]))

# **3c. Cedar 2022 1----
load(here('output/ARIMA_results/cedar_2022_arima_output.Rdata'))

cdr_results_2022_1 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cedar_2022_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) 

# 4. Import Cliff data-----------------------------------------------


# **4a. Cliff 2018 ----
load(here('output/ARIMA_results/cliff_2018_arima_output.Rdata'))

clf_results_2018_1 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cliff_2018_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) 

# **4b. Cliff 2019----
load(here('output/ARIMA_results/cliff_2019_arima_output.Rdata'))

clf_results_2019_1 <- tibble(output.list[[9]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cliff_2019_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'cliff_2019_1.2', 
          drift = output.list[[10]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']])) %>% 
  add_row(segment = 'cliff_2019_1.3', 
          drift = output.list[[11]][['coef']][['drift']],
          start = first(output.list[[5]][['date_time']]),
          end = last(output.list[[5]][['date_time']]))

# **4c. Cliff 2019_2----
load(here('output/ARIMA_results/cliff_2019_2_arima_output.Rdata'))

clf_results_2019_2 <- tibble(output.list[[9]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cliff_2019_2.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'cliff_2019_2.2', 
          drift = output.list[[10]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']])) %>% 
  add_row(segment = 'cliff_2019_2.3', 
          drift = output.list[[11]][['coef']][['drift']],
          start = first(output.list[[5]][['date_time']]),
          end = last(output.list[[5]][['date_time']]))

# **4d. Cliff 2019_3----
load(here('output/ARIMA_results/cliff_2019_3_arima_output.Rdata'))

clf_results_2019_3 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cliff_2019_3.1'),
    start = first(output.list[[1]][['date_time']]),
    end = last(output.list[[1]][['date_time']])
  ) 

# **4e. Cliff 2020 1----
load(here('output/ARIMA_results/cliff_2020_arima_output.Rdata'))

clf_results_2020_1 <- tibble(output.list[[9]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cliff_2020_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'cliff_2020_1.2', 
          drift = output.list[[10]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']]))

# **4f. Cliff 2020 2----
load(here('output/ARIMA_results/cliff_2020_2_arima_output.Rdata'))

clf_results_2020_2 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cliff_2020_2.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) 
# **4g. Cliff 2021 1----
load(here('output/ARIMA_results/cliff_2021_arima_output.Rdata'))

clf_results_2021_1 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cliff_2021_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) 

# **4h. Cliff 2021 2----
load(here('output/ARIMA_results/cliff_2021_2_arima_output.Rdata'))

clf_results_2021_2 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cliff_2021_2.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'cliff_2021_2.2', 
          drift = output.list[[8]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']]))

# **4i. Cliff 2022 1----
load(here('output/ARIMA_results/cliff_2022_arima_output.Rdata'))

clf_results_2022_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cliff_2022_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'cliff_2022_1.2', 
          drift = output.list[[8]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']]))
# **4i. Cliff 2022 2----
load(here('output/ARIMA_results/cliff_2022_2_arima_output.Rdata'))

clf_results_2022_2 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('cliff_2022_2.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  )


# 5. Import Gumboot data---------------------------------------------

# **5a. Gumboot 2020 1----
load(here('output/ARIMA_results/gumboot_2020_arima_output.Rdata'))

gb_results_2020_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('gumboot_2020_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'gumboot_2020_1.2', 
          drift = output.list[[8]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']]))

# **5b. Gumboot 2020 2----
load(here('output/ARIMA_results/gumboot_2020_2_arima_output.Rdata'))

gb_results_2020_2 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('gumboot_2020_2.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  )

# **5c. Gumboot 2021 1----
load(here('output/ARIMA_results/gumboot_2021_arima_output.Rdata'))

gb_results_2021_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('gumboot_2021_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'gumboot_2021_1.2', 
          drift = output.list[[8]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']]))

# **5d. Gumboot 2022 1----
load(here('output/ARIMA_results/gumboot_2022_arima_output.Rdata'))

gb_results_2022_1 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('gumboot_2022_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) 

# 6. Import Soapstone data-------------------------------------------

# **6a. Soapstone 2020 1----
load(here('output/ARIMA_results/soapstone_2020_arima_output.Rdata'))

ss_results_2020_1 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('soapstone_2020_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  )

# **6b. Soapstone 2020 2----
load(here('output/ARIMA_results/soapstone_2020_2_arima_output.Rdata'))

ss_results_2020_2 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('soapstone_2020_2.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'soapstone_2020_2.2', 
          drift = output.list[[8]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']]))

# **6c. Soapstone 2021 1----
load(here('output/ARIMA_results/soapstone_2021_arima_output.Rdata'))

ss_results_2021_1 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('soapstone_2021_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) 

# **6d. Soapstone 2021 2----
load(here('output/ARIMA_results/soapstone_2021_2_arima_output.Rdata'))

ss_results_2021_2 <- tibble(output.list[[5]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('soapstone_2021_2.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  )

# **6e. Soapstone 2022 1----
load(here('output/ARIMA_results/soapstone_2022_arima_output.Rdata'))

ss_results_2022_1 <- tibble(output.list[[7]][["coef"]][["drift"]]) %>% 
  rename(drift = 1) %>% 
  mutate(
    segment = c('soapstone_2022_1.1'),
    start = first(output.list[[3]][['date_time']]),
    end = last(output.list[[3]][['date_time']])
  ) %>% 
  add_row(segment = 'soapstone_2022_1.2', 
          drift = output.list[[8]][["coef"]][["drift"]],
          start = first(output.list[[4]][['date_time']]),
          end = last(output.list[[4]][['date_time']]))

# 7. Aggregate all lake DO decay rates-------------------------------------

# Bind the above data

do_decay <- bind_rows(
  #Rows organized by year. Therefore, all sections in a row belong
  #to the same year.
  cal_results_2018_1, cal_results_2018_2, cal_results_2018_3, cal_results_2018_4,
  cal_results_2019_1, cal_results_2019_2,
  cal_results_2020_1, cal_results_2020_2, cal_results_2020_3, cal_results_2020_4,
  cal_results_2021_1, cal_results_2021_2, cal_results_2021_3,
  cal_results_2022_1, cal_results_2022_2,
  cdr_results_2020_1, 
  cdr_results_2021_1, 
  cdr_results_2022_1,
  clf_results_2018_1,
  clf_results_2019_1, clf_results_2019_2, clf_results_2019_3,
  clf_results_2020_1, clf_results_2020_2, 
  clf_results_2021_1, clf_results_2021_2,
  clf_results_2022_1, clf_results_2022_2,
  gb_results_2020_1, gb_results_2020_2,
  gb_results_2021_1, 
  gb_results_2022_1,
  ss_results_2020_1, ss_results_2020_2,
  ss_results_2021_1, ss_results_2021_2,
  ss_results_2022_1
) %>% 
  select(
    segment, drift, start, end
  ) %>% 
  separate(segment, c('lake', 'water_year', 'segment', 'changepoint')) %>% 
  mutate(
    duration = as.numeric(end - start)
  )

do_decay %>% filter(duration<5)

#write_csv(do_decay, here('output/ARIMA_results/do_decay_aggregated_updated.csv'))

# 8. Summary statistics-----------------------------------------------

do_decay <- read_csv(here('output/ARIMA_results/do_decay_aggregated_updated.csv')) %>% 
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
    range = range(drift),
    max = max(drift),
    min = min(drift)
  ) %>% 
  mutate(
    range2 = abs(min) - abs(max)
  )

# 9. Data visualization-----------------------------------------------

# **9a. Reorganize DO data----
do_reorder <- do_decay %>% 
  filter(
    duration > 5
  ) %>% 
  #add a "large" and "small" designation.
  mutate(
    size = if_else(
      lake == 'castle' | lake == 'cliff',
      #Large is >5m depth
      as.factor('Deep'),
      #Small is <=5m depth
      as.factor('Shallow')
    )
  )

# **9b. Large v small summary values----

do_decay_summary_size <- do_reorder %>% 
  group_by(size) %>% 
  summarise(
    mean = mean(drift),
    median = median(drift),
    low = max(drift),
    high = min(drift)
  ) 

#Establish the order of the lakes from small to large

do_reorder$lake <- factor(do_reorder$lake, 
                          levels = c('soapstone', 'cedar', 'gumboot', 'cliff', 'castle'))

# ggplot(data = do_reorder)+
#   geom_point(aes(x = water_year, y = drift, color = lake, shape = changepoint), stroke = 1, size = 2)+
#   theme_classic()+
#   facet_wrap(~segment, scales = 'free')

#upper case labels
decay_labels <- c('Soapstone', 'Cedar', 'Gumboot', 'Cliff', 'Castle')

# **9b. All lakes boxplot----

#DO decay plot
do_decay_plt <- ggplot(data = do_reorder %>% mutate(drift = drift*-1))+
  #geom_boxplot(aes( y = drift))+
  geom_boxplot(aes(x = lake, y = drift), outlier.shape = NA)+ #reorder(lake,drift,mean)
  geom_jitter(aes(x = lake, y = drift, shape = water_year), size = 3, width = 0.1, stroke = 1.2)+ #, pch = 21, width = 0.2,
  theme_classic()+
  theme(
    legend.position = c(0.9, 0.8),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_blank()
  )+
  xlab('')+
  ylab(bquote('Rate ('*mg~ L^-1~ d^-1*')'))+
  #ylim(c(-1,0))+
  scale_x_discrete(labels = decay_labels)+
  theme(
    text = element_text(size = 25),
    axis.title.y = element_text(margin = unit(c(0,2,0,0), 'mm'), size = 25),
    #axis.text.x = element_text(angle = 25, hjust = 1)
  )
  #facet_wrap(~lake, scales = 'free')
do_decay_plt

ggsave(here('output/lake_final_plts/do_decay_boxplot_update_2025.04.19.jpeg'), dpi = 300, width = 14, height = 10, units = 'in')

# **9c. Large v. small boxplot----
do_reorder$size <- factor(do_reorder$size, 
                          levels = c('Shallow', 'Deep'))
#upper case labels
size_labels <- c('Shallow', 'Deep')


do_decay_size_plt <- ggplot(data = do_reorder)+
  #geom_boxplot(aes( y = drift))+
  geom_boxplot(aes(x = size, y = drift), outlier.shape = NA)+ #reorder(lake,drift,mean)
  geom_jitter(aes(x = size, y = drift), alpha = 0.7, size = 3, width = 0.1, stroke = 1.2)+ #, pch = 21, width = 0.2,
  theme_classic()+
  theme(
    legend.position = c(0.9, 0.3),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_blank()
  )+
  xlab('')+
  ylab(bquote('Rate ('*mg~ L^-1~ d^-1*')'))+
  #ylim(c(-1,0))+
  scale_x_discrete(labels = size_labels)+
  theme(
    text = element_text(size = 25),
    axis.title.y = element_text(margin = unit(c(0,2,0,0), 'mm'), size = 25),
    #axis.text.x = element_text(angle = 45, hjust = 1)
  )

do_decay_size_plt

#ggsave(here('output/lake_final_plts/do_decay__size_boxplot_2024.11.20.jpeg'), dpi = 300, width = 14, height = 10, units = 'in')

# **9d. Mann-Whitney test between large v small----

size_dif <- wilcox.test(drift ~ size, data = do_reorder, exact = F)

size_dif #Yes, the groups are statistically different, p < 0.05 
        #(exact: p-value = 1.79e-06, n = 53)

#length(do_reorder$lake)









# do_decay_time_plt <- ggplot(data = do_reorder %>% filter(segment == 1, changepoint == 1) %>% mutate(water_year_2 = as.numeric(water_year)))+ #, lake != 'soapstone'
#   #geom_boxplot(aes( y = drift))+
#   #geom_boxplot(aes(x = reorder(lake,drift,mean), y = drift))+
#   geom_point(aes(x = water_year, y = drift, shape = lake), size = 4)+ #, pch = 21,, stroke = 1.2
#   geom_line(aes(x = water_year_2, y = drift, linetype = lake), size = 1.1)+
#   theme_classic()+
#   theme(
#     legend.position = c(0.2, 0.25 ),
#     legend.background = element_rect(fill = "white", color = "white"),
#     legend.title = element_blank()
#   )+
#   xlab('')+
#   ylab(bquote('Oxygen Loss Rate ('*mg~ L^-1~ d^-1*')'))+
#   theme(
#     text = element_text(size = 38),
#     axis.title.y = element_text(margin = unit(c(0,5,0,0), 'mm')),
#     legend.text = element_text(size = 38),
#     legend.key.size = unit(10, 'point')
#   )+
#   scale_shape_discrete(labels = c('Cedar', 'Gumboot', 'Cliff', 'Castle'))+
#   scale_linetype_discrete(labels = c('Cedar', 'Gumboot', 'Cliff', 'Castle'))
#   #facet_wrap(~lake, scales = 'free')
# do_decay_time_plt
# 
# #ggsave(here('output/lake_final_plts/do_by_year.jpeg'), dpi = 300, width = 14, height = 10)
# 


# 10. Supp. plt - winter v summer decay rates -----------------------------

#Eliminate small lakes

do_decay_cal <- do_reorder %>% 
  filter(
    lake == 'castle'
  ) %>% 
  mutate(
    period = if_else(
      #WY 2018
      start >= ymd('2017-12-04') & end <= ymd('2018-04-11') |
      start >= ymd('2018-12-24') & end <= ymd('2019-06-01') |
      start >= ymd('2019-12-20') & end <= ymd('2020-04-25') |
      start >= ymd('2020-12-20') & end <= ymd('2021-05-02') |
      start >= ymd('2021-12-15') & end <= ymd('2022-04-02'),
      as.factor('ice'),
      as.factor('open')
    )
  )

do_decay_clf <- do_reorder %>% 
  filter(
    lake == 'cliff'
  ) %>% 
  mutate(
    period = if_else(
      start >= ymd('2018-12-04') & end <= ymd('2019-05-31') |
      start >= ymd('2019-12-09') & end <= ymd('2020-04-27') |
      start >= ymd('2020-12-01') & end <= ymd('2021-04-30') |
      start >= ymd('2021-12-14') & end <= ymd('2022-04-13'),
      as.factor('ice'),
      as.factor('open')
    )
  )

#Combine Cal and Clf data

do_decay_deep <- do_decay_cal %>% 
  bind_rows(do_decay_clf)

#Deep summary rates

do_decay_deep_season <- do_decay_deep %>% 
  group_by(lake, period) %>% 
  summarise(
    mean = mean(drift),
    median = median(drift),
    low = max(drift),
    high = min(drift)
  ) 

#Establish the order of the lakes from small to large

do_decay_deep$lake <- factor(do_decay_deep$lake, 
                          levels = c('cliff', 'castle'))

#upper case labels
decay_labels_deep <- c('Under Ice', 'Open Water')


#DO decay plot
do_decay_deep_plt <- ggplot(data = do_decay_deep)+
  geom_boxplot(aes(x = period, y = drift), outlier.shape = NA)+ #reorder(lake,drift,mean)
  geom_jitter(aes(x = period, y = drift, shape = lake), size = 3, width = 0.1, stroke = 1.2)+ #, pch = 21, width = 0.2,
  theme_classic()+
  theme(
    legend.position = c(0.9, 0.2),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_blank()
  )+
  xlab('')+
  ylab(bquote('Rate ('*mg~ L^-1~ d^-1*')'))+
  #ylim(c(-1,0))+
  scale_x_discrete(labels = decay_labels_deep)+
  scale_shape_discrete(breaks=c("castle", "cliff"),
                       labels=c("Castle", "Cliff"))+
  theme(
    text = element_text(size = 25)
  )
#facet_wrap(~lake, scales = 'free')
do_decay_deep_plt

# ggsave(here(
#   'output/lake_final_plts/supp_figs/sum_v_win_decay_rates.png'
#   ),
#   dpi = 300,
#   height = 5,
#   width = 7,
#   units = 'in'
# )

wilcox.test(data = do_decay_deep, drift ~ period)


# 11. Shallow v deep plt revised 2025.01.06 -------------------------------

do_reorder_shallow <- do_reorder %>% 
  filter(
    size == 'Shallow'
  ) %>% 
  mutate(
    period = as.factor('ice')
  )

do_all <- do_reorder_shallow %>% 
  bind_rows(do_decay_deep)

do_decay_size_plt2 <- ggplot(data = do_all)+
  #geom_boxplot(aes( y = drift))+
  geom_boxplot(aes(x = size, y = drift), outlier.shape = NA)+ #reorder(lake,drift,mean)
  geom_jitter(aes(x = size, y = drift, shape = period), alpha = 0.7, size = 5, width = 0.2, stroke = 1.2)+ #, pch = 21, width = 0.2,
  theme_classic()+
  theme(
    #legend.position = c(0.9, 0.2),
    legend.position = 'none',
    #legend.background = element_rect(fill = "white", color = "white"),
    #legend.title = element_blank()
  )+
  xlab('')+
  ylab(bquote('Rate ('*mg~ L^-1~ d^-1*')'))+
  ylim(c(-0.32,0))+
  scale_x_discrete(labels = size_labels)+
  #scale_shape_manual(labels = c('Ice', "Open"), values = c('circle', 'triangle'))+
  theme(
    text = element_text(size = 25),
    axis.title.y = element_text(margin = unit(c(0,2,0,0), 'mm'), size = 25),
    #axis.text.x = element_text(angle = 45, hjust = 1)
  )

do_decay_size_plt2

# ggsave(here('output/lake_final_plts/do_decay_size_boxplot_2025.12.06.jpeg'), 
#        dpi = 300, 
#        width = 7, height = 6, 
#        units = 'in')

do_all_summary_lake <- do_all %>% 
  group_by(lake, period) %>% 
  summarise(
    mean = mean(drift),
    median = median(drift),
    max = max(drift),
    min = min(drift)
  )

do_all_summary_size <- do_all %>% 
  group_by(size, period) %>% 
  summarise(
    mean = mean(drift),
    median = median(drift),
    max = max(drift),
    min = min(drift)
  )

do_all_ice <- do_all %>% 
  filter(
    period == 'ice'
  )

wilcox.test(data = do_all_ice, drift ~ size)














cal_test <- do_reorder %>% 
  filter(
    lake == 'castle' | lake == 'cliff'
  ) %>% 
  group_by(lake, water_year) %>% 
  summarise(
    min = min(drift, na.rm = T),
    max = max(drift, na.rm = T),
    range = range(drift)
  )

clf_test <- do_reorder %>% 
  filter(
    lake == 'castle' | lake == 'cliff'
  ) %>% 
  group_by(lake) %>% 
  summarise(
    min = min(drift, na.rm = T),
    max = max(drift, na.rm = T),
    range = range(drift)
  )

test <- do_reorder %>% 
  filter(
    lake == 'castle' | lake == 'cliff',
    drift > -0.1
  ) %>% 
  group_by(lake) %>% 
  summarise(
    range = range(drift)
  )


# 12. Systematic stats analysis -------------------------------------------


# **12a. Deep lake intercomparison ------------------------------------------

#1.	Castle (all data) vs. Cliff (all data)

wilcox.test(
  data = do_all %>% filter(lake == 'castle' | lake == 'cliff'), 
  drift ~ lake
)
# W = 201, p-value = 0.3232
# Castle and Cliff median DO depletion rates 
# are not significantly different.

#2.	Castle (under ice) vs. Castle (open water)

wilcox.test(
  data = do_all %>% filter(lake == 'castle'),
  drift ~ period
)

# W = 86, p-value = 0.01265
# Castle is significantly different between periods 
# (i.e., ice vs open water)

#3.	Cliff (under ice) vs. Cliff (Open water)

wilcox.test(
  data = do_all %>% filter(lake == 'cliff'), 
  drift ~ period
)

# W = 0, p-value = 0.0002498
# Cliff is significantly different between periods 
# (i.e., ice vs open water)

#4.	Castle (under ice) vs. Cliff (under ice)

wilcox.test(
  data = do_all %>% 
    filter(period == 'ice') %>% 
    filter(lake == 'castle' | lake == 'cliff'), 
  drift ~ lake
)

# W = 115, p-value = 0.001145
# Castle and Cliff are significantly different 
# during the ice cover period

#5.	Castle (open water) vs. Cliff (open water)

wilcox.test(
  data = do_all %>% 
    filter(period == 'open') %>% 
    filter(lake == 'castle' | lake == 'cliff'), 
  drift ~ lake
)

# W = 0, p-value = 0.000666
# Castle and Cliff are significantly different 
# during the open water period


# **12b. Shallow lake intercomparison  ------------------------------------

#NOTE: For the shallow lake intercomparison, I use the do_decay dataframe 
#      rather than the do_all (as in the deep lake intercomparison)
#      because do_all removes a data point for Soapstone because the number
#      of days for the rate is <5 days. However, the rate is legitimate,
#      so I've included that point for intercomparison

#6.	Gumboot (under ice) v. Cedar (under ice)

wilcox.test(
  data = do_decay %>% 
    filter(lake == 'gumboot' | lake == 'cedar'), 
  drift ~ lake
)

# W = 8, p-value = 0.2468
# Gumboot and Cedar are not significantly different 

#7.	Gumboot (under ice) v. Soapstone (under ice)

wilcox.test(
  data = do_decay %>% 
    filter(lake == 'gumboot' | lake == 'soapstone'), 
  drift ~ lake
)

# W = 38, p-value = 0.01399
# Gumboot and Soaprstone are not significantly different 

#6.	Cedar (under ice) v. Soapstone (under ice)

wilcox.test(
  data = do_decay %>% 
    filter(lake == 'soapstone' | lake == 'cedar'), 
  drift ~ lake
)

# W = 31, p-value = 0.0303
# Cedar and Soapstone are not significantly different 


# **12c. Shallow vs. deep intercomparison ---------------------------------

#Data to make comparisons
ice_comparison <- do_all %>% 
  filter(
    period == 'ice'
  )

ice_v_open <- do_all %>% 
  mutate(
    period = if_else(
      lake == 'castle' & period == 'ice' | lake == 'cliff' & period == 'ice',
      NA,
      period
    )
  ) %>% 
  na.omit()

#7. Gumboot v Castle open
wilcox.test(
  data = ice_v_open %>% 
    filter(lake == 'gumboot' | lake == 'castle'), 
  drift ~ lake
)

# W = 42, p-value = 0.01998
# Gumboot and Castle open are significantly different

#8. Gumboot v Castle ice
wilcox.test(
  data = ice_comparison %>% 
    filter(lake == 'gumboot' | lake == 'castle'), 
  drift ~ lake
)

# W = 72, p-value = 0.002211
# Gumboot and Castle ice are significantly different

#9. Gumboot v Cliff open
wilcox.test(
  data = ice_v_open %>% 
    filter(lake == 'gumboot' | lake == 'cliff'), 
  drift ~ lake
)

# W = 36, p-value = 0.002165
# Gumboot and Cliff open are significantly different

#10. Gumboot v Cliff ice
wilcox.test(
  data = ice_comparison %>% 
    filter(lake == 'gumboot' | lake == 'cliff'), 
  drift ~ lake
)

# W = 44, p-value = 0.1471
# Gumboot and Cliff ice are not significantly different

#11. Cedar v Castle open
wilcox.test(
  data = ice_v_open %>% 
    filter(lake == 'cedar' | lake == 'castle'), 
  drift ~ lake
)

# W = 38, p-value = 0.006216
# Cedar and Castle open are significantly different

#12. Cedar v Castle ice
wilcox.test(
  data = ice_comparison %>% 
    filter(lake == 'cedar' | lake == 'castle'), 
  drift ~ lake
)

# W = 60, p-value = 0.004435
# Cedar and Castle ice are significantly different

#13. Cedar v Cliff open
wilcox.test(
  data = ice_v_open %>% 
    filter(lake == 'cedar' | lake == 'cliff'), 
  drift ~ lake
)

# W = 0, p-value = 0.004329
# Cedar and Cliff open are significantly different

#14. Cedar v Cliff ice
wilcox.test(
  data = ice_comparison %>% 
    filter(lake == 'cedar' | lake == 'cliff'), 
  drift ~ lake
)

# W = 7, p-value = 0.02797
# Cedar and Cliff ice are significantly different

#NOTE: In the shallow lake intercomparison, I used the do_decay dataframe
#      however, for the shallow deep comparisons with Soapstone I am using 
#      the do_all data (root data of ice_v_open and ice_comparison) because
#      Soapstone was sig. dif. from both lakes even without the extremely low
#      point available in do_decay. So the do_decay data is not required for 
#      the Mann-Whitney U test with Soapstone.

#15. Soapstone v Castle open
wilcox.test(
  data = ice_v_open %>% 
    filter(lake == 'soapstone' | lake == 'castle'), 
  drift ~ lake
)

# W = 39, p-value = 0.003108
# Soapstone and Castle open are significantly different

#16. Soapstone v Castle ice
wilcox.test(
  data = ice_comparison %>% 
    filter(lake == 'soapstone' | lake == 'castle'), 
  drift ~ lake
)

# W = 62, p-value = 0.001634
# Soapstone and Castle ice are significantly different

#17. Soapstone v Cliff open
wilcox.test(
  data = ice_v_open %>% 
    filter(lake == 'soapstone' | lake == 'cliff'), 
  drift ~ lake
)

# W = 30, p-value = 0.004329
# Soapstone and Cliff open are significantly different

#18. Soapstone v Cliff ice
wilcox.test(
  data = ice_comparison %>% 
    filter(lake == 'soapstone' | lake == 'cliff'), 
  drift ~ lake
)

# W = 47, p-value = 0.004662
# Soapstone and Cliff ice are significantly different
