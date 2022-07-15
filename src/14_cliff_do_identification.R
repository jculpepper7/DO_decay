## The goal of this script is to 
## 1. load Cliff data
## 2. do any essential cleaning
## 3. Aggregate data
## 4. Identify do decay periods

# 1. Load libraries-----------------------------------------
library(tidyverse)
library(here)
library(lubridate)

# 2. Import Cliff data (collected by Dr. Adrianne Smits)

#Cliff 2020
cliff_2020_rdata <- load(here('data/raw/cliff/Cliff_2020_winter_data.Rdata'))

cliff_2020 <- tibble(output.list[[1]][['datetime']],
                     output.list[[1]][['temp_3.75']],
                     output.list[[1]][['temp_4.61']],
                     output.list[[1]][['temp_8.41']],
                     output.list[[1]][['temp_13.41']],
                     output.list[[1]][['temp_18.37']],
                     output.list[[1]][['temp_23.33']],
                     output.list[[1]][['temp_24.3']],
                     output.list[[1]][['sediment']],
                     output.list[[1]][['DO_23.33']],
                     output.list[[1]][['Sat_23.33']],
                     output.list[[1]][['light_3.75']],
                     output.list[[1]][['cond_24.3']],
                     output.list[[1]][['level_4.61']]
                     )

cliff_2020_raw <- cliff_2020 %>% 
  select(
    date = 1,
    temp_03.8 = 2,
    temp_04.6 = 3,
    temp_08.4 = 4,
    temp_13.4 = 5,
    temp_18.4 = 6,
    temp_23.3 = 7,
    temp_24.3 = 8,
    temp_sediment = 9,
    do_mg_l = 10,
    do_sat = 11,
    light_3.8 = 12,
    cond_24.3 = 13,
    level_4.6 = 14
  ) %>% 
  mutate(
    water_year = if_else(month(date)>=10, year(date)+1, year(date))
  ) %>% 
  group_by(year(date), month(date), day(date)) %>% 
  summarise(
    temp_03.8 = mean(temp_03.8),
    temp_04.6 = mean(temp_04.6),
    temp_08.4 = mean(temp_08.4),
    temp_13.4 = mean(temp_13.4),
    temp_18.4 = mean(temp_18.4),
    temp_23.3 = mean(temp_23.3),
    temp_24.3 = mean(temp_24.3),
    temp_sediment = mean(temp_sediment),
    do_mg_l = mean(do_mg_l),
    do_sat = mean(do_sat),
    light_3.8 = mean(light_3.8),
    cond_24.3 = mean(cond_24.3),
    level_4.6 = mean(level_4.6)
  ) %>% 
  ungroup() %>% 
  rename(
    year = 1,
    month = 2,
    day = 3
  ) %>% 
  mutate(
    date = make_date(year = year, month = month, day = day),
    lake = as_factor(c('cliff'))
  ) %>% 
  select(-year, -month, -day) %>% 
  select(15, 14, 1:13)

#write_csv(cliff_2020_raw, here('data/raw/cliff/cliff_2020_winter_raw.csv'))

#Cliff 2021
cliff_2021_rdata <- load(here('data/raw/cliff/Cliff_2021_winter_data.Rdata'))

cliff_2021 <- tibble(output.list[[1]][['datetime']],
                     output.list[[1]][['temp_3.76']],
                     output.list[[1]][['temp_4.63']],
                     output.list[[1]][['temp_6.07']],
                     output.list[[1]][['temp_8.45']],
                     output.list[[1]][['temp_13.44']],
                     output.list[[1]][['temp_18.38']],
                     output.list[[1]][['temp_23.33']],
                     output.list[[1]][['temp_24.32']],
                     output.list[[1]][['sediment']],
                     output.list[[1]][['DO_23.33']],
                     output.list[[1]][['Sat_23.33']],
                     output.list[[1]][['light_3.76']],
                     output.list[[1]][['cond_24.32']],
                     output.list[[1]][['level_4.63']]
)

cliff_2021_raw <- cliff_2021 %>% 
  rename(
    date = 1,
    temp_03.8 = 2,
    temp_04.6 = 3,
    temp_06.1 = 4,
    temp_08.4 = 5,
    temp_13.4 = 6,
    temp_18.4 = 7,
    temp_23.3 = 8,
    temp_24.3 = 9,
    temp_sediment = 10,
    do_mg_l = 11,
    do_sat = 12,
    light_3.8 = 13,
    cond_24.3 = 14,
    level_4.6 = 15
  ) %>% 
  mutate(
    water_year = if_else(month(date)>=10, year(date)+1, year(date))
  ) %>% 
  group_by(year(date), month(date), day(date)) %>% 
  summarise(
    temp_03.8 = mean(temp_03.8),
    temp_04.6 = mean(temp_04.6),
    temp_06.1 = mean(temp_06.1),
    temp_08.4 = mean(temp_08.4),
    temp_13.4 = mean(temp_13.4),
    temp_18.4 = mean(temp_18.4),
    temp_23.3 = mean(temp_23.3),
    temp_24.3 = mean(temp_24.3),
    temp_sediment = mean(temp_sediment),
    do_mg_l = mean(do_mg_l),
    do_sat = mean(do_sat),
    light_3.8 = mean(light_3.8),
    cond_24.3 = mean(cond_24.3),
    level_4.6 = mean(level_4.6)
  ) %>% 
  ungroup() %>% 
  rename(
    year = 1,
    month = 2,
    day = 3
  ) %>% 
  mutate(
    date = make_date(year = year, month = month, day = day),
    lake = as_factor(c('cliff'))
  ) %>% 
  select(-year, -month, -day) %>% 
  select(16, 15, 1:14)

#write_csv(cliff_2021_raw, here('data/raw/cliff/cliff_2021_winter_raw.csv'))

# 3. Visialize cliff data--------------------------------------------------

cliff_2020_mean <- read_csv(here('data/raw/cliff/cliff_2020_winter_raw.csv'))
cliff_2021_mean <- read_csv(here('data/raw/cliff/cliff_2021_winter_raw.csv'))

clf_all <- bind_rows(cliff_2020_mean, cliff_2021_mean)

#write_csv(clf_all, here('data/processed/cliff/cliff_clean_agg_data_daily.csv'))

#Cliff 2020 plot
cliff_plt_20 <- ggplot(data = cliff_2020_mean, aes(x = date, y = do_mg_l))+
  geom_line(size = 1.5)+
  theme_classic()+
  xlab('date')+
  ylab('dissolved oxygen')
cliff_plt_20

ggplotly(cliff_plt_20)

#Cliff 2021 plot
cliff_plt_21 <- ggplot(data = cliff_2021_mean, aes(x = date, y = do_mg_l))+
  geom_line(size = 1.5)+
  theme_classic()+
  xlab('date')+
  ylab('dissolved oxygen')
cliff_plt_21

ggplotly(cliff_plt_21)

# 4. Ice on and ice off dates-----------------------------------------------
#NOTE: Derived from DO time series and game camera images

#2020
#ice on: 12-09-2019
#anoxia: 03-25-2020
#ice off: 04-27-2020

#2021
#ice on: 12-01-2020
#anoxia: 03-19-2021
#ice off: 04-30-2021

#NOTE: Early signs of ice on as of 11-30-2021

# 5. Visualizations of Cliff lake------------------------------------

cliff <- cliff_2020_mean %>% 
  bind_rows(cliff_2021_mean) %>% 
  select(1:10, 16, 11:15) %>% 
  pivot_longer(cols = 3:11, names_to = 'depth', values_to = 'temp_c')

#Temperature plot

cliff_temp_plt <- ggplot(data = cliff)+
  geom_line(aes(x = date, y = temp_c, color = depth))+
  geom_line(aes(x = date, y = do_mg_l))+
  theme_classic()+
  xlab("Date")+
  ylab("DO [mg/L] & Temperature [C]")+
  facet_wrap(~water_year)
cliff_temp_plt  

