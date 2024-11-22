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
  select(lake, date, do_mg_l) %>% 
  mutate(period =
    #determine when the lake was ice covered and open
    if_else( 
      #WY 2018
        date>=ymd('2017-12-11') & date<=ymd('2018-04-11') |
      #WY 2019
        date>=ymd('2018-12-24') & date<=ymd('2019-06-01') |
      #WY 2020
        date>=ymd('2019-12-20') & date<=ymd('2020-04-25') |
      #WY 2021
        date>=ymd('2020-12-20') & date<=ymd('2021-05-21') |
      #WY 2022
        date>=ymd('2021-12-15') & date<=ymd('2022-04-02'),
      as.factor('ice'),
      as.factor('open')
    )
  )

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
  select(lake, date, do_mg_l) %>% 
  mutate(period = 
    #determine when the lake was ice covered and open
    if_else(
      #WY 2020
        date>=ymd('2019-11-26') & date<=ymd('2020-04-12') |
      #WY 2021
        date>=ymd('2020-11-18') & date<=ymd('2021-04-17') |
      #WY 2022
        date>=ymd('2021-12-12') & date<=ymd('2022-04-02'),
      as.factor('ice'),
      as.factor('open')
    )
  ) 

#Cliff lake aggregated data and isolate DO
clf_do <- read_csv(here('data/processed/cliff/cliff_clean_agg_data_daily.csv')) %>% 
  mutate(
    lake = 'cliff'
  ) %>% 
  select(lake, date, do_mg_l)%>% 
  mutate(period = 
    #determine when the lake was ice covered and open
    if_else(
        #WY 2019
        date>=ymd('2018-12-04') & date<=ymd('2019-05-31') |
        #WY 2020
        date>=ymd('2019-12-09') & date<=ymd('2020-05-01') |
        #WY 2021
        date>=ymd('2020-12-01') & date<=ymd('2021-04-11') |
        #WY 2022
        date>=ymd('2021-12-14') & date<=ymd('2022-04-13'),
      as.factor('ice'),
      as.factor('open')
    )
  )

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
  select(lake, date, do_mg_l) %>% 
  mutate(period = 
    #determine when the lake was ice covered and open
    if_else(
        #WY 2020
        date>=ymd('2019-11-23') & date<=ymd('2020-05-04') |
        #WY 2021
        date>=ymd('2020-11-18') & date<=ymd('2021-05-02') |
        #WY 2022
        date>=ymd('2021-12-18') & date<=ymd('2022-04-07'),
      as.factor('ice'),
      as.factor('open')
    )
  ) 

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
  select(lake, date, do_mg_l)  %>% 
  mutate(period = 
    #determine when the lake was ice covered and open
    if_else(
      #WY 2020
        date>=ymd('2019-11-21') & date<=ymd('2020-05-04') |
      #WY 2021
        date>=ymd('2020-11-06') & date<=ymd('2021-04-24') |
      #WY 2022
        date>=ymd('2021-12-11') & date<=ymd('2022-04-24'),
      as.factor('ice'),
      as.factor('open')
    )
  )    

#3. Join data------------------------------------------------------------

all_do <- bind_rows(cal_do, cdr_do, clf_do, gb_do, ss_do) %>% 
  mutate(
    water_year = if_else(month(date)>=10, year(date)+1, year(date)),
    year = year(date),
    hypoxic = if_else(
      do_mg_l <= 2, 
      do_mg_l,
      NA
    ),
    ice = if_else(
      period == 'ice',
      do_mg_l,
      NA
    ),
    open = if_else(
      period == 'open',
      do_mg_l,
      NA
    )
  )

#3. Isolate hypoxia periods

#Count the total number of days when oxygen was <=2 mg/L
hypox_total <- all_do %>%
  group_by(lake, year, period) %>% 
  filter(do_mg_l <= 2) %>% # & do_mg_l > 1
  summarise(
    hypox_total = n()
  )


# 4. Hypoxia plot ---------------------------------------------------------

ggplot(all_do)+
  geom_line(aes(x = date, y = hypoxic, color = as.factor(year(date))), 
            alpha = 0.4, linewidth = 2)+
  geom_line(aes(x = date, y = open), color = 'black', alpha = 1)+
  geom_line(aes(x = date, y = ice), color = 'grey50', alpha = 1)+
  geom_vline(xintercept = c(ymd('2018-01-01'), ymd('2019-01-01'), ymd('2020-01-01'), ymd('2021-01-01'), ymd('2022-01-01')))+
  facet_wrap(~lake, ncol = 1)+
  theme_classic()
  

# TK. Anoxia and combined data --------------------------------------------



#Count the total number of days when oxygen was <1 mg/L
# anoxia_total <- all_do %>%
#   group_by(lake, water_year) %>% 
#   filter(do_mg_l <= 1) %>% 
#   summarise(
#     anoxia_total = n()
#   )

#Count the longest period of days when oxygen was <=2 mg/L
# hypox_duration <- all_do %>%
#   group_by(lake, water_year, period) %>% 
#   filter(do_mg_l <= 2) %>% 
#   mutate(
#     diff = date - lag(date)
#   ) %>% 
#   filter(diff == 1) %>% 
#   summarise(
#     hypox_duration = last(date) - first(date)
#   )  

#Count the longest period of days when oxygen was <1 mg/L
# anoxia_duration <- all_do %>%
#   group_by(lake, water_year) %>% 
#   filter(do_mg_l < 1) %>% 
#   mutate(
#     diff = date - lag(date)
#   ) %>% 
#   filter(diff == 1) %>% 
#   summarise(
#     anoxic_duration = last(date) - first(date)
#   )  

#Combine hypoxia and anoxia tibbles
# low_do <- hypox_total %>% 
#   full_join(hypox_duration)

#write_csv(low_do, here('output/hypoxia_results/hypoxia_anoxia_total.csv'))
