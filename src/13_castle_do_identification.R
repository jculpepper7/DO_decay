## The goal of this script is to visualize the Castle data
## and identify DO decay periods

# 1. Load libraries-----------------------------------------

library(tidyverse)
library(lubridate)
library(padr)

# 2. Import Castle clean, aggregated data-------------------

cal <- read_csv(here('data/processed/castle/castle_clean_agg_data.csv'))

# 3. Summarise to mean daily data---------------------------

cal_daily <- cal %>% 
  select(pst, temp_c, do_mg_l, do_sat) %>% 
  mutate(
    year = year(pst),
    month = month(pst),
    day = day(pst)
  ) %>% 
  select(-pst) %>% 
  group_by(year, month, day) %>% 
  summarise(
    temp_c = mean(temp_c),
    do_mg_l = mean(do_mg_l),
    do_sat = mean(do_sat)
  ) %>% 
  ungroup() %>% 
  mutate(
    date = make_date(year = year, month = month, day = day),
    water_year = if_else(month(date)>=10, year(date)+1, year(date))
  ) %>% 
  select(date, water_year, temp_c, do_mg_l, do_sat) 

# 4. Initial visualization---------------------------------

cal_full <- ggplot(data = cal_daily)+
  geom_line(aes(x = date, y = do_mg_l))+
  theme_classic()
ggplotly(cal_full)  

# 5. Cut remaining problem dates-----------------------------

cal_daily_fixed <- cal_daily %>% 
  filter(
    date != as.POSIXct('2019-10-25'),
    date != as.POSIXct('2020-10-08')
    )

#Searching for row numbers for below dates 
#the period between these dates includes when the miniDOT was placed
#in the incorrect place, owing to adverse weather conditions
which(cal_daily_fixed$date == as.POSIXct('2019-08-19'))
which(cal_daily_fixed$date == as.POSIXct('2019-09-05'))

cal_daily_fixed <- cal_daily_fixed[-c(754:771),] #Unclear why this increase occurs

which(cal_daily_fixed$date == as.POSIXct('2020-07-25'))
which(cal_daily_fixed$date == as.POSIXct('2020-07-31'))

cal_daily_fixed <- cal_daily_fixed[-c(1075:1081),] #Unclear why this increase occurs

write_csv(cal_daily_fixed, here('data/processed/castle/castle_clean_agg_data_daily.csv'))
# 6. Visualize fully cleaned data----------------------------

cal_full_fixed <- ggplot(data = cal_daily_fixed)+
  geom_line(aes(x = date, y = do_mg_l))+
  theme_classic()
ggplotly(cal_full_fixed) 

# 7. Identify peaks of DO and when DO decay ends------------

# Water year 2018 -- ice on: 2018-01-06, ice off: 2018-04-11
#First: 2017-12-04 through 2018-01-03
#Second: 2018-01-18 through 2018-02-07
#Third: 2018-02-17 through 2018-04-11
#Fourth (period from ice off to anoxia) 2018-04-20 through 2018-09-22 

# Water year 2019 -- ice on: 2018-12-24, ice off: 2019-06-01
#First: 2018-12-24 through 2019-06-01
#Second (period from ice off to anoxia) 2019-06-11 through 2019-08-18
#   NOTE: Had to stop decay at 8/18 because the data between 8/18 and 9/24 was suspicious
#         Without notes on the sudden DO change (see raw data plot on line 38), I had to cut that data

# Water year 2020 -- ice on: 2019-12-20, ice off: 2020-04-25
#First: 2019-12-20 through 2020-02-20 prior to DO increase while under ice
#Second: 2020-03-06 through 2020-04-25 from increase in DO to ice off
#Third: 2020-04-26 through 2020-06-16 from ice off to a second bump in DO
#Fourth: 2020-06-19 through 2020-08-25 I included a short break from missing data, but the general trend continued.

#Water year 2021 -- ice on: 2020-12-20, ice off: 2021-05-02
#First: 2020-12-20 through 2021-05-02
#second: 2021-05-06 through 2021-06-11
#Third: 2021-06-13 through 2021-07-17 (through to anoxia)

#Water year 2022 -- ice on: 2021-12-15, ice off: 2022-04-02
#First: 2021-12-15 through 2022-01-15 -- trend significantly changes after 2022-02-15 
#Second: 2022-01-16 through 2022-02-01 -- this is the remaining decay to complete annoxia





