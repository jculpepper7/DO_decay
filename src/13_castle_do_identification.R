## The goal of this script is to visualize the Castle data
## and identify DO decay periods

# 1. Load libraries-----------------------------------------

library(tidyverse)
library(lubridate)
library(padr)
library(plotly)
library(patchwork)
library(here)

# 2. Import Castle clean, aggregated data-------------------

cal_30m <- read_csv(here('data/processed/castle/castle_clean_agg_data_30m.csv'))

# 3. Summarise to mean daily data---------------------------

# 3a. 30 m data----
cal_30m_daily <- cal_30m %>% 
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
    water_year = if_else(month(date)>=10, year(date)+1, year(date)),
    depth = as.factor('30m')
  ) %>% 
  select(date, depth, water_year, temp_c, do_mg_l, do_sat) 

# 4. Initial visualization------------------------------------------------------

# cal_full <- ggplot(data = cal_30m_daily)+
#   geom_line(aes(x = date, y = do_mg_l))+
#   theme_classic()
# ggplotly(cal_full)  

# 5. Cut remaining problem dates from 30m data----------------------------------

cal_30m_daily <- cal_30m_daily %>% 
  filter(
    date != as.POSIXct('2019-10-25'),
    date != as.POSIXct('2020-10-08')
    )

#Searching for row numbers for below dates 
#the period between these dates includes when the miniDOT was placed
#in the incorrect place, owing to adverse weather conditions
which(cal_30m_daily$date == as.POSIXct('2019-08-19'))
which(cal_30m_daily$date == as.POSIXct('2019-09-05'))

cal_30m_daily <- cal_30m_daily[-c(754:771),] #Unclear why this increase occurs

which(cal_30m_daily$date == as.POSIXct('2020-07-25'))
which(cal_30m_daily$date == as.POSIXct('2020-07-31'))

cal_30m_daily <- cal_30m_daily[-c(1075:1081),] #Unclear why this increase occurs

#write_csv(cal_30m_daily, here('data/processed/castle/castle_clean_agg_data_daily.csv'))
# 6. Visualize fully cleaned data----------------------------

cal_30m_daily <- cal_30m_daily %>% 
  group_by(water_year) %>% 
  complete(date = seq.Date(min(date), max(date), by = 'day'))

cal_30m_daily_plt <- ggplot(data = cal_30m_daily)+
  geom_line(aes(x = date, y = temp_c))+
  theme_classic()
ggplotly(cal_30m_daily_plt) 

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
#First: 2021-12-15 through 2022-01-15 -- trend significantly changes after 2022-01-15 
#Second: 2022-01-16 through 2022-02-01 -- this is the remaining decay to complete anoxia

# 8. Add temperature data from 3m, 10, and 20m miniDOTs-------------------------

# 8a Import data----
cal_03m <- read_csv(here('data/processed/castle/castle_clean_agg_data_03m.csv'))
cal_10m <- read_csv(here('data/processed/castle/castle_clean_agg_data_10m.csv'))
cal_20m <- read_csv(here('data/processed/castle/castle_clean_agg_data_20m.csv'))

# 8b. Daily averages at 03m----
cal_03m_daily <- cal_03m %>% 
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
    water_year = if_else(month(date)>=10, year(date)+1, year(date)),
    depth = as.factor('03m')
  ) %>% 
  select(date, depth, water_year, temp_c, do_mg_l, do_sat) 

#Remove time periods where values from 03m
which(cal_03m_daily$date == as.POSIXct('2019-08-19')) #669
which(cal_03m_daily$date == as.POSIXct('2019-09-05')) #686

cal_03m_daily <- cal_03m_daily[-c(669:686),] #Unclear why this increase occurs

which(cal_03m_daily$date == as.POSIXct('2020-05-12')) #917
which(cal_03m_daily$date == as.POSIXct('2020-07-01')) #967

cal_03m_daily <- cal_03m_daily[-c(917:967),] #Unclear why this increase and subsequent decrease occurs

which(cal_03m_daily$date == as.POSIXct('2020-07-24')) #972
which(cal_03m_daily$date == as.POSIXct('2020-07-30')) #982

cal_03m_daily <- cal_03m_daily[-c(972:982),] #Unclear why this increase occurs

# 8c. Daily averages at 10m----
cal_10m_daily <- cal_10m %>% 
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
    water_year = if_else(month(date)>=10, year(date)+1, year(date)),
    depth = as.factor('10m')
  ) %>% 
  select(date, depth, water_year, temp_c, do_mg_l, do_sat) 

#Remove anomalously high values from 10m
which(cal_10m_daily$date == as.POSIXct('2019-08-19')) #669
which(cal_10m_daily$date == as.POSIXct('2019-09-05')) #686

cal_10m_daily <- cal_10m_daily[-c(669:686),] #Unclear why this increase occurs

which(cal_10m_daily$date == as.POSIXct('2020-06-18')) #954
which(cal_10m_daily$date == as.POSIXct('2020-07-01')) #967

cal_10m_daily <- cal_10m_daily[-c(954:967),] #Unclear why this increase occurs

which(cal_10m_daily$date == as.POSIXct('2020-07-24')) #972
which(cal_10m_daily$date == as.POSIXct('2020-07-30')) #982

cal_10m_daily <- cal_10m_daily[-c(972:982),] #Unclear why this increase occurs

# 8d. Daily averages at 20m----
cal_20m_daily <- cal_20m %>% 
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
    water_year = if_else(month(date)>=10, year(date)+1, year(date)),
    depth = as.factor('20m')
  ) %>% 
  select(date, depth, water_year, temp_c, do_mg_l, do_sat)  

#Remove anomalously high values from 10m
which(cal_20m_daily$date == as.POSIXct('2019-08-19')) #669
which(cal_20m_daily$date == as.POSIXct('2019-09-05')) #686

cal_20m_daily <- cal_20m_daily[-c(669:686),] #Unclear why this increase occurs

which(cal_20m_daily$date == as.POSIXct('2020-06-18')) #954
which(cal_20m_daily$date == as.POSIXct('2020-07-01')) #967

cal_20m_daily <- cal_20m_daily[-c(954:967),] #Unclear why this increase occurs

which(cal_20m_daily$date == as.POSIXct('2020-07-24')) #972
which(cal_20m_daily$date == as.POSIXct('2020-07-30')) #982

cal_20m_daily <- cal_20m_daily[-c(972:982),] #Unclear why this increase occurs

which(cal_20m_daily$date == as.POSIXct('2020-10-08')) #1030

cal_20m_daily <- cal_20m_daily[-1030,] #Unclear why this increase occurs

# 
# ggplotly(ggplot(data = cal_03m_daily, aes(x = date, y = temp_c))+
#            geom_line(size = 1.2))

# 9. Aggregate the miniDOT data from each depth---------------------------------

cal_all_depths <- bind_rows(cal_03m_daily, cal_10m_daily, cal_20m_daily, cal_30m_daily) %>% 
  pad() %>% 
  mutate(
    lake = 'castle'
  )

#write_csv(cal_all_depths, here('data/processed/cal_hypox_plt.csv'))

# 10. Final plot for Castle-------------------------------------
library(padr)

cal_all_depths <- read_csv(here('data/processed/cal_hypox_plt.csv')) %>% 
  mutate(
    depth = as.factor(depth),
    lake = as.factor(lake)
  ) %>% 
  na.omit() %>% 
  group_by(depth) %>%
  pad(
    interval = 'day'
  ) %>%
  ungroup

# coeff <- 1

castle_avg_plt <- ggplot()+ 
  geom_rect(aes(
    xmin = ymd('2017-12-11'),
    xmax = ymd('2018-04-11'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'grey', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2018-12-24'),
    xmax = ymd('2019-06-01'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2019-12-20'),
    xmax = ymd('2020-04-25'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2020-12-20'),
    xmax = ymd('2021-05-21'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2021-12-15'),
    xmax = ymd('2022-04-02'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_line(
    data = cal_all_depths %>% filter(date>='2017-10-01'), 
    aes(x = date, y = temp_c/2, color = depth), 
    linewidth = 1, 
    alpha = 0.7
  )+ #%>% filter(depth == '03m' | depth == '10m' | depth == '20m' |
  geom_line(
    data = cal_all_depths %>% filter(depth =='30m', date>='2017-10-01'), 
    aes(x = date, y = do_mg_l), 
    linewidth = 1
  )+
  #scale_color_grey(name = 'Depth   ')+
  scale_color_viridis_d()+
  theme_classic()+
  xlab('')+ 
  ylab('')+
  theme(legend.position = 'bottom',
        # legend.title = element_text(size = 14),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_blank()
        )+
  #scale_y_continuous(breaks = c(0,10,20))
  scale_y_continuous(
    name = expression("Dissolved Oxygen (mg L"^{-1}*")"),
    breaks = seq(0,12,3),
    sec.axis = sec_axis(
      ~. *2,
      name = 'Temperature (\u00b0C)',
      breaks = seq(0,24,6)
    )
  )+
  guides(
    color = guide_legend(nrow = 1)
  )
castle_avg_plt
#ggplotly(castle_avg_plt)

ggsave(
  # here('output/lake_final_plts/castle_do_plt_w_temp_update_pdf.png'), 
  # here('output/lake_final_plts/do_ts_fig/update/castle_do_plt_w_temp_update.pdf'), 
  here('output/lake_final_plts/do_ts_fig/update/castle_do_plt_w_temp_update_NAs.pdf'), 
  dpi = 300,
  width = 6.5,
  height = 3.75
)


#combine castle and cliff

#cliff_avg_plt / castle_avg_plt

#ggsave(here('output/lake_final_plts/castle_cliff_combined.jpeg'), dpi = 300, width = 16, height = 12, units = 'in')

test_do <- cal_all_depths %>% 
  filter(
    date >= ymd('2020-12-20') & date <= ymd('2021-05-21'),
    depth == '30m'
  ) %>% 
  select(
    time = date, 
    DO = do_mg_l
  )
  
# write_csv(test_do, here('data/test_do.csv'))

