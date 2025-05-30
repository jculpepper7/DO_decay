## The goal of this script is to
##
## 1. Clean disaggregated  Weatherhawk data
## 2. Aggregate Castle Lake meteorlogical data from WeatherHawk

# 1. Load libraries--------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(padr)
library(plotly)
library(patchwork)
library(cmocean)

#install.packages("devtools")
#devtools::install_github("thomasp85/scico")


# 2. Import weather data---------------------------------------------

# 2a. Import raw weatherhawk data----
weatherhawk_raw <- list.files(
  path = here('data/met_data/weatherhawk'),
  pattern = '*.csv',
  full.names = TRUE
) %>% 
  map_dfr(~read_csv(., col_types = cols(.default = "c"))) %>% 
  rename(wind_speed_avg = 15) %>% 
  select(-18)

#NOTE: upon inspection, the weatherhawk data changes date_time style 
#      (from ymd_hms to dmy_hms) which causes a failure to parse all
#      date_time data. Therefore, below I extract the varied date_time
#      data and correct it. Then I recombine the data into a single tibble.

# 2b. Extract problem rows (9398:16598) and amend data----
weatherhawk_raw_1 <- weatherhawk_raw %>% 
  slice(c(9398:16598)) %>% 
  clean_names() %>% 
  select(-1,-5,-7, -17, -18) %>% 
  mutate(
    date_time = mdy_hm(date_time),
    air_temp_avg = as.numeric(air_temp_avg),
    air_temp_min = as.numeric(air_temp_min),
    #air_temp_min_time = mdy_hm(air_temp_min_time),
    air_temp_max = as.numeric(air_temp_max),
    #air_temp_max_time = mdy_hm(air_temp_max_time),
    humidity = as.numeric(humidity),
    barometer = as.numeric(barometer),
    battery = as.numeric(battery),
    min_battery = as.numeric(min_battery),
    e_to = as.numeric(e_to),
    rain_yearly = as.numeric(rain_yearly),
    solar_avg = as.numeric(solar_avg),
    wind_speed_avg = as.numeric(wind_speed_avg),
    wind_speed_max = as.numeric(wind_speed_max),
    #wind_speed_max_time = mdy_hm(wind_speed_max_time),
    #wind_speed_avg_1 = as.numeric(wind_speed_avg_1),
    #wind_direction = as.numeric(wind_direction)
  ) 

#Take a look
str(weatherhawk_raw_1)  

# 2c. Remove problem rows and amend data----
weatherhawk_raw_2 <- weatherhawk_raw %>% 
  slice(-c(9398:16598)) %>% 
  clean_names() %>% 
  select(-1,-5,-7, -17, -18) %>% 
  mutate(
    date_time = ymd_hms(date_time),
    air_temp_avg = as.numeric(air_temp_avg),
    air_temp_min = as.numeric(air_temp_min),
    #air_temp_min_time = ymd_hms(air_temp_min_time),
    air_temp_max = as.numeric(air_temp_max),
    #air_temp_max_time = ymd_hms(air_temp_max_time),
    humidity = as.numeric(humidity),
    barometer = as.numeric(barometer),
    battery = as.numeric(battery),
    min_battery = as.numeric(min_battery),
    e_to = as.numeric(e_to),
    rain_yearly = as.numeric(rain_yearly),
    solar_avg = as.numeric(solar_avg),
    wind_speed_avg = as.numeric(wind_speed_avg),
    wind_speed_max = as.numeric(wind_speed_max),
    #wind_speed_max_time = ymd_hms(wind_speed_max_time),
    #wind_speed_avg_1 = as.numeric(wind_speed_avg_1),
    #wind_direction = as.numeric(wind_direction)
  ) #%>% 
  #select(-wind_speed_avg_1)

#Take a look
str(weatherhawk_raw_2)  

# 2d. Access 2019 air temp data----
#NOTE: Issues with 2019 raw data from Castle Lake data repo. 
#      Obtained air temperature and solar radiation data for missing segments

# weatherhawk_2019 <- read_csv(here('data/met_data/weatherhawk/cal_weatherhawk_2019.csv')) %>% 
#   clean_names() %>% 
#   select(-record_id) %>% 
#   mutate(
#     date_time = ymd_hms(date_time),
#     air_temp_avg = as.numeric(air_temp_avg),
#     air_temp_min = as.numeric(air_temp_min),
#     air_temp_min_time = ymd_hms(air_temp_min_time),
#     air_temp_max = as.numeric(air_temp_max),
#     air_temp_max_time = ymd_hms(air_temp_max_time),
#     humidity = as.numeric(humidity),
#     barometer = as.numeric(barometer),
#     battery = as.numeric(battery),
#     min_battery = as.numeric(min_battery),
#     e_to = as.numeric(e_to),
#     rain_yearly = as.numeric(rain_yearly),
#     solar_avg = as.numeric(solar_avg),
#     wind_speed_avg = as.numeric(wind_speed_avg),
#     wind_speed_max = as.numeric(wind_speed_max),
#     wind_speed_max_time = ymd_hms(wind_speed_max_time),
#     wind_speed_avg_1 = as.numeric(wind_speed_avg_1),
#     wind_direction = as.numeric(wind_direction)
#   ) %>% 
#   select(-wind_speed_avg_1)

# 3. Combine and clean raw weatherhawk data-------------------------------------

# 3a. Combine data----
weatherhawk <- bind_rows(weatherhawk_raw_1, weatherhawk_raw_2) %>% 
  arrange(date_time) #, weatherhawk_2019

#write_csv(weatherhawk, here('data/met_data/cal_weatherhawk_2017_2022.csv'))

# 3b. Take daily averages for the important variables----

weatherhawk <- read_csv(here('data/met_data/cal_weatherhawk_2017_2022.csv'))

weatherhawk_avg <- weatherhawk %>% 
  #select(1,2,3,5,7,8,12,13,14) %>%
  mutate(
    year = year(date_time),
    month = month(date_time),
    day = day(date_time)
  ) %>% 
  group_by(year, month, day) %>% 
  summarise(
    air_temp_avg = mean(air_temp_avg),
    air_min_avg = mean(air_temp_min),
    air_max_avg = mean(air_temp_max),
    humidity_avg = mean(humidity),
    barometer_avg = mean(barometer),
    rain_yearly = mean(rain_yearly),
    solar_avg = mean(solar_avg),
    wind_speed_avg = mean(wind_speed_avg)
  ) %>% 
  mutate(
    date_time = make_date(year = year, month = month, day = day)
  ) %>% 
  ungroup() %>% 
  select(12, 4:11) %>% 
  pad()

#write_csv(weatherhawk_avg, here('data/met_data/weatherhawk_avg_2017_2022.csv'))

# Add gridmet precip data----

gridmet_precip <- read_csv(here('data/met_data/gridmet/castle_gridmet_precip_2018_2022.csv'))

gridmet_precip <- gridmet_precip %>% 
  rename(
    date_time = 1, 
    precip_mm = 2
  )

temp_plt <- ggplot(data = weatherhawk_avg) +
  geom_line(aes(x = date_time, y = air_temp_avg), size = 1, color = 'black')+
  theme_classic()+
  xlab('')+
  ylab('Air Temperature (ºC)')+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.2, 0.80),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key.height= unit(0.6, 'in'),
    legend.key.width= unit(0.5, 'in'),
    text = element_text(size = 38),
    legend.key.size = unit(5, 'line'),
    axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"), size = 32),
    axis.text.x = element_blank()
  )#Alt+0 = degree symbol
ggplotly(temp_plt)

precip_plt <- ggplot() +
  geom_line(data = gridmet_precip, aes(x = date_time, y = precip_mm), size = 1, color = 'black')+
  theme_classic()+
  xlab('')+
  ylab('Precipitation (mm)')+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.2, 0.80),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key.height= unit(0.6, 'in'),
    legend.key.width= unit(0.5, 'in'),
    text = element_text(size = 38),
    legend.key.size = unit(5, 'line'),
    axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"), size = 32)
  )
  
ggplotly(precip_plt)

#combine temp and precip time series----
temp_plt/precip_plt

ggsave(here('output/lake_final_plts/temp_precip_plt.jpeg'), dpi = 300, width = 14, height = 10, units = 'in')

# 4. Evaluate average seasonal temperatures-------------------------------------

#Read in weatherhawk average data from 3b

weatherhawk_avg <- read_csv(here('data/met_data/weatherhawk_avg_2017_2022.csv'))

# 4a. Create seasonal column----

wh_seasonal <- weatherhawk_avg %>% 
  mutate(
    season = if_else(month(date_time)>=4 & month(date_time)<=6, 'spring',
                     if_else(month(date_time)>=7 & month(date_time)<=9, 'summer',
                             if_else(month(date_time)>=10 & month(date_time)<=12, 'fall', 'winter')))
  ) %>% 
  #na.omit() %>% 
  mutate(
    water_year = if_else(month(date_time)>=10, year(date_time)+1, year(date_time)),
    water_year = as.factor(water_year)
  )

# 4b. Plot temperatures by season----

#Fall average temperature plot
fall_plt <- wh_seasonal %>%
  filter(water_year != 2017,
         season == 'fall') %>%
ggplot()+
  geom_boxplot(aes(x = water_year, y = air_temp_avg))+
  #facet_wrap(~season)+
  theme_classic()+ 
  labs(x = '', y = '')+
  theme(legend.position = 'none')+
  scale_x_discrete("", breaks=factor(2018:2022), drop=FALSE)

#ggsave(here('output/met_results/fall_avg_temp.jpeg'), dpi = 300)  
  
#Winter average temperature plot
winter_plt <- wh_seasonal %>% 
  filter(water_year != 2017,
         season == 'winter') %>% 
  ggplot()+
  geom_boxplot(aes(x = water_year, y = air_temp_avg))+
  #facet_wrap(~)+
  theme_classic()+ 
  labs(x = '', y = '')+
  theme(legend.position = 'none')

#ggsave(here('output/met_results/winter_avg_temp.jpeg'), dpi = 300) 

#spring average temperature plot
spring_plt_2 <- wh_seasonal %>% 
  filter(water_year != 2017,
         season == 'spring') %>% 
  ggplot()+
  geom_boxplot(aes(x = water_year, y = air_temp_avg))+
  #facet_wrap(~)+
  theme_classic()+ 
  labs(x = '', y = '')+
  theme(legend.position = 'none')

#ggsave(here('output/met_results/spring_avg_temp.jpeg'), dpi = 300) 

#Summer average temperature plot
summer_plt <- wh_seasonal %>% 
  filter(water_year != 2017,
         water_year != 2022,
         season == 'summer') %>% 
  ggplot()+
  geom_boxplot(aes(x = water_year, y = air_temp_avg))+
  #facet_wrap(~)+
  theme_classic()+ 
  theme(legend.position = 'none')+
  scale_x_discrete("", breaks=factor(2018:2022), drop=FALSE)+
  ylab('')

#ggsave(here('output/met_results/summer_avg_temp.jpeg'), dpi = 300) 

#Combine temperature boxplots
(fall_plt+winter_plt)/(spring_plt+summer_plt)+
  plot_annotation(tag_levels = 'A')

#ggsave(here('output/lake_final_plts/temperature_boxplt.jpeg'), dpi = 300, width = 15, height = 10, units = 'in')

# 4c. Get output of temperature values------------------------------------------

seasonal_temp_summary <- wh_seasonal %>%
  select(-water_year) %>% #remove water year and add revised water year
  mutate(
    water_year = if_else(month(date_time)>=10, year(date_time)+1, year(date_time)) 
  ) %>% 
  group_by(water_year, season) %>% 
  summarise(
    temp_mean = mean(na.omit(air_temp_avg)),
    temp_med = median(na.omit(air_temp_avg)),
    temp_max_mean = mean(na.omit(air_max_avg)),
    temp_max_med = median(na.omit(air_max_avg)),
    temp_min_mean = mean(na.omit(air_min_avg)),
    temp_min_med = median(na.omit(air_min_avg)),
    solar_avg = mean(na.omit(solar_avg))
  )

#write_csv(seasonal_temp_summary, here('data/met_data/temperature_summary_stats.csv'))

# 5. Clean and plot SNODAS------------------------------------------------------

# 5a. Import SNODAS data----
cal_snodas <- read_csv(here('data/met_data/snodas/cal_snodas.csv')) %>% 
  rename(date = 1, swe_mm = 2) %>% 
  mutate(
    lake = as.factor('castle'),
    date = ymd(date),
    swe_mm = as.numeric(swe_mm)
  )
  
cdr_snodas <- read_csv(here('data/met_data/snodas/cdr_snodas.csv')) %>% 
  rename(date = 1, swe_mm = 2) %>% 
  mutate(
    lake = as.factor('cedar'),
    date = ymd(date),
    swe_mm = as.numeric(swe_mm)
  ) 

clf_snodas <- read_csv(here('data/met_data/snodas/clf_snodas.csv')) %>% 
  rename(date = 1, swe_mm = 2) %>% 
  mutate(
    lake = as.factor('cliff'),
    date = ymd(date),
    swe_mm = as.numeric(swe_mm)
  )  

gb_snodas <- read_csv(here('data/met_data/snodas/gb_snodas.csv')) %>% 
  rename(date = 1, swe_mm = 2) %>% 
  mutate(
    lake = as.factor('gumboot'),
    date = mdy(date), #file downloaded in a different date order (unsure why)
    swe_mm = as.numeric(swe_mm)
  )

ss_snodas <- read_csv(here('data/met_data/snodas/ss_snodas.csv')) %>% 
  rename(date = 1, swe_mm = 2) %>% 
  mutate(
    lake = as.factor('soapstone'),
    date = ymd(date),
    swe_mm = as.numeric(swe_mm)
  )

# 5b. Combine SNODAS data----
snodas <- bind_rows(cal_snodas, cdr_snodas, clf_snodas, gb_snodas, ss_snodas)

# 5c. SNODAS summary stats----

# SNODAS summary for all lakes
snodas_summ_total <- snodas %>% 
  mutate(
    water_year = if_else(month(date)>=10, year(date)+1, year(date))
  ) %>% 
  group_by(water_year) %>% 
  filter(swe_mm != 0) %>% 
  summarise(
    swe_mm_avg = mean(na.omit(swe_mm)),
    swe_mm_med = median(na.omit(swe_mm)),
    swe_mm_max = max(na.omit(swe_mm))
  )

#write_csv(snodas_summ_total, here('data/met_data/snodas/snodas_total.csv'))  

#snodas <- read_csv(here('data/met_data/snodas/snodas_total.csv'))

# SNODAS summary for each lakes
snodas_summ_each <- snodas %>% 
  mutate(
    water_year = if_else(month(date)>=10, year(date)+1, year(date))
  ) %>% 
  group_by(water_year, lake) %>% 
  filter(swe_mm != 0) %>% 
  summarise(
    swe_mm_avg = mean(na.omit(swe_mm)),
    swe_mm_med = median(na.omit(swe_mm)),
    swe_mm_max = max(na.omit(swe_mm))
  ) %>% 
  arrange(lake)

#write_csv(snodas_summ_each, here('data/met_data/snodas/snodas_per_lake.csv'))   
  
# 5c. Plot SNODAS data----
snodas %>% 
  mutate(
    water_year = as.factor(if_else(month(date)>=10, year(date)+1, year(date)))
  ) %>% 
  filter(swe_mm != 0) %>% 
ggplot()+
  geom_boxplot(aes(x = lake, y = swe_mm, fill = lake))+
  theme_classic()+
  labs(x = '', y = 'SWE [mm]')+
  facet_wrap(~water_year, scales = 'free')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#ggsave(here('output/met_results/snodas_by_lake.jpeg'), dpi = 300)  

swe_box <- snodas %>% 
  mutate(
    water_year = as.factor(if_else(month(date)>=10, year(date)+1, year(date)))
  ) %>% 
  filter(swe_mm != 0) %>% 
  ggplot()+
  geom_boxplot(aes(x = water_year, y = swe_mm))+
  theme_classic()+
  labs(x = '', y = 'SWE (mm)')+
  theme(#axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = 'none')

#ggsave(here('output/lake_final_plts/snodas_by_year_2022.08.15.jpeg'), dpi = 300, height = 10, width = 14, units = 'in')  

#SWE image with all years on same panel

snodas_oct <- snodas %>% 
  mutate(
    water_year = as.factor(if_else(month(date)>=10, year(date)+1, year(date))),
    doy = yday(date),
    doy_oct = if_else(doy >= 274, doy-273, doy+92)
  ) %>% 
  filter(lake == 'castle') %>% 
  mutate(
    month = month(date),
    day = day(date),
    date_short = if_else(month(date)>=10, make_date(2020, month, day), make_date(2021, month, day))
  ) %>% 
  filter(month != 7 & month != 8 & month != 9 & month != 10)

snodas_oct_plt <-   ggplot()+
  geom_line(data = snodas_oct, aes(x = date_short, y = swe_mm, color = water_year), 
            size = 1.2
            )+
  #scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_x_date(date_breaks = '1 months', date_labels = "%b ")+
  theme_classic()+
  #labs(x = 'Day of Year (Oct. 1)', y = 'SWE (mm)')+
  labs(x = '', y = 'SWE (mm)')+
  #facet_wrap(~water_year, scales = 'free')+
  #theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1))+
  #xlim(30,261)+
  #scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00"))+
  #scale_color_grey()+
  #scale_color_viridis_d(begin = 0.1, end = 0.9)+
  scale_color_cmocean(name = 'ice', 
                      start = 0.1,
                      end = 0.8,
                      discrete = T)+
  # geom_line(data = snodas_oct %>% filter(water_year == 2021),
  #           aes(x = doy_oct, y = swe_mm, color = water_year), size = 1.2)+
  #scale_linetype_identity(name = c('solid', 'twodash', 'F1', 'dotdash', 'longdash'))+
  #scale_linetype_discrete(c(1,3,5,7,9))+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.1, 0.70),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key.height= unit(0.4, 'in'),
    legend.key.width= unit(0.4, 'in'),
    text = element_text(size = 30),
    legend.key.size = unit(4, 'line'),
    axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"))
  )
snodas_oct_plt
#ggplotly(snodas_oct_plt)  
  
#ggsave(here('output/lake_final_plts/swe_updated_2025.04.13.jpeg'), dpi = 300, height = 10, width = 15, units = 'in')




# **5d. SNODAS water year 2022 plt ----------------------------------------

snodas_2022 <- snodas_oct %>% 
  filter(
    date >= ymd('2021-10-01') & date <= ymd('2022-04-30')
  )

snodas_oct_plt <- ggplot(data = snodas_2022)+
  geom_line(aes(x = date, y = swe_mm), size = 1.2)+
  theme_classic()+
  labs(x = 'Day of Year (10/1)', y = 'SWE (mm)')+
  #facet_wrap(~water_year, scales = 'free')+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1))+
  #xlim(30,261)+
  #scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00"))+
  scale_color_grey()+
  #scale_linetype_identity(name = c('solid', 'twodash', 'F1', 'dotdash', 'longdash'))+
  #scale_linetype_discrete(c(1,3,5,7,9))+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.2, 0.80),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key.height= unit(0.6, 'in'),
    legend.key.width= unit(0.5, 'in'),
    text = element_text(size = 38),
    legend.key.size = unit(5, 'line'),
    axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"))
  )
snodas_oct_plt


# 6. Cumulative Precip for 2021-2022 --------------------------------------

gridmet_precip2 <- gridmet_precip %>% 
  mutate(
    year = year(date_time),
    water_year = if_else(
      month(date_time) >= 10, year(date_time)+1, year(date_time))
    ) %>% 
  group_by(water_year) %>% 
  mutate(
    precip_sum_mm = cumsum(precip_mm)
  ) %>% 
  filter(
    date_time >= ymd('2021-10-01') & date_time <= ymd('2022-04-30')
  )

precip_plt <- ggplot() +
  geom_rect(aes(
    xmin = ymd('2021-12-18'),
    xmax = ymd('2022-04-07'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_line(data = gridmet_precip2, aes(x = date_time, y = precip_sum_mm/3.5), size = 1, color = 'grey')+
  geom_line(data = snodas_2022, aes(x = date, y = swe_mm), size = 1.2)+
  theme_classic()+
  xlab('')+
  scale_y_continuous(
    name = 'SWE (mm)',
    sec.axis = sec_axis(~.*3.5, name = 'Precipitation (mm)')
  )+
  theme(
    #legend.title = element_blank(),
    legend.position = 'none', #c(0.2, 0.80)
    # legend.background = element_rect(fill = "white", color = "white"),
    # legend.key.height= unit(0.6, 'in'),
    # legend.key.width= unit(0.5, 'in'),
    # text = element_text(size = 38),
    # legend.key.size = unit(5, 'line'),
    # axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"), size = 32)
  )
precip_plt
#ggplotly(precip_plt)

ggsave(here('output/lake_final_plts/supp_figs/snow_precip_plt.pdf'),
       dpi = 300, width = 7, height = 5, units = 'in')







test <- ggplot(data = snodas_oct %>% filter(water_year == 2019))+
  geom_line(aes(x = date, y = swe_mm))
ggplotly(test)
