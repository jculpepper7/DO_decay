## The goal of this code is to:
##
## 1. Import cleaned shallow lake / pond data 
## 2. Import meteorological data from Castle Lake station
## 3. Isolate code to times when smoke was confirmed using Sentinel-2 images
## 4. Plot air temperature, solar radiation, water temperature, and dissolved oxygen

# Load libraries----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(viridis)
library(patchwork)
library(rLakeAnalyzer)
library(LakeMetabolizer)

# 1. Import cleaned shallow lake / pond data------------------------------------

cdr <- read_csv(here('data/processed/cedar/cedar_clean_agg_data.csv'), guess_max = 50000)
gb <- read_csv(here('data/processed/gumboot/gumboot_clean_agg_data.csv'), guess_max = 50000)
ss <- read_csv(here('data/processed/soapstone/soapstone_clean_agg_data.csv'), guess_max = 50000)

ponds <- bind_rows(cdr, gb, ss)

# 2. Import Castle lake met data------------------------------------------------

# Import Castle WeatherHawk data from 2020 during a smoke period

cal_met_2020 <- read_csv(here('data/met_data/cal_met_2020.csv')) %>% 
  clean_names() %>% 
  select(date_time, air_temp_avg, solar_avg) %>% 
  mutate(
    date_time = ymd_hms(date_time),
    air_temp_avg = as.numeric(air_temp_avg),
    solar_avg = as.numeric(solar_avg)
  )

# Take a look

str(cal_met_2020) #looks good

# Import Castle WeatherHawk data from 2021 during a smoke period

cal_met_2021 <- read_csv(here('data/met_data/gridmet_data_2021_2021.07.28_2021.08.17.csv')) %>% 
  clean_names() %>% 
  select(date_time = 1, air_temp_avg = 2, solar_avg = 3) %>% 
  mutate(
    date_time = ymd(date_time),
    air_temp_avg = as.numeric(air_temp_avg),
    solar_avg = as.numeric(solar_avg)
  )

# Take a look

str(cal_met_2021)

# Import Castle Licor data from 2020 during a smoke period

cal_licor <- read_csv(here('data/met_data/licor_data_2020.csv')) %>% 
  select(date_time, solar_rad) %>% 
  mutate(
    date_time = mdy_hm(date_time)
  )
str(cal_licor)

# 3. Isolate code to confirmed smoke periods------------------------------------

# Truncate shallow lake / pond data

ponds_trunc_2020 <- ponds %>% 
  filter(date_time >= '2020-09-04', date_time <= '2020-09-17')

ponds_trunc_2021 <- ponds %>% 
  filter(date_time >= '2021-08-10', date_time <= '2021-08-15')

# Truncate Castle met data

cal_met_2020_trunc <- cal_met_2020 %>% 
  filter(date_time >= '2020-09-04', date_time <= '2020-09-17')

cal_met_2021_trunc <- cal_met_2021 #%>% 
  #filter(date_time >= '2020-08-10', date_time <= '2020-08-15')

# Truncate Castle Licor data

cal_licor_trunc <- cal_licor %>% 
  filter(date_time >= '2020-09-04', date_time <= '2020-09-17')

# 4. Plot met data against DO/temp sensors--------------------------------------

# 4a. Plots for 2020 smoke cover------------------------------------------------

#Shortwave radiation and temperature plot

coeff <- 80

air_rad_plt <- ggplot()+
  geom_line(data = cal_met_2020_trunc, aes(x = date_time, y = air_temp_avg), size = 1.5)+
  geom_line(data = cal_licor_trunc, aes(x = date_time, y = solar_rad/coeff), size = 1.5, color = 'light gray')+
  scale_y_continuous(
    name = 'Air T [°C]', #Alt+0176 for degree symbol
    sec.axis = sec_axis(~.*coeff, name = 'Solar Radiation [Wm-2]') #double y axis code from: https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
  )+
  theme_classic()
air_rad_plt  

# Water temp and shortwave plot

water_rad_plt <- ggplot()+
  geom_line(data = ponds_trunc_2020, aes(x = date_time, y = temp_c, color = depth), size = 1.5)+
  geom_line(data = cal_licor_trunc, aes(x = date_time, y = solar_rad/coeff), size = 1.5, color = 'light gray')+
  scale_y_continuous(
    name = 'Water T [°C]',
    sec.axis = sec_axis(~.*coeff, name = 'Solar Radiation [Wm-2]')
  )+
  xlab('')+
  theme_classic()+
  facet_wrap(~lake, ncol = 2)+
  scale_colour_viridis_d()+
  theme(legend.position = 'none')
water_rad_plt
  
# DO and shortwave plot

coeff_2 <- 100

do_rad_plt <- ggplot()+
  geom_line(data = cal_licor_trunc, aes(x = date_time, y = solar_rad/coeff_2), size = 1.5, color = 'light gray')+
  geom_line(data = ponds_trunc_2020, aes(x = date_time, y = do_mg_l), size = 1.5)+
  scale_y_continuous(
    name = 'Water T [°C]',
    sec.axis = sec_axis(~.*coeff, name = 'Solar Radiation [Wm-2]')
  )+
  theme_classic()+
  facet_wrap(~lake, ncol = 1)+
  scale_colour_viridis_d()
do_rad_plt  
  
# 4b. Plots for 2021 smoke cover------------------------------------------------

# Shortwave radiation and temperature plot

coeff <- 15

air_rad_plt <- ggplot()+
  geom_line(data = cal_met_2021_trunc, aes(x = date_time, y = air_temp_avg), size = 1.5)+
  geom_line(data = cal_met_2021_trunc, aes(x = date_time, y = solar_avg/coeff), size = 1.5, color = 'light gray')+
  scale_y_continuous(
    name = 'Air T [°C]', #Alt+0176 for degree symbol
    sec.axis = sec_axis(~.*coeff, name = 'Solar Radiation [Wm-2]') #double y axis code from: https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
  )+
  theme_classic()
air_rad_plt

#Just radiation plot

rad_plt <- ggplot()+
  #geom_line(data = cal_met_2021_trunc, aes(x = date_time, y = air_temp_avg), size = 1.5)+
  geom_line(data = cal_met_2021_trunc, aes(x = date_time, y = solar_avg), size = 1.5, color = 'light gray')+
  scale_y_continuous(
    name = 'Air T [°C]', #Alt+0176 for degree symbol
    #sec.axis = sec_axis(~.*coeff, name = 'Solar Radiation [Wm-2]') #double y axis code from: https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
  )+
  theme_classic()
rad_plt

# Water temp and shortwave plot

water_rad_plt <- ggplot()+
  geom_line(data = ponds_trunc_2021, aes(x = date_time, y = temp_c, color = depth), size = 1.5)+
  #geom_line(data = cal_met_2021_trunc, aes(x = date_time, y = solar_avg/coeff), size = 1.5, color = 'light gray')+
  scale_y_continuous(
    name = 'Water T [°C]',
    sec.axis = sec_axis(~.*coeff, name = 'Solar Radiation [Wm-2]')
  )+
  theme_classic()+
  facet_wrap(~lake, ncol = 1)+
  scale_colour_viridis_d()
water_rad_plt

water_rad_plt / rad_plt


# DO and shortwave plot

coeff_2 <- 100

do_rad_plt <- ggplot()+
  geom_line(data = ponds_trunc_2021, aes(x = date_time, y = do_mg_l), size = 1.5)+
  scale_y_continuous(
    name = 'Water T [°C]',
    sec.axis = sec_axis(~.*coeff, name = 'Solar Radiation [Wm-2]')
  )+
  theme_classic()+
  facet_wrap(~lake, ncol = 1)+
  scale_colour_viridis_d()
do_rad_plt 

ggsave(here('output/2021_do_rad_plt.png'), dpi = 500)

do_rad_plt / rad_plt


# Modeling with LakeMetabolizer-------------------------------------------------

#First combine lakes and average to daily values----
# Used the combined values from "ponds" tibble above

#Temperature hourly average
ponds_temp_avg <- ponds %>% 
  select(-light_intensity_lux, -unix, -utc, -pst, -do_mg_l, -do_sat) %>% 
  mutate(
    year = year(date_time),
    month = month(date_time),
    day = day(date_time)
  ) %>% 
  group_by(lake, depth, year, month, day) %>% 
  summarise(
    temp_c = mean(temp_c)
  ) %>% 
  mutate(
    datetime = make_date(year = year, month = month, day = day)
  ) %>% 
  ungroup() %>% 
  select(-year, -month, -day) %>% 
  pivot_wider(values_from = temp_c, names_from = depth)

#Dissolved oxygen hourly average
ponds_do_avg <- ponds %>% 
  select(lake, date_time, depth, do_mg_l) %>% 
  na.omit %>% 
  mutate(
    year = year(date_time),
    month = month(date_time),
    day = day(date_time)
  ) %>% 
  group_by(lake, depth, year, month, day) %>% 
  summarise(
    doobs = mean(na.omit(do_mg_l))
  ) %>% 
  mutate(
    datetime = make_date(year = year, month = month, day = day)
  ) %>% 
    ungroup() %>% 
    select(-year, -month, -day)

#Combine temp and DO average tibbles
shallow_lakes <- ponds_temp_avg %>% 
  left_join(ponds_do_avg)

#Isolate lakes
cdr_model <- shallow_lakes %>% 
  clean_names() %>% 
  filter(
    lake == 'cedar'
  ) %>% 
  select(-x1_5m, -x2_5m, -depth, -lake, -doobs) %>% 
  rename(wtr_1.0 = x1m,
         #wtr_2.5 = x2_5m,
         wtr_2.0 = x2m,
         wtr_4.0 = sediment)

#Must write file as tab delimited (i.e. '\t')
write_delim(cdr_model, here('data/metab_model/cedar/cdr_wtr.csv'), delim = '\t')

#Get the path for the package example file included 
cdr_path <- file.path(here('data/metab_model/cedar/cdr_wtr.csv'))

#load using load.ts for basically all functions
cdr.temp <- load.ts(cdr_path)

#calculate metalimnion depth
m.d <- ts.meta.depths(cdr.temp)
plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
lines(m.d$datetime, m.d$bottom, col='red')
