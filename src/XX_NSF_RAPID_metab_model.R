## The goal of this code is to:
##
## 1. Import cleaned shallow lake / pond data 
## 2. Import meteorological data from Castle Lake station
## 3. Run lake metabolism models


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

weatherhawk <- read_csv(here('data/met_data/cal_weatherhawk_2017_2022.csv')) %>% 
  select(date_time, air_temp_avg, wind_speed_avg, solar_avg)

# 2. Get weatherhawk data from Castle-------------------------------------------
#NOTE: Just run script 17_aggregate_cal_met_data.R
#      The tibble called "weatherhawk" has the aggregated hourly data

#get the wind for 2020 and 2021
met_2020 <- weatherhawk %>% 
  filter(year(date_time) == 2020,
         month(date_time) > 5,
         month(date_time)<11) %>% 
  select(datetime = date_time,
         wnd_2.0 = wind_speed_avg,
         irr_avg = solar_avg)

met_2021 <- weatherhawk %>% 
  filter(year(date_time) == 2021,
         month(date_time) > 5,
         month(date_time)<11) %>% 
  select(datetime = date_time,
         wnd_2.0 = wind_speed_avg,
         irr_avg = solar_avg)

# 3. Organize DO values for each lake and year----------------------------------

# 3a. Cedar 2020----
cdr_do_2020 <- cdr %>% 
  filter(year(date_time) == 2020) %>% 
  select(datetime = date_time,
         temp_c,
         depth,
         do_mg_l,
         do.sat = do_sat) %>% 
  filter(depth == '1m') %>% 
  mutate(
    year = year(datetime),
    month = month(datetime),
    day = day(datetime),
    hour = hour(datetime)
  ) %>% 
  group_by(year, month, day, hour) %>% 
  summarise(
    wtr_3.0 = mean(temp_c),
    doobs = mean(do_mg_l),
    do.sat = mean(do.sat)
  ) %>% 
  ungroup() %>% 
  mutate(
    datetime = make_datetime(year = year, month = month, day = day, hour = hour),
    datetime = as.POSIXlt(datetime)
  ) %>% 
  filter(
    month(datetime)>5,
    month(datetime)<11
  ) %>% 
  select(
    datetime, wtr_3.0, doobs, do.sat
  ) %>% 
  left_join(met_2020) %>% 
  mutate(
    kd = 1.7/4,  #approximation based on max depth
    z.mix = 3, #based on DO sensor depth
    latitude = 41.2072,
    irr_surface = sw.to.par.base(sw=irr_avg),
    irr = (1-0.1)*irr_surface*(1-exp(-kd*z.mix))/(kd*z.mix),
    U10 = wind.scale.base(wnd=wnd_2.0, wnd.z=14),
    k600 = k.vachon.base(wnd=U10, lake.area=30000),
    k.gas = k600.2.kGAS.base(k600=k600, temperature=wtr_3.0, gas="O2"),
    irr_01 = if_else(
      irr != 0, 1, 0
    )
  ) %>% 
  na.omit %>% 
  distinct(datetime, .keep_all = TRUE)

# 3b. Cedar 2021----
cdr_do_2021 <- cdr %>% 
  filter(year(date_time) == 2021) %>% 
  select(datetime = date_time,
         temp_c,
         depth,
         do_mg_l,
         do.sat = do_sat) %>% 
  filter(depth == '1m') %>% 
  mutate(
    year = year(datetime),
    month = month(datetime),
    day = day(datetime),
    hour = hour(datetime)
  ) %>% 
  group_by(year, month, day, hour) %>% 
  summarise(
    wtr_3.0 = mean(temp_c),
    doobs = mean(do_mg_l),
    do.sat = mean(do.sat)
  ) %>% 
  ungroup() %>% 
  mutate(
    datetime = make_datetime(year = year, month = month, day = day, hour = hour),
    datetime = as.POSIXlt(datetime)
  ) %>% 
  filter(
    month(datetime)>5,
    month(datetime)<11
  ) %>% 
  select(
    datetime, wtr_3.0, doobs, do.sat
  ) %>% 
  left_join(met_2021) %>% 
  mutate(
    kd = 1.7/4,  #approximation based on max depth
    z.mix = 3, #based on DO sensor depth
    latitude = 41.2072,
    irr_surface = sw.to.par.base(sw=irr_avg),
    irr = (1-0.1)*irr_surface*(1-exp(-kd*z.mix))/(kd*z.mix),
    U10 = wind.scale.base(wnd=wnd_2.0, wnd.z=14),
    k600 = k.vachon.base(wnd=U10, lake.area=30000),
    k.gas = k600.2.kGAS.base(k600=k600, temperature=wtr_3.0, gas="O2"),
    irr_01 = if_else(
      irr != 0, 1, 0
    )
  ) %>% 
  na.omit %>% 
  distinct(datetime, .keep_all = TRUE)

# 3c. Gumboot 2020----
gb_do_2020 <- gb %>% 
  filter(year(date_time) == 2020) %>% 
  select(datetime = date_time,
         temp_c,
         depth,
         do_mg_l,
         do.sat = do_sat) %>% 
  filter(depth == '1m') %>% 
  mutate(
    year = year(datetime),
    month = month(datetime),
    day = day(datetime),
    hour = hour(datetime)
  ) %>% 
  group_by(year, month, day, hour) %>% 
  summarise(
    wtr_3.0 = mean(temp_c),
    doobs = mean(do_mg_l),
    do.sat = mean(do.sat)
  ) %>% 
  ungroup() %>% 
  mutate(
    datetime = make_datetime(year = year, month = month, day = day, hour = hour),
    datetime = as.POSIXlt(datetime)
  ) %>% 
  filter(
    month(datetime)>5,
    month(datetime)<11
  ) %>% 
  select(
    datetime, wtr_3.0, doobs, do.sat
  ) %>% 
  left_join(met_2020) %>% 
  mutate(
    kd = 1.7/4.2,  #approximation based on max depth
    z.mix = 3, #based on DO sensor depth
    latitude = 41.2111,
    irr_surface = sw.to.par.base(sw=irr_avg),
    irr = (1-0.1)*irr_surface*(1-exp(-kd*z.mix))/(kd*z.mix),
    U10 = wind.scale.base(wnd=wnd_2.0, wnd.z=14),
    k600 = k.vachon.base(wnd=U10, lake.area=44000),
    k.gas = k600.2.kGAS.base(k600=k600, temperature=wtr_3.0, gas="O2"),
    irr_01 = if_else(
      irr != 0, 1, 0
    )
  ) %>% 
  na.omit %>% 
  distinct(datetime, .keep_all = TRUE)

# 3d. Gumboot 2021----
gb_do_2021 <- gb %>% 
  filter(year(date_time) == 2021) %>% 
  select(datetime = date_time,
         temp_c,
         depth,
         do_mg_l,
         do.sat = do_sat) %>% 
  filter(depth == '1m') %>% 
  mutate(
    year = year(datetime),
    month = month(datetime),
    day = day(datetime),
    hour = hour(datetime)
  ) %>% 
  group_by(year, month, day, hour) %>% 
  summarise(
    wtr_3.0 = mean(temp_c),
    doobs = mean(do_mg_l),
    do.sat = mean(do.sat)
  ) %>% 
  ungroup() %>% 
  mutate(
    datetime = make_datetime(year = year, month = month, day = day, hour = hour),
    datetime = as.POSIXlt(datetime)
  ) %>% 
  filter(
    month(datetime)>5,
    month(datetime)<11
  ) %>% 
  select(
    datetime, wtr_3.0, doobs, do.sat
  ) %>% 
  left_join(met_2021) %>% 
  mutate(
    kd = 1.7/4.2,  #approximation based on max depth
    z.mix = 3, #based on DO sensor depth
    latitude = 41.2111,
    irr_surface = sw.to.par.base(sw=irr_avg),
    irr = (1-0.1)*irr_surface*(1-exp(-kd*z.mix))/(kd*z.mix),
    U10 = wind.scale.base(wnd=wnd_2.0, wnd.z=14),
    k600 = k.vachon.base(wnd=U10, lake.area=44000),
    k.gas = k600.2.kGAS.base(k600=k600, temperature=wtr_3.0, gas="O2"),
    irr_01 = if_else(
      irr != 0, 1, 0
    )
  ) %>% 
  na.omit %>% 
  distinct(datetime, .keep_all = TRUE)

# 3e. Soapstone 2020----
ss_do_2020 <- ss %>% 
  filter(year(date_time) == 2020) %>% 
  select(datetime = date_time,
         temp_c,
         depth,
         do_mg_l,
         do.sat = do_sat) %>% 
  filter(depth == '1m') %>% 
  mutate(
    year = year(datetime),
    month = month(datetime),
    day = day(datetime),
    hour = hour(datetime)
  ) %>% 
  group_by(year, month, day, hour) %>% 
  summarise(
    wtr_3.0 = mean(temp_c),
    doobs = mean(do_mg_l),
    do.sat = mean(do.sat)
  ) %>% 
  ungroup() %>% 
  mutate(
    datetime = make_datetime(year = year, month = month, day = day, hour = hour),
    datetime = as.POSIXlt(datetime)
  ) %>% 
  filter(
    month(datetime)>5,
    month(datetime)<11
  ) %>% 
  select(
    datetime, wtr_3.0, doobs, do.sat
  ) %>% 
  left_join(met_2020) %>% 
  mutate(
    kd = 1.7/2.5,  #approximation based on max depth
    z.mix = 2, #based on DO sensor depth
    latitude = 41.1971,
    irr_surface = sw.to.par.base(sw=irr_avg),
    irr = (1-0.1)*irr_surface*(1-exp(-kd*z.mix))/(kd*z.mix),
    U10 = wind.scale.base(wnd=wnd_2.0, wnd.z=14),
    k600 = k.vachon.base(wnd=U10, lake.area=5000),
    k.gas = k600.2.kGAS.base(k600=k600, temperature=wtr_3.0, gas="O2"),
    irr_01 = if_else(
      irr != 0, 1, 0
    )
  ) %>% 
  na.omit %>% 
  distinct(datetime, .keep_all = TRUE)

# 3f. Soapstone 2021----
ss_do_2021 <- ss %>% 
  filter(year(date_time) == 2021) %>% 
  select(datetime = date_time,
         temp_c,
         depth,
         do_mg_l,
         do.sat = do_sat) %>% 
  filter(depth == '1m') %>% 
  mutate(
    year = year(datetime),
    month = month(datetime),
    day = day(datetime),
    hour = hour(datetime)
  ) %>% 
  group_by(year, month, day, hour) %>% 
  summarise(
    wtr_3.0 = mean(temp_c),
    doobs = mean(do_mg_l),
    do.sat = mean(do.sat)
  ) %>% 
  ungroup() %>% 
  mutate(
    datetime = make_datetime(year = year, month = month, day = day, hour = hour),
    datetime = as.POSIXlt(datetime)
  ) %>% 
  filter(
    month(datetime)>5,
    month(datetime)<11
  ) %>% 
  select(
    datetime, wtr_3.0, doobs, do.sat
  ) %>% 
  left_join(met_2021) %>% 
  mutate(
    kd = 1.7/2.5,  #approximation based on max depth
    z.mix = 2, #based on DO sensor depth
    latitude = 41.1971,
    irr_surface = sw.to.par.base(sw=irr_avg),
    irr = (1-0.1)*irr_surface*(1-exp(-kd*z.mix))/(kd*z.mix),
    U10 = wind.scale.base(wnd=wnd_2.0, wnd.z=14),
    k600 = k.vachon.base(wnd=U10, lake.area=5000),
    k.gas = k600.2.kGAS.base(k600=k600, temperature=wtr_3.0, gas="O2"),
    irr_01 = if_else(
      irr != 0, 1, 0
    )
  ) %>% 
  na.omit %>% 
  distinct(datetime, .keep_all = TRUE)

# 4. Modeling metabolism--------------------------------------------------------

# 4a. Write files as tab delimited (i.e. '\t')----

#wind data
#write.table(wnd_2020, here('data/metab_model/cedar/wnd_2020.txt'), row.names = FALSE, sep = '\t')
#wind data
#write.table(wnd_2021, here('data/metab_model/cedar/wnd_2021.txt'), row.names = FALSE, sep = '\t')
#Cedar 2020
write.table(cdr_do_2020, here('data/metab_model/cedar/cdr_do_2020.txt'), row.names = FALSE, sep = '\t')
#Cedar 2021
write.table(cdr_do_2021, here('data/metab_model/cedar/cdr_do_2021.txt'), row.names = FALSE, sep = '\t')
#Gumboot 2020
write.table(gb_do_2020, here('data/metab_model/cedar/gb_do_2020.txt'), row.names = FALSE, sep = '\t')
#Gumboot 2021
write.table(gb_do_2021, here('data/metab_model/cedar/gb_do_2021.txt'), row.names = FALSE, sep = '\t')
#Soapstone 2020
write.table(ss_do_2020, here('data/metab_model/cedar/ss_do_2020.txt'), row.names = FALSE, sep = '\t')
#Soapstone 2021
write.table(ss_do_2021, here('data/metab_model/cedar/ss_do_2021.txt'), row.names = FALSE, sep = '\t')

# 4a. Cedar 2020 metab----
Sys.setenv(TZ='GMT')

#Get the path for the package example file included 
#wnd_path_2020 <- file.path(here('data/metab_model/cedar/wnd_2020.txt'))
#wnd_path_2021 <- file.path(here('data/metab_model/cedar/wnd_2021.txt'))
cdr_path_2020 <- file.path(here('data/metab_model/cedar/cdr_do_2020.txt'))
cdr_path_2021 <- file.path(here('data/metab_model/cedar/cdr_do_2020.txt'))
gb_path_2020 <- file.path(here('data/metab_model/cedar/gb_do_2020.txt'))
gb_path_2021 <- file.path(here('data/metab_model/cedar/gb_do_2020.txt'))
ss_path_2020 <- file.path(here('data/metab_model/cedar/ss_do_2020.txt'))
ss_path_2021 <- file.path(here('data/metab_model/cedar/ss_do_2020.txt'))

#load using load.ts for basically all functions
cdr2020 = load.ts(cdr_path_2020)

# wnd = load.ts(wnd_path_2020)
# 
# doobs <- doobs %>% 
#   left_join(wnd)
# 
# #Get gas exchange, DO saturation, and irradiance
# k.gas = k600.2.kGAS.base(k.cole.base(doobs[,4]), doobs[,2], 'O2')
# #k.gas = 0
# do.sat = o2.at.sat.base(doobs[,2], altitude=1737)
# # Must supply 1 for daytime timesteps and 0 for nighttime timesteps
# irr = as.integer(is.day(doobs[,1], lat = 41.2))
# metab.bookkeep(doobs[,3], do.sat, k.gas, z.mix=1, irr, datetime=doobs$datetime)

cdr_bookkeep <- metab.bookkeep(do.obs = cdr2020$doobs, 
               do.sat = cdr2020$do.sat, 
               k.gas = cdr2020$k.gas, 
               z.mix = cdr2020$z.mix, 
               irr = cdr2020$irr_01, 
               datetime= cdr2020$datetime)









##########################
# cdr_do_2021 <- cdr %>% 
#   filter(year(date_time) == 2021) %>% 
#   select(datetime = date_time,
#          temp_c,
#          depth,
#          do_mg_l) %>% 
#   filter(depth == '1m') %>% 
#   mutate(
#     year = year(datetime),
#     month = month(datetime),
#     day = day(datetime),
#     hour = hour(datetime)
#   ) %>% 
#   group_by(year, month, day, hour) %>% 
#   summarise(
#     wtr_3.0 = mean(temp_c),
#     doobs = mean(do_mg_l)
#   ) %>% 
#   ungroup() %>% 
#   mutate(
#     datetime = make_datetime(year = year, month = month, day = day, hour = hour)
#   ) %>% 
#   filter(
#     month(datetime)>3,
#     month(datetime)<11
#   ) %>% 
#   select(
#     datetime, wtr_3.0, doobs
#   )

