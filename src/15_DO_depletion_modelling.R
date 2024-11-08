# This script, for one lake at a time, models mean daily under-ice DO data,
#first use changepoint analysis to identify different time segments under ice,
#then models each segment separately using an arima model. This should yield multiple
#DO depletion rates for each winter if there are significant changes that occur 
#(as opposed to calculating a single mean rate via regression)
#Author: Joshua Culpepper, adapted with permission from method by Adrianne Smits -- see Smits et al., 2021 https://doi. org/10.1029/2021JG006277
#Date: 06.01.2022

#1. Load libraries--------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(here)
library(changepoint)
library(forecast)
library(tseries)
library(plotly)

#2. Import aggregated data -----------------------------------------------------

#Cedar lake aggregated data
cdr <- read_csv(here('data/processed/cedar/cedar_clean_agg_data_2022.csv'), guess_max = 50000) 
#NOTE: without guess_max, read_csv() will fail to accurately predict the miniDOT data (columns 6-10) and will therefore exclude it from the tibble.

#Gumboot lake aggregated data
gb <- read_csv(here('data/processed/gumboot/gumboot_clean_agg_data_2022.csv'), guess_max = 50000)

#Soapstone Pond aggregated data
ss <- read_csv(here('data/processed/soapstone/soapstone_clean_agg_data_2022.csv'), guess_max = 50000)

#Join separate lake data
all_lakes <- bind_rows(cdr, gb, ss)

#Castle lake aggregated data
cal <- read_csv(here('data/processed/castle/castle_clean_agg_data_daily.csv')) %>% 
  mutate(
    lake = c('castle'),
    date_time = date
  )

#Cliff lake aggregated data
clf_19 <- read_csv(here('data/raw/cliff/cliff_2019_winter_raw.csv')) %>% 
  rename(date_time = date) %>% 
  mutate(
    water_year = if_else(month(date_time)>=10, year(date_time)+1, year(date_time))
  )

clf_20 <- read_csv(here('data/raw/cliff/cliff_2020_winter_raw.csv')) %>% 
  rename(date_time = date) %>% 
  mutate(
    water_year = if_else(month(date_time)>=10, year(date_time)+1, year(date_time))
  )
  
clf_21 <- read_csv(here('data/raw/cliff/cliff_2021_winter_raw.csv')) %>% 
  rename(date_time = date) %>% 
  mutate(
    water_year = if_else(month(date_time)>=10, year(date_time)+1, year(date_time))
  )

clf_22 <- read_csv(here('data/raw/cliff/cliff_2022_winter_raw.csv')) %>% 
  rename(date_time = date) %>% 
  mutate(
    water_year = if_else(month(date_time)>=10, year(date_time)+1, year(date_time))
  )
#3. Aggregate to daily DO data--------------------------------------------------

#Deal with temperature from hobo loggers, light intensity from pendant, and
#dissolved oxygen from miniDOTs separately.

#Temperature from HOBO loggers
temp_daily <- all_lakes %>% 
  select(lake, depth, date_time, temp_c) %>% 
  group_by(lake, depth, year(date_time), month(date_time), day(date_time)) %>% 
  summarise(temp_c = mean(temp_c)) %>% 
  rename(
    year = 3,
    month = 4, 
    day = 5
  ) %>% 
  mutate(
    date_time = make_datetime(year, month, day)
  ) %>% 
  ungroup() %>% 
  select(lake, date_time, depth, temp_c)

#Light intensity from HOBO light pendants  
light_daily <- all_lakes %>%
   select(lake, date_time, depth, light_intensity_lux) %>%
   na.omit() %>% 
   group_by(lake, depth, year(date_time), month(date_time), day(date_time)) %>%
   summarise(light_intensity_lux = mean(light_intensity_lux)) %>%
   rename(
     year = 3,
     month = 4,
     day = 5
   ) %>%
   mutate(
     date_time = make_datetime(year, month, day)
   ) %>%
   ungroup() %>%
   select(lake, date_time, depth, light_intensity_lux) 
   
#Dissolved oxygen from miniDOTs from Cedar, Gumboot, and Soapstone
do_daily <- all_lakes %>% 
  select(lake, depth, date_time, do_mg_l, do_sat) %>% 
  na.omit() %>% 
  group_by(lake, depth, year(date_time), month(date_time), day(date_time)) %>% 
  summarise(do_mg_l = mean(do_mg_l),
            do_sat = mean(do_sat)) %>% 
  rename(
    year = 3,
    month = 4, 
    day = 5
  ) %>% 
  ungroup() %>%
  mutate(
    date_time = make_datetime(year, month, day),
    year = year(date_time)
  ) %>% 
  group_by(year) %>% 
  mutate(
    water_year = ifelse(month(date_time)>=10, year+1, year)
  ) %>% 
  ungroup %>% 
  select(lake, date_time, year, water_year, depth, do_mg_l, do_sat)



#Check initial plots

#Temp plot
ggplot(temp_daily)+
  geom_line(aes(x = date_time, y = temp_c, color = depth), size = 1.2)+
  theme_bw()+
  facet_wrap(~lake, ncol = 1)

#Light plot
ggplot(light_daily)+
  geom_line(aes(x = date_time, y = light_intensity_lux), size = 1.2)+
  theme_bw()+
  facet_wrap(~lake, ncol = 1)

#DO plots

#Cedar
do_cedar <- do_daily %>% 
  filter(lake == 'cedar') %>% 
  ggplot()+
  geom_line(aes(x = date_time, y = do_mg_l), size = 2)+
  theme_bw()#+
  #facet_wrap(~lake, ncol = 1)
ggplotly(do_cedar) 
#water year: 2020 - 11-25-2019 - 04-12-2020
#water year: 2021 - 12-18-2020 - 04-17-2021

#Gumboot
do_gumboot <- do_daily %>% 
  filter(lake == 'gumboot') %>% 
  ggplot()+
  geom_line(aes(x = date_time, y = do_mg_l), size = 2)+
  theme_bw()#+
#facet_wrap(~lake, ncol = 1)
ggplotly(do_gumboot) 
#water year: 2020 - 11-29-2019 - 05-04-2020
#water year: 2021 - 11-18-2020 - 05-02-2021

#Soapstone
do_soapstone <- do_daily %>% 
  filter(lake == 'soapstone') %>% 
  ggplot()+
  geom_line(aes(x = date_time, y = do_mg_l), size = 2)+
  theme_bw()#+
#facet_wrap(~lake, ncol = 1)
ggplotly(do_soapstone) 
#water year: 2020 - 11-24-2019 - 05-04-2020
#water year: 2021 - 11-20-2020 - 04-27-2021

#4. Isolate ice cover periods---------------------------------------------------

#NOTE: Ice data comes from a combination of on-site game cameras as well as
#      Sentinel-HUB EO Browser

#4a. Cedar-----
cedar_ice_2020 <- do_daily %>% 
  filter(water_year == 2020,
         lake == 'cedar',
         date_time >= as.POSIXct("2019-11-26"),
         date_time <= as.POSIXct("2020-01-26")) #Truncated to lowest point in winter period (61 days)
         #date_time <= as.POSIXct("2020-04-12")) #Complete ice off

cedar_ice_2021 <- do_daily %>% 
  filter(water_year == 2021,
         lake == 'cedar',
         date_time >= as.POSIXct("2020-12-18"), #Truncated to lowest point in winter period (99 days)
         date_time <= as.POSIXct("2021-03-27")) 
         #date_time <= as.POSIXct("2021-04-17")) #Complete ice off

cedar_ice_2022 <- do_daily %>% 
  filter(water_year == 2022,
         lake == 'cedar',
         date_time >= as.POSIXct("2021-12-12"), #
         date_time <= as.POSIXct("2022-01-28")) #Truncated at lowest DO value of 2.66 (111 days) Cedar did not enter hypoxia in water year 2022
         #date_time <= as.POSIXct("2022-04-02")) #Complete ice off

#4b. Gumboot-----

#Water year 2020
gb_ice_2020 <- do_daily %>% 
  filter(water_year == 2020,
         lake == 'gumboot',
         date_time >= as.POSIXct("2019-11-23"),
         date_time <= as.POSIXct("2020-02-01")) #Truncated to first instance of anoxia (70 days)
         #date_time <= as.POSIXct("2020-05-04")) #Complete ice off

#The second decay period likely caused by an incursion of water or snow that raised DO above hypoxia (2.0 mg/L)
#Time series truncated from peak of incursion to nadir
gb_ice_2020_2 <- do_daily %>% 
  filter(water_year == 2020,
         lake == 'gumboot',
         date_time >= as.POSIXct("2020-03-14"),
         date_time <= as.POSIXct("2020-04-08")) #25 days

#Water year 2021
gb_ice_2021 <- do_daily %>% 
  filter(water_year == 2021,
         lake == 'gumboot',
         date_time >= as.POSIXct("2020-11-18"),
         date_time <= as.POSIXct("2021-01-19")) #Truncated to first instance of anoxia (62 days)
         #date_time <= as.POSIXct("2021-05-02")) #Complete ice off

#Water year 2022
gb_ice_2022 <- do_daily %>% 
  filter(water_year == 2022,
         lake == 'gumboot',
         date_time >= as.POSIXct("2021-12-18"),
         date_time <= as.POSIXct("2022-02-14")) #Truncated to first instance of hypoxia (58 days)
         #date_time <= as.POSIXct("2022-04-07")) #Complete ice off
         
#4c. Soapstone-----

#Water year 2020
ss_ice_2020 <- do_daily %>% 
  filter(water_year == 2020,
         lake == 'soapstone',
         date_time >= as.POSIXct("2019-11-21"),
         date_time <= as.POSIXct("2019-12-02"))  #Truncated to first instance of anoxia (12 days)
         #date_time <= as.POSIXct("2020-05-04")) #Complete ice off

#The second decay period likely caused by an incursion of water or snow that raised DO above hypoxia
ss_ice_2020_2 <- do_daily %>% 
  filter(water_year == 2020,
         lake == 'soapstone',
         date_time >= as.POSIXct("2020-03-05"),
         date_time <= as.POSIXct("2020-03-31"))

#Water year 2021
ss_ice_2021 <- do_daily %>% 
  filter(water_year == 2021,
         lake == 'soapstone',
         date_time >= as.POSIXct("2020-11-06"),
         date_time <= as.POSIXct("2020-11-11"))  #Truncated to first instance of anoxia (2 days)
         #date_time <= as.POSIXct("2020-04-24")) #Complete ice off

#The second decay period likely caused by an incursion of water or snow that raised DO above hypoxia
ss_ice_2021_2 <- do_daily %>% 
  filter(water_year == 2021,
         lake == 'soapstone',
         date_time >= as.POSIXct("2020-11-19"),
         date_time <= as.POSIXct("2020-11-28"))

#Water year 2022
ss_ice_2022 <- do_daily %>% 
  filter(water_year == 2022,
         lake == 'soapstone',
         date_time >= as.POSIXct("2021-12-11"),
         date_time <= as.POSIXct("2022-01-04")) #Truncated time series at lowest point before leveling off at hypoxic condition
         #date_time <= as.POSIXct("2022-04-02")) #Complete ice off

#4d. Castle-----

#Water year 2018
cal_ice_2018 <- cal %>% 
  filter(water_year == 2018,
         lake == 'castle',
         date_time >= as.POSIXct("2017-12-04"),
         date_time <= as.POSIXct("2018-01-03")) 

cal_ice_2018_2 <- cal %>% 
  filter(water_year == 2018,
         lake == 'castle',
         date_time >= as.POSIXct("2018-01-18"),
         date_time <= as.POSIXct("2018-02-07"))

cal_ice_2018_3 <- cal %>% 
  filter(water_year == 2018,
         lake == 'castle',
         date_time >= as.POSIXct("2018-02-17"),
         date_time <= as.POSIXct("2018-04-11")) 

#Below period is ice off to anoxia
cal_ice_2018_4 <- cal %>% 
  filter(water_year == 2018,
         lake == 'castle',
         date_time >= as.POSIXct("2018-04-20"),
         date_time <= as.POSIXct("2018-09-22")) 

#Water year 2019
cal_ice_2019 <- cal %>% 
  filter(water_year == 2019,
         lake == 'castle',
         date_time >= as.POSIXct("2018-12-24"),
         date_time <= as.POSIXct("2019-06-01")) 

#Below is the period from ice out to anoxia
cal_ice_2019_2 <- cal %>% 
  filter(water_year == 2019,
         lake == 'castle',
         date_time >= as.POSIXct("2019-06-11"),
         date_time <= as.POSIXct("2019-08-18"))

#Water year 2020
cal_ice_2020 <- cal %>% 
  filter(water_year == 2020,
         lake == 'castle',
         date_time >= as.POSIXct("2019-12-20"),
         date_time <= as.POSIXct("2020-02-20"))

#Period of increased DO while under ice to ice out
cal_ice_2020_2 <- cal %>% 
  filter(water_year == 2020,
         lake == 'castle',
         date_time >= as.POSIXct("2020-03-06"),
         date_time <= as.POSIXct("2020-04-25"))

#Period of ice our to increased DO
cal_ice_2020_3 <- cal %>% 
  filter(water_year == 2020,
         lake == 'castle',
         date_time >= as.POSIXct("2020-04-26"),
         date_time <= as.POSIXct("2020-06-16"))

#Final DO increase to anoxia
cal_ice_2020_4 <- cal %>% 
  filter(water_year == 2020,
         lake == 'castle',
         date_time >= as.POSIXct("2020-06-19"),
         date_time <= as.POSIXct("2020-08-25"))

#Water year 2021

#Ice on to ice off
cal_ice_2021 <- cal %>% 
  filter(water_year == 2021,
         lake == 'castle',
         date_time >= as.POSIXct("2020-12-20"),
         date_time <= as.POSIXct("2021-05-02"))

#Ice off to DO increase
cal_ice_2021_2 <- cal %>% 
  filter(water_year == 2021,
         lake == 'castle',
         date_time >= as.POSIXct("2021-05-06"),
         date_time <= as.POSIXct("2021-06-11"))

#DO increase to DO 
cal_ice_2021_3 <- cal %>% 
  filter(water_year == 2021,
         lake == 'castle',
         date_time >= as.POSIXct("2021-06-13"),
         date_time <= as.POSIXct("2021-07-17"))

#Water year 2022

#Ice on to anoxia
cal_ice_2022 <- cal %>% 
  filter(water_year == 2022,
         lake == 'castle',
         date_time >= as.POSIXct("2021-12-15"),
         date_time <= as.POSIXct("2022-01-15"))

#Trend changed after 2022-01-15
cal_ice_2022_2 <- cal %>% 
  filter(water_year == 2022,
         lake == 'castle',
         date_time >= as.POSIXct("2022-01-16"),
         date_time <= as.POSIXct("2022-02-01"))

#4e. Cliff-----

#Group Cliff data

clf_all <- clf_19 %>% 
  bind_rows(clf_20, clf_21, clf_22)

#Water year 2018
#This is the summer period, but it is incomplete
clf_ice_2018 <- clf_all %>% 
  filter(date_time >= as.POSIXct("2018-06-30"),
        date_time <= as.POSIXct("2018-10-06")) #From beginning of record to complete anoxia

#Water year 2019
clf_ice_2019_1 <- clf_all %>% 
  filter(water_year == 2019,
         date_time >= as.POSIXct("2018-12-06"), #first instance of ice on from Sentinel image
         date_time <= as.POSIXct("2019-02-13")) #low point prior to increase. Unclear why increase happens. Several sunny days in a row using Sentinel.

clf_ice_2019_2 <- clf_all %>% 
  filter(water_year == 2019,
         date_time >= as.POSIXct("2019-02-27"), #first instance of ice on from Sentinel image
         date_time <= as.POSIXct("2019-04-19")) #Lowest point prior to anoxia. Low oxygen recovery in the spring

#Below is the summer decline
clf_ice_2019_3 <- clf_all %>% 
  filter(water_year == 2019,
         date_time >= as.POSIXct("2019-06-27"), #Peak spring mix, though it never increases above 2 mg/L
         date_time <= as.POSIXct("2019-08-11")) #Minimum value of decline prior to leveling off at minimal values

#Water year 2020
clf_ice_2020 <- clf_all %>% 
  filter(water_year == 2020,
         date_time >= as.POSIXct("2019-12-09"),
         date_time <= as.POSIXct("2020-03-25"))

clf_ice_2020_2 <- clf_all %>% 
  filter(water_year == 2020,
         date_time >= as.POSIXct("2020-05-25"), #Peak DO concentration after spring mix
         date_time <= as.POSIXct("2020-08-18")) #minimum DO prior to complete anoxia

#Water year 2021
clf_ice_2021 <- clf_all %>% 
  filter(water_year == 2021,
         date_time >= as.POSIXct("2020-12-01"),
         date_time <= as.POSIXct("2021-03-19"))

clf_ice_2021_2 <- clf_all %>% 
  filter(water_year == 2021,
         date_time >= as.POSIXct("2020-05-26"), #This is the open water period.
         date_time <= as.POSIXct("2020-08-18")) #There is a slight increase in DO at June 16, but it is less than 1 mg/L, so I did not break up the time series.

#5. Apply changepoint analysis to DO  time series-------------------------------
#NOTE: Chanegpoints indicate where DO variance changes

#change data to time series format
DO.ts <- ts(clf_ice_2021_2$do_mg_l, frequency = 365, start = as.POSIXct('2020-05-26'))
# DO.ts <- ts(MeanDaily.clipped[,grep('DO',names(MeanDaily.clipped))],frequency=365, start=c(year(MeanDaily$day[1]),
#                                                                                                month(MeanDaily$day[1]),
#                                                                                                day(MeanDaily$day[1])))
plot.ts(DO.ts)

##De-trend DO data first before applying changepoint analysis 
DO.diff1 <- diff(x=DO.ts, differences=1, lag=1)

#plot to see if first differencing got rid of linear trends
plot.ts(DO.diff1)

##Now apply changepoint analysis to first-differenced (de-trended) data to see where variance changes
out <-cpt.var(DO.diff1,method='PELT')
#out <-cpt.var(DO.diff1,method='BinSeg')
summary(out)
plot(out)
plot.ts(DO.ts)
chgpts <- out@cpts


#6. Split time series at discovered changepoints--------------------------------
#NOTE: Number of changepoints  will vary by dataset

DO.data <- clf_ice_2021_2 %>% 
  mutate(
    day = day(date_time)
  )
chgpts.dates <- DO.data$day[chgpts]

DO.ts1 <- DO.data #use this if there are no significant changepoints

DO.ts1 <- DO.data[1:chgpts[1],]
DO.ts2 <- DO.data[(chgpts[1]+1):chgpts[2],]
# DO.ts3 <- DO.data[(chgpts[2]+1):chgpts[3],]
# DO.ts4 <- DO.data[(chgpts[3]+1):chgpts[4],]
# DO.ts5 <- DO.data[(chgpts[4]+1):chgpts[5],]
# DO.ts6 <- DO.data[(chgpts[5]+1):chgpts[6],]
# DO.ts7 <- DO.data[(chgpts[6]+1):chgpts[7],]
# DO.ts8 <- DO.data[(chgpts[7]+1):chgpts[8],]

#7. ARIMA model for split time series-------------------------------------------

##model each data segment as an arima function (following Obertegger et al. 2017)
#order=c(p,d,q)  
#p= lag in autoregressive term, 
#d= degree of differencing, 
#q= degree of moving average
#d=1 is first difference -- should be sufficient

#Segment 1
#check if data is stationary around a level (if not, should have small p-value)
# kpss1 <-kpss.test(DO.ts1[,grep('DO',names(DO.ts1))], null="Level")
# pacf(DO.ts1[,grep('DO',names(DO.ts1))])#check pacf to see how much AR makes sense...
kpss1 <-kpss.test(DO.ts1$do_mg_l, null="Level") #Test checks for stationary data. p value > 0.01 indicates stationary data
pacf(DO.ts1$do_mg_l) #check pacf to see how much AR makes sense...
fit1 <- Arima(DO.ts1$do_mg_l, order=c(1,1,0), include.constant=TRUE)
summary(fit1) #drift gives the slope of the time series
checkresiduals(fit1) #checks for autocorrelation. A p value >0.05 indicates that the residuals are independently distributed
#plot(fit1$fitted)


#Segment 2
#check if data is stationary around a level (if not, should have small p-value)
kpss2 <- kpss.test(DO.ts2$do_mg_l, null="Level")
pacf(DO.ts2$do_mg_l)
fit2 <- Arima(DO.ts2$do_mg_l,order=c(1,1,1), include.constant=TRUE)
summary(fit2)
checkresiduals(fit2)

#Segment 3
#check if data is stationary around a mean level (if not, should have small p-value)
kpss3 <-kpss.test(DO.ts3$do_mg_l, null="Level")
pacf(DO.ts3$do_mg_l)
fit3 <- Arima(DO.ts3$do_mg_l,order=c(1,1,0), include.constant=TRUE)
summary(fit3)
checkresiduals(fit3)
 

# #Segment 4
# #check if data is stationary around a mean level (if not, should have small p-value)
kpss4 <-kpss.test(DO.ts4$do_mg_l, null="Level")
pacf(DO.ts4$do_mg_l)
fit4 <- Arima(DO.ts4$do_mg_l, order=c(1,1,1), include.constant=TRUE)
summary(fit4)
checkresiduals(fit4)

# #Segment 5
# #check if data is stationary around a mean level (if not, should have small p-value)
kpss5 <-kpss.test(DO.ts5$do_mg_l, null="Level")
pacf(DO.ts5$do_mg_l)
fit5 <- Arima(DO.ts5$do_mg_l, order=c(1,1,1), include.constant=TRUE)
summary(fit5)
checkresiduals(fit5)

# #Segment 6
# #check if data is stationary around a mean level (if not, should have small p-value)
kpss6 <-kpss.test(DO.ts6$do_mg_l, null="Level")
pacf(DO.ts6$do_mg_l)
fit6 <- Arima(DO.ts6$do_mg_l, order=c(1,1,1), include.constant=TRUE)
summary(fit6)
checkresiduals(fit6)

# #Segment 7
# #check if data is stationary around a mean level (if not, should have small p-value)
kpss7 <-kpss.test(DO.ts7$do_mg_l, null="Level")
pacf(DO.ts7$do_mg_l)
fit7 <- Arima(DO.ts7$do_mg_l, order=c(1,1,1), include.constant=TRUE)
summary(fit7)
checkresiduals(fit7)

#8. Save output----------------------------------------------------------------- 
#lake name, 
#mean daily data (under ice period only), 
#dates where there are variance changepoints in the DO data,
#DO time series split by changepoints (however many were found), 
#results of the kpss test for each segment,
#and the arima models that were best fit to those time series segments, including a drift term (i.e. trend in DO)


#output.list <- list(chosen.lake, DO.data, chgpts.dates, DO.ts1, DO.ts2, DO.ts3, kpss1, kpss2, kpss3, fit1,fit2,fit3 )

output.list <- list(DO.data, 
                    chgpts.dates, 
                    DO.ts1, DO.ts2, #DO.ts3, #DO.ts4, DO.ts5, DO.ts6, DO.ts7, 
                    kpss1, kpss2, #kpss3, #kpss4, kpss5, kpss6, kpss7,
                    fit1, fit2#, fit3#, fit4, fit5, fit6, fit7
                    )

save(output.list, file = paste('cliff','2021_2','arima_output.Rdata', sep="_"))


#ARIMA values
#Castle 2018 - section 1 (0,1,0), section 2 (1,1,0)
#Castle 2018 2 - section 1 (1,1,0), section 2 (0,1,0)
#Castle 2018 3 - section 1 (1,1,0), section 2 (1,1,0)
#Castle 2018 4 - section 1 (1,1,1), section 2 (0,1,0)
#Castle 2019 - section 1 (1,1,0), section 2 (1,1,0)
#Castle 2019 2 - section 1 (1,1,5)
#Castle 2020 - section 1 (1,1,0), section 2 (1,1,0)
#Castle 2020 2 - section 1 (1,1,0)
#Castle 2020 3 - section 1 (1,1,1), section 2 (0,1,0), section 3 (1,1,0)
#Castle 2020 4 - section 1 (1,1,0), section 2 (1,1,0)
#Castle 2021 - section 1 (1,1,0), section 2 (2,1,0)
#Castle 2021 2 - section (1,1,0)
#Castle 2021 3 - section 1 (1,1,0)
#Castle 2022 - section 1 (1,1,0)
#Castle 2022 -  section (1,1,0)
#Cedar 2020 - section 1 (1,1,1), section 2 (1,1,0) 
#Cedar 2021 - section 1 (1,1,2), section 2 (1,1,0)
#Cedar 2022 - section 1 (1,1,2)
#Cliff 2019 - section 1 (,,)
#Cliff 2020 - section 1 (1,1,7), section 2 (1,1,0)
#Cliff 2021 - section 1 (1,1,1)
#Cliff 2021 2 - section 1 (1,1,0), section 2 (1,1,1)
#Cliff 2022 - section 1 (,,)
#Gumboot 2020 1 - section 1 (1,1,0), section 2 (1,1,0)
#Gumboot 2020 2 - section 1 (1,1,0), section 2 (1,1,3)
#Gumboot 2021 1 - section 1 (1,1,0)
#Gumboot 2022 - section 1 (1,1,0)
#Soapstone 2020 1 - section 1 (1,1,0)
#Soapstone 2020 2 - section 1 (1,1,0), section 2 (1,1,2)
#Soapstone 2021 1 - section 1 (0,1,0)
#Soapstone 2021 2 - section 1 ()
#Soapstone 2022 - section 1 (1,1,0) , section 2 (1,1,0)

