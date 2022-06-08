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
cdr <- read_csv(here('data/processed/cedar/cedar_clean_agg_data.csv'), guess_max = 50000) 
#NOTE: without guess_max, read_csv() will fail to accurately predict the miniDOT data (columns 6-10) and will therefore exclude it from the tibble.

#Gumboot lake aggregated data
gb <- read_csv(here('data/processed/gumboot/gumboot_clean_agg_data.csv'), guess_max = 50000)

#Soapstone Pond aggregated data
ss <- read_csv(here('data/processed/soapstone/soapstone_clean_agg_data.csv'), guess_max = 50000)

#Join separate lake data
all_lakes <- bind_rows(cdr, gb, ss)

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
   
#Dissolved oxygen from miniDOTs
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

#Cedar
cedar_ice_2020 <- do_daily %>% 
  filter(water_year == 2020,
         lake == 'cedar',
         date_time >= as.POSIXct("2019-11-26"),
         date_time <= as.POSIXct("2020-04-12"))

cedar_ice_2021 <- do_daily %>% 
  filter(water_year == 2021,
         lake == 'cedar',
         date_time >= as.POSIXct("2020-12-18"),
         date_time <= as.POSIXct("2021-04-17"))

#Gumboot

#Water year 2020
gb_ice_2020 <- do_daily %>% 
  filter(water_year == 2020,
         lake == 'gumboot',
         date_time >= as.POSIXct("2019-11-29"),
         date_time <= as.POSIXct("2020-05-04"))

#Water year 2021
gb_ice_2021 <- do_daily %>% 
  filter(water_year == 2021,
         lake == 'gumboot',
         date_time >= as.POSIXct("2020-11-18"),
         date_time <= as.POSIXct("2021-05-02"))

#Soapstone

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
         date_time >= as.POSIXct("2020-11-07"),
         date_time <= as.POSIXct("2020-11-09"))  #Truncated to first instance of anoxia (2 days)
         #date_time <= as.POSIXct("2020-04-24")) #Complete ice off

#The second decay period likely caused by an incursion of water or snow that raised DO above hypoxia
ss_ice_2021_2 <- do_daily %>% 
  filter(water_year == 2020,
         lake == 'soapstone',
         date_time >= as.POSIXct("2020-11-19"),
         date_time <= as.POSIXct("2020-11-28"))

#5. Apply changepoint analysis to DO  time series-------------------------------
#NOTE: Chanegpoints indicate where DO variance changes

#change data to time series format
DO.ts <- ts(cedar_ice_2020$do_mg_l, frequency = 365, start = as.POSIXct('2019-11-25'))
# DO.ts <- ts(MeanDaily.clipped[,grep('DO',names(MeanDaily.clipped))],frequency=365, start=c(year(MeanDaily$day[1]),
#                                                                                                month(MeanDaily$day[1]),
#                                                                                                day(MeanDaily$day[1])))
plot.ts(DO.ts)

##De-trend DO data first before applying changepoint analysis 
DO.diff1 <- diff(x=DO.ts, differences=1,lag=1)

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

#DO.data <-data.frame(MeanDaily.clipped)
#chgpts.dates <- DO.data$day[chgpts]
DO.data <- cedar_ice_2020 %>% 
  mutate(
    day = day(date_time)
  )
chgpts.dates <- DO.data$day[chgpts]

#DO.ts1 <- DO.data #use this if there are no significant changepoints

DO.ts1 <- DO.data[1:chgpts[1],]
DO.ts2 <- DO.data[(chgpts[1]+1):chgpts[2],]
DO.ts3 <- DO.data[(chgpts[2]+1):chgpts[3],]
DO.ts4 <- DO.data[(chgpts[3]+1):chgpts[4],]
DO.ts5 <- DO.data[(chgpts[4]+1):chgpts[5],]
DO.ts6 <- DO.data[(chgpts[5]+1):chgpts[6],]
DO.ts7 <- DO.data[(chgpts[6]+1):chgpts[7],]
DO.ts8 <- DO.data[(chgpts[7]+1):chgpts[8],]

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
kpss1 <-kpss.test(DO.ts1$do_mg_l, null="Level")
pacf(DO.ts1$do_mg_l) #check pacf to see how much AR makes sense...
fit1 <- Arima(DO.ts1$do_mg_l, order=c(1,1,1), include.constant=TRUE)
summary(fit1)
checkresiduals(fit1)
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
fit3 <- Arima(DO.ts3$do_mg_l,order=c(0,1,0), include.constant=TRUE)
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
#and the arima models that were best fit to those time series segments, including a drift term (e.g. trend in DO)


#output.list <- list(chosen.lake, DO.data, chgpts.dates, DO.ts1, DO.ts2, DO.ts3, kpss1, kpss2, kpss3, fit1,fit2,fit3 )
output.list <- list(DO.data, 
                    chgpts.dates, 
                    DO.ts1, DO.ts2, DO.ts3, DO.ts4, DO.ts5, DO.ts6, DO.ts7, 
                    kpss1, kpss2, kpss3, kpss4, kpss5, kpss6, kpss7,
                    fit1, fit2, fit3, fit4, fit5, fit6, fit7)
save(output.list, file = paste("04",'soapstone','2021','arima_output.Rdata', sep="_"))






