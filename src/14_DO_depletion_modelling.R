###############################################################################################################################################################
# This script, for one lake at a time, models mean daily under-ice DO data,
#first use changepoint analysis to identify different time segments under ice,
#then models each segment separately using an arima model. This should yield multiple
#DO depletion rates for each winter if there are significant changes that occur 
#(as opposed to calculating a single mean rate via regression)
#Author: Adrianne Smits
#Date: 04/24/2020
###############################################################################################################################################################
library(dplyr)
library(lubridate)
library(changepoint)
library(forecast)
library(tseries)
###############################################################################################################################################################
##set working directory, load data, select desired year and lake to analyze-Josh you can ignore most of this, just
#get your data into R. My data were structured as list objects (one per lake water year combo), with dataframes
#containing hourly sensor data. Columns were named by sensor type and sensor depth: ex. temp_2.2 = temperature at 2.2 m depth below lake surface

#wd <- 'C:/Users/asmits/Dropbox/Todd_share/Sierra Postdoc/Sierra Multi Lakes Data/R Code/Overwinter_data'

#setwd(wd)

#load calculated ice-on/off dates
winter.calcs <- read.csv('03_winter_calcs_output.csv',header=TRUE)

#Move to folder with sensor data:
wd_datafiles <- paste(wd,'Winter_datafiles',sep='/')
setwd(wd_datafiles)
year <-2020 #choose desired water year
wd_datafiles_yr <- paste(wd_datafiles,year,sep='/')
setwd(wd_datafiles_yr)

files <- list.files()#all the files
lake.names <- strsplit(files,"_")
lakes <- rep(NA,length=length(lake.names))
for(i in 1:length(lake.names)){
  lakes[i]<- lake.names[[i]][1] 
}

##Load pre-processed sensor data (trimmed and time-matched)
#empty list to hold all the data
lakes_data <- list()

#fill list with sensor data from all lakes
for(i in 1:length(lakes)){
  load(files[grep(lakes[i],files)])
  lakes_data[[i]]<- output.list[[1]]
  names(lakes_data)[[i]] <- paste(lakes[i],'data',sep='_')
}

#choose lake to analyze
chosen.lake <- 'Ireland'
lakes_data <- lakes_data[grep(chosen.lake,names(lakes_data))]
names(lakes_data[[1]])

#select winter calcs for chosen lake/year
ice.dates <- winter.calcs[winter.calcs$WY==year&winter.calcs$Lake==chosen.lake,]
ice.dates$ice_on <- as.POSIXct(ice.dates$ice_on, format="%m/%d/%Y %H:%M")
ice.dates$ice_off <- as.POSIXct(ice.dates$ice_off,format="%m/%d/%Y %H:%M")

###############################################################################################################################################################
##Aggregate DO data by day, from original hourly time frequency
lakes_data[[1]]$day <- as.Date(lakes_data[[1]]$datetime)
by_day <- group_by(lakes_data[[1]],day)
MeanDaily <- summarise_if(by_day,is.numeric, mean, na.rm = TRUE)
MeanDaily <- data.frame(MeanDaily)
#plot mean daily data to check
plot(MeanDaily$day,MeanDaily[,grep('DO',names(MeanDaily))],type='l')
abline(v=as.Date(ice.dates$ice_on))
abline(v=as.Date(ice.dates$ice_off))
#clip DO data before and after ice cover
MeanDaily.clipped <- MeanDaily[MeanDaily$day> as.Date(ice.dates$ice_on) & MeanDaily$day< as.Date(ice.dates$ice_off),]
#plot mean daily data to check
plot(MeanDaily.clipped$day,MeanDaily.clipped[,grep('DO',names(MeanDaily))],type='l')
abline(v=as.Date(ice.dates$ice_on))
abline(v=as.Date(ice.dates$ice_off))

###############################################################################################################################################################
##Apply changepoint analysis to DO  time series (e.g points where DO variance changes)
#change data to time series format
DO.ts <- ts(MeanDaily.clipped[,grep('DO',names(MeanDaily.clipped))],frequency=365, start=c(year(MeanDaily$day[1]),
                                                                                               month(MeanDaily$day[1]),
                                                                                               day(MeanDaily$day[1])))
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


###############################################################################################################################################################
##Split DO time series at changepoints (number will vary by dataset)
DO.data <-data.frame(MeanDaily.clipped)
chgpts.dates <- DO.data$day[chgpts]
#DO.ts1 <- DO.data #use this if there are no significant changepoints
DO.ts1 <- DO.data[1:chgpts[1],]
DO.ts2 <- DO.data[(chgpts[1]+1):chgpts[2],]
DO.ts3 <-  DO.data[(chgpts[2]+1):chgpts[3],]
#DO.ts4 <-  DO.data[(chgpts[6]+1):chgpts[7],]
###############################################################################################################################################################
##model each data segment as an arima function (following Obertegger et al. 2017)
#order=c(p,d,q), where p= lag in autoregressive term, d= degree of differencing, q= degree of moving average
#d=1 is first difference, should be sufficient

#Segment 1
#check if data is stationary around a level (if not, should have small p-value)
kpss1 <-kpss.test(DO.ts1[,grep('DO',names(DO.ts1))], null="Level")
pacf(DO.ts1[,grep('DO',names(DO.ts1))])#check pacf to see how much AR makes sense...

fit1 <- Arima(DO.ts1[,grep('DO',names(DO.ts1))],order=c(1,1,1), include.constant=TRUE)
summary(fit1)
checkresiduals(fit1)
#plot(fit1$fitted)


#Segment 2
#check if data is stationary around a level (if not, should have small p-value)
kpss2 <- kpss.test(DO.ts2[,grep('DO',names(DO.ts2))], null="Level")
pacf(DO.ts2[,grep('DO',names(DO.ts2))])
fit2 <- Arima(DO.ts2[,grep('DO',names(DO.ts2))],order=c(2,1,1), include.constant=TRUE)
summary(fit2)
checkresiduals(fit2)

#Segment 3
#check if data is stationary around a mean level (if not, should have small p-value)
kpss3 <-kpss.test(DO.ts3[,grep('DO',names(DO.ts3))], null="Level")
pacf(DO.ts3[,grep('DO',names(DO.ts3))])
fit3 <- Arima(DO.ts3[,grep('DO',names(DO.ts3))],order=c(0,1,0), include.constant=TRUE)
summary(fit3)
checkresiduals(fit3)

#Segment 4
#check if data is stationary around a mean level (if not, should have small p-value)
kpss4 <-kpss.test(DO.ts4[,grep('DO',names(DO.ts3))], null="Level")
pacf(DO.ts4[,grep('DO',names(DO.ts4))])
fit4 <- Arima(DO.ts4[,grep('DO',names(DO.ts4))],order=c(1,1,1), include.constant=TRUE)
summary(fit4)
checkresiduals(fit4)

###############################################################################################################################################################

#Save output: lake name, mean daily data (under ice period only), dates where there are variance changepoints in the DO data,
# DO time series split by changepoints (however many were found), results of the kpss test for each segment,and the arima models that were best fit
#to those time series segments, including a drift term (e.g. trend in DO)
wd <- 'C:/Users/asmits/Dropbox/Todd_share/Sierra Postdoc/Sierra Multi Lakes Data/R Code/Overwinter_data'
setwd(wd)

output.list <- list(chosen.lake,DO.data, chgpts.dates, DO.ts1, DO.ts2, DO.ts3, kpss1, kpss2, kpss3, fit1,fit2,fit3 )
save(output.list, file= paste("04",chosen.lake,year,'arima_output.Rdata',sep="_"))

###############################################################################################################################################################





