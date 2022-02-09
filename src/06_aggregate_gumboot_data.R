## The purpose of this code is to aggregate cleaned data
##
## 1. Load necessary libraries
## 2. Read cleaned data
## 3. 


############################
#SOAPSTONE LAKE DATA AGGREGATION
############################

#libraries
library(here) #for organization
library(tidyverse) #for clear code writing
library(lubridate) #To adjust dates easily
library(padr) #for date_time padding
library(wesanderson) #for color palette
library(viridis) #for color palette
library(scales) # to adjust the date axis and add tick marks
library(plotly) #For interactive plots if desired


#################################
#################################
#soapstone LAKE
#################################
#################################

#read data
#read data from hobo U22 logger placed 2m from sediment  
soapstone_2m_1 <- read_csv(here::here('data', 'processed', 'soapstone', 'soapstone_hobo_2m_2020.06.18_cleaned.csv')) 
soapstone_2m_2 <- read_csv(here::here('data', 'processed', 'soapstone', 'soapstone_hobo_2m_2020.07.16_cleaned.csv'))
soapstone_2m_3 <- read_csv(here::here('data', 'processed', 'soapstone', 'soapstone_hobo_2m_2020.10.10_cleaned.csv'))
soapstone_2m_4 <- read_csv(here::here('data', 'processed', 'soapstone', 'soapstone_hobo_2m_2021.06.12_cleaned.csv'))

#read data from hobo U22 logger placed in sediment 
soapstone_sed_1 <- read_csv(here::here('data', 'processed', 'soapstone', 'soapstone_hobo_sediment_2020.06.18_cleaned.csv'))
soapstone_sed_2 <- read_csv(here::here('data', 'processed', 'soapstone', 'soapstone_hobo_sediment_2020.07.16_cleaned.csv'))
soapstone_sed_3 <- read_csv(here::here('data', 'processed', 'soapstone', 'soapstone_hobo_sediment_2020.10.10_cleaned.csv'))
soapstone_sed_4 <- read_csv(here::here('data', 'processed', 'soapstone', 'soapstone_hobo_sediment_2021.06.12_cleaned.csv'))

#read data from hobo light pendant logger placed ~1m from surface 
soapstone_pend_1 <- read_csv(here::here('data', 'processed', 'soapstone', 'soapstone_light_pendant_2020.06.18_cleaned.csv'))
soapstone_pend_2 <- read_csv(here::here('data', 'processed', 'soapstone', 'soapstone_light_pendant_2020.07.16_cleaned.csv'))
soapstone_pend_3 <- read_csv(here::here('data', 'processed', 'soapstone', 'soapstone_light_pendant_2020.10.10_cleaned.csv'))

#read data from miniDOT logger placed 1m from sediment 
soapstone_DOT_1 <- read_csv(here::here('data', 'processed', 'soapstone', 'soapstone_DOT_2020.06.17_cleaned.csv'))
soapstone_DOT_2 <- read_csv(here::here('data', 'processed', 'soapstone', 'soapstone_DOT_2020.07.15_cleaned.csv'))
soapstone_DOT_3 <- read_csv(here::here('data', 'processed', 'soapstone', 'soapstone_DOT_2020.10.10_cleaned.csv'))
soapstone_DOT_4 <- read_csv(here::here('data', 'processed', 'soapstone', 'soapstone_DOT_2021.06.11_cleaned.csv'))

#bind dataframes
soapstone_2m_all <- rbind(soapstone_2m_1, soapstone_2m_2, soapstone_2m_3, soapstone_2m_4)
soapstone_sed_all <- rbind(soapstone_sed_1, soapstone_sed_2, soapstone_sed_3, soapstone_sed_4)
soapstone_pend_all <- rbind(soapstone_pend_1, soapstone_pend_2, soapstone_pend_3)
soapstone_DOT_all <- rbind(soapstone_DOT_1, soapstone_DOT_2, soapstone_DOT_3, soapstone_DOT_4)

#add depth name so that all dataframes can be combined
soapstone_2m_all$depth <- c('2m') #2m from sediment
soapstone_sed_all$depth <- c('sediment')
soapstone_pend_all$depth <- c('surface') #approx 1m from surface, considering surface temp, since lake is shallow.
soapstone_DOT_all$depth <- c('1m')

#remove DO and additional date_time from soapstone_DOT_all
soapstone_DOT_all_temp <- soapstone_DOT_all[,-c(1,2,5,6)]
#rename columns 
names(soapstone_DOT_all_temp) <- c('date_time', 'temp_C', 'depth')

#remove light intensity from soapstone_pend_all
soapstone_pend_all_temp <- soapstone_pend_all[,-c(3)]

################
#combine all soapstone data
################

#first, complete the time series by correcting time using lubridate and padding time series using padr
#2m
soapstone_2m_all$date_time <- mdy_hm(soapstone_2m_all$date_time) 
soapstone_2m_all$temp_C <- as.numeric(soapstone_2m_all$temp_C)
#str(soapstone_2m_all)
pad(soapstone_2m_all) #this function fills gaps in incomplete date_time variables

#sediment
soapstone_sed_all$date_time <- mdy_hm(soapstone_sed_all$date_time) 
soapstone_sed_all$temp_C <- as.numeric(soapstone_sed_all$temp_C)
#str(soapstone_2m_all)
pad(soapstone_sed_all) #this function fills gaps in incomplete date_time variables

#pendant
soapstone_pend_all_temp$date_time <- mdy_hm(soapstone_pend_all_temp$date_time) 
soapstone_pend_all_temp$temp_C <- as.numeric(soapstone_pend_all_temp$temp_C)
#str(soapstone_2m_all)
pad(soapstone_pend_all_temp) #this function fills gaps in incomplete date_time variables

#1m
#soapstone_DOT_all_temp$date_time <- mdy_hm(soapstone_DOT_all_temp$date_time) 
#soapstone_DOT_all_temp$temp_C <- as.numeric(soapstone_DOT_all_temp$temp_C)
#str(soapstone_2m_all)
#pad(soapstone_DOT_all_temp) #this function fills gaps in incomplete date_time variables

#######
#now combine all soapstone data with rbind
soapstone_all <- rbind(soapstone_2m_all, soapstone_sed_all, soapstone_pend_all_temp, soapstone_DOT_all_temp)
str(soapstone_all)
#check str of soapstone_all because I am having an issue with the below code on creating a mean. "Warning()" says that temp_C is a chr not num
#therefore convert to  num
str(soapstone_all)
soapstone_all$temp_C <- as.numeric(soapstone_all$temp_C)

#visualize soapstone
soapstone_plt <- ggplot(data = soapstone_all, aes(x = date_time, y = temp_C))+
  geom_line(aes(color = depth), size = 2)+
  theme_bw()
soapstone_plt

#########
#A bit noisy to look at, especially in spring and summer. 

#Make a new colum that has just days without time
soapstone_all$date_time2 <- droplevels(cut(soapstone_all$date_time, breaks = "day"))
soapstone_all$date_time2 <- ymd(soapstone_all$date_time2)


#use new date_time column to average daily data
soapstone_all2 <- aggregate(cbind(temp_C) ~ date_time2 + depth, data = soapstone_all, FUN = mean)
str(soapstone_all2)

soapstone_plt2 <- ggplot(data = soapstone_all2, aes(x = date_time2, y = temp_C), color = grey)+
  geom_line(aes(color = depth), size = 1.5)+
  theme_bw()
soapstone_plt2


######################THIS WILL GO IN THE DATA VIZ R SCRIPT IN THE FUTURE
#refine miniDOT data and move to "..._all" dataframe
soapstone_DOT_all2 <- soapstone_DOT_all[,-c(1,2)]
str(soapstone_DOT_all2)

#rename columns 
names(soapstone_DOT_all2) <- c('date_time', 'temp_C', 'DO', 'DO_sat', 'depth')

#temp, DO and DO sat all listed as char rather than num. Convert to num.
soapstone_DOT_all2$temp_C <- as.numeric(soapstone_DOT_all2$temp_C)
soapstone_DOT_all2$DO <- as.numeric(soapstone_DOT_all2$DO)
soapstone_DOT_all2$DO_sat <- as.numeric(soapstone_DOT_all2$DO_sat)

#Use break up date_time by day so that I can use a daily average.
soapstone_DOT_all2$date_time2 <- droplevels(cut(soapstone_DOT_all2$date_time, breaks = "day"))
soapstone_DOT_all2$date_time2 <- ymd(soapstone_DOT_all2$date_time2)
soapstone_all3 <- aggregate(cbind(temp_C, DO, DO_sat) ~ date_time2 + depth, data = soapstone_DOT_all2, FUN = mean)
str(soapstone_all3)


###########
#below commented code not working to 
#bind columns for soapstone_all2 and soapstone_all3
# soapstone_all_tempDO <- bind_rows(soapstone_all2, soapstone_all3)
# 
# #use gather() to plot temp and DO on the same plot
# soapstone_all_tempDO <- soapstone_all_tempDO %>%
#   tidyr::gather("id", "value", 3:4)

#combined plot
soapstone_plt3 <- ggplot()+
  geom_line(data = soapstone_all2, aes(x = date_time2, y = temp_C, color = depth), size = 1.5)+
  geom_line(data = soapstone_all3, aes(x = date_time2, y = DO), size = 1.5)+
  geom_vline(xintercept = as.numeric(as.Date("2019-11-26")), color = " blue", linetype = "dashed", size = 1)+ #formation = 2019-11-26 
  geom_vline(xintercept = as.numeric(as.Date("2020-04-11")), color = "red", linetype = "dashed", size = 1)+ #break up = 2020-04-11
  geom_vline(xintercept = as.numeric(as.Date("2020-11-15")), color = "blue", linetype = "dotted", size = 1)+ #initial formation = 2020-11-16
  geom_vline(xintercept = as.numeric(as.Date("2020-11-18")), color = "red", linetype = "dotted", size = 1)+ #breakup = 2020-11-08
  geom_vline(xintercept = as.numeric(as.Date("2020-11-19")), color = "blue", linetype = "dotted", size = 1)+ #initial formation = 2020-11-19
  geom_vline(xintercept = as.numeric(as.Date("2020-11-20")), color = "red", linetype = "dotted", size = 1)+ #breakup = 2020-11-20
  geom_vline(xintercept = as.numeric(as.Date("2020-11-21")), color = "blue", linetype = "dotted", size = 1)+ #initial formation = 2020-11-21
  geom_vline(xintercept = as.numeric(as.Date("2020-12-08")), color = "red", linetype = "dotted", size = 1)+ #breakup = 2020-12-08
  geom_vline(xintercept = as.numeric(as.Date("2020-12-10")), color = "blue", linetype = "dashed", size = 1)+ #final formation = 2020-12-18
  geom_vline(xintercept = as.numeric(as.Date("2021-04-17")), color = "red", linetype = "dashed", size = 1)+ #breakup = 2021-04-17 - some shore ice still present, but looks to be ~80% ice free
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 1.5)+ #break up = 2020-04-11
  ggtitle("soapstone Lake")+
  xlab("")+
  ylab("Temperature [C] / DO [mg/L]")+
  theme_classic()+
  theme(text = element_text(size = 30),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = wes_palette(name = "Darjeeling1"), name = "Depth")+
  #scale_color_viridis(discrete = TRUE, option = "D")+ #for colorblind friendly option
  scale_x_date(breaks = "month", labels = date_format("%b %Y"))
soapstone_plt3

#save plot to output
ggsave(here("output/plots/soapstone_final_plt.jpeg"), dpi = 500)


#use ggplotly for interactive plot to display data more easily
#ggplotly(soapstone_plt3)

