## The purpose of this code is to aggregate cleaned data
##
## 1. Load necessary libraries
## 2. Read cleaned data
## 3. 


############################
#SOAPSTONE POND DATA AGGREGATION
############################

#libraries
library(here) #for organization
library(tidyverse) #for tidy data
library(lubridate) #To adjust dates easily
library(padr) #for date_time padding
library(wesanderson) #for color palette
library(viridis) #for color palette
library(scales) # to adjust the date axis and add tick marks
library(plotly) #For interactive plots (if desired)


#################################
#################################
#SOAPSTONE POND
#################################
#################################

#read data

#read data from hobo U22 logger placed in sediment 
soapstone_sed_1 <- read_csv(here('data/processed/soapstone/soapstone_hobo_sediment_2020.06.18_cleaned.csv'))
soapstone_sed_2 <- read_csv(here('data/processed/soapstone/soapstone_hobo_sediment_2020.10.10_cleaned.csv'))
soapstone_sed_3 <- read_csv(here('data/processed/soapstone/soapstone_hobo_sediment_2021.06.12_cleaned.csv'))
soapstone_sed_4 <- read_csv(here('data/processed/soapstone/soapstone_hobo_sediment_2021.08.28_cleaned.csv'))
soapstone_sed_5 <- read_csv(here('data/processed/soapstone/soapstone_hobo_sediment_2021.10.14_cleaned.csv'))
soapstone_sed_6 <- read_csv(here('data/processed/soapstone/soapstone_hobo_sediment_2022.06.11_cleaned.csv'))

#read data from hobo light pendant logger placed ~1m from surface 
soapstone_pend_1 <- read_csv(here('data/processed/soapstone/soapstone_light_pendant_2020.06.18_cleaned.csv'))
soapstone_pend_2 <- read_csv(here('data/processed/soapstone/soapstone_light_pendant_2020.10.11_cleaned.csv'))
soapstone_pend_3 <- read_csv(here('data/processed/soapstone/soapstone_light_pendant_2021.06.12_cleaned.csv'))
soapstone_pend_4 <- read_csv(here('data/processed/soapstone/soapstone_light_pendant_2021.08.28_cleaned.csv'))
soapstone_pend_5 <- read_csv(here('data/processed/soapstone/soapstone_light_pendant_2021.10.16_cleaned.csv'))
soapstone_pend_6 <- read_csv(here('data/processed/soapstone/soapstone_light_pendant_2022.06.11_cleaned.csv'))

#read data from miniDOT logger placed 1m from sediment 
soapstone_DOT_1 <- read_csv(here('data/processed/soapstone/soapstone_miniDOT_2020.06.17_cleaned.csv'))
soapstone_DOT_2 <- read_csv(here('data/processed/soapstone/soapstone_miniDOT_2020.10.11_cleaned.csv'))
soapstone_DOT_3 <- read_csv(here('data/processed/soapstone/soapstone_miniDOT_2021.06.11_cleaned.csv'))
soapstone_DOT_4 <- read_csv(here('data/processed/soapstone/soapstone_miniDOT_2021.08.28_cleaned.csv'))
soapstone_DOT_5 <- read_csv(here('data/processed/soapstone/soapstone_miniDOT_2021.10.14_cleaned.csv'))
soapstone_DOT_6 <- read_csv(here('data/processed/soapstone/soapstone_miniDOT_2022.06.11_cleaned.csv'))

#bind dataframes form individual sensors

#MiniDOT sensor
soapstone_sed_all <- bind_rows(soapstone_sed_1, soapstone_sed_2, soapstone_sed_3, soapstone_sed_4, soapstone_sed_5, soapstone_sed_6) %>%
  pad() %>%
  mutate(
    depth = c('sediment'),
    lake = c('soapstone')
  )

#HOBO light pendant
soapstone_pend_all <- bind_rows(soapstone_pend_1, soapstone_pend_2, soapstone_pend_3, soapstone_pend_4, soapstone_pend_5, soapstone_pend_6) %>%
  pad() %>%
  mutate(
    depth = c('1.5m'),
    lake = c('soapstone')
  ) 

#HOBO U22 in sediment
soapstone_DOT_all <- bind_rows(soapstone_DOT_1, soapstone_DOT_2, soapstone_DOT_3, soapstone_DOT_4, soapstone_DOT_5, soapstone_DOT_6) %>%
  mutate(
    date_time = pst, #add date_time as pst in order to bind the dataframe for easier plotting and data manipulation (averaging etc.)
    lake = c('soapstone'),
    depth = c('1m')
  )

#bind all dataframes 
soapstone_all_data <- bind_rows(soapstone_sed_all, soapstone_pend_all, soapstone_DOT_all)

write_csv(soapstone_all_data, here('data/processed/soapstone/soapstone_clean_agg_data_2022.csv'))
