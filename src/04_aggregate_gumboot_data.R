## The purpose of this code is to aggregate cleaned data
##
## 1. Load necessary libraries
## 2. Read cleaned data
## 3. 


############################
#GUMBOOT LAKE DATA AGGREGATION
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
#gumboot LAKE
#################################
#################################

#read data
#read data from hobo U22 logger placed 2m from sediment  
gumboot_2m_1 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_hobo_2m_2020.06.18_cleaned.csv')) 
gumboot_2m_2 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_hobo_2m_2020.07.16_cleaned.csv'))
#for some reason, temp_c reading as a chr rather than num
gumboot_2m_2 <- gumboot_2m_2 %>%
  mutate(temp_c = as.numeric(temp_c))%>%
  na.omit()
#----------
gumboot_2m_3 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_hobo_2m_2020.10.10_cleaned.csv'))
gumboot_2m_4 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_hobo_2m_2021.06.12_cleaned.csv'))
gumboot_2m_5 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_hobo_2m_2021.10.14_cleaned.csv'))

#read data from hobo U22 logger placed in sediment 
gumboot_sed_1 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_hobo_sediment_2020.06.18_cleaned.csv'))
gumboot_sed_2 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_hobo_sediment_2020.07.16_cleaned.csv'))
#for some reason, temp_c reading as a chr rather than num
gumboot_sed_2 <- gumboot_sed_2 %>%
  mutate(temp_c = as.numeric(temp_c))%>%
  na.omit()
#----------
gumboot_sed_3 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_hobo_sediment_2020.10.10_cleaned.csv'))
gumboot_sed_4 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_hobo_sediment_2021.06.12_cleaned.csv'))
gumboot_sed_5 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_hobo_sediment_2021.10.14_cleaned.csv'))

#read data from hobo light pendant logger placed ~1m from surface 
gumboot_pend_1 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_light_pendant_2020.06.18_cleaned.csv'))
gumboot_pend_2 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_light_pendant_2020.07.16_cleaned.csv'))
gumboot_pend_3 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_light_pendant_2020.10.10_cleaned.csv'))

#read data from miniDOT logger placed 1m from sediment 
gumboot_DOT_1 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_DOT_2020.06.17_cleaned.csv'))
gumboot_DOT_2 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_DOT_2020.07.15_cleaned.csv'))
gumboot_DOT_3 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_DOT_2020.10.10_cleaned.csv'))
gumboot_DOT_4 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_DOT_2021.06.11_cleaned.csv'))
gumboot_DOT_5 <- read_csv(here::here('data', 'processed', 'gumboot', 'gumboot_DOT_2021.10.14_cleaned.csv'))

#bind dataframes
gumboot_2m_all <- bind_rows(gumboot_2m_1, gumboot_2m_2, gumboot_2m_3, gumboot_2m_4, gumboot_2m_5)
gumboot_sed_all <- bind_rows(gumboot_sed_1, gumboot_sed_2, gumboot_sed_3, gumboot_sed_4, gumboot_sed_5)
gumboot_pend_all <- bind_rows(gumboot_pend_1, gumboot_pend_2, gumboot_pend_3)
gumboot_DOT_all <- bind_rows(gumboot_DOT_1, gumboot_DOT_2, gumboot_DOT_3, gumboot_DOT_4, gumboot_DOT_5) %>%
  mutate(
    date_time = pst, #add date_time as pst in order to bind the dataframe for easier plotting and data manipulation (averaging etc.)
    lake = c('gumboot'),
    depth = c('1m')
  )

#bind all dataframes 
gumboot_all_data <- bind_rows(gumboot_2m_all, gumboot_sed_all, gumboot_pend_all, gumboot_DOT_all)

#write_csv(gumboot_all_data, here('data/processed/gumboot/gumboot_clean_agg_data.csv'))
