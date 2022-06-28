## The purpose of this code is to aggregate cleaned data
##
## 1. Load necessary libraries
## 2. Read cleaned data
## 3. 


############################
#CEDAR LAKE DATA AGGREGATION
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
#CEDAR LAKE
#################################
#################################

#read data
#read data from hobo U22 logger placed 2m from sediment  
cedar_2m_1 <- read_csv(here('data/processed/cedar/cedar_hobo_2m_2020.06.18_cleaned.csv')) 
cedar_2m_2 <- read_csv(here('data/processed/cedar/cedar_hobo_2m_2020.07.16_cleaned.csv'))
#for some reason, temp_c reading as a chr rather than num
cedar_2m_2 <- cedar_2m_2 %>%
  mutate(temp_c = as.numeric(temp_c))%>%
  na.omit()
#----------
cedar_2m_3 <- read_csv(here('data/processed/cedar/cedar_hobo_2m_2020.10.10_cleaned.csv'))
cedar_2m_4 <- read_csv(here('data/processed/cedar/cedar_hobo_2m_2021.06.12_cleaned.csv'))
cedar_2m_5 <- read_csv(here('data/processed/cedar/cedar_hobo_2m_2021.08.28_cleaned.csv'))
cedar_2m_6 <- read_csv(here('data/processed/cedar/cedar_hobo_2m_2021.10.14_cleaned.csv'))
cedar_2m_7 <- read_csv(here('data/processed/cedar/cedar_hobo_2m_2022.06.11_cleaned.csv'))

#read data from hobo U22 logger placed in sediment 
cedar_sed_1 <- read_csv(here('data/processed/cedar/cedar_hobo_sediment_2020.06.18_cleaned.csv'))
cedar_sed_2 <- read_csv(here('data/processed/cedar/cedar_hobo_sediment_2020.07.16_cleaned.csv'))
#for some reason, temp_c reading as a chr rather than num
cedar_sed_2 <- cedar_sed_2 %>%
  mutate(temp_c = as.numeric(temp_c))%>%
  na.omit()
#----------
cedar_sed_3 <- read_csv(here('data/processed/cedar/cedar_hobo_sediment_2020.10.10_cleaned.csv'))
cedar_sed_4 <- read_csv(here('data/processed/cedar/cedar_hobo_sediment_2021.06.12_cleaned.csv'))
cedar_sed_5 <- read_csv(here('data/processed/cedar/cedar_hobo_sediment_2021.08.28_cleaned.csv'))
cedar_sed_6 <- read_csv(here('data/processed/cedar/cedar_hobo_sediment_2021.10.14_cleaned.csv'))
cedar_sed_7 <- read_csv(here('data/processed/cedar/cedar_hobo_sediment_2022.06.11_cleaned.csv'))

#read data from hobo light pendant logger placed ~1m from surface 
cedar_pend_1 <- read_csv(here('data/processed/cedar/cedar_light_pendant_2020.06.18_cleaned.csv'))
cedar_pend_2 <- read_csv(here('data/processed/cedar/cedar_light_pendant_2020.07.16_cleaned.csv'))
cedar_pend_3 <- read_csv(here('data/processed/cedar/cedar_light_pendant_2020.10.10_cleaned.csv'))
cedar_pend_4 <- read_csv(here('data/processed/cedar/cedar_light_pendant_2022.06.11_cleaned.csv'))
cedar_pend_5 <- read_csv(here('data/processed/cedar/cedar_light_pendant_80cm_2022.06.11_cleaned.csv'))
cedar_pend_6 <- read_csv(here('data/processed/cedar/cedar_light_pendant_150cm_2022.06.11_cleaned.csv'))

#read data from miniDOT logger placed 1m from sediment 
cedar_DOT_1 <- read_csv(here('data/processed/cedar/cedar_DOT_2020.06.17_cleaned.csv'))
cedar_DOT_2 <- read_csv(here('data/processed/cedar/cedar_DOT_2020.07.15_cleaned.csv'))
cedar_DOT_3 <- read_csv(here('data/processed/cedar/cedar_DOT_2020.10.10_cleaned.csv'))
cedar_DOT_4 <- read_csv(here('data/processed/cedar/cedar_DOT_2021.06.11_cleaned.csv'))
cedar_DOT_5 <- read_csv(here('data/processed/cedar/cedar_DOT_2021.08.28_cleaned.csv'))
cedar_DOT_6 <- read_csv(here('data/processed/cedar/cedar_DOT_2021.10.14_cleaned.csv'))
cedar_DOT_7 <- read_csv(here('data/processed/cedar/cedar_DOT_2022.06.11_cleaned.csv'))

#bind dataframes
cedar_2m_all <- bind_rows(cedar_2m_1, cedar_2m_2, cedar_2m_3, cedar_2m_4, cedar_2m_5, cedar_2m_6, cedar_2m_7)
cedar_sed_all <- bind_rows(cedar_sed_1, cedar_sed_2, cedar_sed_3, cedar_sed_4, cedar_sed_5, cedar_sed_6, cedar_sed_7)
cedar_pend_all <- bind_rows(cedar_pend_1, cedar_pend_2, cedar_pend_3, cedar_pend_4, cedar_pend_5, cedar_pend_6)
cedar_DOT_all <- bind_rows(cedar_DOT_1, cedar_DOT_2, cedar_DOT_3, cedar_DOT_4, cedar_DOT_5, cedar_DOT_6, cedar_DOT_7) %>%
  mutate(
    date_time = pst, #add date_time as pst in order to bind the dataframe for easier plotting and data manipulation (averaging etc.)
    lake = c('cedar'),
    depth = c('1m')
    )

#bind all dataframes 
cedar_all_data <- bind_rows(cedar_2m_all, cedar_sed_all, cedar_pend_all, cedar_DOT_all)

write_csv(cedar_all_data, here('data/processed/cedar/cedar_clean_agg_data_2022.csv'))

######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################

#visualize cedar
cedar_plt <- ggplot(data = cedar_all_data, aes(x = date_time, y = temp_c))+
  geom_line(aes(color = depth), size = 2)+
  theme_classic()
cedar_plt

#########
#A bit noisy to look at, especially in spring and summer. 

#Make a new colum that has just days without time
cedar_all$date_time2 <- droplevels(cut(cedar_all$date_time, breaks = "day"))
cedar_all$date_time2 <- ymd(cedar_all$date_time2)


#use new date_time column to average daily data
cedar_all2 <- aggregate(cbind(temp_C) ~ date_time2 + depth, data = cedar_all, FUN = mean)
str(cedar_all2)

cedar_plt2 <- ggplot(data = cedar_all2, aes(x = date_time2, y = temp_C), color = grey)+
  geom_line(aes(color = depth), size = 1.5)+
  theme_bw()
cedar_plt2


######################THIS WILL GO IN THE DATA VIZ R SCRIPT IN THE FUTURE
#refine miniDOT data and move to "..._all" dataframe
cedar_DOT_all2 <- cedar_DOT_all[,-c(1,2)]
str(cedar_DOT_all2)

#rename columns 
names(cedar_DOT_all2) <- c('date_time', 'temp_C', 'DO', 'DO_sat', 'depth')

#temp, DO and DO sat all listed as char rather than num. Convert to num.
cedar_DOT_all2$temp_C <- as.numeric(cedar_DOT_all2$temp_C)
cedar_DOT_all2$DO <- as.numeric(cedar_DOT_all2$DO)
cedar_DOT_all2$DO_sat <- as.numeric(cedar_DOT_all2$DO_sat)

#Use break up date_time by day so that I can use a daily average.
cedar_DOT_all2$date_time2 <- droplevels(cut(cedar_DOT_all2$date_time, breaks = "day"))
cedar_DOT_all2$date_time2 <- ymd(cedar_DOT_all2$date_time2)
cedar_all3 <- aggregate(cbind(temp_C, DO, DO_sat) ~ date_time2 + depth, data = cedar_DOT_all2, FUN = mean)
str(cedar_all3)


###########
#below commented code not working to 
#bind columns for cedar_all2 and cedar_all3
# cedar_all_tempDO <- bind_rows(cedar_all2, cedar_all3)
# 
# #use gather() to plot temp and DO on the same plot
# cedar_all_tempDO <- cedar_all_tempDO %>%
#   tidyr::gather("id", "value", 3:4)

#combined plot
cedar_plt3 <- ggplot()+
  geom_line(data = cedar_all2, aes(x = date_time2, y = temp_C, color = depth), size = 1.5)+
  geom_line(data = cedar_all3, aes(x = date_time2, y = DO), size = 1.5)+
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
  ggtitle("Cedar Lake")+
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
cedar_plt3

#save plot to output
ggsave(here("output/plots/cedar_final_plt.jpeg"), dpi = 500)


#use ggplotly for interactive plot to display data more easily
#ggplotly(cedar_plt3)

