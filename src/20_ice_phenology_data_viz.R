## The goal of this script is to 
## 1. import ice phenology, duration, and hypoxia data created in previous scritps
## 2. provide a plot summarizing those data

# 1. Load libraries-------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(plotly)
library(patchwork)

# 2. Import data----------------------------------------------------------------

ice_phenology <- read_csv(here('data/met_data/lake_ice_phenology/ice_phenology_data3.csv')) %>% 
  clean_names()

ice_phenology <- ice_phenology %>% 
  mutate(
    lake = as.factor(lake),
    ice_on_date = ymd(paste0(water_year - 1, "-01-01")) + days(ice_on_doy - 1),
    ice_off_date = ymd(paste0(water_year, "-01-01")) + days(ice_off_doy - 1)
  )

# 3. Data viz-------------------------------------------------------------------

#Reorder lakes from shallowest to deepest

ice_phenology$lake <- factor(ice_phenology$lake, 
                          levels = c('soapstone', 'cedar', 'gumboot', 'cliff', 'castle'))

#upper case labels
labels <- c('Soapstone', 'Cedar', 'Gumboot', 'Cliff', 'Castle')


# 3a. Ice on/off plot----

#Ice on
ice_on_plt <- ggplot(data = ice_phenology)+
  geom_point(aes(x = water_year, y = ice_on_doy, shape = lake), size = 4)+
  geom_line(aes(x = water_year, y = ice_on_doy, linetype = lake), size = 1.1)+
  theme_classic()+
  labs(x = '', y = 'Ice On DOY')+
  scale_shape_discrete(labels = labels)+
  scale_linetype_discrete(labels = labels)+
  theme(
    legend.position = c(0.2, 0.2),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_blank(),
    text = element_text(size = 25),
    axis.title.y = element_text(margin = unit(c(0,5,0,0), 'mm')),
    legend.key.height = unit(0.1, 'cm'),
    legend.key.width = unit(0.1, 'cm')
  )
ice_on_plt
ggplotly(ice_on_plt)

#ggsave(here('output/lake_final_plts/ice_on_plt.jpeg'), dpi = 300, width = 7, height = 6, units = 'in')

#ice off
ice_off_plt <- ggplot(data = ice_phenology)+
  geom_point(aes(x = water_year, y = ice_off_doy, shape = lake), size = 3)+
  geom_line(aes(x = water_year, y = ice_off_doy, linetype = lake))+
  theme_classic()+
  labs(x = '', y = 'Ice Off DOY')+
  ylim(c(90,155))+
  scale_shape_discrete(labels = labels)+
  scale_linetype_discrete(labels = labels)+
  scale_y_continuous(breaks = seq(90,155,10))+
  theme(
    text = element_text(size = 25),
    legend.position = '',
    axis.title.y = element_text(margin = unit(c(0,5,0,0), 'mm'))
  )
ice_off_plt

ggsave(here('output/lake_final_plts/ice_off_plt.jpeg'), dpi = 300, width = 7, height = 6, units = 'in')


#Multipanel figure for paper

#using the do decay from script 16

(ice_on_plt)/(ice_off_plt)+
  plot_annotation(tag_levels = 'A')

#ice duration

ice_duration_plt <- ggplot(data = ice_phenology)+
  geom_point(aes(x = water_year, y = ice_dur_days, shape = lake), size = 3)+
  geom_line(aes(x = water_year, y = ice_dur_days, linetype = lake))+
  theme_classic()+
  labs(x = '', y = 'Ice Duration')+
  theme(
    legend.position = 'bottom'
  )
ice_duration_plt

#hypoxia duration

hypoxia_plt <- ggplot(data = ice_phenology)+
  geom_point(aes(x = water_year, y = hypoxia_days, shape = lake), size = 3)+
  geom_line(aes(x = water_year, y = hypoxia_days, linetype = lake))+
  theme_classic()+
  labs(x = '', y = 'Hypoxia (days)')+
  theme(
    text = element_text(size = 25),
    legend.position = 'none',
    axis.title.y = element_text(margin = unit(c(0,5,0,0), 'mm'))
  )
hypoxia_plt

#Multipanel figure for paper

#using the do decay from script 16

(ice_on_plt+ice_off_plt)/(hypoxia_plt + plot_spacer())+
  plot_annotation(tag_levels = 'A')

#ggsave(here('output/lake_final_plts/fig_1_4_panel_plt_2022.11.29.jpeg'), dpi = 300, width = 14, height = 10, units = 'in')


#SWE plotting

swe_plt <- ggplot(data = ice_phenology %>% filter(lake == 'castle'))+
  geom_line(aes(x = water_year, y = swe_mean_mm), color = 'black')+
  geom_line(aes(x = water_year, y = swe_max_mm), color = 'gray')+
  #geom_line(aes(x = water_year, y = hypoxia_days, linetype = lake))+
  theme_classic()+
  labs(x = '', y = 'SWE (mm)')+
  theme(
    legend.position = 'none'
  )
swe_plt

#ggsave(here('output/lake_final_plts/swe_plt.jpeg'), dpi = 300, height = 10, width = 14, units = 'in')



