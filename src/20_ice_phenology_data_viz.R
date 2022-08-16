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

ice_phenology <- read_csv(here('data/met_data/lake_ice_phenology/ice_phenology_data.csv')) %>% 
  clean_names()

ice_phenology <- ice_phenology %>% 
  mutate(
    lake = as.factor(lake)
  ) %>% 
  na.omit()

# 3. Data viz-------------------------------------------------------------------

# 3a. Ice on/off plot----

#Ice on
ice_on_plt <- ggplot(data = ice_phenology)+
  geom_point(aes(x = water_year, y = ice_on_doy, shape = lake), size = 3)+
  geom_line(aes(x = water_year, y = ice_on_doy, linetype = lake))+
  theme_classic()+
  labs(x = '', y = 'Ice On DOY')+
  scale_shape_discrete(labels = c('Castle', 'Cedar', 'Cliff', 'Gumboot', 'Soapstone'))+
  scale_linetype_discrete(labels = c('Castle', 'Cedar', 'Cliff', 'Gumboot', 'Soapstone'))+
  theme(
    legend.position = c(0.15, 0.3 ),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_blank()
  )
ice_on_plt

#ice off
ice_off_plt <- ggplot(data = ice_phenology)+
  geom_point(aes(x = water_year, y = ice_off_doy, shape = lake), size = 3)+
  geom_line(aes(x = water_year, y = ice_off_doy, linetype = lake))+
  theme_classic()+
  labs(x = '', y = 'Ice Off DOY')+
  theme(
    legend.position = ''
  )
ice_off_plt

#ice duration

ice_duration_plt <- ggplot(data = ice_phenology)+
  geom_point(aes(x = water_year, y = ice_dur_days, shape = lake), size = 3)+
  geom_line(aes(x = water_year, y = ice_dur_days, linetype = lake))+
  theme_classic()+
  labs(x = '', y = 'Ice Off DOY')+
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
    legend.position = 'none'
  )
hypoxia_plt

#Multipanel figure for paper

#using the do decay from script 16

(ice_on_plt+ice_off_plt)/(hypoxia_plt+do_decay_plt) +
  plot_annotation(tag_levels = 'A')

#ggsave(here('output/lake_final_plts/fig_1_4_panel_plt.jpeg'), dpi = 300, width = 14, height = 10, units = 'in')


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



