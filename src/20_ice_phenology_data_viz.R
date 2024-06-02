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
  geom_point(aes(x = water_year, y = ice_on_doy, shape = lake), size = 4)+
  geom_line(aes(x = water_year, y = ice_on_doy, linetype = lake), size = 1.1)+
  theme_classic()+
  labs(x = '', y = 'Ice On DOY')+
  scale_shape_discrete(labels = c('Castle', 'Cedar', 'Cliff', 'Gumboot', 'Soapstone'))+
  scale_linetype_discrete(labels = c('Castle', 'Cedar', 'Cliff', 'Gumboot', 'Soapstone'))+
  theme(
    legend.position = c(0.2, 0.3),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_blank(),
    text = element_text(size = 25),
    axis.title.y = element_text(margin = unit(c(0,5,0,0), 'mm')),
    legend.key.height = unit(0.1, 'cm'),
    legend.key.width = unit(0.1, 'cm')
  )
ice_on_plt

#ggsave(here('output/lake_final_plts/ice_on_plt.jpeg'), dpi = 300, width = 15, height = 10, units = 'in')

#ice off
ice_off_plt <- ggplot(data = ice_phenology)+
  geom_point(aes(x = water_year, y = ice_off_doy, shape = lake), size = 3)+
  geom_line(aes(x = water_year, y = ice_off_doy, linetype = lake))+
  theme_classic()+
  labs(x = '', y = 'Ice Off DOY')+
  theme(
    text = element_text(size = 25),
    legend.position = '',
    axis.title.y = element_text(margin = unit(c(0,5,0,0), 'mm'))
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
    text = element_text(size = 25),
    legend.position = 'none',
    axis.title.y = element_text(margin = unit(c(0,5,0,0), 'mm'))
  )
hypoxia_plt

#Multipanel figure for paper

#using the do decay from script 16

(ice_on_plt+ice_off_plt)/(hypoxia_plt + plot_spacer())+
  plot_annotation(tag_levels = 'A')

ggsave(here('output/lake_final_plts/fig_1_4_panel_plt_2022.11.29.jpeg'), dpi = 300, width = 14, height = 10, units = 'in')


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



