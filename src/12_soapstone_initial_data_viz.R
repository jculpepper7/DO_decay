## The purpose of this script is to:
## 1. Read clean, aggregated data
## 2. Data viz of unaltered data
## 3. Include moving averages
## 4. Data viz of moving average data

# libraries
library(tidyverse)
library(lubridate)
library(here)
library(padr)
library(patchwork)
library(plotly)
library(zoo) #for time series manipulation (e.g. rolling mean)

# 1. Read cleaned, aggregated data----------------------------------------------

soapstone <- read_csv(here('data/processed/soapstone/soapstone_clean_agg_data_2022.csv'),
                    guess_max = 80000) #use guess max to increase the number of 
#rows that 'readr' reads prior to guess the     
#column type. 
#Did this because the NA values were 
#producing a parsing error. To see what I 
#mean, comment out the guess_max() function
#Another solution is to specify column types
#See vignette("readr") 

# 2. Data viz of unaltered data-------------------------------------------------

soapstone_plt_non_avg <- ggplot(data = soapstone)+
  geom_line(aes(x = date_time, y = temp_c, color = depth), size = 1)+
  geom_line(aes(x = date_time, y = do_mg_l), size = 1)+
  theme_classic()+  
  ggtitle("soapstone Lake")+
  xlab("")+
  ylab("Temperature [C] / DO [mg/L]")+
  theme_classic()+
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = wes_palette(name = "Darjeeling1"), name = "Depth")
#scale_color_viridis(discrete = TRUE, option = "D")+ #for colorblind friendly option
#scale_x_date(breaks = "date_time", labels = date_format("%b %Y"))
soapstone_plt_non_avg

#ggsave(here('output/plots/soapstone_20_21.png'), dpi = 500)

#ggplotly(soapstone_plt_non_avg)

# 3. Include moving averages----------------------------------------------------

soapstone_w_avg <- soapstone %>%
  mutate(
    day = day(date_time),
    month = month(date_time),
    year = year(date_time)
  ) %>%
  select(-date_time) %>% 
  group_by(depth, year, month, day) %>%
  summarise(
    temp_c = mean(temp_c),
    do_mg_l = mean(do_mg_l),
    do_sat = mean(do_sat),
    light_intensity_lux = mean(light_intensity_lux),
  ) %>% 
  ungroup() %>% 
  mutate(
    date = make_date(year = year, month = month, day = day),
    lake = c('soapstone')
  ) %>% 
  group_by(depth) %>% 
  complete(date = seq.Date(min(date), max(date), by = 'day')) %>% #10/2020 through 10/2021 missing (due to faulty sensor)
  ungroup() %>% 
  select(lake, date, depth, light_intensity_lux, temp_c, do_mg_l, do_sat)

# 4. data viz of averaged data (daily average)

soapstone_avg_plt <- ggplot()+ 
  geom_rect(aes(
    xmin = ymd('2019-11-21'),
    xmax = ymd('2020-05-04'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2020-11-06'),
    xmax = ymd('2021-04-24'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2021-12-11'),
    xmax = ymd('2022-04-24'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_line(data = soapstone_w_avg %>% filter(depth == '1m' | depth == 'sediment'), aes(x = date, y = temp_c, color = depth), size = 1.2)+
  geom_line(data = soapstone_w_avg %>% filter(depth == '1m'), aes(x = date, y = do_mg_l), size = 1.5)+
  scale_color_grey(name = 'Depth   ')+
  theme_classic()+
  labs(x = '', y = 'Dissolved Oxygen [mg/L]')+
  scale_y_continuous(
    name = '', #Alt+0176 for degree symbol
    sec.axis = sec_axis(~.*coeff, name = '') #double y axis code from: https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
  )+
  #labs(x = '', y = 'Dissolved Oxygen [mg/L]\nTemperature [C]')+
  #theme(legend.title = 'Depth')#+
  theme(legend.position = 'none',
        legend.title = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 10),
        axis.text.x = element_blank())
soapstone_avg_plt
ggplotly(soapstone_avg_plt)

#ggsave(here('output/lake_final_plts/soapstone_do_plt.jpeg'), dpi = 300)

# 5. Alt visualizations

soapstone_facet <- ggplot(data = soapstone) +
  geom_line(aes(x = date_time, y = temp_c))+
  theme_classic()+  
  facet_wrap(~depth, ncol = 1) +
  ggtitle('Soapstone Temperature')+
  ylim(c(0, 32))
soapstone_facet

#ggsave(here('output/plots/soapstone_facet_20_21.png'), dpi = 500)

soapstone_do_conc <- ggplot(data = soapstone) +
  geom_line(aes(x = date_time, y = do_mg_l)) +
  theme_classic()+
  ggtitle('Soapstone DO [mg/L]')
soapstone_do_conc

ggplotly(soapstone_do_conc)

#ggsave(here('output/plots/soapstone_do_concentration_20_21.png'), dpi = 500)

soapstone_do_sat <- ggplot(data = soapstone)+
  geom_line(aes(x = date_time, y = do_sat))+
  theme_classic()+
  ggtitle('Soapstone DO Saturation [%]')
soapstone_do_sat

ggplotly(soapstone_do_sat)

#ggsave(here('output/plots/soapstone_do_saturation_20_21.png'), dpi = 500)


