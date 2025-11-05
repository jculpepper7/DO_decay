## The purpose of this script is to:
## 1. Read clean, aggregated data
## 2. Data viz of unaltered data
## 3. Include moving averages
## 4. Data viz of moving average data

# libraries
library(tidyverse)
library(here)
library(lubridate)
library(padr)
library(patchwork)
library(plotly)
library(zoo) #for time series manipulation (e.g. rolling mean)

# 1. Read cleaned, aggregated data----------------------------------------------

cedar <- read_csv(here('data/processed/cedar/cedar_clean_agg_data_2022.csv'),
                 guess_max = 80000) %>% 
  mutate(
    lake = as.factor(lake),
    depth = as.factor(depth)
  )
  
  #use guess max to increase the number of 
  #rows that 'readr' reads prior to guess the     
  #column type. 
  #Did this because the NA values were 
  #producing a parsing error. To see what I 
  #mean, comment out the guess_max() function
  #Another solution is to specify column types
                                    
  

# 2. Data viz of unaltered data-------------------------------------------------

cedar_plt_non_avg <- ggplot(data = cedar)+
  # geom_line(aes(x = date_time, y = temp_c, color = depth), linewidth = 0.5)+
  geom_line(aes(x = date_time, y = do_mg_l), linewidth = 1)+
  theme_classic()+
  ggtitle("Cedar Lake")+
  xlab("")+
  ylab("Temperature [C] / DO [mg/L]")+
  theme_classic()+
  theme(text = element_text(size = 20),
        axis.text.x = element_text( hjust = 1, size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(hjust = 0.5))+
  #scale_color_manual(values = wes_palette(name = "Darjeeling1"), name = "Depth")
  scale_color_viridis_d() #for colorblind friendly option
  #scale_x_date(breaks = "month", labels = date_format("%b %Y"))
cedar_plt_non_avg

#ggsave(here('output/plots/cedar_20_21_22.png'), dpi = 300)

ggplotly(cedar_plt_non_avg)

# 3. Include moving averages----------------------------------------------------

cedar_w_avg <- cedar %>%
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
    lake = c('cedar')
  ) %>% 
  group_by(depth) %>% 
  complete(date = seq.Date(min(date), max(date), by = 'day')) %>% #10/2020 through 10/2021 missing (due to faulty sensor)
  ungroup() %>% 
  select(lake, date, depth, light_intensity_lux, temp_c, do_mg_l, do_sat)

#write_csv(cedar_w_avg, here('data/processed/cedar_hypox_plt.csv'))

# 4. data viz of averaged data (daily average)

cedar_avg_plt <- ggplot()+ 
  geom_rect(aes(
    xmin = ymd('2019-11-26'),
    xmax = ymd('2020-04-12'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2020-11-18'),
    xmax = ymd('2021-04-17'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2021-12-12'),
    xmax = ymd('2022-04-02'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  # geom_vline(xintercept = as.numeric(
  #   c(
  #     as.Date("2019-11-26"), 
  #     as.Date("2020-04-12"),
  #     as.Date("2020-11-18"), 
  #     as.Date("2021-04-17"),
  #     as.Date("2021-12-12"), 
  #     as.Date("2022-04-02")
  #     )))+
  #geom_line(data = cedar_w_avg %>% filter(depth == '1m' | depth == '2m'), aes(x = date, y = temp_c, color = depth), size = 0.5, alpha = 0.5)+ #| depth == 'sediment'
  #geom_line(data = cedar_w_avg %>% filter(depth == 'sediment'), aes(x = date, y = temp_c), size = 0.5, alpha = 0.8, color = 'light gray', linetype = 'dashed')+
  geom_line(data = cedar_w_avg %>% filter(depth == '1m'), aes(x = date, y = do_mg_l), size = 1.2)+
  scale_color_grey(name = 'Depth   ')+
  theme_classic()+
  labs(x = '', y = '')+
  # scale_y_continuous(
  #   name = '', #Alt+0176 for degree symbol #Dissolved Oxygen (mg/L)
  #   sec.axis = sec_axis(~.*coeff, name = '') #double y axis code from: https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
  #)+
  #labs(x = '', y = 'Dissolved Oxygen [mg/L]\nTemperature [C]')+
  #theme(legend.title = 'Depth')#+
  scale_y_continuous(breaks = c(0,10,20))+
  theme(legend.position = 'none',
        legend.title = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 22),
        axis.text.x = element_blank())+
  xlim(ymd('2017-10-01'), ymd('2022-06-16'))
cedar_avg_plt
ggplotly(cedar_avg_plt)

#ggsave(here('output/lake_final_plts/cedar_do_plt.jpeg'), dpi = 300)

# 5. Alt visualizations

# cedar_facet <- ggplot(data = cedar_w_avg) +
#   geom_line(aes(x = date, y = temp_c))+
#   theme_classic()+  
#   facet_wrap(~depth) +
#   ggtitle('Cedar Temperature')+
#   ylim(c(0, 32))
# cedar_facet
# 
# #ggsave(here('output/plots/cedar_facet_20_21.png'), dpi = 500)
# 
# cedar_do_conc <- ggplot(data = cedar) +
#   geom_line(aes(x = date_time, y = do_mg_l)) +
#   theme_classic()+
#   ggtitle('Cedar DO [mg/L]')
# cedar_do_conc
# 
# #ggsave(here('output/plots/cedar_do_concentration_20_21.png'), dpi = 500)
# 
# cedar_do_sat <- ggplot(data = cedar)+
#   geom_line(aes(x = date_time, y = do_sat))+
#   theme_classic()+
#   ggtitle('Cedar DO Saturation [%]')
# cedar_do_sat

#ggsave(here('output/plots/cedar_do_saturation_20_21.png'), dpi = 500)
# 
# cedar_light <- ggplot(data = cedar_w_avg)+
#   geom_rect(aes(
#     xmin = ymd('2019-11-26'),
#     xmax = ymd('2020-04-12'),
#     ymin = -Inf,
#     ymax = Inf
#   ), fill = 'light blue', alpha = 0.5)+
#   geom_rect(aes(
#     xmin = ymd('2020-11-18'),
#     xmax = ymd('2021-04-17'),
#     ymin = -Inf,
#     ymax = Inf
#   ), fill = 'light blue', alpha = 0.5)+
#   geom_rect(aes(
#     xmin = ymd('2021-12-12'),
#     xmax = ymd('2022-04-02'),
#     ymin = -Inf,
#     ymax = Inf
#   ), fill = 'light blue', alpha = 0.5)+
#   geom_line(aes(x = date, y = light_intensity_lux, color = depth))+
#   theme_classic()+
#   ggtitle('Light Intensity')
# cedar_light
# 





