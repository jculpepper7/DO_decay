## The purpose of this script is to:
## 1. Read clean, aggregated data
## 2. Data viz of unaltered data
## 3. Include moving averages
## 4. Data viz of moving average data

# libraries
library(tidyverse)
library()
library(padr)
library(patchwork)
library(plotly)
library(zoo) #for time series manipulation (e.g. rolling mean)

# 1. Read cleaned, aggregated data----------------------------------------------

gumboot <- read_csv(here('data/processed/gumboot/gumboot_clean_agg_data_2022.csv'),
                  guess_max = 80000) #use guess max to increase the number of 
#rows that 'readr' reads prior to guess the     
#column type. 
#Did this because the NA values were 
#producing a parsing error. To see what I 
#mean, comment out the guess_max() function
#Another solution is to specify column types
#See vignette("readr") 


# 2. Data viz of unaltered data-------------------------------------------------

# gumboot_plt_non_avg <- ggplot(data = gumboot)+
#   geom_line(aes(x = date_time, y = temp_c, color = depth), size = 1)+
#   geom_line(aes(x = date_time, y = do_mg_l), size = 1)+
#   theme_classic()+  
#   ggtitle("gumboot Lake")+
#   xlab("")+
#   ylab("Temperature [C] / DO [mg/L]")+
#   theme_classic()+
#   theme(text = element_text(size = 20),
#         axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
#         axis.text.y = element_text(size = 20),
#         plot.title = element_text(hjust = 0.5))+
#   scale_color_manual(values = wes_palette(name = "Darjeeling1"), name = "Depth")
# #scale_color_viridis(discrete = TRUE, option = "D")+ #for colorblind friendly option
# #scale_x_date(breaks = "month", labels = date_format("%b %Y"))
# gumboot_plt_non_avg

#ggsave(here('output/plots/gumboot_20_21.png'), dpi = 500)

#ggplotly(gumboot_plt_non_avg)

# 3. Include moving averages----------------------------------------------------

gumboot_w_avg <- gumboot %>%
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
    lake = c('gumboot')
  ) %>% 
  group_by(depth) %>% 
  complete(date = seq.Date(min(date), max(date), by = 'day')) %>% #10/2020 through 10/2021 missing (due to faulty sensor)
  ungroup() %>% 
  select(lake, date, depth, light_intensity_lux, temp_c, do_mg_l, do_sat)

#write_csv(gumboot_w_avg, here('data/processed/gb_hypox_plt.csv'))

# 4. data viz of averaged data (daily average)

#value for secondary axis (temperature)
coeff <- 1

gumboot_avg_plt <- ggplot()+ 
  geom_rect(aes(
    xmin = ymd('2019-11-23'),
    xmax = ymd('2020-05-04'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2020-11-18'),
    xmax = ymd('2021-05-02'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2021-12-18'),
    xmax = ymd('2022-04-07'),
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
  geom_line(data = gumboot_w_avg %>% filter(depth == '1m'  | depth == '2m'), aes(x = date, y = temp_c, color = depth), size = 0.5, alpha = 0.5)+
  geom_line(data = gumboot_w_avg %>% filter(depth == 'sediment'), aes(x = date, y = temp_c), size = 0.8, alpha = 0.5, color = 'light gray', linetype = 'dashed')+
  geom_line(data = gumboot_w_avg %>% filter(depth == '1m'), aes(x = date, y = do_mg_l), size = 1.2)+
  scale_color_grey(name = 'Depth   ')+
  theme_classic()+
  labs(x = '', y = '')+ #Dissolved Oxygen (mg/L)\nTemperature(°C)
  # scale_y_continuous(
  #   name = '', #Alt+0176 for degree symbol
  #   sec.axis = sec_axis(~.*coeff, name = '') #double y axis code from: https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
  # )+
  #labs(x = '', y = 'Dissolved Oxygen [mg/L]\nTemperature [C]')+
  #theme(legend.title = 'Depth')#+
  theme(legend.position = 'none',
        legend.title = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 22),
        axis.text.x = element_blank())+
  xlim(ymd('2017-10-01'), ymd('2022-06-16'))+
  scale_y_continuous(breaks = c(0,10,20))
gumboot_avg_plt
ggplotly(gumboot_avg_plt)

#ggsave(here('output/lake_final_plts/gumboot_do_plt.jpeg'), dpi = 300)

# 5. Alt visualizations

# gumboot_facet <- ggplot(data = gumboot) +
#   geom_line(aes(x = date_time, y = temp_c))+
#   theme_classic()+  
#   facet_wrap(~depth) +
#   ggtitle('gumboot Temperature')+
#   ylim(c(0, 32))
# gumboot_facet
# 
# ggsave(here('output/plots/gumboot_facet_20_21.png'), dpi = 500)
# 
# gumboot_do_conc <- ggplot(data = gumboot) +
#   geom_line(aes(x = date_time, y = do_mg_l)) +
#   theme_classic()+
#   ggtitle('gumboot DO [mg/L]')
# gumboot_do_conc
# 
# ggsave(here('output/plots/gumboot_do_concentration_20_21.png'), dpi = 500)
# 
# gumboot_do_sat <- ggplot(data = gumboot)+
#   geom_line(aes(x = date_time, y = do_sat))+
#   theme_classic()+
#   ggtitle('gumboot DO Saturation [%]')
# gumboot_do_sat
# 
# ggsave(here('output/plots/gumboot_do_saturation_20_21.png'), dpi = 500)










# 4. Combined Plot of DO ts for all lakes ---------------------------------

#Patchwork for fig for paper

castle_avg_plt/ cliff_avg_plt / gumboot_avg_plt/ cedar_avg_plt /soapstone_avg_plt+
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 22))

#ggsave(here('output/lake_final_plts/all_lakes_combined_2024.11.19_alt_dim.jpeg'), dpi = 300, height = 5, width = 6, units = 'in')  








################################################################
#plot for dissertation defense
# 
# gumboot_avg__do_dissertation_plt <- ggplot()+ 
#   geom_rect(aes(
#     xmin = ymd('2019-11-23'),
#     xmax = ymd('2020-05-04'),
#     ymin = -Inf,
#     ymax = Inf
#   ), fill = 'light blue', alpha = 0.5)+
#   geom_rect(aes(
#     xmin = ymd('2020-11-18'),
#     xmax = ymd('2021-05-02'),
#     ymin = -Inf,
#     ymax = Inf
#   ), fill = 'light blue', alpha = 0.5)+
#   geom_rect(aes(
#     xmin = ymd('2021-12-18'),
#     xmax = ymd('2022-04-07'),
#     ymin = -Inf,
#     ymax = Inf
#   ), fill = 'light blue', alpha = 0.5)+
#   # geom_vline(xintercept = as.numeric(
#   #   c(
#   #     as.Date("2019-11-26"), 
#   #     as.Date("2020-04-12"),
#   #     as.Date("2020-11-18"), 
#   #     as.Date("2021-04-17"),
#   #     as.Date("2021-12-12"), 
#   #     as.Date("2022-04-02")
#   #     )))+
#   #geom_line(data = gumboot_w_avg %>% filter(depth == '1m'  | depth == '2m'), aes(x = date, y = temp_c, color = depth), size = 0.5, alpha = 0.5)+
#   #geom_line(data = gumboot_w_avg %>% filter(depth == 'sediment'), aes(x = date, y = temp_c), size = 0.8, alpha = 0.5, color = 'light gray', linetype = 'dashed')+
#   geom_line(data = gumboot_w_avg %>% filter(depth == '1m'), aes(x = date, y = do_mg_l), size = 1.2)+
#   #scale_color_grey(name = 'Depth   ')+
#   theme_classic()+
#   xlab('')+ #Dissolved Oxygen (mg/L)\nTemperature(°C)
#   ylab(bquote('Dissolved Oxygen ('*mg~ L^-1*')'))+
#   theme(
#     text = element_text(size = 38),
#     axis.text.y = element_text(margin(unit(c(0,8,0,0), 'mm')))
#   )
#   #xlim(ymd('2017-10-01'), ymd('2022-06-16'))
# 
# gumboot_avg__do_dissertation_plt
# 
# #ggsave(here('output/lake_final_plts/gumboot_DO_plt.jpeg'), dpi = 300, width = 15, height = 10, unit = 'in')
# 
# 
# #Light intensity visualizaiton
# 
# gb_light <- ggplot(data = gumboot_w_avg)+
#   geom_rect(aes(
#     xmin = ymd('2019-11-23'),
#     xmax = ymd('2020-05-04'),
#     ymin = -Inf,
#     ymax = Inf
#   ), fill = 'light blue', alpha = 0.5)+
#   geom_rect(aes(
#     xmin = ymd('2020-11-18'),
#     xmax = ymd('2021-05-02'),
#     ymin = -Inf,
#     ymax = Inf
#   ), fill = 'light blue', alpha = 0.5)+
#   geom_rect(aes(
#     xmin = ymd('2021-12-18'),
#     xmax = ymd('2022-04-07'),
#     ymin = -Inf,
#     ymax = Inf
#   ), fill = 'light blue', alpha = 0.5)+
#   geom_line(aes(x = date, y = light_intensity_lux, color = depth))+
#   theme_classic()+
#   ggtitle('Light Intensity')
# gb_light
