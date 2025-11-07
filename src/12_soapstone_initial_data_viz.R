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
        plot.title = element_text(hjust = 0.5))#+
  #scale_color_manual(values = wes_palette(name = "Darjeeling1"), name = "Depth")
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

# write_csv(soapstone_w_avg, here('data/processed/ss_hypox_plt.csv'))

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
  geom_line(
    data = soapstone_w_avg %>% filter(depth == '1m' | depth == '1.5m'), 
    aes(x = date, y = temp_c/2, color = depth), 
    linewidth = 1, 
    alpha = 0.7
  )+
  # geom_line(data = soapstone_w_avg %>% filter(depth == 'sediment'), aes(x = date, y = temp_c), size = 0.8, alpha = 0.5, color = 'light gray', linetype = 'dashed')+
  geom_line(
    data = soapstone_w_avg %>% filter(depth == '1m'), 
    aes(x = date, y = do_mg_l), 
    linewidth = 1
  )+
  # scale_color_grey(name = 'Depth   ')+
  scale_color_viridis_d(
    direction = -1
  )+
  theme_classic()+
  labs(x = '', y = '')+
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12)#,
        #axis.text.x = element_blank()
        )+
  xlim(ymd('2017-10-01'), ymd('2022-06-16'))+
  scale_y_continuous(breaks = c(0,10,20))+
  scale_y_continuous(
    name = expression("Dissolved Oxygen (mg L"^{-1}*")"),
    breaks = seq(0,12,3),
    sec.axis = sec_axis(
      ~. *2,
      name = 'Temperature (\u00b0C)',
      breaks = seq(0,24,6)
    )
  )+
  guides(
    color = guide_legend(nrow = 1)
  )  
soapstone_avg_plt
# ggplotly(soapstone_avg_plt)

#ggsave(here('output/lake_final_plts/soapstone_do_plt.jpeg'), dpi = 300)
ggsave(
  # here('output/lake_final_plts/castle_do_plt_w_temp_update_pdf.png'), 
  # here('output/lake_final_plts/do_ts_fig/update/castle_do_plt_w_temp_update.pdf'), 
  here('output/lake_final_plts/do_ts_fig/update/ss_do_plt_w_temp_update_NAs2.pdf'), 
  dpi = 300,
  width = 6,
  height = 1.5
)

# 5. Alt visualizations

# soapstone_facet <- ggplot(data = soapstone) +
#   geom_line(aes(x = date_time, y = temp_c))+
#   theme_classic()+  
#   facet_wrap(~depth, ncol = 1) +
#   ggtitle('Soapstone Temperature')+
#   ylim(c(0, 32))
# soapstone_facet
# 
# #ggsave(here('output/plots/soapstone_facet_20_21.png'), dpi = 500)
# 
# soapstone_do_conc <- ggplot(data = soapstone) +
#   geom_line(aes(x = date_time, y = do_mg_l)) +
#   theme_classic()+
#   ggtitle('Soapstone DO [mg/L]')
# soapstone_do_conc
# 
# ggplotly(soapstone_do_conc)
# 
# #ggsave(here('output/plots/soapstone_do_concentration_20_21.png'), dpi = 500)
# 
# soapstone_do_sat <- ggplot(data = soapstone)+
#   geom_line(aes(x = date_time, y = do_sat))+
#   theme_classic()+
#   ggtitle('Soapstone DO Saturation [%]')
# soapstone_do_sat
# 
# ggplotly(soapstone_do_sat)

#ggsave(here('output/plots/soapstone_do_saturation_20_21.png'), dpi = 500)

test <- soapstone_w_avg %>% 
  select(-c(depth, temp_c, do_sat, do_mg_l)) %>% 
  arrange(date) %>% 
  na.omit()

test2 <- soapstone_w_avg %>% 
  select(-c(depth, temp_c, do_sat, light_intensity_lux)) %>% 
  arrange(date) %>% 
  na.omit()

test3 <- test %>% 
  inner_join(test2) %>% 
  filter(
    between(
      date, 
      ymd('2019-11-21'), 
      ymd('2020-05-04')
    ) |
    between(
      date, 
      ymd('2020-11-06'), 
      ymd('2021-04-24')
    ) |
    between(
      date, 
      ymd('2021-12-11'), 
      ymd('2022-04-24')
    )
  ) %>% 
  mutate(
    year = as.factor(if_else(
      month(date) >= 10, year(date)+1, year(date)
    ))
  ) %>% 
  pad()




ggplot(
  data = test3, 
  mapping = aes(x = light_intensity_lux, y = do_mg_l, color = year),
  size = 2.5,
  alpha = 0.7
)+
  geom_point()+
  theme_classic()

library(patchwork) 
library(plotly)
  
p1 <- ggplot(data = test3)+
  geom_line(
    mapping = aes(x = date, y = light_intensity_lux)
  )+
  geom_vline(
    xintercept = c(ymd('2020-02-07'), ymd('2022-02-13')),
    linetype = 'dashed',
    color = 'blue'
  )
p1
ggplotly(p1)

p2 <- ggplot(data = test3)+
  geom_line(
    mapping = aes(x = date, y = do_mg_l),
    color = 'red'
  )+
  geom_vline(
    xintercept = c(ymd('2020-02-07'), ymd('2022-02-13')),
    linetype = 'dashed',
    color = 'blue'
  )
p2
ggplotly(p2)
 

p1/p2

