# The goal of this script is to combine the cleaned and aggregated 
# time series of DO for each lake


# 1. Libraries ------------------------------------------------------------

library(tidyverse)
library(here)
library(plotly)


# 2. Read data ------------------------------------------------------------

castle <- read_csv(here('data/processed/cal_hypox_plt.csv'))
cedar <- read_csv(here('data/processed/cedar_hypox_plt.csv'))
#cliff <- read_csv(here('data/processed/cliff/cliff_clean_agg_data_daily_w_temp.csv'))
cliff <- read_csv(here('data/processed/cliff/clf_hypox_plt.csv'))
gumboot <- read_csv(here('data/processed/gb_hypox_plt.csv'))
soapstone <- read_csv(here('data/processed/ss_hypox_plt.csv'))


# 3. Data viz -------------------------------------------------------------

# **3a. Castle Lake -------------------------------------------------------

castle_avg_plt <- ggplot()+ 
  geom_rect(aes(
    xmin = ymd('2017-12-11'),
    xmax = ymd('2018-04-11'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'grey', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2018-12-24'),
    xmax = ymd('2019-06-01'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2019-12-20'),
    xmax = ymd('2020-04-25'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2020-12-20'),
    xmax = ymd('2021-05-21'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2021-12-15'),
    xmax = ymd('2022-04-02'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_line(data = castle %>% filter(depth == '30m', date>='2017-10-01'), 
            aes(x = date, y = temp_c), 
            color = 'grey35', size = 0.5, alpha = 0.5)+ 
  geom_line(data = castle %>% filter(depth == '20m', date>='2017-10-01'), 
            aes(x = date, y = temp_c), 
            color = 'grey55', size = 0.5, alpha = 0.5)+ 
  geom_line(data = castle %>% filter(depth == '10m', date>='2017-10-01'), 
            aes(x = date, y = temp_c), 
            color = 'grey65', size = 0.5, alpha = 0.5)+ 
  geom_line(data = castle %>% filter(depth == '03m', date>='2017-10-01'), 
            aes(x = date, y = temp_c), 
            color = 'grey75', size = 0.5, alpha = 0.5)+ 
  geom_line(
    data = castle %>% filter(depth =='30m', date>='2017-10-01'), 
    aes(x = date, y = do_mg_l), 
    size = 1.2
  )+
  scale_color_grey(name = 'Depth   ')+
  theme_classic()+
  xlab('')+ 
  ylab('')+
  scale_y_continuous(breaks = seq(0,24,6), limits = c(0,24))+
  theme(legend.position = 'none',
        legend.title = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
  )

castle_avg_plt

ggplotly(castle_avg_plt)

ggsave(
  here('output/lake_final_plts/do_ts_fig/castle_do_plt_w_temp.pdf'), 
  dpi = 300,
  width = 6.5,
  height = 2,
  units = 'in'
)


# **3b. Cedar Lake --------------------------------------------------------


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
  geom_line(data = cedar %>% filter(depth == '1m'), 
            aes(x = date, y = temp_c), 
            size = 0.5, color = 'grey50', alpha = 0.5)+
  #geom_line(data = cedar_w_avg %>% filter(depth == 'sediment'), aes(x = date, y = temp_c), size = 0.5, alpha = 0.8, color = 'light gray', linetype = 'dashed')+
  geom_line(
    data = cedar %>% filter(depth == '1m'), 
    aes(x = date, y = do_mg_l), 
    size = 1.2
  )+
  scale_color_grey(name = 'Depth   ')+
  theme_classic()+
  labs(x = '', y = '')+
  scale_y_continuous(breaks = seq(0,24,6), limits = c(0,26))+
  theme(legend.position = 'none',
        legend.title = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
  )+
  xlim(ymd('2017-10-01'), ymd('2022-06-16'))
cedar_avg_plt

ggsave(
  here('output/lake_final_plts/do_ts_fig/cdr_do_plt_w_temp.pdf'), 
  dpi = 300,
  width = 6.5,
  height = 2,
  units = 'in'
)

# **3c. Cliff Lake ------------------------------------------------------------

cliff_avg_plt <- ggplot()+ 
  geom_rect(aes(
    xmin = ymd('2018-12-04'),
    xmax = ymd('2019-05-31'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2019-12-09'),
    xmax = ymd('2020-05-01'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2020-12-01'),
    xmax = ymd('2021-04-11'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_rect(aes(
    xmin = ymd('2021-12-14'),
    xmax = ymd('2022-04-13'),
    ymin = -Inf,
    ymax = Inf
  ), fill = 'light blue', alpha = 0.5)+
  geom_line(data = cliff %>% filter(depth == 'temp_24'), 
            aes(x = date, y = temp_c), 
            color = 'grey25', size = 0.5, alpha = 0.5)+
  geom_line(data = cliff %>% filter(depth == 'temp_23'), 
            aes(x = date, y = temp_c), 
            color = 'grey35', size = 0.5, alpha = 0.5)+
  geom_line(data = cliff %>% filter(depth == 'temp_18'), 
            aes(x = date, y = temp_c), 
            color = 'grey45', size = 0.5, alpha = 0.5)+
  geom_line(data = cliff %>% filter(depth == 'temp_13'), 
            aes(x = date, y = temp_c), 
            color = 'grey55', size = 0.5, alpha = 0.5)+
  geom_line(data = cliff %>% filter(depth == 'temp_08'), 
            aes(x = date, y = temp_c), 
            color = 'grey65', size = 0.5, alpha = 0.5)+
  geom_line(data = cliff %>% filter(depth == 'temp_05'), 
            aes(x = date, y = temp_c), 
            color = 'grey75', size = 0.5, alpha = 0.5)+
  geom_line(data = cliff %>% filter(depth == 'temp_04'), 
            aes(x = date, y = temp_c), 
            color = 'grey85', size = 0.5, alpha = 0.5)+
  geom_line(
    data = cliff, 
    aes(x = date, y = do_mg_l), 
    size = 1.2
  )+
  scale_color_grey(name = 'Depth   ')+
  theme_classic()+
  xlab('')+ 
  ylab('')+
  theme(legend.position = 'none',
        legend.title = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
  )+
  xlim(ymd('2017-10-01'), ymd('2022-06-16'))+
  scale_y_continuous(breaks = seq(0,24,6), limits = c(0,24.5))
cliff_avg_plt  

ggsave(
  here('output/lake_final_plts/do_ts_fig/cliff_do_plt_w_temp.pdf'), 
  dpi = 300,
  width = 6.5,
  height = 2,
  units = 'in'
)


# **3d. Gumboot Lake ------------------------------------------------------

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
  geom_line(data = gumboot %>% filter(depth == '1m'), 
            aes(x = date, y = temp_c), 
            color = 'grey50', size = 0.5, alpha = 0.5)+
  #geom_line(data = gumboot %>% filter(depth == 'sediment'), aes(x = date, y = temp_c), size = 0.8, alpha = 0.5, color = 'light gray', linetype = 'dashed')+
  geom_line(
    data = gumboot %>% filter(depth == '1m'), 
    aes(x = date, y = do_mg_l), 
    size = 1.2
  )+
  theme_classic()+
  labs(x = '', y = '')+ 
  theme(legend.position = 'none',
        legend.title = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
  )+
  xlim(ymd('2017-10-01'), ymd('2022-06-16'))+
  scale_y_continuous(breaks = seq(0,24,6), limits = c(0,25))
gumboot_avg_plt

ggsave(
  here('output/lake_final_plts/do_ts_fig/gb_do_plt_w_temp.pdf'), 
  dpi = 300,
  width = 6.5,
  height = 2,
  units = 'in'
)


# **3e. Soapstone Pond ----------------------------------------------------

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
  geom_line(data = soapstone %>% filter(depth == '1m'), 
            aes(x = date, y = temp_c), 
            color = 'grey50', size = 0.5, alpha = 0.5)+
  #geom_line(data = soapstone %>% filter(depth == 'sediment'), aes(x = date, y = temp_c), size = 0.8, alpha = 0.5, color = 'light gray', linetype = 'dashed')+
  geom_line(data = soapstone %>% filter(depth == '1m'), aes(x = date, y = do_mg_l), size = 1.2)+
  scale_color_grey(name = 'Depth   ')+
  theme_classic()+
  labs(x = '', y = '')+
  theme(legend.position = 'none',
        legend.title = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 15)
  )+
  xlim(ymd('2017-10-01'), ymd('2022-06-16'))+
  scale_y_continuous(breaks = seq(0,24,6), limits = c(0,26))
soapstone_avg_plt


ggsave(
  here('output/lake_final_plts/do_ts_fig/soapstone_do_plt_w_temp.pdf'), 
  dpi = 300,
  width = 6.5,
  height = 2,
  units = 'in'
)

# 4. Combine figure -------------------------------------------------------

#Patchwork for fig for paper

#NOTE: You can use the below code to make a combined figure
#      However, I put the individual plots into Inkscape (inkscape.org)
#      for the final figure that appears in the paper.

# castle_avg_plt/ cliff_avg_plt / gumboot_avg_plt/ cedar_avg_plt /soapstone_avg_plt+
#   plot_annotation(tag_levels = 'A') #&
  #theme(plot.tag = element_text(size = 22))

#ggsave(here('output/lake_final_plts/all_lakes_combined_2024.11.19_alt_dim.jpeg'), dpi = 300, height = 5, width = 6, units = 'in')  

