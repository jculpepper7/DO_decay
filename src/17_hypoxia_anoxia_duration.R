## The goal of this script is 1) to detect hypoxia in all time series and 
## 2) to extract the duration of each of those periods of hypoxia and
## 3) detect anoxia in all time series and
## 4) to exctract the duration of each of those periods of anoxia

# 1. Load libraries------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(janitor)
library(cropgrowdays)
library(ggpmisc)
library(ggpubr)
library(plotly)
library(cowplot)

# 2. Import data---------------------------------------------------------

#Castle lake aggregated data and isolate DO
cal_do <- read_csv(here('data/processed/castle/castle_clean_agg_data_daily.csv')) %>% 
  mutate(
    lake = 'castle'
  ) %>% 
  select(lake, date, do_mg_l) %>% 
  mutate(period =
    #determine when the lake was ice covered and open
    if_else( 
      #WY 2018
        date>=ymd('2017-12-04') & date<=ymd('2018-04-11') |
      #WY 2019
        date>=ymd('2018-12-24') & date<=ymd('2019-06-01') |
      #WY 2020
        date>=ymd('2019-12-20') & date<=ymd('2020-04-25') |
      #WY 2021
        date>=ymd('2020-12-20') & date<=ymd('2021-05-02') |
      #WY 2022
        date>=ymd('2021-12-15') & date<=ymd('2022-04-02'),
      as.factor('ice'),
      as.factor('open')
    )
  )

#Cedar lake aggregated data and isolate DO
cdr_do <- read_csv(here('data/processed/cedar/cedar_clean_agg_data_2022.csv'), guess_max = 100000) %>% 
  select(lake, pst, do_mg_l) %>% 
  na.omit() %>% 
  mutate(
    water_year = ifelse(month(pst) >= 10, year(pst)+1, year(pst)),
    day = day(pst),
    month = month(pst),
    year = year(pst)
  ) %>%
  select(-pst) %>% 
  group_by(year, month, day) %>%
  summarise(
    do_mg_l = mean(do_mg_l)
  ) %>% 
  ungroup() %>% 
  mutate(
    date = make_date(year = year, month = month, day = day),
    lake = c('cedar')
  ) %>% 
  select(lake, date, do_mg_l) %>% 
  mutate(period = 
    #determine when the lake was ice covered and open
    if_else(
      #WY 2020
        date>=ymd('2019-11-26') & date<=ymd('2020-04-12') |
      #WY 2021
        date>=ymd('2020-11-18') & date<=ymd('2021-04-17') |
      #WY 2022
        date>=ymd('2021-12-12') & date<=ymd('2022-04-02'),
      as.factor('ice'),
      as.factor('open')
    )
  ) 

#Cliff lake aggregated data and isolate DO
clf_do <- read_csv(here('data/processed/cliff/cliff_clean_agg_data_daily.csv')) %>% 
  mutate(
    lake = 'cliff'
  ) %>% 
  select(lake, date, do_mg_l)%>% 
  mutate(period = 
    #determine when the lake was ice covered and open
    if_else(
        #WY 2019
        date>=ymd('2018-12-04') & date<=ymd('2019-05-31') |
        #WY 2020
        date>=ymd('2019-12-09') & date<=ymd('2020-04-27') |
        #WY 2021
        date>=ymd('2020-12-01') & date<=ymd('2021-04-30') |
        #WY 2022
        date>=ymd('2021-12-14') & date<=ymd('2022-04-13'),
      as.factor('ice'),
      as.factor('open')
    )
  )

#Gumboot lake aggregated data and isolate DO
gb_do <- read_csv(here('data/processed/gumboot/gumboot_clean_agg_data_2022.csv'), guess_max = 100000) %>% 
  select(lake, pst, do_mg_l) %>% 
  na.omit() %>% 
  mutate(
    water_year = ifelse(month(pst) >= 10, year(pst)+1, year(pst)),
    day = day(pst),
    month = month(pst),
    year = year(pst)
  ) %>% 
  select(-pst) %>% 
  group_by(year, month, day) %>%
  summarise(
    do_mg_l = mean(do_mg_l)
  ) %>% 
  ungroup() %>% 
  mutate(
    date = make_date(year = year, month = month, day = day),
    lake = c('gumboot')
  ) %>% 
  select(lake, date, do_mg_l) %>% 
  mutate(period = 
    #determine when the lake was ice covered and open
    if_else(
        #WY 2020
        date>=ymd('2019-11-23') & date<=ymd('2020-05-04') |
        #WY 2021
        date>=ymd('2020-11-18') & date<=ymd('2021-05-02') |
        #WY 2022
        date>=ymd('2021-12-18') & date<=ymd('2022-04-07'),
      as.factor('ice'),
      as.factor('open')
    )
  ) 

#Soapstone Pond aggregated data and isolate DO
ss_do <- read_csv(here('data/processed/soapstone/soapstone_clean_agg_data_2022.csv'), guess_max = 50000) %>% 
  select(lake, pst, do_mg_l) %>% 
  na.omit() %>% 
  mutate(
    water_year = ifelse(month(pst) >= 10, year(pst)+1, year(pst)),
    day = day(pst),
    month = month(pst),
    year = year(pst)
  ) %>% 
  select(-pst) %>% 
  group_by(year, month, day) %>%
  summarise(
    do_mg_l = mean(do_mg_l)
  ) %>% 
  ungroup() %>% 
  mutate(
    date = make_date(year = year, month = month, day = day),
    lake = c('soapstone')
  ) %>% 
  select(lake, date, do_mg_l)  %>% 
  mutate(period = 
    #determine when the lake was ice covered and open
    if_else(
      #WY 2020
        date>=ymd('2019-11-21') & date<=ymd('2020-05-04') |
      #WY 2021
        date>=ymd('2020-11-06') & date<=ymd('2021-04-24') |
      #WY 2022
        date>=ymd('2021-12-11') & date<=ymd('2022-04-24'),
      as.factor('ice'),
      as.factor('open')
    )
  )    

#3. Join data------------------------------------------------------------

all_do <- bind_rows(cal_do, cdr_do, clf_do, gb_do, ss_do) %>% 
  mutate(
    water_year = if_else(month(date)>=10, year(date)+1, year(date)),
    year = year(date),
    hypoxic = if_else(
      do_mg_l <= 2, 
      do_mg_l,
      NA
    ),
    ice = if_else(
      period == 'ice',
      do_mg_l,
      NA
    ),
    open = if_else(
      period == 'open',
      do_mg_l,
      NA
    ),
    size = if_else(
      lake == 'cedar' | lake == 'gumboot' | lake == 'soapstone',
      as.factor('small'),
      as.factor('large')
    )
  )

write_csv(
  all_do, 
  here('data/processed/do_ts_all_lakes.csv')
)

#3. Isolate hypoxia periods

#Count the total number of days when oxygen was <=2 mg/L
hypox_large <- all_do %>%
  filter(size == 'large') %>% 
  group_by(lake, year, period) %>% 
  filter(do_mg_l <= 2) %>% # & do_mg_l > 1
  summarise(
    hypox_total = n()
  )

hypox_small <- all_do %>%
  filter(size == 'small') %>% 
  group_by(lake, water_year, period) %>% 
  filter(do_mg_l <= 2) %>% # & do_mg_l > 1
  summarise(
    hypox_total = n()
  )

hypox_total <- hypox_large %>% 
  bind_rows(hypox_small)
# 4. Hypoxia plot ---------------------------------------------------------

DO_ts_hypox_plt <- ggplot(all_do)+
  geom_line(aes(x = date, y = hypoxic, color = as.factor(year(date))), 
            alpha = 0.4, linewidth = 2)+
  geom_line(aes(x = date, y = open), linetype = 'dashed')+
  geom_line(aes(x = date, y = ice) )+
  geom_vline(xintercept = c(ymd('2018-01-01'), ymd('2019-01-01'), ymd('2020-01-01'), ymd('2021-01-01'), ymd('2022-01-01')))+
  geom_vline(xintercept = c(ymd('2017-10-01'), ymd('2018-10-01'), ymd('2019-10-01'), ymd('2020-10-01'), ymd('2021-10-01'), ymd('2022-10-01')), linetype = 'dashed')+
  geom_vline(xintercept = ymd('2017-12-11'), color = 'red')+
  facet_wrap(~lake, ncol = 1)+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    text = element_text(size = 25)
  )

DO_ts_hypox_plt
#ggplotly(DO_ts_hypox_plt)
#library(plotly)
# ggsave(here('output/lake_final_plts/supp_figs/supp_fig_DO_hypox.png'),
#        dpi = 300, height = 14, width = 15, units = 'in')

# TK. Anoxia and combined data --------------------------------------------



#Count the total number of days when oxygen was <1 mg/L
# anoxia_total <- all_do %>%
#   group_by(lake, water_year) %>% 
#   filter(do_mg_l <= 1) %>% 
#   summarise(
#     anoxia_total = n()
#   )

#Count the longest period of days when oxygen was <=2 mg/L
# hypox_duration <- all_do %>%
#   group_by(lake, water_year, period) %>% 
#   filter(do_mg_l <= 2) %>% 
#   mutate(
#     diff = date - lag(date)
#   ) %>% 
#   filter(diff == 1) %>% 
#   summarise(
#     hypox_duration = last(date) - first(date)
#   )  

#Count the longest period of days when oxygen was <1 mg/L
# anoxia_duration <- all_do %>%
#   group_by(lake, water_year) %>% 
#   filter(do_mg_l < 1) %>% 
#   mutate(
#     diff = date - lag(date)
#   ) %>% 
#   filter(diff == 1) %>% 
#   summarise(
#     anoxic_duration = last(date) - first(date)
#   )  

#Combine hypoxia and anoxia tibbles
# low_do <- hypox_total %>% 
#   full_join(hypox_duration)

#write_csv(low_do, here('output/hypoxia_results/hypoxia_anoxia_total.csv'))



#CAL yday starting from October 1

doy_func <- function(start, end){
  x <- cropgrowdays::day_of_year(start, type = 'other', base = list(d = 1, m = 10))
  y <- cropgrowdays::day_of_year(end, type = 'other', base = list(d = 1, m = 10))
  z <- end-start
  a <- lubridate::yday(start)
  b <- lubridate::yday(end)
  print(c(a,b,z))
  }

doy_func(start=ymd('2020-12-01'), end = ymd('2021-04-30'))



# 5. Summer v winter hypoxia - Deep lakes ---------------------------------

sum_v_win_hypox_plt <- ggplot(data = hypox_large)+
  geom_boxplot(aes(x = period, y = hypox_total))+
  geom_jitter(aes(x = period, y = hypox_total))+
  theme_classic()+
  xlab('Hypoxia Duration (days)')+
  ylab('Season')

sum_v_win_hypox_plt

# ggsave(here('output/lake_final_plts/supp_figs/supp_fig_DO_hypox_deep.png'),
#        dpi = 300, height = 14, width = 15, units = 'in')



# 6. winter hypox v ice duration ------------------------------------------

supp_tbl <- read_csv(
  here('data/met_data/lake_ice_phenology/ice_phenology_data3.csv')
) %>% 
  mutate(
    lake = as.factor(lake)
  )

#Reorder lakes by max depth
supp_tbl$lake <- fct_relevel(supp_tbl$lake, "castle", "cliff", "gumboot", 'cedar', 'soapstone')

hypox_ice_dur_plt <- ggplot(data = supp_tbl )+
  geom_point(
    aes(
      x = ice_dur_days, 
      y = winter_hypoxia_days,
      color = lake
    ),
    size = 5
  )+
  geom_smooth(
    aes(
      x = ice_dur_days, 
      y = winter_hypoxia_days
    ), 
    method = 'lm'
  )+
  theme_classic()+
  xlab('Ice Duation (days)')+
  ylab('Winter Hypoxia Duration (days)')+
  theme(
    text = element_text(size = 20),
    legend.title = element_blank(),
    legend.position = 'bottom'
  )+
  scale_color_viridis_d(
    labels = c('Castle', 'Cliff', 'Gumboot', 'Cedar', 'Soapstone')
  )

#+
  # stat_poly_eq(aes(x = ice_dur_days, y = winter_hypoxia_days), label.y = 0.98, label.x = 0.1)#+
  #stat_cor(label.y = 0.5, p.digits = 1)+
  # stat_fit_glance(aes(x = ice_dur_days, y = winter_hypoxia_days),
  #   #formula = formula,
  #   aes(label = paste("P = ", signif(..p.value.., digits = 2), sep = "")),
  #   label.y = 0.9, label.x = 1)

hypox_ice_dur_plt

lm <- lm(data = supp_tbl, winter_hypoxia_days~ice_dur_days)

summary(lm) #p = 0.01145, adj.R2 = 0.2962

# ggsave(here('output/lake_final_plts/supp_figs/supp_fig_wint_hypox_v_ice_dur.png'),
#        dpi = 300, height = 6.5, width = 6.5, units = 'in')


# 7. winter hypox v ice on ------------------------------------------

supp_tbl <- read_csv(here('data/met_data/lake_ice_phenology/ice_phenology_data3.csv')) 

hypox_ice_on_plt <- ggplot(data = supp_tbl)+
  geom_point(aes(x = ice_on_doy, y = winter_hypoxia_days))+
  geom_smooth(aes(x = ice_on_doy, y = winter_hypoxia_days), method = 'lm')+
  theme_classic()+
  xlab('Ice On (DOY)')+
  ylab('Winter Hypoxia Duration (days)')+
  theme(
    text = element_text(size = 20)
  )

hypox_ice_on_plt

# ggsave(here('output/lake_final_plts/supp_figs/supp_fig_wint_hypox_v_ice_on.png'),
#        dpi = 300, height = 5, width = 6, units = 'in')


# 8. summer hypox v ice off ------------------------------------------

supp_tbl <- read_csv(here('data/met_data/lake_ice_phenology/ice_phenology_data3.csv')) 

hypox_ice_off_plt <- ggplot(data = supp_tbl %>% filter(lake == 'castle' | lake == 'cliff'))+
  geom_point(aes(x = ice_off_doy, y = summer_hypoxia_days))+
  geom_smooth(aes(x = ice_off_doy, y = summer_hypoxia_days), method = 'lm')+
  theme_classic()+
  xlab('Ice Off (DOY)')+
  ylab('Summer Hypoxia Duration (days)')+
  theme(
    text = element_text(size = 20)
  )

hypox_ice_off_plt

# ggsave(here('output/lake_final_plts/supp_figs/supp_fig_sum_hypox_v_ice_off.png'),
#        dpi = 300, height = 5, width = 6, units = 'in')


# 9. Hypoxia duration plt ------------------------------------------------

supp_tbl$lake <- factor(supp_tbl$lake, 
                          levels = c('castle', 'cliff', 'gumboot', 'cedar', 'soapstone'))

#**9a. Winter hypoxia plt----

w_hypoxia_plt <- ggplot(data = supp_tbl)+
  geom_line(aes(x = water_year, y = winter_hypoxia_days, linetype = lake))+
  geom_point(aes(x = water_year, y = winter_hypoxia_days, shape = lake), size = 3)+
  theme_classic()+
  scale_shape_discrete(labels = c('Castle', 'Cliff', 'Gumboot', 'Cedar', 'Soapstone'))+
  scale_linetype_discrete(labels = c('Castle', 'Cliff', 'Gumboot', 'Cedar', 'Soapstone'))+
  labs(x = '', y = 'Winter Hypoxia (days)')+
  theme(
    text = element_text(size = 25),
    legend.position = c(0.15, 0.85),
    legend.title = element_blank(),
    #axis.title.y = element_text(margin = unit(c(0,5,0,0), 'mm'))
  )
w_hypoxia_plt

# ggsave(here('output/lake_final_plts/w_hypox_plt_2024.12.06.pdf'),
#        dpi = 300, height = 5, width = 8, units = 'in')

#**9b. Summer hypoxia plt----

s_hypoxia_plt <- ggplot(data = supp_tbl %>% filter(lake == 'castle' | lake == 'cliff'))+
  geom_line(aes(x = water_year, y = summer_hypoxia_days, linetype = lake))+
  geom_point(aes(x = water_year, y = summer_hypoxia_days, shape = lake), size = 3)+
  theme_classic()+
  scale_shape_discrete(labels = c('Castle', 'Cliff'))+
  scale_linetype_discrete(labels = c('Castle', 'Cliff'))+
  labs(x = '', y = 'Summer Hypoxia (days)')+
  theme(
    text = element_text(size = 25),
    legend.position = 'none',
    legend.title = element_blank(),
    #axis.title.y = element_text(margin = unit(c(0,5,0,0), 'mm'))
  )
s_hypoxia_plt

# ggsave(here('output/lake_final_plts/s_hypox_plt_2024.12.06.pdf'),
#        dpi = 300, height = 5, width = 8, units = 'in')
# 

# Peak DO calc ------------------------------------------------------------


peak_do_ice <- all_do %>% 
  filter(period == 'ice') %>% 
  group_by(
    lake, 
    water_year
  ) %>% 
  summarise(
    peak_do = max(do_mg_l, na.rm = T)
  )

peak_do_open <- all_do %>% 
  filter(period == 'open',
         month(date) == 4 | month(date) == 5 | month(date) == 6
        ) %>% 
  group_by(
    lake, 
    year
  ) %>% 
  summarise(
    peak_do = max(do_mg_l, na.rm = T)
  )


# Peak DO plts -------------------------------------------------------------

#Summer plot
supp_tbl <- read_csv(here('data/met_data/lake_ice_phenology/ice_phenology_data3.csv')) %>% 
  mutate(
    lake = as.factor(lake)
  )

summer_do_v_hyopx <- ggplot(data = supp_tbl %>% filter(lake == 'castle' | lake == 'cliff'))+
  geom_smooth(
    aes(
      x = spring_peak_do, 
      y = summer_hypoxia_days
    ), 
    method = 'lm'
  )+
  geom_point(
    aes(
      x = spring_peak_do, 
      y = summer_hypoxia_days, 
      color = lake
    ),
    size = 5
  )+
  theme_classic()+
  xlab('Peak Spring DO (mg/L)')+
  ylab('Summer Hypoxia Duration (days)')+
  theme(
    text = element_text(size = 20),
    legend.title = element_blank(),
    legend.position = 'bottom'
  )+
  scale_color_viridis_d(
    begin = 0.2, 
    end = 0.7,
    labels = c('Castle', 'Cliff')
  )

summer_do_v_hyopx

lm_summer <- lm(supp_tbl$summer_hypoxia_days~supp_tbl$spring_peak_do) 

summary(lm_summer)

# ggsave(here('output/lake_final_plts/supp_figs/supp_fig_sum_hypox_v_spring_do.png'),
#        dpi = 300, height = 6.5, width = 6.5, units = 'in')

#Winter plot

supp_tbl <- read_csv(here('data/met_data/lake_ice_phenology/ice_phenology_data3.csv')) 

wint_do_v_hyopx <- ggplot(data = supp_tbl)+
  geom_point(aes(x = winter_peak_do, y = winter_hypoxia_days))+
  geom_smooth(aes(x = winter_peak_do, y = winter_hypoxia_days), method = 'lm')+
  theme_classic()+
  xlab('Peak Winter DO (mg/L)')+
  ylab('Winter Hypoxia Duration (days)')+
  theme(
    text = element_text(size = 20)
  )

wint_do_v_hyopx

# ggsave(here('output/lake_final_plts/supp_figs/supp_fig_wint_hypox_v_wint_do.png'),
#        dpi = 300, height = 5, width = 6, units = 'in')
