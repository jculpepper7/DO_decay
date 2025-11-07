## The goal of this script is to 
## 1. load Cliff data
## 2. do any essential cleaning
## 3. Aggregate data
## 4. Identify do decay periods


# 1. Load libraries -------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(plotly)


# 2. Import Cliff data  ---------------------------------------------------

#NOTE: All Cliff Lake data collected by Dr. Adrianne Smits, UC Davis, ORCID: https://orcid.org/0000-0001-9967-5419

#**2a. Cliff 2019----
cliff_2019_rdata <- load(here('data/raw/cliff/Cliff_2019_winter_data.Rdata'))

cliff_2019 <- tibble(output.list[[1]][['datetime']],
                     output.list[[1]][['temp_3.91']],
                     output.list[[1]][['temp_4.73']],
                     output.list[[1]][['temp_8.53']],
                     output.list[[1]][['temp_13.55']],
                     output.list[[1]][['temp_18.53']],
                     output.list[[1]][['temp_23.52']],
                     output.list[[1]][['temp_24.5']],
                     output.list[[1]][['sediment']],
                     output.list[[1]][['DO_23.52']],
                     output.list[[1]][['light_3.91']],
                     output.list[[1]][['cond_24.5']],
                     output.list[[1]][['level_4.73']]
)

cliff_2019_raw <- cliff_2019 %>% 
  select(
    date = 1,
    temp_03.91 = 2, #Column names will be rounded to the nearest meter
    temp_04.73 = 3,
    temp_08.53 = 4,
    temp_13.55 = 5,
    temp_18.53 = 6,
    temp_23.52 = 7,
    temp_24.5 = 8,
    temp_sediment = 9,
    do_mg_l = 10,
    light_3.91 = 11,
    cond_24.5 = 12,
    level_4.73 = 13
  ) %>% 
  mutate(
    water_year = if_else(month(date)>=10, year(date)+1, year(date))
  ) %>% 
  group_by(year(date), month(date), day(date)) %>% 
  summarise(
    temp_04 = mean(temp_03.91),
    temp_05 = mean(temp_04.73),
    temp_08 = mean(temp_08.53),
    temp_13 = mean(temp_13.55),
    temp_18 = mean(temp_18.53),
    temp_23 = mean(temp_23.52),
    temp_24 = mean(temp_24.5),
    temp_sediment = mean(temp_sediment),
    do_mg_l = mean(do_mg_l),
    light_04 = mean(light_3.91),
    cond_24 = mean(cond_24.5),
    level_05 = mean(level_4.73)
  ) %>% 
  ungroup() %>% 
  rename(
    year = 1,
    month = 2,
    day = 3
  ) %>% 
  mutate(
    date = make_date(year = year, month = month, day = day),
    lake = as_factor(c('cliff'))
  ) %>% 
  select(-year, -month, -day) %>% 
  select(14, 13, 1:12)

#write_csv(cliff_2019_raw, here('data/raw/cliff/cliff_2019_winter_raw.csv'))


#**2b. Cliff 2020----
cliff_2020_rdata <- load(here('data/raw/cliff/Cliff_2020_winter_data.Rdata'))

cliff_2020 <- tibble(output.list[[1]][['datetime']],
                     output.list[[1]][['temp_3.75']],
                     output.list[[1]][['temp_4.61']],
                     output.list[[1]][['temp_8.41']],
                     output.list[[1]][['temp_13.41']],
                     output.list[[1]][['temp_18.37']],
                     output.list[[1]][['temp_23.33']],
                     output.list[[1]][['temp_24.3']],
                     output.list[[1]][['sediment']],
                     output.list[[1]][['DO_23.33']],
                     output.list[[1]][['Sat_23.33']],
                     output.list[[1]][['light_3.75']],
                     output.list[[1]][['cond_24.3']],
                     output.list[[1]][['level_4.61']]
                     )

cliff_2020_raw <- cliff_2020 %>% 
  select(
    date = 1,
    temp_04 = 2,
    temp_05 = 3,
    temp_08 = 4,
    temp_13 = 5,
    temp_18 = 6,
    temp_23 = 7,
    temp_24 = 8,
    temp_sediment = 9,
    do_mg_l = 10,
    do_sat = 11,
    light_04 = 12,
    cond_24 = 13,
    level_05 = 14
  ) %>% 
  mutate(
    water_year = if_else(month(date)>=10, year(date)+1, year(date))
  ) %>% 
  group_by(year(date), month(date), day(date)) %>% 
  summarise(
    temp_04 = mean(temp_04),
    temp_05 = mean(temp_05),
    temp_08 = mean(temp_08),
    temp_13 = mean(temp_13),
    temp_18 = mean(temp_18),
    temp_23 = mean(temp_23),
    temp_24 = mean(temp_24),
    temp_sediment = mean(temp_sediment),
    do_mg_l = mean(do_mg_l),
    do_sat = mean(do_sat),
    light_04 = mean(light_04),
    cond_24 = mean(cond_24),
    level_05 = mean(level_05)
  ) %>% 
  ungroup() %>% 
  rename(
    year = 1,
    month = 2,
    day = 3
  ) %>% 
  mutate(
    date = make_date(year = year, month = month, day = day),
    lake = as_factor(c('cliff'))
  ) %>% 
  select(-year, -month, -day) %>% 
  select(15, 14, 1:13)

#write_csv(cliff_2020_raw, here('data/raw/cliff/cliff_2020_winter_raw.csv'))

#**2c. Cliff 2021----
cliff_2021_rdata <- load(here('data/raw/cliff/Cliff_2021_winter_data.Rdata'))

cliff_2021 <- tibble(output.list[[1]][['datetime']],
                     output.list[[1]][['temp_3.76']],
                     output.list[[1]][['temp_4.63']],
                     output.list[[1]][['temp_6.07']],
                     output.list[[1]][['temp_8.45']],
                     output.list[[1]][['temp_13.44']],
                     output.list[[1]][['temp_18.38']],
                     output.list[[1]][['temp_23.33']],
                     output.list[[1]][['temp_24.32']],
                     output.list[[1]][['sediment']],
                     output.list[[1]][['DO_23.33']],
                     output.list[[1]][['Sat_23.33']],
                     output.list[[1]][['light_3.76']],
                     output.list[[1]][['cond_24.32']],
                     output.list[[1]][['level_4.63']]
)

cliff_2021_raw <- cliff_2021 %>% 
  rename(
    date = 1,
    temp_04 = 2,
    temp_05 = 3,
    temp_06 = 4,
    temp_08 = 5,
    temp_13 = 6,
    temp_18 = 7,
    temp_23 = 8,
    temp_24 = 9,
    temp_sediment = 10,
    do_mg_l = 11,
    do_sat = 12,
    light_04 = 13,
    cond_24 = 14,
    level_05 = 15
  ) %>% 
  mutate(
    water_year = if_else(month(date)>=10, year(date)+1, year(date))
  ) %>% 
  group_by(year(date), month(date), day(date)) %>% 
  summarise(
    temp_04 = mean(temp_04),
    temp_05 = mean(temp_05),
    temp_06 = mean(temp_06),
    temp_08 = mean(temp_08),
    temp_13 = mean(temp_13),
    temp_18 = mean(temp_18),
    temp_23 = mean(temp_23),
    temp_24 = mean(temp_24),
    temp_sediment = mean(temp_sediment),
    do_mg_l = mean(do_mg_l),
    do_sat = mean(do_sat),
    light_04 = mean(light_04),
    cond_24 = mean(cond_24),
    level_05 = mean(level_05)
  ) %>% 
  ungroup() %>% 
  rename(
    year = 1,
    month = 2,
    day = 3
  ) %>% 
  mutate(
    date = make_date(year = year, month = month, day = day),
    lake = as_factor(c('cliff'))
  ) %>% 
  select(-year, -month, -day) %>% 
  select(16, 15, 1:14)

#write_csv(cliff_2021_raw, here('data/raw/cliff/cliff_2021_winter_raw.csv'))

#**2d. Cliff 2022----
cliff_2022_rdata <- load(here('data/raw/cliff/Cliff_2022_winter_data.Rdata'))

cliff_2022 <- tibble(output.list[[1]][['datetime']],
                     output.list[[1]][['temp_2.67']],
                     output.list[[1]][['temp_3.61']],
                     output.list[[1]][['temp_5.05']],
                     output.list[[1]][['temp_7.42']],
                     output.list[[1]][['temp_12.42']],
                     output.list[[1]][['temp_17.36']],
                     output.list[[1]][['temp_22.31']],
                     output.list[[1]][['temp_23.42']],
                     output.list[[1]][['sediment']],
                     output.list[[1]][['DO_22.31']],
                     output.list[[1]][['light_2.67']],
                     output.list[[1]][['cond_23.42']],
                     output.list[[1]][['level_3.61']]
)

cliff_2022_raw <- cliff_2022 %>% 
  rename(
    date = 1,
    temp_04 = 2,
    temp_05 = 3,
    temp_06 = 4,
    temp_08 = 5,
    temp_13 = 6,
    temp_18 = 7,
    temp_23 = 8,
    temp_24 = 9,
    temp_sediment = 10,
    do_mg_l = 11,
    light_04 = 12,
    cond_24 = 13,
    level_05 = 14
  ) %>% 
  mutate(
    water_year = if_else(month(date)>=10, year(date)+1, year(date))
  ) %>% 
  group_by(year(date), month(date), day(date)) %>% 
  summarise(
    temp_04 = mean(temp_04),
    temp_05 = mean(temp_05),
    temp_06 = mean(temp_06),
    temp_08 = mean(temp_08),
    temp_13 = mean(temp_13),
    temp_18 = mean(temp_18),
    temp_23 = mean(temp_23),
    temp_24 = mean(temp_24),
    temp_sediment = mean(temp_sediment),
    do_mg_l = mean(do_mg_l),
    light_04 = mean(light_04),
    cond_24 = mean(cond_24),
    level_05 = mean(level_05)
  ) %>% 
  ungroup() %>% 
  rename(
    year = 1,
    month = 2,
    day = 3
  ) %>% 
  mutate(
    date = make_date(year = year, month = month, day = day),
    lake = as_factor(c('cliff'))
  ) %>% 
  select(-year, -month, -day) %>% 
  select(15, 14, 1:13)

#write_csv(cliff_2022_raw, here('data/raw/cliff/cliff_2022_winter_raw.csv'))

# 3. Visualize cliff data--------------------------------------------------

cliff_2019_mean <- read_csv(here('data/raw/cliff/cliff_2019_winter_raw.csv'))
cliff_2020_mean <- read_csv(here('data/raw/cliff/cliff_2020_winter_raw.csv'))
cliff_2021_mean <- read_csv(here('data/raw/cliff/cliff_2021_winter_raw.csv'))
cliff_2022_mean <- read_csv(here('data/raw/cliff/cliff_2022_winter_raw.csv'))


clf_all <- bind_rows(cliff_2019_mean, cliff_2020_mean, cliff_2021_mean, cliff_2022_mean)

#write_csv(clf_all, here('data/processed/cliff/cliff_clean_agg_data_daily.csv'))

#Cliff 2019 plot
cliff_plt_19 <- ggplot(data = cliff_2019_mean, aes(x = date, y = do_mg_l))+
  geom_line(size = 1.5)+
  theme_classic()+
  xlab('date')+
  ylab('dissolved oxygen')
cliff_plt_19

ggplotly(cliff_plt_19)

#Cliff 2020 plot
cliff_plt_20 <- ggplot(data = cliff_2020_mean, aes(x = date, y = do_mg_l))+
  geom_line(size = 1.5)+
  theme_classic()+
  xlab('date')+
  ylab('dissolved oxygen')
cliff_plt_20

ggplotly(cliff_plt_20)

#Cliff 2021 plot
cliff_plt_21 <- ggplot(data = cliff_2021_mean, aes(x = date, y = do_mg_l))+
  geom_line(size = 1.5)+
  theme_classic()+
  xlab('date')+
  ylab('dissolved oxygen')
cliff_plt_21

#ggplotly(cliff_plt_21)

#Cliff 2022 plot
cliff_plt_22 <- ggplot(data = cliff_2022_mean, aes(x = date, y = do_mg_l))+
  geom_line(size = 1.5)+
  theme_classic()+
  xlab('date')+
  ylab('dissolved oxygen')
cliff_plt_22

ggplotly(cliff_plt_22)

# 4. Ice on and ice off dates-----------------------------------------------
#NOTE: Derived from DO time series and game camera images.
#NOTE: Game camera images are not available in  winters 2019 or 2022.
#      Used a combination of DO time series and Sentinel-2 L2A images.

#2019
#ice on: 12-04-2018 #Ice on uncertain, but between 12/1 and 12/6.
#anoxia: 04-13-2019 #First point when DO was <1 mg/L for a sustained period.
#ice off: 05-31-2019 #Ice off uncertain between 05/28 and 06/04. Signs of thin ice on 05/28. Complete ice off certain on 06/04.

#2020
#ice on: 12-09-2019
#anoxia: 03-25-2020
#ice off: 04-27-2020

#2021
#ice on: 12-01-2020 
#anoxia: 03-19-2021
#ice off: 04-30-2021

#NOTE: Early signs of ice on as of 11-30-2021

#2022
#ice on: 12-14-2021 #Ice on uncertain between 12/10 and 12/18. Definitely ice covered on 12/18. Apparently not covered on 12/10. DO indicates probably 12/14.
#anoxia: 03-31-2022 #Never goes below 1 mg/L. Lowest point 3/31 with 1.2 mg/L
#ice off: 04-13-2022 #Signs of breakup start on 04/07. About 50% on 04/09. Break up end definite by 04/17.

# 5. Visualizations of Cliff lake------------------------------------

cliff <- cliff_2019_mean %>% 
  bind_rows(cliff_2020_mean, cliff_2021_mean, cliff_2022_mean) %>% 
  select(1:9, 11) %>%
  pivot_longer(cols = 3:9, names_to = 'depth', values_to = 'temp_c') %>%
  mutate(
    lake = as.factor(lake),
    depth = as.factor(sub('temp_', '', depth))
  )
write_csv(
  cliff,
  here('data/processed/cliff/cliff_clean_update_2025.11.06')
)
  #Below for DO timeseries figure
  # select(
  #   lake, date, do_mg_l, 
  #   temp_c = temp_24
  # )

#write_csv(cliff, here('data/processed/cliff/cliff_clean_agg_data_daily.csv'))
#write_csv(cliff, here('data/processed/cliff/clf_hypox_plt.csv'))
#Save for script 21 - DO time series fig
#write_csv(cliff, here('data/processed/cliff/cliff_clean_agg_data_daily_w_temp.csv'))

#Temperature plot

cliff <- read_csv(
  here('data/processed/cliff/cliff_clean_update_2025.11.06')
) %>% 
  mutate(
    lake = as.factor(lake),
    depth = as.factor(depth)
  )

cliff_temp_plt <- ggplot(data = cliff)+
  geom_line(aes(x = date, y = temp_c, color = depth))+
  geom_line(aes(x = date, y = do_mg_l))+
  theme_classic()+
  xlab("Date")+
  ylab("DO [mg/L] & Temperature [C]")
  #facet_wrap(~water_year)
cliff_temp_plt  

ggplotly(cliff_temp_plt)

# 6. Final plot for Cliff---------------------------------------------

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
  geom_line(
    data = cliff, 
    aes(x = date, y = temp_c/2, color = depth), 
    linewidth = 1, 
    alpha = 0.7
  )+
  geom_line(
    data = cliff, 
    aes(x = date, y = do_mg_l), 
    linewidth = 1
  )+
  # scale_color_grey(name = 'Depth   ')+
  theme_classic()+
  xlab('')+ 
  ylab('')+
  theme(legend.position = 'bottom',
        # legend.title = element_text(size = 14),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_blank()
  )+
  scale_color_viridis_d()+
  xlim(ymd('2017-10-01'), ymd('2022-06-16'))+
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
cliff_avg_plt
#ggplotly(cliff_avg_plt)

#ggsave(here('output/lake_final_plts/cliff_do_plt_w_temp.jpeg'), dpi = 300)
ggsave(
  # here('output/lake_final_plts/castle_do_plt_w_temp_update_pdf.png'), 
  # here('output/lake_final_plts/do_ts_fig/update/castle_do_plt_w_temp_update.pdf'), 
  here('output/lake_final_plts/do_ts_fig/update/cliff_do_plt_w_temp_update_NAs.pdf'), 
  dpi = 300,
  width = 6.5,
  height = 3.75
)

# 7. Write Cliff data to CSV ----------------------------------------------

# cliff_write <- cliff_2019_mean %>% 
#   bind_rows(cliff_2020_mean, cliff_2021_mean, cliff_2022_mean) %>% 
#   select(lake, date, do_mg_l)

#write_csv(cliff_write, here('data/processed/cliff/cliff_clean_agg_data_daily.csv'))

