## The purpose of this script is to:
## 1) Import and clean water chemistry, isotope, and sediment data
## 2) Visualize water chemistry, isotope, and sediment data
##
# Load libraries----------------------------------------------------------------

library(tidyverse)
library(plotly)
library(here)
library(stringi)
library(janitor)
library(lubridate)
library(viridis)

# 1. Import data----------------------------------------------------------------

#1a. import ash free dry mass---------------------------------------------------

ash_free_dry_mass <- read_csv(here('data/processed/water_chemistry/ash_free_dry_mass.csv')) %>%
  rename(lakename = sample_name) %>%
  #change lakenames from abbreviations to full names
  mutate(
    lakename = stri_replace_all_regex(
      lakename,
      pattern = c('CAL', 'CDR', 'CLF', 'GB', 'SS'),
      replacement = c('castle', 'cedar', 'cliff', 'gumboot', 'soapstone'),
      vectorize = FALSE
    ),
    date = ymd(date)
  ) %>%
  arrange(lakename, date)

#take a look
str(ash_free_dry_mass)

#Summarise ash free dry mass
afdm_summ <- ash_free_dry_mass %>% 
  group_by(lakename) %>% 
  summarise(
    afdm_mean = mean(ash_free_dry_mass),
    afdm_med = median(ash_free_dry_mass)
  )

#1b. import water chemistry-----------------------------------------------------

water_chem <- read_csv(here('data/processed/water_chemistry/cdr_gb_ss_water_chem.csv')) %>%
  clean_names() %>%
  select(
    lakename = site, 
    date = sample_date,
    time = sample_time,
    depth_m = depth,
    no3_no2_ppb,
    srp_ppb,
    tp_ppb,
    nh4_ppb
    ) %>%
  mutate(
    lakename = stri_replace_all_regex(
      lakename,
      pattern = c('CAL', 'CDR', 'CLF', 'GB', 'SS'),
      replacement = c('castle', 'cedar', 'cliff', 'gumboot', 'soapstone'),
      vectorize = FALSE
    ),
    depth_m = stri_replace_all_regex(
      depth_m,
      pattern = '3m',
      replacement = 'sed',
      vectorize = FALSE
    )
  ) %>%
  slice(-c(41:66)) %>%
  mutate(
    date = ymd(date),
    time = hms(time),
    date_time = date + time
  ) %>%
  #select(-date, -time) %>%
  pivot_longer(
    cols = c(5:8),
    names_to = 'species',
    values_to = 'ppb'
  ) %>%
  arrange(depth_m, date_time)

#take a look
str(water_chem)

#1c. import sediment C:N ratio--------------------------------------------------

sediment_CN <- read_csv(here('data/processed/water_chemistry/sediment_CN_ratio_2021.csv')) %>%
  mutate(
    lakename = stri_replace_all_regex(
      lakename,
      pattern = c('CAL', 'CDR', 'CLF', 'GB', 'SS'),
      replacement = c('castle', 'cedar', 'cliff', 'gumboot', 'soapstone'),
      vectorize = FALSE
    ),
    date = ymd(date),
    CN_ratio = C_mcgC_g / N_mcgN_g
  ) %>%
  na.omit()

#take a look
str(sediment_CN)

#1d. import water column C:N ratio----------------------------------------------

#where values are NA, the value was below the detectable limit.

water_CN <- read_csv(here('data/processed/water_chemistry/water_CN_ratio_2021.csv')) %>%
  mutate(
    lakename = stri_replace_all_regex(
      lakename,
      pattern = c('CAL', 'CDR', 'CLF', 'GB', 'SS'),
      replacement = c('castle', 'cedar', 'cliff', 'gumboot', 'soapstone'),
      vectorize = FALSE
    ),
    date = ymd(date),
    CN_ratio = C_ug_L / N_ug_L,
    depth = stri_replace_all_regex(
      depth,
      pattern = '3m',
      replacement = 'sed',
      vectorize = FALSE
  )
)
#take a look
str(water_CN)

#1e. import and merge O2 isotope data-------------------------------------------

#Isotopes from 2021.06.13
d18_isotope_1 <- read_csv(here('data/processed/water_chemistry/O2_d18O_2021.06.13.csv')) %>%
  mutate(
    lakename = stri_replace_all_regex(
      lakename,
      pattern = c('CAL', 'CDR', 'CLF', 'GB', 'SS'),
      replacement = c('castle', 'cedar', 'cliff', 'gumboot', 'soapstone'),
      vectorize = FALSE
    ),
    date = ymd(date)
  )

#take a look
str(d18_isotope_1)

#Isotopes from 2021.08.28
d18_isotope_2 <- read_csv(here('data/processed/water_chemistry/O2_d18O_2021.08.28.csv')) %>%
  mutate(
    lakename = stri_replace_all_regex(
      lakename,
      pattern = c('CAL', 'CDR', 'CLF', 'GB', 'SS'),
      replacement = c('castle', 'cedar', 'cliff', 'gumboot', 'soapstone'),
      vectorize = FALSE
    ),
    date = ymd(date)
  )

#take a look
str(d18_isotope_2)

#Isotopes from 2021.09.29
d18_isotope_3 <- read_csv(here('data/processed/water_chemistry/O2_d18O_2021.09.29.csv')) %>%
  mutate(
    lakename = stri_replace_all_regex(
      lakename,
      pattern = c('CAL', 'CDR', 'CLF', 'GB', 'SS'),
      replacement = c('castle', 'cedar', 'cliff', 'gumboot', 'soapstone'),
      vectorize = FALSE
    ),
    date = ymd(date)
  )

#take a look
str(d18_isotope_3)

#Isotopes from 2021.10.14
d18_isotope_4 <- read_csv(here('data/processed/water_chemistry/O2_d18O_2021.10.14.csv')) %>%
  mutate(
    lakename = stri_replace_all_regex(
      lakename,
      pattern = c('CAL', 'CDR', 'CLF', 'GB', 'SS'),
      replacement = c('castle', 'cedar', 'cliff', 'gumboot', 'soapstone'),
      vectorize = FALSE
    ),
    date = ymd(date)
  )

#take a look
str(d18_isotope_4)

#Merge the above isotope tibbles
d18_isotope <- bind_rows(d18_isotope_1, d18_isotope_2, d18_isotope_3, d18_isotope_4) %>%
  arrange(lakename, date)

#take a look
str(d18_isotope)

#1f. import and merge water vsmow

#VSMOW from 2021.06.13
water_d18O_vsmow_1 <- read_csv(here('data/processed/water_chemistry/water_d18O_vsmow_2021.06.13.csv')) %>%
  mutate(
    lakename = stri_replace_all_regex(
      lakename,
      pattern = c('CAL', 'CDR', 'CLF', 'GB', 'SS'),
      replacement = c('castle', 'cedar', 'cliff', 'gumboot', 'soapstone'),
      vectorize = FALSE
    ),
    date = ymd(date)
  )

#take a look
str(water_d18O_vsmow_1)

#VSMOW from 2021.08.28
water_d18O_vsmow_2 <- read_csv(here('data/processed/water_chemistry/water_d18O_vsmow_2021.08.28.csv')) %>%
  mutate(
    lakename = stri_replace_all_regex(
      lakename,
      pattern = c('CAL', 'CDR', 'CLF', 'GB', 'SS'),
      replacement = c('castle', 'cedar', 'cliff', 'gumboot', 'soapstone'),
      vectorize = FALSE
    ),
    date = ymd(date)
  )

#take a look
str(water_d18O_vsmow_2)

#VSMOW from 2021.09.23
water_d18O_vsmow_3 <- read_csv(here('data/processed/water_chemistry/water_d18O_vsmow_2021.09.23.csv')) %>%
  mutate(
    lakename = stri_replace_all_regex(
      lakename,
      pattern = c('CAL', 'CDR', 'CLF', 'GB', 'SS'),
      replacement = c('castle', 'cedar', 'cliff', 'gumboot', 'soapstone'),
      vectorize = FALSE
    ),
    date = ymd(date)
  )

#take a look
str(water_d18O_vsmow_3)

#VSMOW from 2021.10.14
water_d18O_vsmow_4 <- read_csv(here('data/processed/water_chemistry/water_d18O_vsmow_2021.10.14.csv')) %>%
  mutate(
    lakename = stri_replace_all_regex(
      lakename,
      pattern = c('CAL', 'CDR', 'CLF', 'GB', 'SS'),
      replacement = c('castle', 'cedar', 'cliff', 'gumboot', 'soapstone'),
      vectorize = FALSE
    ),
    date = ymd(date)
  )

#take a look
str(water_d18O_vsmow_4)

#Merge the above isotope tibbles
water_d18O_vsmow <- bind_rows(water_d18O_vsmow_1, water_d18O_vsmow_2, water_d18O_vsmow_3, water_d18O_vsmow_4) %>%
  arrange(lakename, date)

# 2. Visualize the imported data------------------------------------------------

# 2a. ash free dry mass viz-----------------------------------------------------

dry_mass_plt <- ggplot(data = ash_free_dry_mass)+
  # geom_point(aes(x = date, y = ash_free_dry_mass, color = lakename), size = 5)+
  # geom_line(aes(x = date, y = ash_free_dry_mass, color = lakename), size = 1.5)+
  geom_boxplot(aes(x = lakename, y = ash_free_dry_mass, fill = lakename))+
  theme_classic()+
  theme(legend.position = 'none')+
  xlab('')+
  ylab('Ash-Free Dry Mass [g]')+
  ggtitle('Ash-Free Dry Mass')+
  scale_color_viridis_d(begin = 0.1, end = 0.9)
  
dry_mass_plt

#ggsave(here('output/sediment_plots/afdm_plot.jpeg'), dpi = 300)

# 2b. sediment C:N ratio viz----------------------------------------------------

sed_C_and_N_plt <- ggplot(data = sediment_CN)+
  geom_point(aes(x = date, y = C_mcgC_g, color = lakename), size = 5)+
  geom_line(aes(x = date, y = C_mcgC_g, color = lakename), size = 1.5) +
  geom_point(aes(x = date, y = N_mcgN_g, color = lakename), size = 5)+
  geom_line(aes(x = date, y = N_mcgN_g, color = lakename), size = 1.5)+
  theme_classic()+
  xlab('')+
  ylab('C and N [ug/g]')+
  ggtitle('Sediment C & N Concentration')+
  scale_color_viridis_d(begin = 0.1, end = 0.9)
  
sed_C_and_N_plt

sed_CN_plt <- ggplot(data = sediment_CN)+
  # geom_point(aes(x = date, y = CN_ratio, color = lakename), size = 5)+
  # geom_line(aes(x = date, y = CN_ratio, color = lakename), size = 1.5)+
  geom_boxplot(aes(x = lakename, y = CN_ratio, fill = lakename))+
  theme_classic()+
  xlab('')+
  ylab('C:N Ratio')+
  ggtitle('Sediment C:N Ratio')+
  scale_color_viridis_d(begin = 0.1, end = 0.9)+
  theme(legend.position = 'none')

sed_CN_plt

#ggsave(here('output/sediment_plots/sediment_CN_ratio.jpeg'), dpi = 300)

# 2c. water chemistry viz-------------------------------------------------------

#Here I'm trying to reorder the lakename and depth so that the depths will be "stacked" on top of one another, rather than side-by-side (as they are currently in the below ggplot)
water_chem_reorder <- water_chem %>%
  mutate(
    lake_depth = paste(lakename, depth_m),
    #factor(levels = c('cedar 0m', 'gumboot 0m', 'soapstone 0m','cedar 2m', 'gumboot 2m', 'soapstone sed','cedar 4m', 'gumboot 4m'))
  ) %>%
  arrange(lakename, depth_m)

water_chem_plt <- ggplot(data = water_chem_reorder)+
  geom_point(aes(x = date_time, y = ppb, color = species), size = 5)+
  geom_line(aes(x = date_time, y = ppb, color = species), size = 1.5)+
  # geom_point(aes(x = date_time, y = srp_ppb, color = depth_m), size = 5)+
  # geom_line(aes(x = date_time, y = srp_ppb, color = depth_m), size = 5)+
  # geom_point(aes(x = date_time, y = tp_ppb, color = depth_m), size = 5)+
  # geom_line(aes(x = date_time, y = tp_ppb, color = depth_m), size = 5)+
  # geom_point(aes(x = date_time, y = nh4_ppb, color = depth_m), size = 5)+
  # geom_line(aes(x = date_time, y = nh4_ppb, color = depth_m), size = 5)+
  theme_classic()+
  #facet_wrap(~lakename+depth_m)+
  facet_wrap(~lake_depth)+
  scale_color_viridis_d(begin = 0.1, end = 0.9)+
  ggtitle('Water Chemistry')+
  ylab('Chemical Species [ppb]')+
  xlab('')
water_chem_plt  
  
# 2d. water column C:N ratio viz------------------------------------------------

water_CN_plt <- ggplot(data = water_CN)+
  geom_point(aes(x = date, y = CN_ratio, color = depth), size = 5)+
  geom_line(aes(x = date, y = CN_ratio, color = depth), size = 1.5)+
  xlab('')+
  ylab('C:N Ratio')+
  ggtitle('Water Column C:N Ratio')+
  theme_classic()+
  scale_color_viridis_d(begin = 0.1, end = 0.9)+
  facet_wrap(~lakename, ncol = 1)
water_CN_plt

# 2e. dO18 isotope viz----------------------------------------------------------

d18O_isotope_plt <- ggplot(data = d18_isotope)+
  geom_point(aes(x = date, y = O2_d18O, color = depth), size = 5)+
  geom_line(aes(x = date, y = O2_d18O, color = depth), size = 1.5)+
  xlab('')+
  ylab('d18-O2 Isotope')+
  ggtitle('d18-O2 Isotope')+
  scale_color_viridis_d(begin = 0.1, end = 0.9)+
  facet_wrap(~lakename+sample,  ncol =2)
d18O_isotope_plt

# 2f. water d18O VSMOW viz------------------------------------------------------

water_isotope <- ggplot(data = water_d18O_vsmow)+
  geom_line(aes(x = date, y = d18O_vsmow, color = depth), size = 2)+
  theme_classic()+
  facet_wrap(~lakename)
water_isotope  

# 3. Averaging across depth and whole lake--------------------------------------

# 3a. Mean ash-free drymass (grams)----

afdm_mean <- ash_free_dry_mass %>% 
  group_by(lakename) %>% 
  summarise(
    afdm_mean = mean(ash_free_dry_mass),
    afdm_median = median(ash_free_dry_mass)
  )

# 3b. Mean sediment C:N ratio----

sediment_CN_mean <- sediment_CN %>% 
  filter(duplicate == 'no') %>% 
  na.omit() %>% 
  group_by(lakename) %>% 
  summarise(
    mass_mcg_mean = mean(mass_mcg),
    mass_mcg_med = median(mass_mcg),
    C_mcgC_g_mean = mean(C_mcgC_g),
    C_mcgC_g_med = median(C_mcgC_g),
    N_mcgN_g_mean = mean(N_mcgN_g),
    N_mcgN_g_med = median(N_mcgN_g),
    CN_ratio_mean = mean(CN_ratio),
    CN_ratio_med = median(CN_ratio)
  )

# 3c. Mean water chemistry----

#water chem by lake and depth
water_chem_mean <- water_chem %>% 
  select(-time) %>% 
  group_by(lakename, species, depth_m) %>% 
  summarise(
    mean = mean(ppb),
    med = median(ppb)
  ) %>% 
  pivot_wider(names_from = species, values_from = c(mean, med))

#water chem averaged over whole lake
water_chem_mean_total <-  water_chem %>% 
  group_by(lakename, species) %>% 
  summarise(
    mean_total = mean(ppb),
    med_total = median(ppb)
  )

# 3d. Water CN average----

water_CN_mean <- water_CN %>% 
  group_by(lakename, depth) %>% 
  summarise(
    C_ug_L_mean = mean(na.omit(C_ug_L)),
    N_ug_L_mean = mean(na.omit(N_ug_L)),
    CN_ratio_mean = mean(na.omit(CN_ratio)),
    C_ug_L_med = median(na.omit(C_ug_L)),
    N_ug_L_med = median(na.omit(N_ug_L)),
    CN_ratio_med = median(na.omit(CN_ratio))
  )

# 3e. Water CN total average----

water_CN_total_mean <- water_CN %>% 
  group_by(lakename) %>% 
  summarise(
    C_ug_L_mean_total = mean(na.omit(C_ug_L)),
    N_ug_L_mean_total = mean(na.omit(N_ug_L)),
    CN_ratio_mean_total = mean(na.omit(CN_ratio)),
    C_ug_L_med_total = median(na.omit(C_ug_L)),
    N_ug_L_med_total = median(na.omit(N_ug_L)),
    CN_ratio_med_total = median(na.omit(CN_ratio))
  )

# 3f. Oxygen isotope averaging----

isotope_avg <- d18_isotope %>% 
  group_by(lakename, depth) %>% 
  summarise(
    O2_d18O_mean = mean(na.omit(O2_d18O)),
    O2_d18O_med = median(na.omit(O2_d18O))
  )

# 3g. Oxygen isotope total averaging----

isotope_total_avg <- d18_isotope %>% 
  group_by(lakename) %>% 
  summarise(
    O2_d18O_mean_total = mean(na.omit(O2_d18O)),
    O2_d18O_med_total = median(na.omit(O2_d18O))
  )

# 3h. d18O VSMOW averaging----

VSMOW_avg <- water_d18O_vsmow %>% 
  group_by(lakename, depth) %>% 
  summarise(
    d18O_vsmow_mean = mean(na.omit(d18O_vsmow)),
    d18O_vsmow_med = median(na.omit(d18O_vsmow)),
    dD_vsmow_mean = mean(na.omit(dD_vsmow)),
    dD_vsmow_med = median(na.omit(dD_vsmow))
  )

# 3i. d18O VSMOW total averaging----

VSMOW_total_avg <- water_d18O_vsmow %>% 
  group_by(lakename) %>% 
  summarise(
    d18O_vsmow_total_mean = mean(na.omit(d18O_vsmow)),
    d18O_vsmow_total_med = median(na.omit(d18O_vsmow)),
    dD_vsmow_total_mean = mean(na.omit(dD_vsmow)),
    dD_vsmow_total_med = median(na.omit(dD_vsmow))
  )

# 4. Combine the averaged values------------------------------------------------

# 4a. Combine by lake----

mean_val_by_lake <- afdm_mean %>% 
  full_join(sediment_CN_mean, by = 'lakename') %>%  
  full_join(water_chem_mean_total, by = 'lakename') %>% 
  full_join(water_CN_total_mean, by = 'lakename') %>% 
  full_join(isotope_total_avg, by = 'lakename') %>% 
  full_join(VSMOW_total_avg, by = 'lakename')
  
# 4b Combine by lake and depth----

mean_val_by_depth <- water_chem_mean %>% 
  rename(depth = depth_m) %>% 
  full_join(water_CN_mean) %>% 
  full_join(isotope_avg) %>% 
  full_join(VSMOW_avg)

# 5. Visualize water chemistry--------------------------------------------------

# 5a. NH4 plots----
ggplot(data = mean_val_by_depth)+
  geom_boxplot(aes(x = lakename, y = mean_nh4_ppb, fill = lakename))+
  theme_classic()+
  labs( x = '', y = 'NH4 [ppb]')

#ggsave(here('output/water_chem_boxplots/nh4.jpeg'), dpi = 300)  

# 5b. NO3-NO2 plots----
ggplot(data = mean_val_by_depth)+
  geom_boxplot(aes(x = lakename, y = mean_no3_no2_ppb, fill = lakename))+
  theme_classic()+
  labs( x = '', y = 'NO3-NO2 [ppb]')

#ggsave(here('output/water_chem_boxplots/no3_no2.jpeg'), dpi = 300) 

# 5c. SRP plots----
ggplot(data = mean_val_by_depth)+
  geom_boxplot(aes(x = lakename, y = mean_srp_ppb, fill = lakename))+
  theme_classic()+
  labs( x = '', y = 'SRP [ppb]')

#ggsave(here('output/water_chem_boxplots/srp.jpeg'), dpi = 300) 

# 5d. TP plots----
ggplot(data = mean_val_by_depth)+
  geom_boxplot(aes(x = lakename, y = mean_tp_ppb, fill = lakename))+
  theme_classic()+
  labs( x = '', y = 'TP [ppb]')

#ggsave(here('output/water_chem_boxplots/tp.jpeg'), dpi = 300) 

# 5e. C ug_L plots----
ggplot(data = mean_val_by_depth)+
  geom_boxplot(aes(x = lakename, y = C_ug_L_mean, fill = lakename))+
  theme_classic()+
  labs( x = '', y = 'C [ug/L]')

#ggsave(here('output/water_chem_boxplots/C_ug_L.jpeg'), dpi = 300) 

# 5f. N ug_L plots----
ggplot(data = mean_val_by_depth)+
  geom_boxplot(aes(x = lakename, y = N_ug_L_mean, fill = lakename))+
  theme_classic()+
  labs( x = '', y = 'N [ug/L]')

#ggsave(here('output/water_chem_boxplots/N_ug_L.jpeg'), dpi = 300) 

# 5g. C:N ratio plots----
ggplot(data = mean_val_by_depth)+
  geom_boxplot(aes(x = lakename, y = CN_ratio_mean, fill = lakename))+
  theme_classic()+
  labs( x = '', y = 'C:N')

#ggsave(here('output/water_chem_boxplots/CN_ratio.jpeg'), dpi = 300) 

# 5h. O2_d18O_mean plots----
ggplot(data = mean_val_by_depth)+
  geom_boxplot(aes(x = lakename, y = O2_d18O_mean, fill = lakename))+
  theme_classic()+
  labs( x = '', y = 'd18O')

#ggsave(here('output/water_chem_boxplots/O2_d18O_mean.jpeg'), dpi = 300) 

# 5i. d18O_vsmow_mean plots----
ggplot(data = mean_val_by_depth)+
  geom_boxplot(aes(x = lakename, y = d18O_vsmow_mean, fill = lakename))+
  theme_classic()+
  labs( x = '', y = 'VSMOW')

#ggsave(here('output/water_chem_boxplots/d18O_vsmow_mean.jpeg'), dpi = 300) 

# 5j. DOC and TN mean plots----

doc_tn <- read_csv(here('data/processed/water_chemistry/shallow_lake_nutrients.csv')) %>% 
  mutate(
    date = mdy(date),
    lake = as.factor(lake),
    depth = as.factor(depth)
  )

doc_tn_mean <- doc_tn %>% 
  mutate(
    year = year(date),
    month = month(date),
    day = day(date)
  ) %>% 
  group_by(lake) %>% 
  summarise(
    tn_ppb_mean = mean(tn_ppb),
    tn_ppb_med = median(tn_ppb),
    doc_ppm_mean = mean(doc_ppm),
    doc_ppm_med = median(doc_ppm)
  ) %>%
  ungroup()

# 5k. TN plot----
ggplot(data = doc_tn)+
  geom_boxplot(aes(x = lake, y = tn_ppb, fill = lake))+
  theme_classic()

#ggsave(here('output/water_chem_boxplots/tn_ppb.jpeg'), dpi = 300)

# 5l. DOC plot----
ggplot(data = doc_tn)+
  geom_boxplot(aes(x = lake, y = doc_ppm, fill = lake))+
  theme_classic()

#ggsave(here('output/water_chem_boxplots/doc_ppm.jpeg'), dpi = 300)

# 6. Castle and cliff water chemistry, chl-a, and PPr---------------------------

#NOTE: Castle and Cliff data collected by Castle Lake reseach team. See
#      script_00 for data citations

# 6a. Import Castle and Cliff data----

#Castle DOC
cal_doc <- read_csv(here('data/nutrients/cal_clf_DIC_DOC_TN/castle_doc.csv')) %>% 
  mutate(
    date = mdy(date),
    lake = as.factor(lake),
    depth = as.factor(depth)
  )

#Cliff DOC
clf_doc <- read_csv(here('data/nutrients/cal_clf_DIC_DOC_TN/cliff_doc.csv')) %>% 
  mutate(
    date = mdy(date),
    lake = as.factor(lake),
    depth = as.factor(depth)
  )

#Castle TN
cal_tn <- read_csv(here('data/nutrients/cal_clf_DIC_DOC_TN/castle_tn.csv')) %>% 
  mutate(
    date = mdy(date),
    lake = as.factor(lake),
    depth = as.factor(depth)
  ) %>% 
  rename(tn_ppb = tn_ppm) %>% 
  filter(tn_ppb>0)

#Cliff TN
clf_tn <- read_csv(here('data/nutrients/cal_clf_DIC_DOC_TN/cliff_tn.csv')) %>% 
  mutate(
    date = mdy(date),
    lake = as.factor(lake),
    depth = as.factor(depth)
  ) %>% 
  rename(tn_ppb = tn_ppm) %>% 
  filter(tn_ppb>0)

# 6b. Combine datasets----

cal_clf_doc_tn <- bind_rows(cal_doc, clf_doc, cal_tn, clf_tn) 

# 6c. DOC data viz----
ggplot(data = cal_clf_doc_tn %>% filter(doc_ppm<13))+ #eliminate two strong outliers
  geom_boxplot(aes(x = lake, y = doc_ppm, fill = lake))+
  geom_jitter(aes(x = lake, y = doc_ppm, fill = lake, color = year(date)), size = 1)+
  theme_classic()+
  scale_color_viridis()+
  facet_wrap(~year(date))

#ggsave(here('output/water_chem_boxplots/cal_clf_DOC_yearly.jpeg'), dpi = 300)

# 6d. TN data viz----
ggplot(data = cal_clf_doc_tn)+
  geom_boxplot(aes(x = lake, y = tn_ppb, fill = lake))+
  geom_jitter(aes(x = lake, y = tn_ppb, fill = lake, color = year(date)), size = 1)+
  theme_classic()+
  scale_color_viridis()+
  facet_wrap(~year(date))

#ggsave(here('output/water_chem_boxplots/cal_clf_TN_yearly.jpeg'), dpi = 300)

# 6e. Import shallow lakes DOC and TN----

shallow_lake_doc_tn <- read_csv(here('data/processed/water_chemistry/shallow_lake_nutrients.csv')) %>% 
  mutate(
    date = mdy(date),
    lake = as.factor(lake),
    depth = as.factor(depth)
  )

# 6f. DOC data viz----
ggplot(data = shallow_lake_doc_tn)+ 
  geom_boxplot(aes(x = lake, y = doc_ppm, fill = lake))+
  #geom_jitter(aes(x = lake, y = doc_ppm, fill = lake))+
  theme_classic()+
  scale_color_viridis()

ggsave(here('output/water_chem_boxplots/shallow_DOC.jpeg'), dpi = 300)

# 6g. TN data viz----
ggplot(data = shallow_lake_doc_tn)+
  geom_boxplot(aes(x = lake, y = tn_ppb, fill = lake))+
  #geom_jitter(aes(x = lake, y = tn_ppb, fill = lake))+
  theme_classic()+
  scale_color_viridis()

ggsave(here('output/water_chem_boxplots/shallow_TN.jpeg'), dpi = 300)

# 7. Water chemistry------------------------------------------------------------

# 7a. Import Chl-a data----

cal_chla <- read_csv(here('data/nutrients/cal_clf_nutrients/cal_chla.csv')) %>% 
  select(1:4) %>% 
  mutate(
    date = mdy(date),
    lake = as.factor(lake),
    depth = as.factor(depth)
  )

# 7b. Import Chl-a data----

clf_chla <- read_csv(here('data/nutrients/cal_clf_nutrients/clf_chla.csv')) %>% 
  select(1:4) %>% 
  mutate(
    date = mdy(date),
    lake = as.factor(lake),
    depth = as.factor(depth)
  )

# 7c. Combine data----

cal_clf_chla <- bind_rows(cal_chla, clf_chla)

# 7b. Plot Chl-a----

ggplot(data = cal_clf_chla %>% filter(chla_ug_l<3.5))+ #removing outliers 
  geom_boxplot(aes(x = lake, y = chla_ug_l, fill = lake))+
  #geom_jitter(aes(x = lake, y = chla_ug_l, color = year(date)))+
  theme_classic()

#ggsave(here('output/water_chem_boxplots/cal_clf_chla.jpeg'), dpi = 300)

# 7c. Import PPr----

#Castle PPr
cal_ppr <- read_csv(here('data/nutrients/cal_clf_nutrients/cal_ppr.csv')) %>% 
  mutate(
    date = mdy(date),
    lake = as.factor(lake),
    depth = as.factor(depth)
  )

#Cliff PPr
clf_ppr <- read_csv(here('data/nutrients/cal_clf_nutrients/clf_ppr.csv')) %>% 
  mutate(
    date = mdy(date),
    lake = as.factor(lake),
    depth = as.factor(depth)
  )

# 7d. Combine data----
cal_clf_ppr <- bind_rows(cal_ppr, clf_ppr)

# 7e. Plot Castle and Cliff PPr----
ggplot(data = cal_clf_ppr)+
  geom_boxplot(aes(x = lake, y = net_ppr, fill = lake))+
  #geom_jitter(aes(x = lake, y = net_ppr))+
  theme_classic()+
  facet_wrap(~year(date))

# 7f. Import water chemistry----

#Castle water chem
cal_chem <- read_csv(here('data/nutrients/cal_clf_nutrients/cal_water_chemistry.csv')) %>% 
  clean_names() %>% 
  mutate(
    date = dmy(date),
    lake = as.factor(lake),
    depth = as.factor(depth_m),
    nh4_ppb = as.numeric(nh4_n_ppb),
    no3_no2_ppb = as.numeric(no3_n_ppb),
    srp_ppb = as.numeric(srp_p_ppb),
    tp_ppb = as.numeric(tp_ppb)
  ) %>% 
  select(date, lake, depth, nh4_ppb, no3_no2_ppb, srp_ppb, tp_ppb) %>% 
  mutate( #negative values are a math artifact, indicating concentration is "below detectable limits". Removing those values
    nh4_ppb = replace(nh4_ppb, which(nh4_ppb<0), NA),
    no3_no2_ppb = replace(no3_no2_ppb, which(no3_no2_ppb<0), NA),
    srp_ppb = replace(srp_ppb, which(srp_ppb<0), NA),
    tp_ppb = replace(tp_ppb, which(tp_ppb<0), NA)
  )

#Cliff water chem
clf_chem <- read_csv(here('data/nutrients/cal_clf_nutrients/clf_water_chemistry.csv')) %>% 
  clean_names() %>% 
  mutate(
    date = dmy(date),
    lake = as.factor(lake),
    depth = as.factor(depth_m),
    nh4_ppb = as.numeric(nh4_n_ppb),
    no3_no2_ppb = as.numeric(no3_n_ppb),
    srp_ppb = as.numeric(srp_p_ppb),
    tp_ppb = as.numeric(tp_ppb)
  ) %>% 
  select(date, lake, depth, nh4_ppb, no3_no2_ppb, srp_ppb, tp_ppb) %>% 
  mutate( #negative values are a math artifact, indicating concentration is "below detectable limits". Removing those values
    nh4_ppb = replace(nh4_ppb, which(nh4_ppb<0), NA),
    no3_no2_ppb = replace(no3_no2_ppb, which(no3_no2_ppb<0), NA),
    srp_ppb = replace(srp_ppb, which(srp_ppb<0), NA),
    tp_ppb = replace(tp_ppb, which(tp_ppb<0), NA)
  )

# 7d. Combine data----
cal_clf_chem <- bind_rows(cal_chem, clf_chem) %>% 
  pivot_longer(
    cols = c(4:7),
    names_to = 'species',
    values_to = 'ppb'
  ) %>% 
  filter(
    depth != 'spring',
    depth != 'OUTFLOW',
    depth !='OUTPUT', 
    depth != 'OUTLET', 
    depth != 'B12', 
    depth != 'B13', 
    depth != 'B15', 
    depth != 'OF'
  )

#Combine with shallow lake data

all_lakes_water_chem <- water_chem %>%
  select(
    lake = lakename,
    date,
    depth = depth_m,
    species,
    ppb
  ) %>% 
  mutate(
    lake = as.factor(lake),
    depth = as.factor(depth)
  ) %>% 
  bind_rows(cal_clf_chem)
  
  

# 7e. Plot all lakes NH4----
ggplot(data = all_lakes_water_chem %>% filter(species == 'nh4_ppb'))+
  geom_boxplot(aes(x = lake, y = ppb, fill = lake))+
  #geom_jitter(aes(x = lake, y = net_ppr, color = depth))+
  theme_classic()+
  ylab('NH4 [ppb]')
  # facet_wrap(~year(date))

ggsave(here('output/water_chem_boxplots/all_lakes_nh4.jpeg'), dpi = 300)

# 7f. Plot all lakes NO3 NO2----
ggplot(data = all_lakes_water_chem %>% filter(species == 'no3_no2_ppb'))+
  geom_boxplot(aes(x = lake, y = ppb, fill = lake))+
  #geom_jitter(aes(x = lake, y = net_ppr, color = depth))+
  #facet_wrap(~year(date))+
  theme_classic()+
  ylab('NO3 - NO2 [ppb]')

ggsave(here('output/water_chem_boxplots/all_lakes_no3.jpeg'), dpi = 300)

# 7g. Plot all lakes TP----
ggplot(data = all_lakes_water_chem %>% filter(species == 'tp_ppb'))+
  geom_boxplot(aes(x = lake, y = ppb, fill = lake))+
  #geom_jitter(aes(x = lake, y = net_ppr, color = depth))+
  #facet_wrap(~year(date))+
  theme_classic()+
  ylab('TP [ppb]')

ggsave(here('output/water_chem_boxplots/all_lakes_tp.jpeg'), dpi = 300)

# 7h. Plot all lakes SRP----
ggplot(data = all_lakes_water_chem %>% filter(species == 'srp_ppb'))+
  geom_boxplot(aes(x = lake, y = ppb, fill = lake))+
  #geom_jitter(aes(x = lake, y = net_ppr, color = depth))+
  #facet_wrap(~year(date))+
  theme_classic()+
  ylab('SRP [ppb]')

ggsave(here('output/water_chem_boxplots/all_lakes_srp.jpeg'), dpi = 300)







