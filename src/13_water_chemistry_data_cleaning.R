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

#2. Visualize the imported data-------------------------------------------------

#ash free dry mass viz----------------------------------------------------------

dry_mass_plt <- ggplot(data = ash_free_dry_mass)+
  geom_point(aes(x = date, y = ash_free_dry_mass, color = lakename), size = 5)+
  geom_line(aes(x = date, y = ash_free_dry_mass, color = lakename), size = 1.5)+
  theme_classic()+
  xlab('')+
  ylab('Ash-Free Dry Mass [g]')+
  ggtitle('Ash-Free Dry Mass')+
  scale_color_viridis_d(begin = 0.1, end = 0.9)
  
dry_mass_plt

#sediment C:N ratio viz---------------------------------------------------------

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
  geom_point(aes(x = date, y = CN_ratio, color = lakename), size = 5)+
  geom_line(aes(x = date, y = CN_ratio, color = lakename), size = 1.5)+
  theme_classic()+
  xlab('')+
  ylab('C:N Ratio')+
  ggtitle('Sediment C:N Ratio')+
  scale_color_viridis_d(begin = 0.1, end = 0.9)

sed_CN_plt

#water chemistry viz------------------------------------------------------------

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
  
#water column C:N ratio viz-----------------------------------------------------

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

#dO18 isotope viz---------------------------------------------------------------

d18O_isotope_plt <- ggplot(data = d18_isotope)+
  geom_point(aes(x = date, y = O2_d18O, color = depth), size = 5)+
  geom_line(aes(x = date, y = O2_d18O, color = depth), size = 1.5)+
  xlab('')+
  ylab('d18-O2 Isotope')+
  ggtitle('d18-O2 Isotope')+
  scale_color_viridis_d(begin = 0.1, end = 0.9)+
  facet_wrap(~lakename+sample,  ncol =2)
d18O_isotope_plt

#water d18O VSMOW viz-----------------------------------------------------------

water_isotope <- ggplot(data = water_d18O_vsmow)+
  geom_point(aes(x = date, y = ))
  








