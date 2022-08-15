## The goal of this script is to 
## 1. clean the Castle data
## 2. aggregate the cleaned data

# 1. Load libraries----------------------------------------

library(tidyverse)
library(here)

# 2. Import Castle raw data--------------------------------

#First import all Castle raw data into a list
cal_raw <- 
  list.files(
    path = here('data/castle_raw/20m'), #insert path to desired depth
    pattern = "*.TXT",
    full.names = TRUE
  ) %>% 
  map(~read_delim(., col_types = cols(.default = "c"), delim = ',', skip = 9)) 


#Rename the columns, and cut the first and last 6 hours 
#I do not have individual notes on when the miniDOT was removed from the water, therefore I am cutting the first and last 6 hours

#Will exclude cal_1 from 30m sample as it is only 2 hours. I do not have notes on this, but I suspect it may be a calibration.

cal_01 <- cal_raw[[1]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_02 <- cal_raw[[2]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)
  
cal_03 <- cal_raw[[3]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_04 <- cal_raw[[4]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_05 <- cal_raw[[5]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_06 <- cal_raw[[6]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_07 <- cal_raw[[7]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_08 <- cal_raw[[8]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_09 <- cal_raw[[9]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_10 <- cal_raw[[10]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_11 <- cal_raw[[11]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_12 <- cal_raw[[12]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_13 <- cal_raw[[13]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_14 <- cal_raw[[14]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_15 <- cal_raw[[15]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_16 <- cal_raw[[16]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_17 <- cal_raw[[17]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_18 <- cal_raw[[18]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_19 <- cal_raw[[19]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_20 <- cal_raw[[20]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_21 <- cal_raw[[21]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_22 <- cal_raw[[22]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_23 <- cal_raw[[23]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_24 <- cal_raw[[24]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_25 <- cal_raw[[25]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_26 <- cal_raw[[26]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_27 <- cal_raw[[27]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_28 <- cal_raw[[28]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_29 <- cal_raw[[29]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_30 <- cal_raw[[30]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_31 <- cal_raw[[31]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_32 <- cal_raw[[32]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_33 <- cal_raw[[33]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_34 <- cal_raw[[34]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_35 <- cal_raw[[35]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_36 <- cal_raw[[36]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_37 <- cal_raw[[37]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_38 <- cal_raw[[38]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_39 <- cal_raw[[39]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_40 <- cal_raw[[40]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

cal_41 <- cal_raw[[41]] %>% 
  select(unix = 1, 
         utc = 2, 
         pst = 3, 
         batt = 4, 
         temp_c = 5, 
         do_mg_l = 6, 
         do_sat = 7, 
         q = 8) %>% 
  slice(-c(1:36)) %>% 
  head(.,-36)

# 3. Aggregate individual, cleaned data------------------

cal_cleaned <- bind_rows(cal_01,
                         cal_02, cal_03, cal_04, cal_05, cal_06, cal_07, cal_08, cal_09, cal_10,
                         cal_11, cal_12, cal_13, cal_14, cal_15, cal_16, cal_17, cal_18, cal_19, cal_20,
                         cal_21, cal_22, cal_23, cal_24, cal_25, cal_26, cal_27, cal_28, cal_29, cal_30,
                         cal_31, cal_32, cal_33, cal_34, cal_35, cal_36#, cal_37, cal_38, cal_39, cal_40,
                         #cal_41
                         )

# 4. Write csv for cleaned data

write_csv(cal_cleaned, here('data/processed/castle/castle_clean_agg_data_20m.csv'))





