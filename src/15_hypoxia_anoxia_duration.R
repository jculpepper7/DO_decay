## The goal of this script is 1) to detect hypoxia in all time series and 
## 2) to extract the duration of each of those periods of hypoxia and
## 3) detect anoxia in all time series and
## 4) to exctract the duration of each of those periods of anoxia

# 1. Load libraries------------------------------------------------------

library(tidyverse)
library(here)

# 2. Import data---------------------------------------------------------

cdr_do <- read_csv(here(''))
gb_do <- read_csv(here(''))
ss_do <- read_csv(here(''))

#3. Join data------------------------------------------------------------

all_do <- bind_rows(cdr_do, gb_do, ss_do)

#3. Isolate hypoxia periods

#Something like this
hypox <- all_do %>%
  group_by(lake, water_year)
  filter(do_mg_l <= 2) %>% 
  mutate(
    hypox_duration = #last instance - first instance
  )