## The purpose of this script is to:
## 1. Read clean, aggregated data
## 2. Data viz of unaltered data
## 3. Include moving averages
## 4. Data viz of moving average data

# libraries
library(tidyverse)
library(padr)
library(patchwork)
library(plotly)
library(zoo) #for time series manipulation (e.g. rolling mean)

# 1. Read cleaned, aggregated data----------------------------------------------

gumboot <- read_csv(here('data/processed/gumboot/gumboot_clean_agg_data.csv'),
                  guess_max = 80000) #use guess max to increase the number of 
#rows that 'readr' reads prior to guess the     
#column type. 
#Did this because the NA values were 
#producing a parsing error. To see what I 
#mean, comment out the guess_max() function
#Another solution is to specify column types
#See vignette("readr") 


# 2. Data viz of unaltered data-------------------------------------------------

gumboot_plt_non_avg <- ggplot(data = gumboot)+
  geom_line(aes(x = date_time, y = temp_c, color = depth), size = 1)+
  geom_line(aes(x = date_time, y = do_mg_l), size = 1)+
  theme_classic()+  
  ggtitle("gumboot Lake")+
  xlab("")+
  ylab("Temperature [C] / DO [mg/L]")+
  theme_classic()+
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = wes_palette(name = "Darjeeling1"), name = "Depth")
#scale_color_viridis(discrete = TRUE, option = "D")+ #for colorblind friendly option
#scale_x_date(breaks = "month", labels = date_format("%b %Y"))
gumboot_plt_non_avg

ggsave(here('output/plots/gumboot_20_21.png'), dpi = 500)

ggplotly(gumboot_plt_non_avg)

# 3. Include moving averages----------------------------------------------------

gumboot_w_avg <- gumboot %>%
  mutate(
    day = day(date_time),
    month = month(date_time),
    year = year(date_time)
  ) %>%
  group_by(depth, year, month, day) %>%
  mutate(
    temp_c_avg = mean(temp_c),
    do_mg_l_avg = mean(do_mg_l),
    do_sat_avg = mean(do_sat),
    daily = make_date(year = year, month = month, day = day)
  )

# 4. data viz of averaged data (daily average)

gumboot_avg_plt <- ggplot(data = gumboot_w_avg)+
  #geom_line(aes(x = daily, y = temp_c_avg, color = depth), size = 1.5)+
  geom_line(aes(x = date_time, y = do_mg_l), size = 1.5, color = 'black')+
  theme_classic()
gumboot_avg_plt

# 5. Alt visualizations

gumboot_facet <- ggplot(data = gumboot) +
  geom_line(aes(x = date_time, y = temp_c))+
  theme_classic()+  
  facet_wrap(~depth) +
  ggtitle('gumboot Temperature')+
  ylim(c(0, 32))
gumboot_facet

ggsave(here('output/plots/gumboot_facet_20_21.png'), dpi = 500)

gumboot_do_conc <- ggplot(data = gumboot) +
  geom_line(aes(x = date_time, y = do_mg_l)) +
  theme_classic()+
  ggtitle('gumboot DO [mg/L]')
gumboot_do_conc

ggsave(here('output/plots/gumboot_do_concentration_20_21.png'), dpi = 500)

gumboot_do_sat <- ggplot(data = gumboot)+
  geom_line(aes(x = date_time, y = do_sat))+
  theme_classic()+
  ggtitle('gumboot DO Saturation [%]')
gumboot_do_sat

ggsave(here('output/plots/gumboot_do_saturation_20_21.png'), dpi = 500)
