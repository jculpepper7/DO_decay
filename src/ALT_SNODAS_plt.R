# 1. Load libraries--------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(padr)
library(plotly)
library(patchwork)
library(zoo)
library(scico)
library(scales)

# Add gridmet precip data----

gridmet_precip <- read_csv(here('data/met_data/gridmet/castle_gridmet_precip_2018_2022.csv'))

gridmet_precip <- gridmet_precip %>% 
  rename(
    date = 1, 
    precip_mm = 2
  ) 

# Add gridmet temp data ---------------------------------------------------

gridmet_temp <- read_csv(here('data/met_data/gridmet/gridmet_temp.csv')) %>%
  mutate(
    date = mdy(date)
  ) %>% 
  filter(
    date <= ymd('2022-06-11')
  )

#Combine gridmet data

gridmet <- gridmet_temp %>% 
  bind_cols(gridmet_precip) %>% 
  select(
    date = 1, 2:4, 6
  ) %>% 
  mutate(
    precip_phase = if_else(
      temp_mean <= 0, as.factor('snow'), as.factor('rain')
    ),
    year = as.factor(year(date)),
    yday = yday(date),
    wday = if_else(
      yday >= 274, yday-273, yday+92
    )
  )


# Import SNODAS -----------------------------------------------------------



# 5a. Import SNODAS data----
snodas <- read_csv(here('data/met_data/snodas/cal_snodas.csv')) %>% 
  rename(date = 1, swe_mm = 2) %>% 
  mutate(
    date = ymd(date),
    swe_mm = as.numeric(swe_mm)
  ) %>% 
  filter(
    date <= ymd('2022-06-11')
  )

# Combine Gridmet and SNODAS----

met <- gridmet %>%
  inner_join(snodas) %>%
  mutate(
    water_year = as.integer(year(date + months(3)))
  )

# -----------------------------
# 1. Create a water-year column
# -----------------------------
# Water year = calendar year of Oct 1 → Sep 30
met <- met %>%
  mutate(
    water_year = year(date + months(3)) %>% as.integer(),  # ensure integer
    wday = yday(date + months(3))  # day-of-water-year (1 = Oct 1)
  ) %>% 
  na.omit

# -----------------------------
# 2. Define interpolation function
# -----------------------------
interp_fun <- function(df, key) {
  yr <- as.integer(key$water_year[1])
  
  # Origin = Oct 1 of previous calendar year
  origin_date <- as.Date(paste0(yr - 1, "-10-01"))
  
  # Interpolation grid
  interp_grid <- tibble(wday = seq(min(df$wday), max(df$wday), by = 0.1))
  
  # Interpolate swe_mm and temp_mean
  interp_grid %>%
    mutate(
      swe_mm = approx(df$wday, df$swe_mm, xout = wday)$y,
      temp_mean = approx(df$wday, df$temp_mean, xout = wday)$y,
      temp_min = approx(df$wday, df$temp_min, xout = wday)$y,
      temp_max = approx(df$wday, df$temp_max, xout = wday)$y,
      date = origin_date + (wday - 1)
    )
}

# -----------------------------
# 3. Apply interpolation by water year
# -----------------------------
met_interp <- met %>%
  arrange(water_year, wday) %>%
  group_by(water_year) %>%
  group_modify(~ interp_fun(.x, .y)) %>%
  ungroup()

# -----------------------------
# 4. Optional: remove July–October
# -----------------------------
met_interp <- met_interp %>%
  filter(!month(date) %in% 6:10)

# -----------------------------
# 5. Check results
# -----------------------------
glimpse(met_interp)
range(met_interp$date)
head(met_interp)
tail(met_interp)


# Plot smooth line + real daily points
# test <- ggplot() +
#   geom_path(
#     data = met_interp,
#     aes(x = wday, y = swe_mm, color = temp_mean),
#     size = 1.2,
#     lineend = "round"
#   )+ 
# ggplot(met_interp, aes(x = date, y = swe_mm, color = temp_mean)) +
#   geom_path(size = 1.2, lineend = "round") +
#   scale_color_gradientn(
#     colors = c("#041C44", "#08306B", "#2171B5", "#6BAED6", "#F7F7F7",
#                "#FCAE91", "#FB6A4A", "#CB181D", "#67000D"),
#     #values = scales::rescale(c(-5, -3, -1, 0, 5, 10, 15, 20, 25)),
#     values = scales::rescale(c(-5, -4, -3, -1, 0, 5, 10, 15, 25)),
#     limits = c(-6.5, 27.7),
#     space = "Lab"
#   )+
#   scale_x_date(date_breaks = '1 month', date_labels = "%b ")+
#   theme_classic(base_size = 14) +
#   theme(
#     legend.position = 'bottom'
#   )+
#   labs(
#     x = "Day of Year",
#     y = "SWE (mm)",
#     color = "Mean Temp (°C)"
#   )+
#   facet_wrap(
#     ~water_year, 
#     ncol = 2,
#     scales = 'free'
#   )
# 
# 
# ggsave(
#   here('output/test_temp_snow_facet.png'),
#   dpi = 300,
#   width = 6.5, 
#   height = 7.5,
#   type = 'cairo-png'
# )

ggplot(met_interp, aes(x = date, y = swe_mm, color = temp_mean)) +
  geom_path(size = 1.2, lineend = "round") +
  scale_color_gradientn(
    colors = c("#02203C", "#08306B", "#2171B5", "#6BAED6", "#F7F7F7",
               "#FCAE91", "#FB6A4A", "#CB181D", "#67000D"),
    values = scales::rescale(c(-5, -4, -3, -1, 0, 5, 10, 15, 25)),
    limits = c(-4.9, 25.1),
    space = "Lab"
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = 'bottom',
    strip.background = element_blank(),
    strip.text = element_text(size = 18)
  )+
  guides(color = guide_colorbar(ticks.colour = NA))+
  labs(
    x = "Month",
    y = "SWE (mm)",
    color = "Mean Temp (°C)"
  ) +
  facet_wrap(~water_year, ncol = 2, scales = "free_x")

ggsave(
  here('output/test_temp_snow_facet2_same_yaxis.png'),
  dpi = 300,
  width = 6.5, 
  height = 7.5,
  type = 'cairo-png'
)


# ALT. Color Scheme -------------------------------------------------------


# Ensure sorted data
met_interp <- met_interp %>%
  arrange(water_year, wday)

# Plot with enhanced midrange contrast
ggplot(met_interp, aes(x = date, y = swe_mm, color = temp_mean)) +
  geom_path(size = 1.2, lineend = "round") +
  scale_color_gradientn(
    colors = c("#021B35", "#08306B", "#1E5BAA", "#5FA5D1", "#F0F0F0",
               "#FDBB84", "#FB6A4A", "#DE2D26", "#67000D"),
    values = scales::rescale(c(-5, -3, -1, 0, 3, 8, 12, 18, 25)),
    limits = c(-4.9, 25.1),
    space = "Lab",
    name = "Mean Temp (°C)"
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b"
  ) +
  theme_classic(
    base_size = 15
  ) +
  labs(
    x = "Month",
    y = "SWE (mm)"
  ) +
  facet_wrap(~water_year, ncol = 2, scales = "free_x") +

# Theme tweaks for legend placement and readability
    theme(
      legend.position = "bottom",
      legend.key.height = unit(0.6, "cm"),
      legend.key.width = unit(3.5, "cm"),
      legend.title = element_text(size = 12, vjust = 1, hjust = 0.5),
      legend.text = element_text(size = 10),
      legend.title.align = 0.5,  # center title above bar
      legend.background = element_blank(),
      legend.key = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 18)
    ) +
    guides(
      color = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = 15,
        barheight = 0.6,
        #ticks = FALSE  # removes white ticks
        ticks.colour = NA #removes white ticks
      )
    )

ggsave(
  here('output/test_temp_snow_facet3_preferred.png'),
  dpi = 300,
  width = 6.5, 
  height = 7.5,
  type = 'cairo-png'
)

ggsave(
  filename = here::here("output/test_temp_snow_facet3_preferred_ver2.png"),
  dpi = 600,                   # high-quality print-ready
  width = 7,                   # width in inches
  height = 8,                  # height in inches
  units = "in",
  type = "cairo-png",          # preserves colors and smooth edges
  bg = "white"                 # ensures white background (not transparent)
)

ggsave(
  filename = here::here("output/test_temp_snow_facet3_preferred_ver3.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 8,
  bg = "white"
)

ggsave(
  filename = here::here("output/test_temp_snow_facet3_preferred_web.png"),
  dpi = 300,
  width = 6,
  height = 6.5,
  units = "in",
  type = "cairo-png",
  bg = "white"
)


# ALT. - Optional color-blind friendly contrasts --------------------------


# # Plot: color-blind-friendly and publication-ready
# ggplot(met_interp, aes(x = date, y = swe_mm, color = temp_mean)) +
#   geom_path(size = 1.2, lineend = "round") +
#   
#   # Color-blind-friendly diverging scale (blue–grey–orange–red)
#   scale_color_gradientn(
#     colors = c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0E0E0",
#                "#FDAE61", "#F46D43", "#D73027", "#A50026"),
#     values = scales::rescale(c(-5, -3, -1, 0, 3, 8, 12, 18, 25)),
#     limits = c(-4.9, 25.1),
#     space = "Lab",
#     name = "Mean Temp (°C)"
#   ) +
#   
#   scale_x_date(
#     date_breaks = "1 month",
#     date_labels = "%b"
#   ) +
#   
#   theme_classic(base_size = 14) +
#   
#   labs(
#     x = "Month",
#     y = "SWE (mm)"
#   ) +
#   
#   facet_wrap(~water_year, ncol = 2, scales = "free_x") +
#   
#   # Theme tweaks for legend placement and readability
#   theme(
#     legend.position = "bottom",
#     legend.key.height = unit(0.6, "cm"),
#     legend.key.width = unit(3.5, "cm"),
#     legend.title = element_text(size = 12, vjust = 1, hjust = 0.5),
#     legend.text = element_text(size = 10),
#     legend.title.align = 0.5,  # center title above bar
#     legend.background = element_blank(),
#     legend.key = element_blank()
#   ) +
#   guides(
#     color = guide_colorbar(
#       title.position = "top",
#       title.hjust = 0.5,
#       barwidth = 15,
#       barheight = 0.6,
#       #ticks = FALSE  # removes white ticks
#       ticks.colour = NA #removes white ticks
#     )
#   )
