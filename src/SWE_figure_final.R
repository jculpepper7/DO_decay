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

# --- Data Prep ---
met <- gridmet %>%
  inner_join(snodas) %>%
  drop_na()  # ensure no missing values in key columns

# --- Interpolation Function ---
interp_fun <- function(df, key) {
  yr <- as.integer(key$year[1])  # water year label
  
  interp_grid <- tibble(wday = seq(min(df$wday), max(df$wday), by = 0.1))
  
  origin_date <- as.Date(paste0(yr - 1, "-10-01"))
  
  interp_grid %>%
    mutate(
      swe_mm = approx(df$wday, df$swe_mm, xout = wday)$y,
      temp_mean = approx(df$wday, df$temp_mean, xout = wday)$y,
      date = origin_date + (wday - 1),
      water_year = yr
    )
}

# --- Apply Interpolation by Year ---
met_interp <- met %>%
  arrange(year, wday) %>%
  group_by(year) %>%
  group_modify(~ interp_fun(.x, .y)) %>%
  ungroup()

# --- Plot ---

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

# --- Save Publication-Quality Image ---
ggsave(
  filename = here("output/temp_swe_facet_publication.png"),
  # filename = here("output/temp_swe_facet_publication_free_yaxis.png"),
  dpi = 300,
  width = 7,   # good for 2-column layout
  height = 8,  # balanced for facet panels
  units = "in",
  type = "cairo-png"
)
