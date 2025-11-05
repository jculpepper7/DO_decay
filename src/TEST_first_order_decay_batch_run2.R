# 1. Libraries ------------------------------------------------------------

library(tidyverse)
library(here)
library(patchwork)
library(janitor)

# 2. Import data ----------------------------------------------------------


# **All DO data -----------------------------------------------------------

do_ts <- read_csv(here('data/processed/do_ts_all_lakes.csv')) %>% 
  mutate(
    lake = as.factor(lake),
    period = as.factor(period),
    size = as.factor(size),
    decay = as.factor(decay)
  )

# **Ice data --------------------------------------------------------------

data_all <- do_ts %>% 
  select(
    lake, date, year = water_year, do_mg_l, period, decay
  ) %>% 
  filter(
    period == 'ice',
    decay != 'increase'
  ) %>% 
  select(
    -period
  )


# 3. Identify individual decay periods ------------------------------------

# test <- ggplot(
#   data = data_all %>% 
#     filter(lake == 'cliff') #select lake timeseries to identify
# )+
#   geom_line(
#     mapping = aes(x = date, y = do_mg_l)
#   )+
#   theme_classic()+
#   facet_wrap(
#     ~year,
#     scales  = 'free',
#     nrow = 1
#   )
# test
# library(plotly)
# ggplotly(test)

#Added these manually into 'data/processed/do_ts_all_lakes.csv'


# 4. Add decay periods within the under ice period ------------------------



# 5. DO depletion function ------------------------------------------------

fit_DO_decay <- function(data, lake_name, year, decay_id, out_dir = "output", plot_results = TRUE) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # --- Data prep and validation ---
  data <- data %>%
    mutate(
      date = suppressWarnings(
        as.Date(
          date, 
          tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d")
        )
    )) %>%
    filter(!is.na(date)) %>%
    arrange(date)
  
  # Compute time in days safely
  if (nrow(data) == 0 || all(is.na(data$date))) {
    message("Skipping ", lake_name, " ", year, " ", decay_id, ": no valid date entries.")
    return(tibble(Lake = lake_name, Year = year, Decay = decay_id,
                  DO_init_fixed = NA, DO_init_free = NA,
                  Kexp_free = NA, Kexp_fixed = NA,
                  AIC_free = NA, AIC_fixed = NA))
  }
  
  data <- data %>%
    mutate(time_days = as.numeric(date - min(date, na.rm = TRUE))) %>%
    filter(!is.na(time_days), is.finite(time_days))
  
  # Filter invalid DO
  data <- data %>% filter(!is.na(do_mg_l), do_mg_l > 0)
  
  # Abort if too few valid points or constant time
  if (nrow(data) < 3 || length(unique(data$time_days)) < 2) {
    message("Skipping ", lake_name, " ", year, " ", decay_id, ": insufficient or constant time data.")
    return(tibble(Lake = lake_name, Year = year, Decay = decay_id,
                  DO_init_fixed = NA, DO_init_free = NA,
                  Kexp_free = NA, Kexp_fixed = NA,
                  AIC_free = NA, AIC_fixed = NA))
  }
  
  # --- Estimate starting values ---
  lm_fit <- lm(log(do_mg_l) ~ time_days, data = data)
  start_DO <- exp(coef(lm_fit)[1])
  start_K  <- abs(coef(lm_fit)[2])
  
  # --- Free fit ---
  nls_free <- tryCatch(
    nls(do_mg_l ~ DO_init * exp(-K * time_days),
        data = data,
        start = list(DO_init = start_DO, K = start_K),
        control = nls.control(maxiter = 200, warnOnly = TRUE)),
    error = function(e) {
      message("Free fit failed for ", lake_name, " ", year, " ", decay_id, ": ", e$message)
      NULL
    }
  )
  
  # --- Fixed fit ---
  DO_init_fixed <- data$do_mg_l[1]
  nls_fixed <- tryCatch(
    nls(do_mg_l ~ DO_init_fixed * exp(-K * (time_days - min(time_days))),
        data = data,
        start = list(K = start_K),
        control = nls.control(maxiter = 200, warnOnly = TRUE)),
    error = function(e) {
      message("Fixed fit failed for ", lake_name, " ", year, " ", decay_id, ": ", e$message)
      NULL
    }
  )
  
  # --- Extract results safely ---
  K_free <- if (!is.null(nls_free)) coef(nls_free)["K"] else NA
  K_fixed <- if (!is.null(nls_fixed)) coef(nls_fixed)["K"] else NA
  DO_init_free <- if (!is.null(nls_free)) coef(nls_free)["DO_init"] else NA
  AIC_free <- if (!is.null(nls_free)) AIC(nls_free) else NA
  AIC_fixed <- if (!is.null(nls_fixed)) AIC(nls_fixed) else NA
  
  # --- Plot if requested ---
  if (plot_results && !is.null(nls_free) && all(is.finite(range(data$time_days)))) {
    t_seq <- seq(min(data$time_days), max(data$time_days), length.out = 200)
    DO_pred_free  <- DO_init_free * exp(-K_free * (t_seq - min(data$time_days)))
    DO_pred_fixed <- DO_init_fixed * exp(-K_fixed * (t_seq - min(data$time_days)))
    
    df_pred <- tibble(
      time_days = rep(t_seq, 2),
      DO_pred = c(DO_pred_free, DO_pred_fixed),
      Model = rep(c("Free", "Fixed"), each = length(t_seq))
    )
    
    data <- data %>%
      mutate(
        res_free = if (!is.null(nls_free)) residuals(nls_free) else NA,
        res_fixed = if (!is.null(nls_fixed)) residuals(nls_fixed) else NA
      )
    
    # Fit plot
    p1 <- ggplot() +
      geom_point(data = data, aes(x = time_days, y = do_mg_l, color = "Observed"), size = 2) +
      geom_line(data = df_pred, aes(x = time_days, y = DO_pred, color = Model, linetype = Model), size = 1.1) +
      scale_color_manual(values = c("Observed" = "black", "Free" = "blue", "Fixed" = "darkgreen")) +
      scale_linetype_manual(values = c("Free" = "solid", "Fixed" = "dashed")) +
      labs(title = paste0("DO Decay — ", lake_name, " (", year, ") ", decay_id),
           y = "Dissolved Oxygen (mg/L)", x = "Time (days)",
           color = "", linetype = "") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", legend.box = "horizontal")
    
    # Residuals plots
    p2 <- ggplot(data) +
      geom_point(aes(x = time_days, y = res_free), color = "blue", alpha = 0.7) +
      geom_point(aes(x = time_days, y = res_fixed), color = "darkgreen", alpha = 0.7, shape = 17) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      labs(y = "Residual (mg/L)", x = "Time (days)", title = "Residuals Over Time") +
      theme_minimal(base_size = 13)
    
    p3 <- ggplot(data, aes(x = fitted(nls_free), y = res_free)) +
      geom_point(color = "blue", alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      labs(x = "Fitted DO (mg/L)", y = "Residual", title = "Residuals vs Fitted") +
      theme_minimal(base_size = 13)
    
    combined <- p1 / (p2 | p3)
    out_file <- file.path(out_dir, paste0("DO_decay_", lake_name, "_", year, "_", decay_id, ".png"))
    ggsave(out_file, plot = combined, width = 10, height = 8, dpi = 300)
  }
  
  # --- Return summary row ---
  tibble(
    Lake = lake_name,
    Year = year,
    Decay = decay_id,
    DO_init_fixed = DO_init_fixed,
    DO_init_free = DO_init_free,
    Kexp_free = K_free,
    Kexp_fixed = K_fixed,
    AIC_free = AIC_free,
    AIC_fixed = AIC_fixed
  )
}

# --- Batch run ---
data_all <- data_all %>%
  rename_with(tolower) %>%
  rename(lake = matches("lake|site"), year = matches("year")) %>%
  filter(!is.na(lake), !is.na(year), !is.na(decay))

results <- data_all %>%
  group_by(lake, year, decay) %>%
  group_split() %>%
  map_dfr(~ fit_DO_decay(.x,
                         lake_name = unique(.x$lake),
                         year = unique(.x$year),
                         decay_id = unique(.x$decay),
                         out_dir = "plots",
                         plot_results = TRUE))

write.csv(results, here("DO_decay_summary.csv"), row.names = FALSE)
print(results)


# 6. Plot the exponential decay values ------------------------------------

rslt_plt <- results %>% 
  clean_names() %>% 
  mutate(
    year = as.factor(year)
  )

ggplot(data = rslt_plt)+
  geom_jitter(
    mapping = aes(x = lake, y = kexp_free, color = year),
    size = 3,
    width = 0.15,
    alpha = 0.7
  ) +
  theme_classic()+
  scale_color_viridis_d()
  
  
  