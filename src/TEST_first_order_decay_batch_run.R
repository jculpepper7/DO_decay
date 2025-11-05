

# 1. Libraries ------------------------------------------------------------

library(tidyverse)
library(here)

# 2. Import data ----------------------------------------------------------


# **All DO data -----------------------------------------------------------

do_ts <- read_csv(here('data/processed/do_ts_all_lakes.csv')) %>% 
  mutate(
    lake = as.factor(lake),
    period = as.factor(period),
    size = as.factor(size)
  )

# **Ice data --------------------------------------------------------------

data_all <- do_ts %>% 
  select(
    lake, date, year = water_year, do_mg_l, period
  ) %>% 
  filter(
    period == 'ice'
  ) %>% 
  select(
    -period
  )


# 3. DO Depletion function ------------------------------------------------

fit_DO_decay <- function(data, plot_results = TRUE, lake_name = "Unknown Lake") {
  library(ggplot2)
  library(dplyr)
  library(patchwork)  # for multi-panel ggplot layout
  
  # --- Ensure structure ---
  stopifnot(all(c("date", "do_mg_l") %in% names(data)))
  
  # --- Prepare data ---
  data <- data %>%
    mutate(date = as.Date(date),
           time_days = as.numeric(date - min(date)))
  
  # --- Starting values from log-linear fit ---
  lm_fit <- lm(log(do_mg_l) ~ time_days, data = data)
  start_DO_init <- exp(coef(lm_fit)[1])
  start_K <- -coef(lm_fit)[2]
  
  # --- Fit both models ---
  nls_free <- nls(do_mg_l ~ DO_init * exp(-K * (time_days - min(time_days))),
                  data = data,
                  start = list(DO_init = start_DO_init, K = start_K),
                  control = nls.control(maxiter = 100))
  
  DO_init_fixed <- data$do_mg_l[1]
  t0 <- min(data$time_days)
  
  nls_fixed <- nls(do_mg_l ~ DO_init_fixed * exp(-K * (time_days - t0)),
                   data = data,
                   start = list(K = start_K),
                   control = nls.control(maxiter = 100))
  
  # --- Extract results ---
  coef_free  <- coef(nls_free)
  coef_fixed <- coef(nls_fixed)
  K_free  <- coef_free["K"]
  K_fixed <- coef_fixed["K"]
  
  AIC_free  <- AIC(nls_free)
  AIC_fixed <- AIC(nls_fixed)
  
  y <- data$do_mg_l
  yhat_free  <- fitted(nls_free)
  yhat_fixed <- fitted(nls_fixed)
  SS_tot <- sum((y - mean(y))^2)
  
  R2_free  <- 1 - sum((y - yhat_free)^2) / SS_tot
  R2_fixed <- 1 - sum((y - yhat_fixed)^2) / SS_tot
  delta_AIC <- AIC_fixed - AIC_free
  
  # --- Print concise summary ---
  cat("\n==========", lake_name, "==========\n")
  cat(sprintf("Free  DO_init: K = %.5f, AIC = %.2f, R² = %.3f\n", K_free,  AIC_free,  R2_free))
  cat(sprintf("Fixed DO_init: K = %.5f, AIC = %.2f, R² = %.3f\n", K_fixed, AIC_fixed, R2_fixed))
  cat(sprintf("ΔAIC (Fixed - Free) = %.2f\n", delta_AIC))
  if(delta_AIC > 2)  cat("→ Free model fits meaningfully better.\n")
  if(delta_AIC < -2) cat("→ Fixed model fits meaningfully better.\n")
  if(abs(delta_AIC) <= 2) cat("→ Both models fit comparably well.\n")
  
  # --- Plot diagnostics (ggplot) ---
  if (plot_results) {
    # Predictions
    t_seq <- seq(min(data$time_days), max(data$time_days), length.out = 200)
    DO_pred_free  <- coef_free["DO_init"] * exp(-K_free * (t_seq - min(data$time_days)))
    DO_pred_fixed <- DO_init_fixed * exp(-K_fixed * (t_seq - t0))
    
    df_pred <- data.frame(
      time_days = rep(t_seq, 2),
      DO_pred = c(DO_pred_free, DO_pred_fixed),
      Model = rep(c("Free", "Fixed"), each = length(t_seq))
    )
    
    # Residuals
    data <- data %>%
      mutate(res_free = residuals(nls_free),
             res_fixed = residuals(nls_fixed))
    
    # Plot 1 — Fits with legend
    p1 <- ggplot() +
      geom_point(data = data, aes(x = time_days, y = do_mg_l, color = "Observed"), size = 2) +
      geom_line(data = df_pred,
                aes(x = time_days, y = DO_pred, color = Model, linetype = Model),
                size = 1.1) +
      scale_color_manual(values = c("Observed" = "black",
                                    "Free" = "blue",
                                    "Fixed" = "darkgreen")) +
      scale_linetype_manual(values = c("Free" = "solid", "Fixed" = "dashed")) +
      labs(title = paste("DO Decay Fits —", lake_name),
           y = "Dissolved Oxygen (mg/L)", x = "Time (days)", color = "", linetype = "") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "top",
            legend.box = "horizontal")
    
    # Plot 2 — Residuals vs Time
    p2 <- ggplot(data) +
      geom_point(aes(x = time_days, y = res_free), color = "blue", alpha = 0.7) +
      geom_point(aes(x = time_days, y = res_fixed), color = "darkgreen", alpha = 0.7, shape = 17) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      labs(y = "Residual (mg/L)", x = "Time (days)", title = "Residuals Over Time") +
      theme_minimal(base_size = 13)
    
    # Plot 3 — Residuals vs Fitted (Free)
    p3 <- ggplot(data, aes(x = fitted(nls_free), y = res_free)) +
      geom_point(color = "blue", alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      labs(x = "Fitted DO (Free Model)", y = "Residual", title = "Residuals vs Fitted") +
      theme_minimal(base_size = 13)
    
    # Combine plots using patchwork
    p_combined <- p1 / (p2 | p3)
    print(p_combined)
  }
  
  
  # --- Return key results ---
  return(list(
    lake = lake_name,
    DO_init_fixed = DO_init_fixed,
    free = list(K = K_free, AIC = AIC_free, R2 = R2_free),
    fixed = list(K = K_fixed, AIC = AIC_fixed, R2 = R2_fixed),
    delta_AIC = delta_AIC,
    model_free = nls_free,
    model_fixed = nls_fixed
  ))
}

#TEST run
fit_DO_decay(subset(data_all, lake == "castle" & year == 2021),
             plot_results = TRUE,
             lake_name = "Castle Lake 2021")
