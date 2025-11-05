# --- 1. Load and prepare data ---
data <- read.csv(here("data/test_do.csv"))
data$date <- as.Date(data$date)
data$time_days <- as.numeric(data$date - min(data$date))

# --- 2. Fix DO_init as the first observation ---
DO_init_fixed <- data$do_mg_l[1]
t0 <- min(data$time_days)

cat(sprintf("Fixed DO_init = %.3f mg/L at time t0 = %.1f days\n", DO_init_fixed, t0))

# --- 3. Fit model with nls(), estimating only K_exp ---
nls_fixed <- nls(do_mg_l ~ DO_init_fixed * exp(-K * (time_days - t0)),
                 data = data,
                 start = list(K = 0.01))  # a small positive starting value usually works

summary(nls_fixed)

# --- 4. Extract K estimate ---
K_exp_est <- coef(nls_fixed)["K"]
cat(sprintf("\nEstimated K_exp = %.6f per day\n", K_exp_est))

# --- 5. Compute half-life ---
t_half <- log(2) / K_exp_est
cat(sprintf("Half-life t1/2 = %.2f days\n", t_half))

# --- 6. Predicted curve ---
t_seq <- seq(min(data$time_days), max(data$time_days), length.out = 200)
DO_pred <- DO_init_fixed * exp(-K_exp_est * (t_seq - t0))

# --- 7. Plot observed data, fitted curve, and half-life ---
plot(data$time_days, data$do_mg_l, pch = 19, col = "red",
     xlab = "Time (days since first measurement)",
     ylab = "Dissolved Oxygen (mg/L)",
     main = "Exponential Decay with Fixed DO_init (first observation)")
lines(t_seq, DO_pred, col = "blue", lwd = 2)
abline(v = t_half, col = "darkgreen", lty = 2, lwd = 2)
abline(h = DO_init_fixed / 2, col = "gray40", lty = 3)
text(t_half, DO_init_fixed / 2,
     labels = sprintf("Half-life = %.2f days", t_half),
     pos = 4, col = "darkgreen", cex = 0.9)
legend("topright",
       legend = c("Observed DO", "nls fit", "Half-life", "Half DO_init"),
       col = c("red", "blue", "darkgreen", "gray40"),
       pch = c(19, NA, NA, NA),
       lty = c(NA, 1, 2, 3),
       lwd = c(NA, 2, 2, 1))




#Test free and set DO models

# --- 1. Load and prepare data ---
data <- read.csv(here("data/test_do.csv"))
data$date <- as.Date(data$date)
data$time_days <- as.numeric(data$date - min(data$date))

# --- 2. Get starting values for free model ---
lm_fit <- lm(log(do_mg_l) ~ time_days, data = data)
start_DO_init <- exp(coef(lm_fit)[1])
start_K <- -coef(lm_fit)[2]

# --- 3. Fit both models ------------------------------------------------------

# (A) Free DO_init model
nls_free <- nls(do_mg_l ~ DO_init * exp(-K * (time_days - min(time_days))),
                data = data,
                start = list(DO_init = start_DO_init, K = start_K),
                control = nls.control(maxiter = 100))

# (B) Fixed DO_init model (use first observation)
DO_init_fixed <- data$do_mg_l[1]
t0 <- min(data$time_days)
nls_fixed <- nls(do_mg_l ~ DO_init_fixed * exp(-K * (time_days - t0)),
                 data = data,
                 start = list(K = start_K),
                 control = nls.control(maxiter = 100))

# --- 4. Compare models numerically ------------------------------------------
AIC_free  <- AIC(nls_free)
AIC_fixed <- AIC(nls_fixed)
RSS_free  <- sum(residuals(nls_free)^2)
RSS_fixed <- sum(residuals(nls_fixed)^2)

cat("Model comparison:\n")
cat(sprintf("  Free  DO_init: AIC = %.2f, RSS = %.5f\n", AIC_free, RSS_free))
cat(sprintf("  Fixed DO_init: AIC = %.2f, RSS = %.5f\n", AIC_fixed, RSS_fixed))
delta_AIC <- AIC_fixed - AIC_free
cat(sprintf("\n  ΔAIC (Fixed - Free) = %.2f\n", delta_AIC))

# --- 5. Compute pseudo-R² ----------------------------------------------------
y <- data$do_mg_l
yhat_free <- fitted(nls_free)
yhat_fixed <- fitted(nls_fixed)
SS_tot <- sum((y - mean(y))^2)
R2_free <- 1 - sum((y - yhat_free)^2) / SS_tot
R2_fixed <- 1 - sum((y - yhat_fixed)^2) / SS_tot
cat(sprintf("\nR² (free)  = %.4f\nR² (fixed) = %.4f\n", R2_free, R2_fixed))

# --- 6. Create residual data for plotting ------------------------------------
residuals_df <- data.frame(
  time_days = data$time_days,
  residual_free = residuals(nls_free),
  residual_fixed = residuals(nls_fixed)
)

# --- 7. Plot observed data + fits --------------------------------------------
t_seq <- seq(min(data$time_days), max(data$time_days), length.out = 200)
DO_pred_free <- coef(nls_free)["DO_init"] * exp(-coef(nls_free)["K"] * (t_seq - min(data$time_days)))
DO_pred_fixed <- DO_init_fixed * exp(-coef(nls_fixed)["K"] * (t_seq - t0))

par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

# (A) Observed data and fits
plot(data$time_days, data$do_mg_l, pch = 19, col = "black",
     xlab = "Time (days since first measurement)",
     ylab = "Dissolved Oxygen (mg/L)",
     main = "Free vs. Fixed DO_init (nls fits)")
lines(t_seq, DO_pred_free, col = "blue", lwd = 2)
lines(t_seq, DO_pred_fixed, col = "darkgreen", lwd = 2, lty = 2)
legend("topright",
       legend = c("Observed", "nls free DO_init", "nls fixed DO_init"),
       col = c("black", "blue", "darkgreen"),
       pch = c(19, NA, NA), lty = c(NA, 1, 2), lwd = c(NA, 2, 2))

# (B) Residuals comparison
plot(residuals_df$time_days, residuals_df$residual_free,
     type = "b", pch = 19, col = "blue",
     xlab = "Time (days)", ylab = "Residual (Observed - Fitted)",
     main = "Residuals Comparison: Free vs. Fixed DO_init",
     ylim = range(c(residuals_df$residual_free, residuals_df$residual_fixed)))
lines(residuals_df$time_days, residuals_df$residual_fixed,
      type = "b", pch = 17, col = "darkgreen", lty = 2)
abline(h = 0, col = "gray40", lty = 3)
legend("topright",
       legend = c("Free DO_init", "Fixed DO_init"),
       col = c("blue", "darkgreen"),
       pch = c(19, 17),
       lty = c(1, 2),
       lwd = 2)

# --- 5. Predicted values and residuals ---------------------------------------
data$resid_free  <- residuals(nls_free)
data$resid_fixed <- residuals(nls_fixed)
data$fitted_free  <- fitted(nls_free)
data$fitted_fixed <- fitted(nls_fixed)

# --- 6. Diagnostic plots ------------------------------------------------------

par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))

# (1) Observed vs fitted - Free
plot(data$time_days, data$do_mg_l, pch = 19, col = "gray30",
     main = "Free DO_init: Observed vs Fitted",
     xlab = "Time (days)", ylab = "DO (mg/L)")
lines(data$time_days, data$fitted_free, col = "blue", lwd = 2)

# (2) Observed vs fitted - Fixed
plot(data$time_days, data$do_mg_l, pch = 19, col = "gray30",
     main = "Fixed DO_init: Observed vs Fitted",
     xlab = "Time (days)", ylab = "DO (mg/L)")
lines(data$time_days, data$fitted_fixed, col = "darkgreen", lwd = 2)

# (3) Residuals over time - Free
plot(data$time_days, data$resid_free, type = "b", pch = 19, col = "blue",
     main = "Residuals vs Time (Free)",
     xlab = "Time (days)", ylab = "Residuals")
abline(h = 0, col = "gray40", lty = 2)

# (4) Residuals over time - Fixed
plot(data$time_days, data$resid_fixed, type = "b", pch = 17, col = "darkgreen",
     main = "Residuals vs Time (Fixed)",
     xlab = "Time (days)", ylab = "Residuals")
abline(h = 0, col = "gray40", lty = 2)

# (5) Residuals vs fitted - Free
plot(data$fitted_free, data$resid_free, pch = 19, col = "blue",
     main = "Residuals vs Fitted (Free)",
     xlab = "Fitted DO", ylab = "Residuals")
abline(h = 0, col = "gray40", lty = 2)

# (6) Residuals vs fitted - Fixed
plot(data$fitted_fixed, data$resid_fixed, pch = 17, col = "darkgreen",
     main = "Residuals vs Fitted (Fixed)",
     xlab = "Fitted DO", ylab = "Residuals")
abline(h = 0, col = "gray40", lty = 2)

# --- 7. QQ-plots (Normality check) -------------------------------------------
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
qqnorm(data$resid_free, main = "QQ Plot (Free DO_init)", pch = 19, col = "blue")
qqline(data$resid_free, col = "gray40", lwd = 2)
qqnorm(data$resid_fixed, main = "QQ Plot (Fixed DO_init)", pch = 17, col = "darkgreen")
qqline(data$resid_fixed, col = "gray40", lwd = 2)
