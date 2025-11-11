#!/usr/bin/env Rscript

#' PhD Unemployment Annual Trend Analysis
#'
#' Analyzes 2000-2025 PhD unemployment trends using CPS ASEC (March) data.
#' Since CPS ASEC data is only collected annually in March, this analysis focuses
#' on long-term trends rather than seasonal patterns.
#'
#' Date: 2025-11-11

library(here)
library(data.table)
library(mgcv)
library(ggplot2)

# Source functions
source(here("R", "file-io.R"))
source(here("R", "data-processing.R"))

cat("=== PhD Unemployment Annual Trend Analysis ===\n\n")
cat("Date:", format(Sys.Date(), "%Y-%m-%d"), "\n")
cat("Note: CPS ASEC data is collected annually in March only\n\n")

# ============================================================================
# 1. LOAD AND PROCESS DATA
# ============================================================================

cat("1. Loading CPS data (2000-2025)...\n")
rds_file <- here("data-raw", "ipums_data.rds")

if (!file.exists(rds_file)) {
  stop("CPS data file not found: ", rds_file, "\n",
       "Please run download-full-dataset.R first.")
}

cps_data <- readRDS(rds_file)
cat("   Loaded:", nrow(cps_data), "observations\n")

cat("2. Processing PhD unemployment rates...\n")
phd_monthly <- process_cps_data(cps_data)

# Filter to March only (ASEC supplement)
phd_annual <- phd_monthly[!is.na(phd_monthly$unemployment_rate), ]
cat("   Processed:", nrow(phd_annual), "annual observations (March ASEC)\n")

# Add time index for GAM
phd_annual$time_index <- seq_len(nrow(phd_annual))
phd_annual$year_numeric <- as.numeric(phd_annual$YEAR)

cat("\n   Time range:", min(phd_annual$YEAR), "-", max(phd_annual$YEAR), "\n")
cat("   Sample size per year (mean):", round(mean(phd_annual$n_obs)), "\n")
cat("   Overall mean unemployment rate:", sprintf("%.2f%%", 100 * mean(phd_annual$unemployment_rate)), "\n")
cat("   Min unemployment rate:", sprintf("%.2f%%", 100 * min(phd_annual$unemployment_rate)),
    " (", phd_annual$YEAR[which.min(phd_annual$unemployment_rate)], ")\n", sep = "")
cat("   Max unemployment rate:", sprintf("%.2f%%", 100 * max(phd_annual$unemployment_rate)),
    " (", phd_annual$YEAR[which.max(phd_annual$unemployment_rate)], ")\n\n", sep = "")

# ============================================================================
# 2. FIT TIME TREND MODEL
# ============================================================================

cat("3. Fitting time trend GAM...\n")
cat("   Formula: unemployment_rate ~ s(time_index)\n")
cat("   Method: REML\n\n")

# Fit GAM with smooth trend only (no seasonal component)
model <- mgcv::gam(
  unemployment_rate ~ s(time_index, bs = "cr", k = 10),
  data = phd_annual,
  method = "REML"
)

cat("   Model converged:", model$converged, "\n")
cat("   Effective degrees of freedom:", round(sum(model$edf), 2), "\n")
cat("   R-squared:", sprintf("%.3f", summary(model)$r.sq), "\n")
cat("   Deviance explained:", sprintf("%.1f%%", 100 * summary(model)$dev.expl), "\n\n")

# ============================================================================
# 3. MODEL DIAGNOSTICS
# ============================================================================

cat("4. Running model diagnostics...\n")

# Residuals
residuals_model <- residuals(model)
cat("   Residual SD:", sprintf("%.4f", sd(residuals_model)), "\n")

# Durbin-Watson (autocorrelation)
dw_stat <- sum(diff(residuals_model)^2) / sum(residuals_model^2)
cat("   Durbin-Watson statistic:", sprintf("%.3f", dw_stat),
    ifelse(dw_stat > 1.5 & dw_stat < 2.5, "(no strong autocorrelation)", "(check autocorrelation)"), "\n")

# Shapiro-Wilk (normality)
shapiro_test <- shapiro.test(residuals_model)
cat("   Shapiro-Wilk normality: p =", sprintf("%.3f", shapiro_test$p.value),
    ifelse(shapiro_test$p.value > 0.05, "(normal)", "(non-normal)"), "\n\n")

# ============================================================================
# 4. COMPARE LINEAR VS SMOOTH TREND
# ============================================================================

cat("5. Comparing model specifications...\n\n")

# Linear model
model_linear <- lm(unemployment_rate ~ year_numeric, data = phd_annual)

# Comparison
comparison <- data.frame(
  model_name = c("Linear trend", "Smooth GAM"),
  aic = c(AIC(model_linear), AIC(model)),
  bic = c(BIC(model_linear), BIC(model)),
  r_squared = c(summary(model_linear)$r.squared, summary(model)$r.sq),
  edf = c(2, round(sum(model$edf), 2))
)
comparison$rank <- rank(comparison$aic)

print(comparison)
cat("\n")

# ============================================================================
# 5. VISUALIZE TRENDS
# ============================================================================

cat("6. Creating diagnostic plots...\n")

# Get predictions
phd_annual$fitted <- fitted(model)
phd_annual$fitted_linear <- fitted(model_linear)

plot_file <- here("reports", "phd-annual-trend.pdf")
pdf(plot_file, width = 12, height = 8)

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# Panel 1: Time series with fitted curves
plot(phd_annual$YEAR, phd_annual$unemployment_rate,
     type = "b", pch = 19, col = "black",
     xlab = "Year", ylab = "Unemployment Rate",
     main = "PhD Unemployment Rate (March ASEC)",
     ylim = c(0, max(phd_annual$unemployment_rate) * 1.1))
lines(phd_annual$YEAR, phd_annual$fitted, col = "blue", lwd = 2)
lines(phd_annual$YEAR, phd_annual$fitted_linear, col = "red", lwd = 2, lty = 2)
legend("topleft",
       legend = c("Observed", "GAM smooth", "Linear trend"),
       col = c("black", "blue", "red"),
       lty = c(1, 1, 2),
       pch = c(19, NA, NA),
       lwd = c(1, 2, 2))

# Panel 2: Residuals over time
plot(phd_annual$YEAR, residuals_model,
     type = "b", pch = 19,
     xlab = "Year", ylab = "Residuals",
     main = "GAM Residuals Over Time")
abline(h = 0, col = "red", lty = 2)

# Panel 3: QQ plot
qqnorm(residuals_model, main = "Normal Q-Q Plot")
qqline(residuals_model, col = "red")

# Panel 4: Residuals vs fitted
plot(fitted(model), residuals_model,
     pch = 19,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)
lowess_fit <- lowess(fitted(model), residuals_model)
lines(lowess_fit, col = "blue", lwd = 2)

dev.off()

cat("   Saved diagnostic plots to:", plot_file, "\n\n")

# ============================================================================
# 6. SAVE RESULTS
# ============================================================================

cat("7. Saving results...\n")

# Save fitted models
model_file <- here("models", "phd-annual-trend-gam.rds")
dir.create(here("models"), showWarnings = FALSE, recursive = TRUE)
saveRDS(list(
  model_gam = model,
  model_linear = model_linear,
  data = phd_annual,
  comparison = comparison,
  fit_date = Sys.Date()
), model_file)
cat("   Saved models to:", model_file, "\n")

# Save time series for further analysis
output_file <- here("data", "phd-annual-unemployment.rds")
dir.create(here("data"), showWarnings = FALSE, recursive = TRUE)
saveRDS(phd_annual, output_file)
cat("   Saved processed data to:", output_file, "\n\n")

# ============================================================================
# 7. SUMMARY
# ============================================================================

cat("=== ANALYSIS COMPLETE ===\n\n")
cat("Key findings:\n")
cat("  - Model explains", sprintf("%.1f%%", 100 * summary(model)$dev.expl), "of variance in PhD unemployment\n")
cat("  - Smooth trend uses", round(sum(model$edf), 1), "effective degrees of freedom\n")
cat("  - Unemployment rate range:", sprintf("%.2f%% - %.2f%%",
     100 * min(phd_annual$unemployment_rate),
     100 * max(phd_annual$unemployment_rate)), "\n")

# Calculate trend direction
first_fitted <- fitted(model)[1]
last_fitted <- fitted(model)[nrow(phd_annual)]
trend_direction <- ifelse(last_fitted > first_fitted, "increasing", "decreasing")
trend_magnitude <- abs(last_fitted - first_fitted)

cat("  - Overall trend:", trend_direction, "by", sprintf("%.2f%%", 100 * trend_magnitude),
    "from", phd_annual$YEAR[1], "to", phd_annual$YEAR[nrow(phd_annual)], "\n\n")

cat("Limitations:\n")
cat("  - Data limited to March (ASEC supplement) - no seasonal analysis possible\n")
cat("  - Cannot model within-year seasonal patterns\n")
cat("  - For monthly data, would need basic CPS monthly files\n\n")

cat("Next steps:\n")
cat("  1. Review diagnostic plots:", plot_file, "\n")
cat("  2. Compare PhD trends to other education levels\n")
cat("  3. Consider acquiring monthly CPS data for seasonal analysis\n")
cat("  4. Investigate cyclical patterns vs economic indicators\n\n")
