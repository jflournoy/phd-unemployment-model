#!/usr/bin/env Rscript

#' PhD Unemployment Monthly Trend and Seasonal Analysis
#'
#' Applies validated seasonal GAM model to real 2000-2025 CPS monthly data.
#' Uses proper WTFINL/ASECWT weighting and identifies:
#' - Long-term trends in PhD unemployment
#' - Seasonal patterns (academic calendar effects)
#' - Model fit diagnostics
#'
#' Date: 2025-11-11

library(here)
library(data.table)
library(mgcv)
library(ggplot2)

# Source functions
source(here("R", "file-io.R"))
source(here("R", "data-processing.R"))
source(here("R", "seasonal-gam.R"))

cat("=== PhD Unemployment Monthly Trend and Seasonal Analysis ===\n\n")
cat("Date:", format(Sys.Date(), "%Y-%m-%d"), "\n")
cat("Using validated seasonal GAM model with monthly CPS data\n\n")

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

cat("2. Processing PhD monthly unemployment rates...\n")
phd_monthly <- process_cps_data(cps_data)
cat("   Processed:", nrow(phd_monthly), "monthly observations\n")

# Verify we have the required columns
required_cols <- c("YEAR", "MONTH", "date", "time_index", "unemployment_rate", "n_obs")
if (!all(required_cols %in% names(phd_monthly))) {
  stop("Missing required columns. Expected: ", paste(required_cols, collapse=", "))
}

cat("\n   Time range:", min(phd_monthly$YEAR), "-", max(phd_monthly$YEAR), "\n")
cat("   Months covered:", nrow(phd_monthly), "(", min(phd_monthly$MONTH), "-", max(phd_monthly$MONTH), "across years)\n")
cat("   Sample size per month (mean):", round(mean(phd_monthly$n_obs)), "\n")
cat("   Overall mean unemployment rate:", sprintf("%.2f%%", 100 * mean(phd_monthly$unemployment_rate, na.rm = TRUE)), "\n")
cat("   Date range:", format(min(phd_monthly$date), "%Y-%m-%d"), "to", format(max(phd_monthly$date), "%Y-%m-%d"), "\n\n")

# ============================================================================
# 2. FIT SEASONAL GAM MODEL
# ============================================================================

cat("3. Fitting seasonal GAM model...\n")
cat("   Formula: unemployment_rate ~ s(time_index) + s(month, bs='cc')\n")
cat("   Method: REML\n\n")

# Add lowercase month column for GAM (expects lowercase)
phd_monthly$month <- phd_monthly$MONTH

# Fit seasonal GAM with cyclic spline for month
model <- fit_seasonal_gam(phd_monthly, k_month = 10, k_trend = 20)

cat("   Model converged:", model$converged, "\n")
cat("   Effective degrees of freedom:", round(sum(model$edf), 1), "\n")
cat("   R-squared:", sprintf("%.3f", summary(model)$r.sq), "\n")
cat("   Deviance explained:", sprintf("%.1f%%", 100 * summary(model)$dev.expl), "\n\n")

# ============================================================================
# 3. EXTRACT COMPONENTS
# ============================================================================

cat("4. Extracting model components...\n")

seasonal_effects <- extract_seasonal_component(model, phd_monthly)
trend_effects <- extract_trend_component(model, phd_monthly)

# Calculate seasonal amplitude
seasonal_amplitude <- (max(seasonal_effects$seasonal_effect) - min(seasonal_effects$seasonal_effect)) / 2
cat("   Seasonal amplitude (peak-to-trough/2):", sprintf("%.2f%%", 100 * seasonal_amplitude), "\n")

peak_month <- seasonal_effects$month[which.max(seasonal_effects$seasonal_effect)]
low_month <- seasonal_effects$month[which.min(seasonal_effects$seasonal_effect)]
cat("   Peak unemployment month:", month.name[peak_month], "\n")
cat("   Low unemployment month:", month.name[low_month], "\n\n")

# ============================================================================
# 4. MODEL DIAGNOSTICS
# ============================================================================

cat("5. Running model diagnostics...\n")

# Residual autocorrelation
residuals <- residuals(model)
dw_stat <- sum(diff(residuals)^2) / sum(residuals^2)
cat("   Durbin-Watson statistic:", sprintf("%.3f", dw_stat),
    ifelse(dw_stat > 1.5 & dw_stat < 2.5, "(good)", "(check autocorrelation)"), "\n")

# Residual normality
if (length(residuals) > 5000) {
  shapiro_test <- shapiro.test(sample(residuals, 5000))
} else {
  shapiro_test <- shapiro.test(residuals)
}
cat("   Shapiro-Wilk normality:", sprintf("p = %.3f", shapiro_test$p.value),
    ifelse(shapiro_test$p.value > 0.05, "(normal)", "(non-normal)"), "\n\n")

# ============================================================================
# 5. COMPARE MODEL FORMULATIONS
# ============================================================================

cat("6. Comparing alternative model specifications...\n\n")

comparison <- compare_model_formulations(phd_monthly)
print(comparison)
cat("\n")

# ============================================================================
# 6. VISUALIZE DECOMPOSITION
# ============================================================================

cat("7. Creating diagnostic plots using ggplot2...\n")

# Create ggplot2 decomposition plots
decomp_plots <- plot_seasonal_decomposition_ggplot(model, phd_monthly)

# Save decomposition plots
plot_file <- here("reports", "phd-monthly-seasonal-decomposition.pdf")
pdf(plot_file, width = 12, height = 10)

# Combine plots vertically using patchwork if available, otherwise plot separately
if (requireNamespace("patchwork", quietly = TRUE)) {
  print(decomp_plots$observed_fitted / decomp_plots$trend / decomp_plots$seasonal)
} else {
  # Plot each panel separately
  print(decomp_plots$observed_fitted)
  print(decomp_plots$trend)
  print(decomp_plots$seasonal)
}

dev.off()

cat("   Saved diagnostic plots to:", plot_file, "\n\n")

# Time series visualization with ggplot2
cat("8. Creating time series visualization using ggplot2...\n")

# Add fitted values and components to data
phd_monthly$fitted <- fitted(model)
phd_monthly$trend <- trend_effects$trend_effect[match(phd_monthly$time_index, trend_effects$time_index)]
phd_monthly$seasonal <- seasonal_effects$seasonal_effect[match(phd_monthly$MONTH, seasonal_effects$month)]

# Create ggplot2 time series plots
ts_plots <- create_timeseries_ggplot(phd_monthly, seasonal_effects)

# Save time series plots
ts_plot_file <- here("reports", "phd-monthly-timeseries.pdf")
pdf(ts_plot_file, width = 14, height = 8)

# Combine plots vertically
if (requireNamespace("patchwork", quietly = TRUE)) {
  print(ts_plots$timeseries / ts_plots$seasonal)
} else {
  # Plot each panel separately
  print(ts_plots$timeseries)
  print(ts_plots$seasonal)
}

dev.off()

cat("   Saved time series plots to:", ts_plot_file, "\n\n")

# ============================================================================
# 7. SAVE RESULTS
# ============================================================================

cat("9. Saving results...\n")

# Save fitted model
model_file <- here("models", "phd-monthly-seasonal-gam.rds")
dir.create(here("models"), showWarnings = FALSE, recursive = TRUE)
saveRDS(list(
  model = model,
  data = phd_monthly,
  seasonal = seasonal_effects,
  trend = trend_effects,
  comparison = comparison,
  fit_date = Sys.Date()
), model_file)
cat("   Saved model to:", model_file, "\n")

# Save time series for further analysis
output_file <- here("data", "phd-monthly-unemployment.rds")
dir.create(here("data"), showWarnings = FALSE, recursive = TRUE)
saveRDS(phd_monthly, output_file)
cat("   Saved processed data to:", output_file, "\n\n")

# ============================================================================
# 8. SUMMARY
# ============================================================================

cat("=== ANALYSIS COMPLETE ===\n\n")
cat("Data summary:\n")
cat("  - Monthly observations:", nrow(phd_monthly), "(2000-2025)\n")
cat("  - Mean sample size per month:", round(mean(phd_monthly$n_obs)), "PhD holders\n")
cat("  - Overall unemployment rate:", sprintf("%.2f%%", 100 * mean(phd_monthly$unemployment_rate)), "\n\n")

cat("Model performance:\n")
cat("  - Deviance explained:", sprintf("%.1f%%", 100 * summary(model)$dev.expl), "\n")
cat("  - R-squared:", sprintf("%.3f", summary(model)$r.sq), "\n")
cat("  - Effective degrees of freedom:", round(sum(model$edf), 1), "\n")
cat("  - Model convergence:", model$converged, "\n\n")

cat("Seasonal patterns:\n")
cat("  - Amplitude:", sprintf("%.2f%%", 100 * seasonal_amplitude), "\n")
cat("  - Peak month:", month.name[peak_month], "\n")
cat("  - Low month:", month.name[low_month], "\n")
cat("  - Peak-to-low difference:", sprintf("%.2f%%",
     100 * (max(seasonal_effects$seasonal_effect) - min(seasonal_effects$seasonal_effect))), "\n\n")

cat("Next steps:\n")
cat("  1. Review diagnostic plots:", plot_file, "\n")
cat("  2. Examine time series visualization:", ts_plot_file, "\n")
cat("  3. Compare PhD seasonal patterns to other education levels\n")
cat("  4. Investigate relationship with academic calendar (graduation, hiring cycles)\n")
cat("  5. Compare with general unemployment seasonal patterns\n\n")
