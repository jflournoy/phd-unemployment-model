#!/usr/bin/env Rscript
# Generate report figures from v4 time-varying equilibrium model
# Usage: Rscript R/generate-report-figures.R

library(cmdstanr)
library(data.table)

cat("Loading v4 fit...\n")
wrap <- readRDS("models/ode-state-space-edu-parallel-fit-v7.rds")
fit <- wrap$fit
stan_data <- wrap$stan_data

cat("Extracting draws...\n")
draws <- fit$draws(format = "draws_array")

counts <- readRDS("data/education-spectrum-counts.rds")
dt <- as.data.table(counts)

edu_levels <- stan_data$education_levels
time_points <- stan_data$time_points
n_time <- stan_data$T
n_edu <- stan_data$N_edu
month_vec <- stan_data$month
year_frac <- stan_data$year_frac

param_names <- dimnames(draws)[[3]]

cat("Computing residuals and equilibrium trajectories...\n")

# Pre-allocate
res_list <- list()
idx <- 1

for (ei in 1:n_edu) {
  edu_name <- edu_levels[ei]
  cat(sprintf("  %s...\n", edu_name))

  for (ti in 1:n_time) {
    # Observed data
    yr <- floor(year_frac[ti])
    mo <- month_vec[ti]
    obs_row <- dt[education == edu_name & year == yr & month == mo]
    obs_val <- if (nrow(obs_row) > 0) obs_row$unemployment_rate else NA

    # Parameter names
    u_pn <- sprintf("u[%d,%d]", ti, ei)
    trend_pn <- sprintf("u_trend[%d,%d]", ti, ei)
    eq_pn <- sprintf("u_eq[%d,%d]", ti, ei)

    if (!(u_pn %in% param_names && trend_pn %in% param_names && eq_pn %in% param_names)) next

    u_d <- as.vector(draws[, , u_pn])
    trend_d <- as.vector(draws[, , trend_pn])
    eq_d <- as.vector(draws[, , eq_pn])

    # Observation residual: observed - full model
    obs_resid <- if (!is.na(obs_val)) obs_val - u_d else rep(NA, length(u_d))

    # Seasonal component: u - u_trend (full - trend)
    seasonal_comp <- u_d - trend_d

    # Shock component: u_trend - u_eq (trend - equilibrium)
    shock_comp <- trend_d - eq_d

    res_list[[idx]] <- data.table(
      time_index = ti, edu_idx = ei, education = edu_name,
      year = yr, month = mo,
      year_frac = year_frac[ti],
      observed = if (!is.na(obs_val)) obs_val else NA_real_,
      # Observation residual
      obs_resid_mean = if (!is.na(obs_val)) mean(obs_resid, na.rm = TRUE) else NA_real_,
      obs_resid_q5  = if (!is.na(obs_val)) quantile(obs_resid, 0.05, na.rm = TRUE) else NA_real_,
      obs_resid_q95 = if (!is.na(obs_val)) quantile(obs_resid, 0.95, na.rm = TRUE) else NA_real_,
      # Equilibrium trajectory
      eq_mean = mean(eq_d),
      eq_q5 = quantile(eq_d, 0.05),
      eq_q95 = quantile(eq_d, 0.95),
      # Shock component
      shock_mean = mean(shock_comp),
      shock_q5 = quantile(shock_comp, 0.05),
      shock_q95 = quantile(shock_comp, 0.95),
      # Full model
      u_mean = mean(u_d),
      u_q5 = quantile(u_d, 0.05),
      u_q95 = quantile(u_d, 0.95),
      # Trend
      trend_mean = mean(trend_d),
      trend_q5 = quantile(trend_d, 0.05),
      trend_q95 = quantile(trend_d, 0.95)
    )
    idx <- idx + 1
  }
}

results <- rbindlist(res_list)
results[, date := as.Date(paste(year, month, "01", sep = "-"))]

# Education labels for plotting
edu_labels_pretty <- gsub("_", " ", edu_levels)
results[, edu_label := factor(education,
  levels = rev(c("less_than_hs", "high_school", "some_college", "bachelors",
                 "masters", "phd", "professional")),
  labels = rev(c("less than hs", "high school", "some college", "bachelors",
                 "masters", "phd", "professional")))]

# Filter to 2015+ for legibility
results_plot <- results[year >= 2015]

# ===========================================================================
# PLOT 1: Observation Residual (observed - full model prediction)
# ===========================================================================
cat("\nPlotting observation residuals...\n")
png("reports/figures/residual-observation.png",
    width = 12, height = 9, units = "in", res = 150)

par(mfrow = c(4, 2), mar = c(3, 3.5, 2.5, 1), mgp = c(2, 0.6, 0))

for (ed in rev(levels(results_plot$edu_label))) {
  rd <- results_plot[edu_label == ed][order(date)]

  plot(rd$date, rd$obs_resid_mean * 100, type = "n",
       ylim = c(-3, 3),
       main = ed,
       xlab = "", ylab = "Residual (pp)",
       las = 1, cex.main = 1.2)
  abline(h = 0, col = "gray80", lwd = 1)

  # 90% CI
  polygon(c(rd$date, rev(rd$date)),
          c(rd$obs_resid_q5 * 100, rev(rd$obs_resid_q95 * 100)),
          col = rgb(0.2, 0.4, 0.8, 0.2), border = NA)
  lines(rd$date, rd$obs_resid_mean * 100, col = rgb(0.2, 0.4, 0.8), lwd = 1.5)
  points(rd$date, rd$obs_resid_mean * 100, cex = 0.5, col = rgb(0.2, 0.4, 0.8))
}

plot.new()
legend("center", legend = c("Mean residual (observed - model)", "90% CI"), bty = "n",
       col = c(rgb(0.2, 0.4, 0.8), rgb(0.2, 0.4, 0.8, 0.2)),
       lwd = c(2, 10), cex = 1.2)

dev.off()
cat("Saved: reports/figures/residual-observation.png\n")

# ===========================================================================
# PLOT 2: Time-Varying Equilibrium Trajectory
# Shows u_eq[t] with the observed data and full model fit
# ===========================================================================
cat("Plotting equilibrium trajectories...\n")
png("reports/figures/residual-spline-component.png",
    width = 12, height = 9, units = "in", res = 150)

par(mfrow = c(4, 2), mar = c(3, 3.5, 2.5, 1), mgp = c(2, 0.6, 0))

for (ed in rev(levels(results_plot$edu_label))) {
  rd <- results_plot[edu_label == ed][order(date)]

  ylo <- min(c(rd$eq_q5, rd$observed), na.rm = TRUE) * 100
  yhi <- max(c(rd$eq_q95, rd$observed), na.rm = TRUE) * 100
  ypad <- (yhi - ylo) * 0.1
  ylo <- ylo - ypad
  yhi <- yhi + ypad

  plot(rd$date, rd$observed * 100, type = "n",
       ylim = c(ylo, yhi),
       main = ed,
       xlab = "", ylab = "Unemployment Rate (%)",
       las = 1, cex.main = 1.2)

  # Time-varying equilibrium (green band)
  polygon(c(rd$date, rev(rd$date)),
          c(rd$eq_q5 * 100, rev(rd$eq_q95 * 100)),
          col = rgb(0.15, 0.6, 0.25, 0.25), border = NA)
  lines(rd$date, rd$eq_mean * 100, col = rgb(0.15, 0.6, 0.25), lwd = 2)

  # Full model fit (blue band)
  polygon(c(rd$date, rev(rd$date)),
          c(rd$u_q5 * 100, rev(rd$u_q95 * 100)),
          col = rgb(0.2, 0.4, 0.8, 0.15), border = NA)
  lines(rd$date, rd$u_mean * 100, col = rgb(0.2, 0.4, 0.8), lwd = 1.5)

  # Observed data points
  points(rd$date, rd$observed * 100, cex = 0.4, col = "gray40")
}

plot.new()
legend("center",
       legend = c("u_eq[t] (equilibrium)", "Full model fit", "Observed"),
       bty = "n",
       col = c(rgb(0.15, 0.6, 0.25), rgb(0.2, 0.4, 0.8), "gray40"),
       lwd = c(2, 2, NA), pch = c(NA, NA, 1), cex = 1.2)

dev.off()
cat("Saved: reports/figures/residual-spline-component.png\n")

cat("\nDone.\n")
