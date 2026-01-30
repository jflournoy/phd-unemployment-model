#!/usr/bin/env Rscript
# Diagnostic plots for shock smooth functional forms

library(phdunemployment)
library(mgcv)
library(data.table)
library(ggplot2)

# Load the fitted model
cat("\n=== Loading Model ===\n")
model_result <- readRDS("data/model_education_binomial.rds")

# Extract the actual bam model
model <- model_result$model
model_data <- model_result$data

cat("Model deviance explained:", round(model_result$summary_stats$dev.expl * 100, 1), "%\n")
cat("Dispersion:", round(model_result$summary_stats$dispersion, 2), "\n")

cat("\n=== Extracting Smooth Terms ===\n")

# Use gratia or manual extraction
education_levels <- c("bachelors", "high_school", "less_than_hs", "masters", "phd", "professional", "some_college")

# Create visualization data
plot_data_2008 <- list()
plot_data_2020 <- list()

for (edu in education_levels) {

  # Create grid for 2008 shock
  grid_2008 <- data.table(
    education = factor(edu, levels = education_levels),
    shock_2008_intensity = seq(0, 1, length.out = 100),
    shock_2020_intensity = 0,
    time_index = median(model_data$time_index),
    month = 1,  # Keep as numeric for cyclic smooth
    n_unemployed = 100,
    n_employed = 900
  )

  # Predict for 2008 shock
  pred_2008 <- predict(model, newdata = grid_2008, type = "link", se.fit = TRUE)

  # Now zero out the 2008 shock to get baseline
  grid_baseline <- copy(grid_2008)
  grid_baseline$shock_2008_intensity <- 0
  pred_baseline <- predict(model, newdata = grid_baseline, type = "link", se.fit = TRUE)

  # Shock effect = prediction with shock - prediction without shock
  plot_data_2008[[edu]] <- data.table(
    education = edu,
    intensity = grid_2008$shock_2008_intensity,
    effect = pred_2008$fit - pred_baseline$fit,
    se = sqrt(pred_2008$se.fit^2 + pred_baseline$se.fit^2)
  )

  # Create grid for 2020 shock
  grid_2020 <- data.table(
    education = factor(edu, levels = education_levels),
    shock_2008_intensity = 0,
    shock_2020_intensity = seq(0, 1, length.out = 100),
    time_index = median(model_data$time_index),
    month = 1,  # Keep as numeric for cyclic smooth
    n_unemployed = 100,
    n_employed = 900
  )

  pred_2020 <- predict(model, newdata = grid_2020, type = "link", se.fit = TRUE)

  grid_baseline2 <- copy(grid_2020)
  grid_baseline2$shock_2020_intensity <- 0
  pred_baseline2 <- predict(model, newdata = grid_baseline2, type = "link", se.fit = TRUE)

  plot_data_2020[[edu]] <- data.table(
    education = edu,
    intensity = grid_2020$shock_2020_intensity,
    effect = pred_2020$fit - pred_baseline2$fit,
    se = sqrt(pred_2020$se.fit^2 + pred_baseline2$se.fit^2)
  )
}

shock_2008_effects <- rbindlist(plot_data_2008)
shock_2020_effects <- rbindlist(plot_data_2020)

cat("\n=== Creating Plots ===\n")

# Plot 2008 crisis response curves
p1 <- ggplot(shock_2008_effects, aes(x = intensity, y = effect,
                                      color = education, fill = education)) +
  geom_ribbon(aes(ymin = effect - 2*se, ymax = effect + 2*se), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  labs(
    title = "2008 Crisis: Unemployment Response to Shock Intensity",
    subtitle = "Smooth function s(shock_2008_intensity, by=education, k=8, id=1)",
    x = "Shock Intensity (0 = no shock, 1 = peak)",
    y = "Effect on Log-Odds of Unemployment",
    color = "Education",
    fill = "Education"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)

# Plot 2020 pandemic response curves
p2 <- ggplot(shock_2020_effects, aes(x = intensity, y = effect,
                                      color = education, fill = education)) +
  geom_ribbon(aes(ymin = effect - 2*se, ymax = effect + 2*se), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  labs(
    title = "2020 Pandemic: Unemployment Response to Shock Intensity",
    subtitle = "Smooth function s(shock_2020_intensity, by=education, k=4, id=2)",
    x = "Shock Intensity (0 = no shock, 1 = peak)",
    y = "Effect on Log-Odds of Unemployment",
    color = "Education",
    fill = "Education"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)

# Save plots
dir.create("reports/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("reports/figures/shock-2008-response-curves.png", p1,
       width = 10, height = 6, dpi = 300)
ggsave("reports/figures/shock-2020-response-curves.png", p2,
       width = 10, height = 6, dpi = 300)

cat("\n=== Plots saved ===\n")
cat("- reports/figures/shock-2008-response-curves.png\n")
cat("- reports/figures/shock-2020-response-curves.png\n")

# Print summary statistics
cat("\n=== 2008 Crisis Response Summary ===\n")
shock_2008_summary <- shock_2008_effects[, .(
  max_effect = max(effect),
  min_effect = min(effect),
  range = max(effect) - min(effect),
  monotonic = all(diff(effect) >= -1e-6)  # Check if monotonically increasing
), by = education]
print(shock_2008_summary)

cat("\n=== 2020 Pandemic Response Summary ===\n")
shock_2020_summary <- shock_2020_effects[, .(
  max_effect = max(effect),
  min_effect = min(effect),
  range = max(effect) - min(effect),
  monotonic = all(diff(effect) >= -1e-6)
), by = education]
print(shock_2020_summary)

cat("\n=== Diagnostic Complete ===\n")
