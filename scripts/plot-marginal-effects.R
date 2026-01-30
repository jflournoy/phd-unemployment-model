#!/usr/bin/env Rscript
# Comprehensive Marginal Effects Visualization
#
# This script generates publication-quality plots showing:
# 1. Baseline time trends by education (no shock, month=6)
# 2. 2008 crisis shock effects by education
# 3. 2020 pandemic shock effects by education
# 4. Seasonal patterns by education
# 5. Education gradient comparisons
# 6. Counterfactual vs observed during crises

library(mgcv)
library(ggplot2)
library(data.table)

# Set theme
theme_set(theme_minimal(base_size = 12))

# Load model
source("R/education-binomial-gam.R")
data <- readRDS("data/education-spectrum-counts.rds")

cat("Fitting model with time_k=60...\n")
result <- fit_education_binomial_gam(data, use_quasi = TRUE, time_k = 60)
model <- result$model

cat("Model converged:", result$convergence_info$converged, "\n")
cat("Deviance explained:", round(result$summary_stats$deviance_explained * 100, 1), "%\n\n")

# Create output directory
dir.create("reports/figures", showWarnings = FALSE, recursive = TRUE)

# Color palette for education levels
edu_colors <- c(
  "phd" = "#1f77b4",
  "professional" = "#ff7f0e",
  "masters" = "#2ca02c",
  "bachelors" = "#d62728",
  "some_college" = "#9467bd",
  "high_school" = "#8c564b",
  "less_than_hs" = "#e377c2"
)

edu_labels <- c(
  "phd" = "PhD",
  "professional" = "Professional",
  "masters" = "Master's",
  "bachelors" = "Bachelor's",
  "some_college" = "Some College",
  "high_school" = "High School",
  "less_than_hs" = "Less than HS"
)

# =============================================================================
# 1. BASELINE TIME TRENDS BY EDUCATION (shock=baseline, month=6)
# =============================================================================
cat("1. Generating baseline time trends by education...\n")

# Create prediction grid
time_seq <- seq(1, 308, by = 1)  # Monthly from 2000 to 2025
edu_levels <- levels(result$data$education)

baseline_preds <- data.frame()

for (edu in edu_levels) {
  pred_data <- data.frame(
    education = factor(edu, levels = edu_levels),
    time_index = time_seq,
    month = 6,
    shock_period = factor("baseline", levels = levels(result$data$shock_period))
  )

  preds <- predict(model, newdata = pred_data, type = "response", se.fit = TRUE)

  baseline_preds <- rbind(baseline_preds, data.frame(
    education = edu,
    time_index = time_seq,
    year = 2000 + (time_seq - 1) / 12,
    fitted = as.numeric(preds$fit),
    se = as.numeric(preds$se.fit),
    ci_lower = pmax(0, as.numeric(preds$fit) - 1.96 * as.numeric(preds$se.fit)),
    ci_upper = pmin(1, as.numeric(preds$fit) + 1.96 * as.numeric(preds$se.fit))
  ))
}

p1 <- ggplot(baseline_preds, aes(x = year, y = fitted * 100, color = education, fill = education)) +
  geom_ribbon(aes(ymin = ci_lower * 100, ymax = ci_upper * 100), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = edu_colors, labels = edu_labels) +
  scale_fill_manual(values = edu_colors, labels = edu_labels) +
  labs(
    title = "Baseline Unemployment Trends by Education Level",
    subtitle = "Counterfactual trends (no shock effects), June average",
    x = "Year",
    y = "Unemployment Rate (%)",
    color = "Education",
    fill = "Education"
  ) +
  theme(legend.position = "right")

ggsave("reports/figures/01-baseline-trends-by-education.png", p1, width = 12, height = 7, dpi = 150)
cat("  Saved: reports/figures/01-baseline-trends-by-education.png\n")

# =============================================================================
# 2. 2008 CRISIS SHOCK EFFECTS BY EDUCATION
# =============================================================================
cat("2. Generating 2008 crisis shock effects by education...\n")

# Time range for 2008 crisis (2007-2010)
crisis_2008_time <- seq(85, 132, by = 0.5)  # Jan 2007 to Dec 2010

shock_2008_preds <- data.frame()

for (edu in edu_levels) {
  for (t in crisis_2008_time) {
    # Crisis prediction
    crisis_data <- data.frame(
      education = factor(edu, levels = edu_levels),
      time_index = t,
      month = 6,
      shock_period = factor("crisis_2008", levels = levels(result$data$shock_period))
    )

    # Baseline prediction (counterfactual)
    baseline_data <- data.frame(
      education = factor(edu, levels = edu_levels),
      time_index = t,
      month = 6,
      shock_period = factor("baseline", levels = levels(result$data$shock_period))
    )

    pred_crisis <- predict(model, newdata = crisis_data, type = "response", se.fit = TRUE)
    pred_baseline <- predict(model, newdata = baseline_data, type = "response", se.fit = TRUE)

    shock_effect <- as.numeric(pred_crisis$fit) - as.numeric(pred_baseline$fit)
    se_effect <- sqrt(as.numeric(pred_crisis$se.fit)^2 + as.numeric(pred_baseline$se.fit)^2)

    shock_2008_preds <- rbind(shock_2008_preds, data.frame(
      education = edu,
      time_index = t,
      year = 2000 + (t - 1) / 12,
      crisis_rate = as.numeric(pred_crisis$fit),
      baseline_rate = as.numeric(pred_baseline$fit),
      shock_effect = shock_effect,
      se = se_effect,
      ci_lower = shock_effect - 1.96 * se_effect,
      ci_upper = shock_effect + 1.96 * se_effect
    ))
  }
}

p2 <- ggplot(shock_2008_preds, aes(x = year, y = shock_effect * 100, color = education, fill = education)) +
  geom_ribbon(aes(ymin = ci_lower * 100, ymax = ci_upper * 100), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = edu_colors, labels = edu_labels) +
  scale_fill_manual(values = edu_colors, labels = edu_labels) +
  labs(
    title = "2008 Financial Crisis: Unemployment Shock Effects by Education",
    subtitle = "Shock effect = Crisis prediction - Baseline counterfactual",
    x = "Year",
    y = "Shock Effect (percentage points)",
    color = "Education",
    fill = "Education"
  ) +
  theme(legend.position = "right")

ggsave("reports/figures/02-shock-2008-by-education.png", p2, width = 12, height = 7, dpi = 150)
cat("  Saved: reports/figures/02-shock-2008-by-education.png\n")

# =============================================================================
# 3. 2020 PANDEMIC SHOCK EFFECTS BY EDUCATION
# =============================================================================
cat("3. Generating 2020 pandemic shock effects by education...\n")

# Time range for 2020 crisis (2019-2021)
crisis_2020_time <- seq(229, 264, by = 0.5)  # Jan 2019 to Dec 2021

shock_2020_preds <- data.frame()

for (edu in edu_levels) {
  for (t in crisis_2020_time) {
    # Crisis prediction
    crisis_data <- data.frame(
      education = factor(edu, levels = edu_levels),
      time_index = t,
      month = 6,
      shock_period = factor("crisis_2020", levels = levels(result$data$shock_period))
    )

    # Baseline prediction (counterfactual)
    baseline_data <- data.frame(
      education = factor(edu, levels = edu_levels),
      time_index = t,
      month = 6,
      shock_period = factor("baseline", levels = levels(result$data$shock_period))
    )

    pred_crisis <- predict(model, newdata = crisis_data, type = "response", se.fit = TRUE)
    pred_baseline <- predict(model, newdata = baseline_data, type = "response", se.fit = TRUE)

    shock_effect <- as.numeric(pred_crisis$fit) - as.numeric(pred_baseline$fit)
    se_effect <- sqrt(as.numeric(pred_crisis$se.fit)^2 + as.numeric(pred_baseline$se.fit)^2)

    shock_2020_preds <- rbind(shock_2020_preds, data.frame(
      education = edu,
      time_index = t,
      year = 2000 + (t - 1) / 12,
      crisis_rate = as.numeric(pred_crisis$fit),
      baseline_rate = as.numeric(pred_baseline$fit),
      shock_effect = shock_effect,
      se = se_effect,
      ci_lower = shock_effect - 1.96 * se_effect,
      ci_upper = shock_effect + 1.96 * se_effect
    ))
  }
}

p3 <- ggplot(shock_2020_preds, aes(x = year, y = shock_effect * 100, color = education, fill = education)) +
  geom_ribbon(aes(ymin = ci_lower * 100, ymax = ci_upper * 100), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = edu_colors, labels = edu_labels) +
  scale_fill_manual(values = edu_colors, labels = edu_labels) +
  labs(
    title = "2020 COVID-19 Pandemic: Unemployment Shock Effects by Education",
    subtitle = "Shock effect = Crisis prediction - Baseline counterfactual",
    x = "Year",
    y = "Shock Effect (percentage points)",
    color = "Education",
    fill = "Education"
  ) +
  theme(legend.position = "right")

ggsave("reports/figures/03-shock-2020-by-education.png", p3, width = 12, height = 7, dpi = 150)
cat("  Saved: reports/figures/03-shock-2020-by-education.png\n")

# =============================================================================
# 4. SEASONAL PATTERNS BY EDUCATION
# =============================================================================
cat("4. Generating seasonal patterns by education...\n")

month_seq <- seq(1, 12, by = 0.25)
time_point <- 150  # Mid-series, non-shock period

seasonal_preds <- data.frame()

for (edu in edu_levels) {
  for (m in month_seq) {
    pred_data <- data.frame(
      education = factor(edu, levels = edu_levels),
      time_index = time_point,
      month = m,
      shock_period = factor("baseline", levels = levels(result$data$shock_period))
    )

    preds <- predict(model, newdata = pred_data, type = "response", se.fit = TRUE)

    seasonal_preds <- rbind(seasonal_preds, data.frame(
      education = edu,
      month = m,
      fitted = as.numeric(preds$fit),
      se = as.numeric(preds$se.fit),
      ci_lower = pmax(0, as.numeric(preds$fit) - 1.96 * as.numeric(preds$se.fit)),
      ci_upper = pmin(1, as.numeric(preds$fit) + 1.96 * as.numeric(preds$se.fit))
    ))
  }
}

p4 <- ggplot(seasonal_preds, aes(x = month, y = fitted * 100, color = education, fill = education)) +
  geom_ribbon(aes(ymin = ci_lower * 100, ymax = ci_upper * 100), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_manual(values = edu_colors, labels = edu_labels) +
  scale_fill_manual(values = edu_colors, labels = edu_labels) +
  labs(
    title = "Seasonal Unemployment Patterns by Education Level",
    subtitle = "Baseline period (no shock), mid-series time point",
    x = "Month",
    y = "Unemployment Rate (%)",
    color = "Education",
    fill = "Education"
  ) +
  theme(legend.position = "right")

ggsave("reports/figures/04-seasonal-by-education.png", p4, width = 12, height = 7, dpi = 150)
cat("  Saved: reports/figures/04-seasonal-by-education.png\n")

# =============================================================================
# 5. EDUCATION GRADIENT AT FIXED TIME POINTS
# =============================================================================
cat("5. Generating education gradient comparisons...\n")

# Compare education levels at key time points
key_times <- c(
  "2005 (pre-crisis)" = 60,
  "2009 (2008 crisis peak)" = 109,
  "2015 (recovery)" = 180,
  "2020 (pandemic peak)" = 244
)

gradient_preds <- data.frame()

for (period_name in names(key_times)) {
  t <- key_times[period_name]
  year <- 2000 + floor((t - 1) / 12)

  # Determine shock_period
  if (year >= 2007 && year <= 2010) {
    sp <- "crisis_2008"
  } else if (year >= 2019 && year <= 2021) {
    sp <- "crisis_2020"
  } else {
    sp <- "baseline"
  }

  for (edu in edu_levels) {
    pred_data <- data.frame(
      education = factor(edu, levels = edu_levels),
      time_index = t,
      month = 6,
      shock_period = factor(sp, levels = levels(result$data$shock_period))
    )

    preds <- predict(model, newdata = pred_data, type = "response", se.fit = TRUE)

    gradient_preds <- rbind(gradient_preds, data.frame(
      period = period_name,
      education = edu,
      fitted = as.numeric(preds$fit),
      se = as.numeric(preds$se.fit),
      ci_lower = pmax(0, as.numeric(preds$fit) - 1.96 * as.numeric(preds$se.fit)),
      ci_upper = pmin(1, as.numeric(preds$fit) + 1.96 * as.numeric(preds$se.fit))
    ))
  }
}

# Order education levels
gradient_preds$education <- factor(gradient_preds$education,
                                    levels = c("phd", "professional", "masters", "bachelors",
                                              "some_college", "high_school", "less_than_hs"))

p5 <- ggplot(gradient_preds, aes(x = education, y = fitted * 100, fill = education)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower * 100, ymax = ci_upper * 100), width = 0.2) +
  facet_wrap(~ period, nrow = 1) +
  scale_x_discrete(labels = edu_labels) +
  scale_fill_manual(values = edu_colors, guide = "none") +
  labs(
    title = "Education Gradient in Unemployment at Key Time Points",
    subtitle = "Higher education consistently provides unemployment protection",
    x = "Education Level",
    y = "Unemployment Rate (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("reports/figures/05-education-gradient-comparison.png", p5, width = 14, height = 6, dpi = 150)
cat("  Saved: reports/figures/05-education-gradient-comparison.png\n")

# =============================================================================
# 6. COUNTERFACTUAL VS OBSERVED DURING 2008 CRISIS (PhD focus)
# =============================================================================
cat("6. Generating counterfactual vs observed comparison...\n")

# Focus on PhD during 2008 crisis
phd_2008 <- shock_2008_preds[shock_2008_preds$education == "phd", ]

# Get observed data
phd_obs <- result$predictions[result$predictions$education == "phd" &
                               result$predictions$shock_2008_2009 == 1, ]

p6a <- ggplot() +
  # Counterfactual (baseline) trajectory
  geom_line(data = phd_2008, aes(x = year, y = baseline_rate * 100),
            color = "gray50", linewidth = 1, linetype = "dashed") +
  # Crisis trajectory
  geom_ribbon(data = phd_2008, aes(x = year,
                                    ymin = (baseline_rate - 1.96 * se) * 100,
                                    ymax = (crisis_rate + 1.96 * se) * 100),
              alpha = 0.2, fill = "#d62728") +
  geom_line(data = phd_2008, aes(x = year, y = crisis_rate * 100),
            color = "#d62728", linewidth = 1) +
  # Observed points
  geom_point(data = phd_obs, aes(x = year + (month - 1) / 12, y = observed_rate * 100),
             alpha = 0.5, size = 1.5, color = "black") +
  labs(
    title = "PhD Unemployment: 2008 Financial Crisis",
    subtitle = "Red = Crisis trajectory, Dashed = Counterfactual (no shock), Points = Observed",
    x = "Year",
    y = "Unemployment Rate (%)"
  ) +
  annotate("text", x = 2008.5, y = max(phd_2008$crisis_rate) * 100 + 0.2,
           label = "Crisis effect", color = "#d62728", hjust = 0) +
  annotate("text", x = 2008.5, y = min(phd_2008$baseline_rate) * 100 - 0.1,
           label = "Counterfactual", color = "gray50", hjust = 0)

ggsave("reports/figures/06a-phd-2008-counterfactual.png", p6a, width = 10, height = 6, dpi = 150)
cat("  Saved: reports/figures/06a-phd-2008-counterfactual.png\n")

# Same for high school to show contrast
hs_2008 <- shock_2008_preds[shock_2008_preds$education == "high_school", ]
hs_obs <- result$predictions[result$predictions$education == "high_school" &
                              result$predictions$shock_2008_2009 == 1, ]

p6b <- ggplot() +
  geom_line(data = hs_2008, aes(x = year, y = baseline_rate * 100),
            color = "gray50", linewidth = 1, linetype = "dashed") +
  geom_ribbon(data = hs_2008, aes(x = year,
                                   ymin = (baseline_rate - 1.96 * se) * 100,
                                   ymax = (crisis_rate + 1.96 * se) * 100),
              alpha = 0.2, fill = "#d62728") +
  geom_line(data = hs_2008, aes(x = year, y = crisis_rate * 100),
            color = "#d62728", linewidth = 1) +
  geom_point(data = hs_obs, aes(x = year + (month - 1) / 12, y = observed_rate * 100),
             alpha = 0.5, size = 1.5, color = "black") +
  labs(
    title = "High School Unemployment: 2008 Financial Crisis",
    subtitle = "Red = Crisis trajectory, Dashed = Counterfactual (no shock), Points = Observed",
    x = "Year",
    y = "Unemployment Rate (%)"
  )

ggsave("reports/figures/06b-highschool-2008-counterfactual.png", p6b, width = 10, height = 6, dpi = 150)
cat("  Saved: reports/figures/06b-highschool-2008-counterfactual.png\n")

# =============================================================================
# 7. PhD vs HIGH SCHOOL SHOCK EFFECT COMPARISON
# =============================================================================
cat("7. Generating PhD vs High School shock comparison...\n")

# Combine 2008 and 2020 shocks for PhD and High School
combined_shocks <- rbind(
  data.frame(shock_2008_preds[shock_2008_preds$education %in% c("phd", "high_school"), ],
             crisis = "2008 Financial Crisis"),
  data.frame(shock_2020_preds[shock_2020_preds$education %in% c("phd", "high_school"), ],
             crisis = "2020 COVID-19 Pandemic")
)

p7 <- ggplot(combined_shocks, aes(x = year, y = shock_effect * 100,
                                   color = education, fill = education)) +
  geom_ribbon(aes(ymin = ci_lower * 100, ymax = ci_upper * 100), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~ crisis, scales = "free_x") +
  scale_color_manual(values = c("phd" = "#1f77b4", "high_school" = "#8c564b"),
                     labels = c("phd" = "PhD", "high_school" = "High School")) +
  scale_fill_manual(values = c("phd" = "#1f77b4", "high_school" = "#8c564b"),
                    labels = c("phd" = "PhD", "high_school" = "High School")) +
  labs(
    title = "Economic Shock Effects: PhD vs High School",
    subtitle = "Higher education provides substantial protection during economic crises",
    x = "Year",
    y = "Shock Effect (percentage points)",
    color = "Education",
    fill = "Education"
  ) +
  theme(legend.position = "bottom")

ggsave("reports/figures/07-phd-vs-highschool-shocks.png", p7, width = 12, height = 6, dpi = 150)
cat("  Saved: reports/figures/07-phd-vs-highschool-shocks.png\n")

# =============================================================================
# 8. PEAK SHOCK EFFECTS SUMMARY
# =============================================================================
cat("8. Generating peak shock effects summary...\n")

# Calculate peak shock effects for each education and crisis
peak_effects <- data.frame()

for (edu in edu_levels) {
  # 2008 peak (around late 2009)
  edu_2008 <- shock_2008_preds[shock_2008_preds$education == edu, ]
  peak_idx_2008 <- which.max(edu_2008$shock_effect)

  # 2020 peak (around April-May 2020)
  edu_2020 <- shock_2020_preds[shock_2020_preds$education == edu, ]
  peak_idx_2020 <- which.max(edu_2020$shock_effect)

  peak_effects <- rbind(peak_effects, data.frame(
    education = edu,
    crisis = "2008 Financial Crisis",
    peak_year = edu_2008$year[peak_idx_2008],
    peak_effect = edu_2008$shock_effect[peak_idx_2008],
    peak_se = edu_2008$se[peak_idx_2008]
  ))

  peak_effects <- rbind(peak_effects, data.frame(
    education = edu,
    crisis = "2020 COVID-19",
    peak_year = edu_2020$year[peak_idx_2020],
    peak_effect = edu_2020$shock_effect[peak_idx_2020],
    peak_se = edu_2020$se[peak_idx_2020]
  ))
}

peak_effects$education <- factor(peak_effects$education,
                                  levels = c("phd", "professional", "masters", "bachelors",
                                            "some_college", "high_school", "less_than_hs"))

p8 <- ggplot(peak_effects, aes(x = education, y = peak_effect * 100, fill = crisis)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = (peak_effect - 1.96 * peak_se) * 100,
                    ymax = (peak_effect + 1.96 * peak_se) * 100),
                position = position_dodge(width = 0.8), width = 0.2) +
  scale_x_discrete(labels = edu_labels) +
  scale_fill_manual(values = c("2008 Financial Crisis" = "#d62728",
                               "2020 COVID-19" = "#ff7f0e")) +
  labs(
    title = "Peak Unemployment Shock Effects by Education and Crisis",
    subtitle = "Maximum increase above counterfactual baseline",
    x = "Education Level",
    y = "Peak Shock Effect (percentage points)",
    fill = "Crisis"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

ggsave("reports/figures/08-peak-shock-effects-summary.png", p8, width = 12, height = 7, dpi = 150)
cat("  Saved: reports/figures/08-peak-shock-effects-summary.png\n")

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n", strrep("=", 60), "\n")
cat("MARGINAL EFFECTS PLOTS COMPLETE\n")
cat(strrep("=", 60), "\n\n")

cat("Generated plots:\n")
cat("  1. Baseline trends by education (no shocks)\n")
cat("  2. 2008 crisis shock effects by education\n")
cat("  3. 2020 pandemic shock effects by education\n")
cat("  4. Seasonal patterns by education\n")
cat("  5. Education gradient at key time points\n")
cat("  6a. PhD 2008 counterfactual vs observed\n")
cat("  6b. High School 2008 counterfactual vs observed\n")
cat("  7. PhD vs High School shock comparison\n")
cat("  8. Peak shock effects summary\n\n")

cat("All plots saved to: reports/figures/\n")

# Print key findings
cat("\n", strrep("=", 60), "\n")
cat("KEY FINDINGS\n")
cat(strrep("=", 60), "\n\n")

cat("Peak 2008 Crisis Effects:\n")
for (edu in c("phd", "high_school")) {
  pe <- peak_effects[peak_effects$education == edu & peak_effects$crisis == "2008 Financial Crisis", ]
  cat(sprintf("  %s: +%.2f pp (peak in %.1f)\n",
              edu_labels[edu], pe$peak_effect * 100, pe$peak_year))
}

cat("\nPeak 2020 Pandemic Effects:\n")
for (edu in c("phd", "high_school")) {
  pe <- peak_effects[peak_effects$education == edu & peak_effects$crisis == "2020 COVID-19", ]
  cat(sprintf("  %s: +%.2f pp (peak in %.1f)\n",
              edu_labels[edu], pe$peak_effect * 100, pe$peak_year))
}

cat("\nEducation Protection Ratio (High School / PhD shock effect):\n")
phd_2008 <- peak_effects[peak_effects$education == "phd" & peak_effects$crisis == "2008 Financial Crisis", "peak_effect"]
hs_2008 <- peak_effects[peak_effects$education == "high_school" & peak_effects$crisis == "2008 Financial Crisis", "peak_effect"]
phd_2020 <- peak_effects[peak_effects$education == "phd" & peak_effects$crisis == "2020 COVID-19", "peak_effect"]
hs_2020 <- peak_effects[peak_effects$education == "high_school" & peak_effects$crisis == "2020 COVID-19", "peak_effect"]

cat(sprintf("  2008 Crisis: %.1fx (HS shock %.1f pp vs PhD %.1f pp)\n",
            hs_2008 / phd_2008, hs_2008 * 100, phd_2008 * 100))
cat(sprintf("  2020 Pandemic: %.1fx (HS shock %.1f pp vs PhD %.1f pp)\n",
            hs_2020 / phd_2020, hs_2020 * 100, phd_2020 * 100))
