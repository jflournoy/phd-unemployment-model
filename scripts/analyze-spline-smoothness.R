#!/usr/bin/env Rscript
# Analyze education-specific spline smoothness parameters
#
# This script extracts and visualizes the hierarchical spline smoothness
# parameters from the fitted Stan model to understand how much smoothness
# differs across education levels.
#
# Usage: Rscript scripts/analyze-spline-smoothness.R

library(data.table)
library(ggplot2)
library(cmdstanr)

# Load fitted model
model_path <- here::here("models", "ode-state-space-efficient-fit.qs")
if (!file.exists(model_path)) {
  stop("Fitted model not found. Run targets::tar_make(model_ode_state_space_efficient) first.")
}

cat("Loading fitted model...\n")
result <- qs::qread(model_path)

# Check if we have draws directly in the result (new format)
# or need to extract from fit object (old format)
if ("draws" %in% names(result) && "summary" %in% names(result)) {
  cat("Using pre-extracted draws and summary...\n")
  draws_df <- result$draws
  all_summary <- result$summary
} else {
  # Fallback for old format (requires CSV files)
  cat("Extracting draws from fit object...\n")
  fit <- result$fit
  draws_df <- fit$draws(format = "df")
  all_summary <- fit$summary()
}

# Education levels (in order from prepare_stan_data)
# Default order: alphabetical from unique(education)
edu_levels <- c("bachelors", "high_school", "less_than_hs", "masters", "phd",
                "professional", "some_college")

# ============================================================================
# 1. HIERARCHICAL PARAMETERS: Population-level smoothness
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("HIERARCHICAL SPLINE SMOOTHNESS PARAMETERS\n")
cat(strrep("=", 80), "\n\n")

# Population mean (on log scale)
mu_log_sigma <- all_summary[all_summary$variable == "mu_log_sigma_spline", ]
cat("Population mean (log scale):\n")
cat(sprintf("  mu_log_sigma_spline: %.3f [%.3f, %.3f]\n",
            mu_log_sigma$mean, mu_log_sigma$q5, mu_log_sigma$q95))
cat(sprintf("  exp(mu) = %.3f (median smoothness)\n\n", exp(mu_log_sigma$median)))

# Between-education SD
sigma_log_sigma <- all_summary[all_summary$variable == "sigma_log_sigma_spline", ]
cat("Between-education variation (log scale):\n")
cat(sprintf("  sigma_log_sigma_spline: %.3f [%.3f, %.3f]\n",
            sigma_log_sigma$mean, sigma_log_sigma$q5, sigma_log_sigma$q95))

# Interpret the variation
cat("\nInterpretation:\n")
if (sigma_log_sigma$mean < 0.1) {
  cat("  → Very tight pooling: education levels have nearly identical smoothness\n")
} else if (sigma_log_sigma$mean < 0.3) {
  cat("  → Moderate pooling: some education-specific variation in smoothness\n")
} else {
  cat("  → Weak pooling: substantial differences in smoothness across education\n")
}

# ============================================================================
# 2. EDUCATION-SPECIFIC SMOOTHNESS VALUES
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("EDUCATION-SPECIFIC SPLINE SMOOTHNESS (sigma_spline)\n")
cat(strrep("=", 80), "\n\n")

# Extract education-specific smoothness
sigma_spline_summary <- all_summary[grepl("^sigma_spline\\[", all_summary$variable), ]
sigma_spline_summary$education <- edu_levels

cat("Education-specific smoothness parameters:\n")
for (i in seq_along(edu_levels)) {
  row <- sigma_spline_summary[i, ]
  cat(sprintf("  %-10s: %.3f [%.3f, %.3f]  (SD: %.3f)\n",
              edu_levels[i],
              row$mean,
              row$q5,
              row$q95,
              row$sd))
}

# Compute coefficient of variation
cv <- sigma_spline_summary$sd[1] / sigma_spline_summary$mean[1]
cat(sprintf("\nCoefficient of variation (PhD): %.1f%%\n", cv * 100))

# ============================================================================
# 3. POSTERIOR DRAWS FOR DETAILED COMPARISON
# ============================================================================

# Extract posterior draws (already have them in draws_df)
# Select only the variables we need
N_edu <- length(edu_levels)
spline_vars <- c(paste0("sigma_spline[", 1:N_edu, "]"),
                 "mu_log_sigma_spline", "sigma_log_sigma_spline")
keep_cols <- c(".chain", ".iteration", ".draw", spline_vars)
draws_dt <- as.data.table(draws_df[, keep_cols])

# Reshape for plotting
draws_long <- melt(draws_dt,
                   id.vars = c(".chain", ".iteration", ".draw"),
                   measure.vars = patterns("^sigma_spline\\["),
                   variable.name = "parameter",
                   value.name = "sigma_spline")

# Add education labels
draws_long[, education := factor(
  gsub("sigma_spline\\[(\\d+)\\]", "\\1", parameter),
  levels = 1:N_edu,
  labels = edu_levels
)]

# ============================================================================
# 4. VISUALIZATION
# ============================================================================

cat("\nGenerating visualization...\n")

# Plot 1: Posterior distributions by education
p1 <- ggplot(draws_long, aes(x = sigma_spline, fill = education)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = exp(mu_log_sigma$mean), linetype = "dashed",
             color = "black", size = 0.5) +
  annotate("text", x = exp(mu_log_sigma$mean), y = Inf,
           label = "Population mean",
           vjust = 1.5, hjust = 0, angle = 90, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Education-Specific Spline Smoothness",
    subtitle = "Hierarchical posterior distributions",
    x = "Spline smoothness (sigma_spline)",
    y = "Posterior density",
    fill = "Education"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(here::here("reports", "figures", "spline-smoothness-by-education.png"),
       p1, width = 8, height = 5, dpi = 300)

# Plot 2: Uncertainty in education differences (key comparisons)
sigma_wide <- dcast(draws_long, .draw ~ education, value.var = "sigma_spline")
sigma_wide[, phd_vs_bachelors := phd - bachelors]
sigma_wide[, masters_vs_bachelors := masters - bachelors]
sigma_wide[, phd_vs_masters := phd - masters]
sigma_wide[, phd_vs_hs := phd - high_school]
sigma_wide[, bachelors_vs_hs := bachelors - high_school]

diff_long <- melt(sigma_wide,
                  id.vars = ".draw",
                  measure.vars = c("phd_vs_bachelors", "masters_vs_bachelors",
                                   "phd_vs_masters", "phd_vs_hs", "bachelors_vs_hs"),
                  variable.name = "comparison",
                  value.name = "difference")

p2 <- ggplot(diff_long, aes(x = difference, fill = comparison)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_brewer(palette = "Set1",
                    labels = c("PhD - Bachelors",
                              "Masters - Bachelors",
                              "PhD - Masters",
                              "PhD - High School",
                              "Bachelors - High School")) +
  labs(
    title = "Pairwise Differences in Spline Smoothness",
    subtitle = "Positive = first group is smoother than second",
    x = "Difference in sigma_spline",
    y = "Posterior density",
    fill = "Comparison"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(here::here("reports", "figures", "spline-smoothness-differences.png"),
       p2, width = 8, height = 5, dpi = 300)

cat("Saved plots to reports/figures/\n")

# ============================================================================
# 5. SUMMARY STATISTICS
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("PAIRWISE COMPARISONS\n")
cat(strrep("=", 80), "\n\n")

comparisons <- list(
  "PhD vs Bachelors" = sigma_wide$phd_vs_bachelors,
  "Masters vs Bachelors" = sigma_wide$masters_vs_bachelors,
  "PhD vs Masters" = sigma_wide$phd_vs_masters,
  "PhD vs High School" = sigma_wide$phd_vs_hs,
  "Bachelors vs High School" = sigma_wide$bachelors_vs_hs
)

for (comp_name in names(comparisons)) {
  diff <- comparisons[[comp_name]]

  cat(sprintf("%s:\n", comp_name))
  cat(sprintf("  Mean difference: %.3f\n", mean(diff)))
  cat(sprintf("  95%% CI: [%.3f, %.3f]\n", quantile(diff, 0.025), quantile(diff, 0.975)))
  cat(sprintf("  P(difference > 0): %.1f%%\n", mean(diff > 0) * 100))

  # Practical significance
  if (abs(mean(diff)) < 0.05) {
    cat("  → Negligible difference\n")
  } else if (abs(mean(diff)) < 0.2) {
    cat("  → Small but meaningful difference\n")
  } else {
    cat("  → Substantial difference\n")
  }
  cat("\n")
}

# ============================================================================
# 6. IMPLICATIONS
# ============================================================================

cat(strrep("=", 80), "\n")
cat("INTERPRETATION\n")
cat(strrep("=", 80), "\n\n")

cat("Spline smoothness controls the 'wiggliness' of unemployment trajectories\n")
cat("after accounting for ODE dynamics and seasonal effects:\n\n")

cat("  • Higher sigma_spline → more residual variation allowed\n")
cat("  • Lower sigma_spline → smoother trajectories\n\n")

if (sigma_log_sigma$mean < 0.2) {
  cat("Finding: Education levels have similar residual smoothness.\n")
  cat("This suggests that after accounting for:\n")
  cat("  - Different equilibrium rates\n")
  cat("  - Different adjustment speeds\n")
  cat("  - Different shock responses\n")
  cat("  - Different seasonal patterns\n")
  cat("...the remaining unexplained variation is similar across education.\n\n")
} else {
  cat("Finding: Education levels differ in residual smoothness.\n")
  cat("This suggests some groups have more unpredictable month-to-month\n")
  cat("variation even after accounting for all the structural dynamics.\n\n")
}

cat(strrep("=", 80), "\n")
