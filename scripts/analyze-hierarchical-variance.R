#!/usr/bin/env Rscript
# Comprehensive Analysis of Hierarchical Parameter Variance
#
# This script examines the between-education variation (pooling strength)
# across ALL hierarchical parameter groups in the Stan model:
#   1. Equilibrium unemployment (u_eq)
#   2. Adjustment speed (adj_speed)
#   3. Shock effects (2008, 2020)
#   4. Decay rates (2008, 2020)
#   5. Seasonal effects (amplitude, phase)
#   6. Spline smoothness (sigma_spline)
#
# Key question: Which unemployment dynamics vary most across education levels?
#
# Usage: Rscript scripts/analyze-hierarchical-variance.R

library(data.table)
library(ggplot2)

# Load fitted model
model_path <- here::here("models", "ode-state-space-efficient-fit.qs")
if (!file.exists(model_path)) {
  stop("Fitted model not found. Run targets::tar_make(model_ode_state_space_efficient_file) first.")
}

cat("Loading fitted model...\n")
result <- qs::qread(model_path)

# Extract draws and summary
if ("draws" %in% names(result) && "summary" %in% names(result)) {
  cat("Using pre-extracted draws and summary...\n")
  draws_df <- result$draws
  all_summary <- result$summary
} else {
  cat("Extracting draws from fit object...\n")
  fit <- result$fit
  draws_df <- fit$draws(format = "df")
  all_summary <- fit$summary()
}

# Education levels (from prepare_stan_data)
edu_levels <- c("bachelors", "high_school", "less_than_hs", "masters", "phd",
                "professional", "some_college")
N_edu <- length(edu_levels)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Extract hierarchical parameter info
extract_hierarchical_info <- function(summary, mu_var, sigma_var, edu_var_pattern) {
  mu <- summary[summary$variable == mu_var, ]
  sigma <- summary[summary$variable == sigma_var, ]
  edu_vals <- summary[grepl(edu_var_pattern, summary$variable), ]

  list(
    mu_mean = mu$mean,
    mu_sd = mu$sd,
    sigma_mean = sigma$mean,
    sigma_sd = sigma$sd,
    sigma_q5 = sigma$q5,
    sigma_q95 = sigma$q95,
    edu_mean = mean(edu_vals$mean),
    edu_sd = sd(edu_vals$mean),
    edu_cv = sd(edu_vals$mean) / mean(edu_vals$mean),  # Coefficient of variation
    n_edu = nrow(edu_vals)
  )
}

# Interpret pooling strength
interpret_pooling <- function(sigma, scale = "log") {
  if (scale == "log") {
    if (sigma < 0.1) return("Very tight (nearly identical)")
    if (sigma < 0.2) return("Tight (similar with small variation)")
    if (sigma < 0.4) return("Moderate (some education differences)")
    if (sigma < 0.8) return("Weak (substantial differences)")
    return("Very weak (large differences)")
  } else {
    # For non-log scale parameters
    if (sigma < 0.05) return("Very tight (nearly identical)")
    if (sigma < 0.1) return("Tight (similar with small variation)")
    if (sigma < 0.2) return("Moderate (some education differences)")
    if (sigma < 0.5) return("Weak (substantial differences)")
    return("Very weak (large differences)")
  }
}

# ============================================================================
# EXTRACT ALL HIERARCHICAL PARAMETERS
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("HIERARCHICAL VARIANCE ANALYSIS\n")
cat(strrep("=", 80), "\n\n")

# 1. EQUILIBRIUM UNEMPLOYMENT
eq_info <- extract_hierarchical_info(
  all_summary,
  mu_var = "mu_logit_u_eq",
  sigma_var = "sigma_logit_u_eq",
  edu_var_pattern = "^u_eq\\[\\d+\\]$"
)

# 2. ADJUSTMENT SPEED
adj_info <- extract_hierarchical_info(
  all_summary,
  mu_var = "mu_log_adj_speed",
  sigma_var = "sigma_log_adj_speed",
  edu_var_pattern = "^adj_speed\\[\\d+\\]$"
)

# 3. 2008 SHOCK EFFECTS
shock_2008_info <- extract_hierarchical_info(
  all_summary,
  mu_var = "mu_log_shock_2008",
  sigma_var = "sigma_log_shock_2008",
  edu_var_pattern = "^shock_2008_effect\\[\\d+\\]$"
)

# 4. 2020 SHOCK EFFECTS
shock_2020_info <- extract_hierarchical_info(
  all_summary,
  mu_var = "mu_log_shock_2020",
  sigma_var = "sigma_log_shock_2020",
  edu_var_pattern = "^shock_2020_effect\\[\\d+\\]$"
)

# 5. 2008 DECAY RATE
decay_2008_info <- extract_hierarchical_info(
  all_summary,
  mu_var = "mu_decay_2008",
  sigma_var = "sigma_decay_2008",
  edu_var_pattern = "^decay_2008\\[\\d+\\]$"
)

# 6. 2020 DECAY RATE
decay_2020_info <- extract_hierarchical_info(
  all_summary,
  mu_var = "mu_decay_2020",
  sigma_var = "sigma_decay_2020",
  edu_var_pattern = "^decay_2020\\[\\d+\\]$"
)

# 7. SEASONAL EFFECTS
seasonal_info <- extract_hierarchical_info(
  all_summary,
  mu_var = "mu_seasonal[1]",  # Just need one element to get info
  sigma_var = "sigma_seasonal",
  edu_var_pattern = "^seasonal_u\\[1,\\d+\\]$"  # First month across education levels
)

# 8. SPLINE SMOOTHNESS
spline_info <- extract_hierarchical_info(
  all_summary,
  mu_var = "mu_log_sigma_spline",
  sigma_var = "sigma_log_sigma_spline",
  edu_var_pattern = "^sigma_spline\\[\\d+\\]$"
)

# ============================================================================
# SUMMARY TABLE
# ============================================================================

# Create summary data.table
variance_summary <- data.table(
  parameter = c(
    "Equilibrium unemployment (u_eq)",
    "Adjustment speed (adj_speed)",
    "2008 shock effect",
    "2020 shock effect",
    "2008 decay rate",
    "2020 decay rate",
    "Seasonal effects",
    "Spline smoothness"
  ),
  sigma_mean = c(
    eq_info$sigma_mean,
    adj_info$sigma_mean,
    shock_2008_info$sigma_mean,
    shock_2020_info$sigma_mean,
    decay_2008_info$sigma_mean,
    decay_2020_info$sigma_mean,
    seasonal_info$sigma_mean,
    spline_info$sigma_mean
  ),
  sigma_q5 = c(
    eq_info$sigma_q5,
    adj_info$sigma_q5,
    shock_2008_info$sigma_q5,
    shock_2020_info$sigma_q5,
    decay_2008_info$sigma_q5,
    decay_2020_info$sigma_q5,
    seasonal_info$sigma_q5,
    spline_info$sigma_q5
  ),
  sigma_q95 = c(
    eq_info$sigma_q95,
    adj_info$sigma_q95,
    shock_2008_info$sigma_q95,
    shock_2020_info$sigma_q95,
    decay_2008_info$sigma_q95,
    decay_2020_info$sigma_q95,
    seasonal_info$sigma_q95,
    spline_info$sigma_q95
  ),
  edu_cv = c(
    eq_info$edu_cv,
    adj_info$edu_cv,
    shock_2008_info$edu_cv,
    shock_2020_info$edu_cv,
    decay_2008_info$edu_cv,
    decay_2020_info$edu_cv,
    seasonal_info$edu_cv,
    spline_info$edu_cv
  ),
  scale = c(
    "logit", "log", "log", "log", "logit", "logit", "linear", "log"
  )
)

# Add pooling interpretation
variance_summary[, pooling := mapply(interpret_pooling, sigma_mean, scale)]

# Sort by sigma (weakest to strongest pooling)
setorder(variance_summary, -sigma_mean)

cat("Between-Education Variance (sigma) for All Hierarchical Parameters:\n\n")
cat(sprintf("%-35s %10s %15s %15s %s\n",
            "Parameter", "Sigma", "90% CI", "Edu CV", "Pooling Strength"))
cat(strrep("-", 100), "\n")

for (i in 1:nrow(variance_summary)) {
  row <- variance_summary[i]
  cat(sprintf("%-35s %10.3f [%5.3f, %5.3f] %14.1f%% %s\n",
              row$parameter,
              row$sigma_mean,
              row$sigma_q5,
              row$sigma_q95,
              row$edu_cv * 100,
              row$pooling))
}

cat("\n")

# ============================================================================
# KEY FINDINGS
# ============================================================================

cat(strrep("=", 80), "\n")
cat("KEY FINDINGS\n")
cat(strrep("=", 80), "\n\n")

# Find parameters with weakest/strongest pooling
most_variable <- variance_summary[1, parameter]
most_variable_sigma <- variance_summary[1, sigma_mean]

least_variable <- variance_summary[.N, parameter]
least_variable_sigma <- variance_summary[.N, sigma_mean]

cat("Most education-specific variation:\n")
cat(sprintf("  → %s (σ = %.3f)\n", most_variable, most_variable_sigma))
cat("    Education levels differ substantially in this parameter\n\n")

cat("Least education-specific variation:\n")
cat(sprintf("  → %s (σ = %.3f)\n", least_variable, least_variable_sigma))
cat("    Education levels are very similar in this parameter\n\n")

# Group by pooling strength
tight_pooling <- variance_summary[sigma_mean < 0.15, parameter]
moderate_pooling <- variance_summary[sigma_mean >= 0.15 & sigma_mean < 0.4, parameter]
weak_pooling <- variance_summary[sigma_mean >= 0.4, parameter]

if (length(tight_pooling) > 0) {
  cat("Parameters with tight pooling (σ < 0.15):\n")
  for (p in tight_pooling) cat("  •", p, "\n")
  cat("\n")
}

if (length(moderate_pooling) > 0) {
  cat("Parameters with moderate pooling (0.15 ≤ σ < 0.4):\n")
  for (p in moderate_pooling) cat("  •", p, "\n")
  cat("\n")
}

if (length(weak_pooling) > 0) {
  cat("Parameters with weak pooling (σ ≥ 0.4):\n")
  for (p in weak_pooling) cat("  •", p, "\n")
  cat("\n")
}

# ============================================================================
# VISUALIZATION 1: Sigma comparison across parameters
# ============================================================================

cat("Generating visualizations...\n")

# Reorder for plotting (weakest to strongest pooling)
variance_summary[, parameter := factor(parameter, levels = rev(parameter))]

p1 <- ggplot(variance_summary, aes(x = sigma_mean, y = parameter)) +
  geom_point(size = 3, color = "#2166AC") +
  geom_errorbar(aes(xmin = sigma_q5, xmax = sigma_q95),
                width = 0.2, color = "#2166AC", orientation = "y") +
  geom_vline(xintercept = c(0.15, 0.4), linetype = "dashed",
             color = "gray50", alpha = 0.5) +
  annotate("text", x = 0.15, y = Inf, label = "Tight pooling",
           vjust = 1.5, hjust = -0.1, size = 3, color = "gray50") +
  annotate("text", x = 0.4, y = Inf, label = "Weak pooling",
           vjust = 1.5, hjust = -0.1, size = 3, color = "gray50") +
  labs(
    title = "Between-Education Variance Across All Hierarchical Parameters",
    subtitle = "Smaller σ = stronger pooling = more similar across education levels",
    x = "Between-education standard deviation (σ)",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(here::here("reports", "figures", "hierarchical-variance-comparison.png"),
       p1, width = 10, height = 6, dpi = 300)

# ============================================================================
# VISUALIZATION 2: Education-specific values for select parameters
# ============================================================================

# Extract education-specific values for visualization
extract_edu_values <- function(summary, var_pattern, param_name) {
  edu_vals <- summary[grepl(var_pattern, summary$variable), ]
  edu_vals$education <- edu_levels
  edu_vals$parameter <- param_name
  edu_vals
}

# Select most interesting parameters (most and least variable)
eq_edu <- extract_edu_values(all_summary, "^u_eq\\[\\d+\\]$", "Equilibrium unemployment")
adj_edu <- extract_edu_values(all_summary, "^adj_speed\\[\\d+\\]$", "Adjustment speed")
shock_2008_edu <- extract_edu_values(all_summary, "^shock_2008_effect\\[\\d+\\]$", "2008 shock")
spline_edu <- extract_edu_values(all_summary, "^sigma_spline\\[\\d+\\]$", "Spline smoothness")

# Combine
edu_combined <- rbindlist(list(eq_edu, adj_edu, shock_2008_edu, spline_edu))

p2 <- ggplot(edu_combined, aes(x = mean, y = education, color = parameter)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(xmin = q5, xmax = q95), width = 0.2, alpha = 0.6, orientation = "y") +
  facet_wrap(~ parameter, scales = "free_x", ncol = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Education-Specific Parameter Values",
    subtitle = "Comparing parameters with different pooling strengths",
    x = "Parameter value (posterior mean with 90% CI)",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )

ggsave(here::here("reports", "figures", "education-specific-parameters.png"),
       p2, width = 10, height = 6, dpi = 300)

cat("Saved plots to reports/figures/\n")

# ============================================================================
# INTERPRETATION
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("INTERPRETATION\n")
cat(strrep("=", 80), "\n\n")

cat("The hierarchical model allows us to compare HOW MUCH education levels differ\n")
cat("across different aspects of unemployment dynamics:\n\n")

cat("Parameters with TIGHT pooling (similar across education):\n")
cat("  → These dynamics are fundamental to all education levels\n")
cat("  → The data supports a common mechanism\n")
cat("  → Education-specific differences are small relative to uncertainty\n\n")

cat("Parameters with WEAK pooling (substantial education differences):\n")
cat("  → These dynamics genuinely differ by education level\n")
cat("  → The data supports education-specific mechanisms\n")
cat("  → Pooling provides modest shrinkage but preserves differences\n\n")

cat("This hierarchical variance analysis reveals:\n")
cat("  1. Which unemployment dynamics are universal vs. education-specific\n")
cat("  2. Where to focus when interpreting education differences\n")
cat("  3. Which parameters benefit most from hierarchical pooling\n\n")

cat(strrep("=", 80), "\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

output_path <- here::here("results", "hierarchical-variance-summary.rds")
dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(variance_summary, output_path)

cat("\nSaved variance summary to:", output_path, "\n")
