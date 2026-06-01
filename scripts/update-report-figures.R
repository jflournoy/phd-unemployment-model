#!/usr/bin/env Rscript
# Regenerate all report figures from the current model fit
# Called by scripts/deploy-report.sh before rendering

library(cmdstanr)
library(data.table)
library(ggplot2)

result <- readRDS("models/ode-state-space-edu-parallel-fit.rds")
fit <- result$fit
all_summary <- as.data.table(fit$summary())

edu_labels <- c("PhD", "Professional", "Masters", "Bachelor's",
                 "Associate's", "Some college", "Less than HS")

# ============================================================================
# FIGURE 1: Hierarchical variance comparison
# (Between-education SD for each hierarchical parameter)
# ============================================================================

sigma_params <- c(
  "sigma_logit_u_eq" = "Equilibrium U rate",
  "sigma_log_adj_speed" = "Adjustment speed",
  "sigma_log_shock_2008" = "2008 shock effect",
  "sigma_log_shock_2020" = "2020 shock effect",
  "sigma_decay_2008" = "2008 decay rate",
  "sigma_decay_2020" = "2020 decay rate",
  "sigma_seasonal" = "Seasonal amplitude"
)

sigma_df <- rbindlist(lapply(names(sigma_params), function(p) {
  s <- all_summary[variable == p, ]
  data.table(parameter = sigma_params[p],
             sigma_mean = s$mean, sigma_sd = s$sd,
             sigma_q5 = s$q5, sigma_q95 = s$q95)
}))

# Reorder by sigma_mean
sigma_df[, parameter := factor(parameter, levels = rev(sigma_df$parameter[order(sigma_df$sigma_mean)]))]

p1 <- ggplot(sigma_df, aes(x = sigma_mean, y = parameter)) +
  geom_point(size = 3, color = "#2166AC") +
  geom_errorbar(aes(xmin = sigma_q5, xmax = sigma_q95),
                width = 0.2, color = "#2166AC", orientation = "y") +
  geom_vline(xintercept = c(0.15, 0.4), linetype = "dashed",
             color = "gray50", alpha = 0.5) +
  annotate("text", x = 0.15, y = Inf, label = "Tight pooling",
           vjust = 1.5, hjust = -0.1, size = 3, color = "gray50") +
  annotate("text", x = 0.4, y = Inf, label = "Weak pooling",
           vjust = 1.5, hjust = -0.1, size = 3, color = "gray50") +
  labs(title = "Between-Education Variance Across Hierarchical Parameters",
       subtitle = "Smaller σ = stronger pooling across education levels",
       x = "Between-education SD (σ)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())

ggsave("reports/figures/hierarchical-variance-comparison.png",
       p1, width = 10, height = 6, dpi = 300)
cat("  Saved: reports/figures/hierarchical-variance-comparison.png\n")

# ============================================================================
# FIGURE 2: Education-specific parameter values
# ============================================================================

# Equilibrium unemployment by education
ueq <- all_summary[grepl("^u_equilibrium\\[", variable), ]
ueq[, edu_label := factor(edu_labels, levels = rev(edu_labels))]

p2 <- ggplot(ueq, aes(x = mean * 100, y = edu_label)) +
  geom_point(size = 3, color = "#2166AC") +
  geom_errorbar(aes(xmin = q5 * 100, xmax = q95 * 100),
                width = 0.2, color = "#2166AC", orientation = "y") +
  labs(title = "Equilibrium Unemployment by Education Level",
       subtitle = "Long-term unemployment rate from ODE dynamics",
       x = "Equilibrium unemployment rate (%)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank())

ggsave("reports/figures/education-specific-parameters.png",
       p2, width = 10, height = 5, dpi = 300)
cat("  Saved: reports/figures/education-specific-parameters.png\n")

cat("All report figures regenerated.\n")
