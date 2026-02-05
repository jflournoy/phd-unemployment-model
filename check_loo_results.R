#!/usr/bin/env Rscript
library(targets)
library(loo)

cat("=== CURRENT LOO-CV RESULTS FROM OVERNIGHT REFITTING ===\n\n")

# Read the comparison results
comp <- tar_read('loo_comparison')
cat("LOO Comparison matrix:\n")
print(comp$loo_compare)

cat("\nSummary table:\n")
print(comp$summary)

cat("\n=== ELPD DIFFERENCE ===\n")
cat(sprintf("Education-trend - Edu-parallel: %.3f ± %.3f\n", comp$elpd_diff, comp$se_diff))
cat(sprintf("Difference in SE units: %.3f\n", comp$elpd_diff / comp$se_diff))

# Get Pareto k diagnostics
edu_loo <- tar_read('loo_edu_parallel')
edu_trend_loo <- tar_read('loo_education_trend')

cat("\n=== PARETO K DIAGNOSTICS ===\n")
edu_k <- edu_loo$diagnostics$pareto_k
edu_trend_k <- edu_trend_loo$diagnostics$pareto_k

cat(sprintf("Edu-parallel: %d of %d (%.1f%%) have k > 0.7\n",
            sum(edu_k > 0.7), length(edu_k), 100*sum(edu_k > 0.7)/length(edu_k)))
cat(sprintf("  Max k: %.3f\n", max(edu_k)))

cat(sprintf("Education-trend: %d of %d (%.1f%%) have k > 0.7\n",
            sum(edu_trend_k > 0.7), length(edu_trend_k), 100*sum(edu_trend_k > 0.7)/length(edu_trend_k)))
cat(sprintf("  Max k: %.3f\n", max(edu_trend_k)))

# Try to extract education trend beta parameters
cat("\n=== ATTEMPTING TO EXTRACT EDUCATION TREND PARAMETERS ===\n")
tryCatch({
  fit_obj <- tar_read('model_ode_state_space_education_trend')

  if (inherits(fit_obj$fit, "CmdStanMCMC")) {
    cat("Found CmdStanMCMC fit object\n")

    # Beta parameters
    beta_params <- c("beta_logit_u_eq", "beta_log_adj_speed",
                     "beta_log_shock_2008_effect", "beta_log_shock_2020_effect",
                     "beta_decay_2008", "beta_decay_2020", "beta_log_sigma_spline")

    cat("\nBeta parameter summary (education trend slopes):\n")
    draws <- fit_obj$fit$draws(variables = beta_params, format = "df")

    for (param in beta_params) {
      if (param %in% names(draws)) {
        mean_val <- mean(draws[[param]], na.rm = TRUE)
        sd_val <- sd(draws[[param]], na.rm = TRUE)
        q5 <- quantile(draws[[param]], 0.05, na.rm = TRUE)
        q95 <- quantile(draws[[param]], 0.95, na.rm = TRUE)
        ci_includes_zero <- q5 < 0 && q95 > 0

        cat(sprintf("%-30s: %7.3f ± %.3f, 90%% CI [%6.3f, %6.3f] %s\n",
                    param, mean_val, sd_val, q5, q95,
                    ifelse(ci_includes_zero, "(includes zero)", "(excludes zero)")))
      }
    }
  } else {
    cat("Fit object type:", class(fit_obj$fit)[1], "\n")
  }
}, error = function(e) {
  cat("Error extracting trend parameters:", e$message, "\n")
})

cat("\n=== INTERPRETATION ===\n")
if (abs(comp$elpd_diff) < 2 * comp$se_diff) {
  cat("The ELPD difference is NOT statistically significant (|diff| < 2×SE).\n")
  cat("Adding education trends does not meaningfully improve predictive performance.\n")
} else if (comp$elpd_diff > 0) {
  cat("Education-trend model has BETTER predictive performance (positive ELPD diff).\n")
} else {
  cat("Edu-parallel model has BETTER predictive performance (negative ELPD diff).\n")
}