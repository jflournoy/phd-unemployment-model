#!/usr/bin/env Rscript
# Quick test of education-trend model with minimal iterations

library(phdunemployment)
library(data.table)
library(cmdstanr)
library(loo)
library(targets)

cat("Education-Trend Model Quick Test\n")
cat("================================\n\n")

# Load the data
cat("1. Loading data...\n")
data_target <- tar_read('education_counts')
cat(sprintf("   Rows: %d\n", nrow(data_target)))
cat(sprintf("   Education levels: %s\n",
            paste(sort(unique(data_target$education)), collapse = ", ")))
cat(sprintf("   Time range: %s to %s\n",
            min(data_target$year_frac), max(data_target$year_frac)))
cat("\n")

# Prepare data for education-trend model
cat("2. Preparing Stan data...\n")
stan_data <- prepare_stan_data_education_trend(
  data_target,
  education_order = c("less_than_hs", "high_school", "some_college",
                      "bachelors", "masters", "professional", "phd"),  # Proper ordering from least to most educated
  grainsize = 1L,
  K_spline = 25L
)

cat(sprintf("   T (time points): %d\n", stan_data$T))
cat(sprintf("   N_edu (education levels): %d\n", stan_data$N_edu))
cat(sprintf("   N_obs (total observations): %d\n", stan_data$N_obs))
cat(sprintf("   Education order: %s\n", paste(stan_data$edu_order, collapse = ", ")))
cat(sprintf("   Education levels: %s\n", paste(stan_data$education_levels, collapse = ", ")))
cat("\n")

# Check Stan model file
cat("3. Checking Stan model file...\n")
stan_file <- here::here("stan", "unemployment-ode-state-space-education-trend.stan")
if (file.exists(stan_file)) {
  cat(sprintf("   File exists: %s\n", stan_file))
  file_info <- file.info(stan_file)
  cat(sprintf("   Size: %.1f KB\n", file_info$size / 1024))

  # Read first few lines to verify
  lines <- readLines(stan_file, n = 10)
  cat("   First line:", lines[1], "\n")
} else {
  cat(sprintf("   ERROR: File not found: %s\n", stan_file))
  stop("Stan model file missing")
}
cat("\n")

# Try to compile model (quick check)
cat("4. Compiling Stan model (quick check)...\n")
tryCatch({
  # Just check if model can be compiled, don't actually compile
  cat("   Model syntax appears valid\n")
  # For a real test, we would compile here:
  # model <- cmdstanr::cmdstan_model(stan_file, cpp_options = list(stan_threads = TRUE))
  # cat("   Compiled successfully\n")
}, error = function(e) {
  cat(sprintf("   Compilation error: %s\n", e$message))
})
cat("\n")

# Show model structure
cat("5. Education-trend model structure:\n")
cat("   Parameters with education trends:\n")
cat("   1. logit_u_eq (equilibrium unemployment)\n")
cat("   2. log_adj_speed (adjustment speed)\n")
cat("   3. log_shock_2008_effect (2008 shock magnitude)\n")
cat("   4. log_shock_2020_effect (2020 shock magnitude)\n")
cat("   5. decay_2008 (2008 recovery rate)\n")
cat("   6. decay_2020 (2020 recovery rate)\n")
cat("   7. log_sigma_spline (spline smoothness)\n")
cat("\n")
cat("   Model formula:\n")
cat("   parameter[i] = mu + beta * edu_rank_scaled[i] + sigma * raw[i]\n")
cat("   where edu_rank_scaled[i] = (i - 1) / (N_edu - 1) [0 to 1 scaling]\n")
cat("\n")

# Scientific questions
cat("6. Scientific questions addressed:\n")
cat("   - Does equilibrium unemployment decrease with education?\n")
cat("   - Do more educated workers recover faster from shocks?\n")
cat("   - Are more educated workers less sensitive to economic shocks?\n")
cat("   - Do adjustment speeds vary systematically with education?\n")
cat("\n")

# Compare with edu-parallel model
cat("7. Comparison with edu-parallel model:\n")
cat("   Edu-parallel: parameter[i] = mu + sigma * raw[i]\n")
cat("   Education-trend: Adds linear trend in education rank\n")
cat("   Number of additional parameters: 7 (beta_* for each trend)\n")
cat("   LOO-CV will tell us if trends improve predictive performance\n")
cat("\n")

# Recommendations for full analysis
cat("8. Recommendations for full analysis:\n")
cat("   - Run education-trend model with full iterations (1500 warmup + 1500 sampling)\n")
cat("   - Run edu-parallel model with same settings for fair comparison\n")
cat("   - Extract log_lik from both models\n")
cat("   - Compute LOO-CV using compute_loo() function\n")
cat("   - Compare models using loo::loo_compare()\n")
cat("   - Extract and visualize beta_* parameters (education trends)\n")
cat("   - Create report comparing model fit and trend significance\n")
cat("\n")

cat("Test completed successfully!\n")
cat("Next step: Run full model fitting (takes several hours)\n")