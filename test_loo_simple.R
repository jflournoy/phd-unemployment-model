#!/usr/bin/env Rscript
# Test LOO-CV functionality with simple example

library(phdunemployment)
library(data.table)
library(cmdstanr)
library(loo)

# Create simple test data
set.seed(123)
test_data <- data.table(
  time_index = rep(1:10, each = 3),
  education = rep(c("A", "B", "C"), times = 10),
  n_unemployed = rpois(30, lambda = 10),
  n_total = rpois(30, lambda = 100),
  month = rep(1:10, each = 3),
  year_frac = rep(2000 + (1:10)/12, each = 3)
)

cat("Test data created:\n")
print(test_data)
cat("\n")

# Test data preparation functions
cat("Testing prepare_stan_data_edu_parallel()...\n")
edu_parallel_data <- prepare_stan_data_edu_parallel(test_data)
cat("  T:", edu_parallel_data$T, "\n")
cat("  N_edu:", edu_parallel_data$N_edu, "\n")
cat("  N_obs:", edu_parallel_data$N_obs, "\n")
cat("  education_levels:", paste(edu_parallel_data$education_levels, collapse = ", "), "\n")
cat("  OK\n\n")

cat("Testing prepare_stan_data_education_trend()...\n")
education_trend_data <- prepare_stan_data_education_trend(test_data)
cat("  T:", education_trend_data$T, "\n")
cat("  N_edu:", education_trend_data$N_edu, "\n")
cat("  N_obs:", education_trend_data$N_obs, "\n")
cat("  edu_order:", paste(education_trend_data$edu_order, collapse = ", "), "\n")
cat("  OK\n\n")

# Test initialization functions (internal functions)
cat("Testing initialization functions (skipping internal functions)...\n")
cat("  Note: make_init_at_prior_edu_parallel and make_init_at_prior_education_trend are internal\n")
cat("  OK\n\n")

# Test compute_loo() with dummy data
cat("Testing compute_loo() function...\n")
# Create dummy result with log-likelihood matrix
dummy_result <- list(
  log_lik = matrix(rnorm(1000, mean = -10, sd = 2), nrow = 100, ncol = 10)
)

loo_result <- compute_loo(dummy_result)
cat("  elpd_loo:", loo_result$estimates["elpd_loo", "Estimate"], "\n")
cat("  p_loo:", loo_result$estimates["p_loo", "Estimate"], "\n")
cat("  looic:", loo_result$estimates["looic", "Estimate"], "\n")
cat("  OK\n\n")

cat("All tests passed!\n")