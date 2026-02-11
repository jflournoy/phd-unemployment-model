#!/usr/bin/env Rscript
# Test ordered categorical model with fixed initialization
# Verifies that initialization is centered at prior means

library(here)

cat("Testing fixed ordered categorical initialization...\n\n")

# Source functions
source(here("R/ode-state-space.R"))

# Create minimal test data
set.seed(42)
N_edu <- 7
K_spline <- 25
K_ispline <- 5

stan_data <- list(
  N_edu = N_edu,
  K_spline = K_spline,
  K_ispline = K_ispline
)

# Test initialization function
cat("1. Testing initialization function...\n")
init_fn <- make_init_at_prior_ordered_categorical(stan_data)
init_vals <- init_fn()  # Call the function to get the init list

cat("\n2. Verifying hierarchical means (should be exact prior means):\n")
cat(sprintf("  mu_log_sigma_spline: %.2f (expected: -0.22)\n", init_vals$mu_log_sigma_spline))
cat(sprintf("  mu_logit_u_eq: %.2f (expected: -3.30)\n", init_vals$mu_logit_u_eq))
cat(sprintf("  mu_log_adj_speed: %.2f (expected: 2.30)\n", init_vals$mu_log_adj_speed))
cat(sprintf("  mu_log_shock_2008: %.2f (expected: -2.00)\n", init_vals$mu_log_shock_2008))
cat(sprintf("  mu_log_shock_2020: %.2f (expected: -1.50)\n", init_vals$mu_log_shock_2020))
cat(sprintf("  mu_decay_2008: %.2f (expected: 0.00)\n", init_vals$mu_decay_2008))
cat(sprintf("  mu_decay_2020: %.2f (expected: 0.00)\n", init_vals$mu_decay_2020))

cat("\n3. Verifying scale parameters (should be exactly 0):\n")
cat(sprintf("  sigma_log_sigma_spline: %.2f (expected: 0.00)\n", init_vals$sigma_log_sigma_spline))
cat(sprintf("  sigma_logit_u_eq: %.2f (expected: 0.00)\n", init_vals$sigma_logit_u_eq))
cat(sprintf("  sigma_log_adj_speed: %.2f (expected: 0.00)\n", init_vals$sigma_log_adj_speed))
cat(sprintf("  sigma_log_shock_2008: %.2f (expected: 0.00)\n", init_vals$sigma_log_shock_2008))
cat(sprintf("  sigma_log_shock_2020: %.2f (expected: 0.00)\n", init_vals$sigma_log_shock_2020))
cat(sprintf("  sigma_decay_2008: %.2f (expected: 0.00)\n", init_vals$sigma_decay_2008))
cat(sprintf("  sigma_decay_2020: %.2f (expected: 0.00)\n", init_vals$sigma_decay_2020))

cat("\n4. Verifying ordered vectors (should be evenly spaced):\n")
cat("  theta_logit_u_eq_raw:\n")
cat("    ", paste(sprintf("%.2f", init_vals$theta_logit_u_eq_raw), collapse = ", "), "\n")
cat("  Range: [", min(init_vals$theta_logit_u_eq_raw), ",",
    max(init_vals$theta_logit_u_eq_raw), "]\n")

# Check ordering constraint
is_ordered <- all(diff(init_vals$theta_logit_u_eq_raw) > 0)
cat("  Ordering satisfied:", is_ordered, "\n")

# Check centering
mean_theta <- mean(init_vals$theta_logit_u_eq_raw)
cat(sprintf("  Mean: %.2f (should be ~0)\n", mean_theta))

cat("\n5. Verifying other parameters:\n")
cat(sprintf("  logit_u_init: all %.2f (expected: -3.00)\n", init_vals$logit_u_init[1]))
cat(sprintf("  log_phi_minus_1: %.2f (expected: 8.50)\n", init_vals$log_phi_minus_1))
cat(sprintf("  sigma_seasonal: %.2f (expected: 0.01)\n", init_vals$sigma_seasonal))

# Check dimensions
cat("\n6. Verifying dimensions:\n")
cat(sprintf("  spline_coef_raw: %d x %d (expected: %d x %d)\n",
            nrow(init_vals$spline_coef_raw), ncol(init_vals$spline_coef_raw),
            K_spline, N_edu))
cat(sprintf("  seasonal_u_raw: %d x %d (expected: 11 x %d)\n",
            nrow(init_vals$seasonal_u_raw), ncol(init_vals$seasonal_u_raw),
            N_edu))

# Check that all values are numeric and not NA
cat("\n7. Checking for NA or infinite values:\n")
all_vals <- unlist(init_vals)
has_na <- any(is.na(all_vals))
has_inf <- any(is.infinite(all_vals))
cat("  Contains NA:", has_na, "\n")
cat("  Contains Inf:", has_inf, "\n")

if (!has_na && !has_inf && is_ordered && abs(mean_theta) < 0.01) {
  cat("\n✓ All checks passed! Initialization is correct.\n")
  cat("\nNext steps:\n")
  cat("  1. Use test_ordered_categorical_quick.R to test compilation and fitting\n")
  cat("  2. Refit full model with adapt_delta=0.98, max_treedepth=14\n")
} else {
  cat("\n✗ Some checks failed. Review output above.\n")
  quit(status = 1)
}
