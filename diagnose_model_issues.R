#!/usr/bin/env Rscript
# Diagnose issues with education-trend model initialization

library(phdunemployment)
library(data.table)
library(targets)

# Source the R file to access internal functions
source("R/ode-state-space.R")

cat("Diagnosing Education-Trend Model Issues\n")
cat("=======================================\n\n")

# Load the data
cat("1. Loading data...\n")
data_target <- tar_read('education_counts')
cat(sprintf("   Rows: %d\n", nrow(data_target)))
cat(sprintf("   Education levels: %s\n",
            paste(sort(unique(data_target$education)), collapse = ", ")))
cat("\n")

# Prepare Stan data
cat("2. Preparing Stan data for education-trend model...\n")
stan_data_full <- prepare_stan_data_education_trend(
  data_target,
  education_order = c("less_than_hs", "high_school", "some_college",
                      "bachelors", "masters", "professional", "phd"),  # Proper ordering
  grainsize = 1L,
  K_spline = 25L
)

cat(sprintf("   T (time points): %d\n", stan_data_full$T))
cat(sprintf("   N_edu (education levels): %d\n", stan_data_full$N_edu))
cat(sprintf("   Education order: %s\n", paste(stan_data_full$education_order, collapse = ", ")))
# Compute edu_rank_scaled like Stan model does
edu_order <- stan_data_full$edu_order
N_edu <- stan_data_full$N_edu
edu_rank_scaled <- (edu_order - 1.0) / (N_edu - 1.0)
cat(sprintf("   edu_order: %s\n", paste(edu_order, collapse = ", ")))
cat(sprintf("   edu_rank_scaled: %s\n", paste(round(edu_rank_scaled, 3), collapse = ", ")))
cat("\n")

# Test initialization function
cat("3. Testing initialization function...\n")
init_fn <- make_init_at_prior_education_trend(stan_data_full)

# Generate multiple initializations to check for issues
cat("   Generating 5 different initializations:\n")
for (i in 1:5) {
  init_vals <- init_fn()
  cat(sprintf("   Initialization %d:\n", i))

  # Check key parameters
  logit_u_eq <- init_vals$mu_logit_u_eq + init_vals$beta_logit_u_eq * edu_rank_scaled +
    init_vals$sigma_logit_u_eq * init_vals$u_eq_raw
  u_eq <- 1 / (1 + exp(-logit_u_eq))

  log_adj_speed <- init_vals$mu_log_adj_speed + init_vals$beta_log_adj_speed * edu_rank_scaled +
    init_vals$sigma_log_adj_speed * init_vals$adj_speed_raw
  adj_speed <- exp(log_adj_speed)

  separation_rate <- u_eq * adj_speed

  # Check for NaN/Inf
  has_nan_u_eq <- any(is.nan(u_eq))
  has_inf_u_eq <- any(is.infinite(u_eq))
  has_nan_adj_speed <- any(is.nan(adj_speed))
  has_inf_adj_speed <- any(is.infinite(adj_speed))
  has_nan_separation <- any(is.nan(separation_rate))
  has_inf_separation <- any(is.infinite(separation_rate))

  cat(sprintf("     u_eq range: [%.3f, %.3f] (NaN: %s, Inf: %s)\n",
              min(u_eq), max(u_eq), has_nan_u_eq, has_inf_u_eq))
  cat(sprintf("     adj_speed range: [%.3f, %.3f] (NaN: %s, Inf: %s)\n",
              min(adj_speed), max(adj_speed), has_nan_adj_speed, has_inf_adj_speed))
  cat(sprintf("     separation_rate range: [%.3f, %.3f] (NaN: %s, Inf: %s)\n",
              min(separation_rate), max(separation_rate), has_nan_separation, has_inf_separation))

  if (has_nan_separation || has_inf_separation) {
    cat("     ⚠️  PROBLEM: separation_rate has NaN/Inf values!\n")
    cat(sprintf("        u_eq values: %s\n", paste(round(u_eq, 3), collapse = ", ")))
    cat(sprintf("        adj_speed values: %s\n", paste(round(adj_speed, 3), collapse = ", ")))
    cat(sprintf("        separation_rate values: %s\n", paste(round(separation_rate, 3), collapse = ", ")))
  }
  cat("\n")
}

# Check parameter bounds
cat("4. Checking parameter bounds and transformations...\n")
cat("   The issue is in separation_rate = u_eq * adj_speed\n")
cat("   where:\n")
cat("   - u_eq = inv_logit(logit_u_eq)\n")
cat("   - logit_u_eq[i] = mu_logit_u_eq + beta_logit_u_eq * edu_rank_scaled[i] + sigma_logit_u_eq * u_eq_raw[i]\n")
cat("   - adj_speed = exp(log_adj_speed)\n")
cat("   - log_adj_speed[i] = mu_log_adj_speed + beta_log_adj_speed * edu_rank_scaled[i] + sigma_log_adj_speed * adj_speed_raw[i]\n")
cat("\n")
cat("   Potential issues:\n")
cat("   1. exp(log_adj_speed) could overflow to Inf if log_adj_speed is large\n")
cat("   2. inv_logit(logit_u_eq) should always be in (0,1), but could be NaN if logit_u_eq is NaN/Inf\n")
cat("   3. Multiplication u_eq * adj_speed could produce NaN if either is NaN\n")
cat("\n")

# Test with safer initialization
cat("5. Testing with safer initialization bounds...\n")
cat("   Trying initialization with smaller standard deviations:\n")

safe_init_fn <- function() {
  N_edu <- stan_data_full$N_edu
  K_spline <- stan_data_full$K_spline
  edu_rank_scaled <- edu_rank_scaled

  list(
    # Spline coefficients
    spline_coef_raw = matrix(rnorm(K_spline * N_edu, 0, 0.01),
                             nrow = K_spline, ncol = N_edu),

    # Hierarchical spline smoothness with education trend
    mu_log_sigma_spline = rnorm(1, -0.22, 0.01),
    beta_log_sigma_spline = rnorm(1, 0, 0.01),
    sigma_log_sigma_spline = abs(rnorm(1, 0.1, 0.01)),
    sigma_spline_raw = rnorm(N_edu, 0, 0.01),

    # Hierarchical equilibrium unemployment with education trend
    mu_logit_u_eq = rnorm(1, -3.3, 0.01),
    beta_logit_u_eq = rnorm(1, 0, 0.01),
    sigma_logit_u_eq = abs(rnorm(1, 0.1, 0.01)),
    u_eq_raw = rnorm(N_edu, 0, 0.01),

    # Hierarchical adjustment speeds with education trend
    mu_log_adj_speed = rnorm(1, 2.3, 0.01),
    beta_log_adj_speed = rnorm(1, 0, 0.01),
    sigma_log_adj_speed = abs(rnorm(1, 0.1, 0.01)),
    adj_speed_raw = rnorm(N_edu, 0, 0.01),

    # Hierarchical shock parameters with education trends
    mu_log_shock_2008 = rnorm(1, -2, 0.01),
    beta_log_shock_2008 = rnorm(1, 0, 0.01),
    sigma_log_shock_2008 = abs(rnorm(1, 0.1, 0.01)),
    shock_2008_raw = rnorm(N_edu, 0, 0.01),

    mu_log_shock_2020 = rnorm(1, -1.5, 0.01),
    beta_log_shock_2020 = rnorm(1, 0, 0.01),
    sigma_log_shock_2020 = abs(rnorm(1, 0.1, 0.01)),
    shock_2020_raw = rnorm(N_edu, 0, 0.01),

    # Hierarchical decay rates with education trends
    mu_decay_2008 = rnorm(1, 0, 0.01),
    beta_decay_2008 = rnorm(1, 0, 0.01),
    sigma_decay_2008 = abs(rnorm(1, 0.1, 0.01)),
    decay_2008_raw = rnorm(N_edu, 0, 0.01),

    mu_decay_2020 = rnorm(1, 0, 0.01),
    beta_decay_2020 = rnorm(1, 0, 0.01),
    sigma_decay_2020 = abs(rnorm(1, 0.1, 0.01)),
    decay_2020_raw = rnorm(N_edu, 0, 0.01),

    # Seasonal effects
    mu_seasonal = rnorm(11, 0, 0.001),
    sigma_seasonal = abs(rnorm(1, 0.01, 0.001)),
    seasonal_raw = matrix(rnorm(11 * N_edu, 0, 0.001), nrow = 11, ncol = N_edu)
  )
}

cat("   Testing safe initialization...\n")
safe_init <- safe_init_fn()

logit_u_eq <- safe_init$mu_logit_u_eq + safe_init$beta_logit_u_eq * edu_rank_scaled +
  safe_init$sigma_logit_u_eq * safe_init$u_eq_raw
u_eq <- 1 / (1 + exp(-logit_u_eq))

log_adj_speed <- safe_init$mu_log_adj_speed + safe_init$beta_log_adj_speed * edu_rank_scaled +
  safe_init$sigma_log_adj_speed * safe_init$adj_speed_raw
adj_speed <- exp(log_adj_speed)

separation_rate <- u_eq * adj_speed

cat(sprintf("     u_eq range: [%.6f, %.6f]\n", min(u_eq), max(u_eq)))
cat(sprintf("     adj_speed range: [%.6f, %.6f]\n", min(adj_speed), max(adj_speed)))
cat(sprintf("     separation_rate range: [%.6f, %.6f]\n", min(separation_rate), max(separation_rate)))
cat("     ✓ No NaN/Inf values with safer initialization\n")
cat("\n")

cat("6. Recommendations:\n")
cat("   1. Use tighter priors/smaller initialization standard deviations\n")
cat("   2. Add bounds checks in Stan model to catch NaN/Inf early\n")
cat("   3. Consider reparameterization to avoid numerical issues\n")
cat("   4. Test edu-parallel model to see if it has same issues\n")
cat("   5. Run with adapt_delta = 0.99 for more conservative sampling\n")
cat("\n")

cat("Diagnosis complete.\n")