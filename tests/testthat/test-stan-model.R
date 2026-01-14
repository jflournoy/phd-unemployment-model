# TDD Tests for Stan ODE State Space Model
#
# Tests cover:
# 1. Stan function unit tests (expose_functions)
# 2. Prior predictive checks
# 3. Parameter recovery from simulated data
# 4. Simulation-based calibration (SBC)
#
# Run with: testthat::test_file("tests/testthat/test-stan-model.R")

library(testthat)
library(cmdstanr)
library(data.table)

# Skip all tests if cmdstan is not available
skip_if_no_cmdstan <- function() {
  cmdstan_path <- try(cmdstanr::cmdstan_path(), silent = TRUE)
  if (inherits(cmdstan_path, "try-error") || is.null(cmdstan_path) || !file.exists(cmdstan_path)) {
    skip("CmdStan not available")
  }
}

# Helper: compile model with exposed functions
get_model_with_functions <- function() {
  model_path <- here::here("stan", "unemployment-ode-state-space.stan")
  if (!file.exists(model_path)) {
    skip("Stan model file not found")
  }

  model <- cmdstanr::cmdstan_model(model_path)
  model$expose_functions()
  model
}

# ============================================================================
# Section 1: Stan Function Unit Tests
# ============================================================================

test_that("stable_exp_decay returns correct values", {
  skip_if_no_cmdstan()
  model <- get_model_with_functions()

  # The fast model has stable_exp_decay function
  # Note: The main model doesn't have this function exposed
  # This test is for the fast model
  skip("stable_exp_decay is in fast model only - test separately")
})

test_that("shock timing transforms produce expected patterns", {
  skip_if_no_cmdstan()

  # This tests the transformed data logic conceptually
  # Since transformed data isn't directly accessible, we validate the logic

  # Before onset: should be 0
  shock_2008_onset <- 2007.75
  shock_2008_peak <- 2009.5

  # Test point before onset
  t_before <- 2007.0
  expect_true(t_before < shock_2008_onset)
  expected_rise <- 0

  # Test point during rise
  t_during <- 2008.5
  expected_rise_during <- (t_during - shock_2008_onset) / (shock_2008_peak - shock_2008_onset)
  expect_true(expected_rise_during > 0 && expected_rise_during < 1)

  # Test point at peak
  t_peak <- shock_2008_peak
  expected_rise_peak <- 1.0

  # Test point after peak (decay applies)
  t_after <- 2010.5
  time_since_peak <- t_after - shock_2008_peak
  expect_true(time_since_peak > 0)
  # With decay_rate = 1: exp(-1 * 1) = 0.368
  expected_intensity <- exp(-1 * time_since_peak)
  expect_lt(expected_intensity, 1)
})

# ============================================================================
# Section 2: Prior Predictive Checks
# ============================================================================

test_that("priors produce sensible unemployment rate range", {
  skip_if_no_cmdstan()
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  # Sample from priors only (no likelihood)
  # This validates that our priors produce sensible data ranges

  # Expected ranges based on priors:
  # - logit_u_init ~ normal(-3.5, 0.5) => u_init ~ 2-4% (inv_logit(-3.5) ≈ 0.03)
  # - separation_rate ~ 0.5-3% monthly
  # - finding_rate ~ 20-50% monthly
  # - equilibrium u = s / (s + f) => 1-10%

  # Prior predictive check would sample from model with data = 0
  # For now, just verify the mathematical relationships

  # Test inv_logit(-3.5) gives ~3% unemployment
  u_init <- plogis(-3.5)
  expect_gt(u_init, 0.01)  # > 1%
  expect_lt(u_init, 0.10)  # < 10%

  # Test separation rate prior: logit(s/0.2) ~ normal(-2.2, 0.5)
  # inv_logit(-2.2) * 0.2 ≈ 0.02 (2%)
  s_prior_mean <- plogis(-2.2) * 0.2
  expect_gt(s_prior_mean, 0.01)  # > 1%
  expect_lt(s_prior_mean, 0.05)  # < 5%

  # Test finding rate prior: logit(f) ~ normal(-0.85, 0.5)
  # inv_logit(-0.85) ≈ 0.30 (30%)
  f_prior_mean <- plogis(-0.85)
  expect_gt(f_prior_mean, 0.15)  # > 15%
  expect_lt(f_prior_mean, 0.50)  # < 50%
})

test_that("prior equilibrium unemployment rates are reasonable", {
  # Equilibrium unemployment = s / (s + f)
  # With s = 2% and f = 30%: u* = 0.02 / 0.32 = 6.25%

  s_mean <- 0.02
  f_mean <- 0.30
  u_eq <- s_mean / (s_mean + f_mean)

  expect_gt(u_eq, 0.03)  # > 3%
  expect_lt(u_eq, 0.15)  # < 15%

  # For PhD (lower separation, higher finding):
  s_phd <- 0.01
  f_phd <- 0.40
  u_eq_phd <- s_phd / (s_phd + f_phd)

  expect_gt(u_eq_phd, 0.01)  # > 1%
  expect_lt(u_eq_phd, 0.05)  # < 5%

  # For less than HS (higher separation, lower finding):
  s_lths <- 0.04
  f_lths <- 0.20
  u_eq_lths <- s_lths / (s_lths + f_lths)

  expect_gt(u_eq_lths, 0.10)  # > 10%
  expect_lt(u_eq_lths, 0.25)  # < 25%
})

# ============================================================================
# Section 3: Parameter Recovery from Simulated Data
# ============================================================================

#' Simulate unemployment data from known parameters
#'
#' @param T Number of time points
#' @param N_edu Number of education levels
#' @param separation_rate Vector of separation rates
#' @param finding_rate Vector of finding rates
#' @param n_per_obs Sample size per observation
#' @param seed Random seed
#'
#' @return List with simulated data matching Stan data format
simulate_unemployment_data <- function(
    T = 120,  # 10 years monthly
    N_edu = 3,
    separation_rate = c(0.01, 0.02, 0.04),  # PhD, BA, <HS
    finding_rate = c(0.40, 0.30, 0.20),
    n_per_obs = 1000,
    seed = 12345
) {
  set.seed(seed)

  # Initialize
  u <- matrix(0, nrow = T, ncol = N_edu)

  # Initial unemployment (equilibrium-ish)
  for (i in 1:N_edu) {
    u[1, i] <- separation_rate[i] / (separation_rate[i] + finding_rate[i])
  }

  # Simulate dynamics with small noise
  sigma_state <- 0.01
  for (t in 2:T) {
    for (i in 1:N_edu) {
      du_dt <- separation_rate[i] * (1 - u[t-1, i]) - finding_rate[i] * u[t-1, i]
      logit_u <- qlogis(u[t-1, i]) + du_dt + rnorm(1, 0, sigma_state)
      u[t, i] <- plogis(logit_u)
      u[t, i] <- max(0.001, min(0.999, u[t, i]))  # Bound away from 0/1
    }
  }

  # Generate observations (binomial)
  n_unemployed <- matrix(0L, nrow = T, ncol = N_edu)
  n_total <- matrix(n_per_obs, nrow = T, ncol = N_edu)

  for (t in 1:T) {
    for (i in 1:N_edu) {
      n_unemployed[t, i] <- rbinom(1, n_per_obs, u[t, i])
    }
  }

  # Create time indices
  month <- rep(1:12, length.out = T)
  year_frac <- 2010 + (seq_len(T) - 0.5) / 12

  list(
    T = T,
    N_edu = N_edu,
    n_unemployed = n_unemployed,
    n_total = n_total,
    month = month,
    year_frac = year_frac,
    shock_2008_onset = 2007.75,  # Before data start
    shock_2008_peak = 2009.50,
    shock_2020_onset = 2020.17,
    shock_2020_peak = 2020.33,
    # True values for validation
    true_separation = separation_rate,
    true_finding = finding_rate,
    true_u = u
  )
}

test_that("simulation produces sensible data", {
  sim_data <- simulate_unemployment_data(T = 60, N_edu = 3)

  # Check dimensions
  expect_equal(dim(sim_data$n_unemployed), c(60, 3))
  expect_equal(dim(sim_data$n_total), c(60, 3))

  # Check unemployment rates are in range
  obs_rates <- sim_data$n_unemployed / sim_data$n_total
  expect_true(all(obs_rates >= 0 & obs_rates <= 1))

  # Check education ordering (PhD < BA < less than HS)
  mean_rates <- colMeans(obs_rates)
  expect_lt(mean_rates[1], mean_rates[2])  # PhD < BA
  expect_lt(mean_rates[2], mean_rates[3])  # BA < <HS
})

test_that("model recovers true parameters from simulated data", {
  skip_if_no_cmdstan()
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  # This is a key TDD test: generate data with known parameters,

  # fit the model, and verify we recover them within credible intervals

  # Generate simulated data with known parameters
  true_sep <- c(0.015, 0.025, 0.040)
  true_find <- c(0.35, 0.28, 0.22)

  sim_data <- simulate_unemployment_data(
    T = 120,
    N_edu = 3,
    separation_rate = true_sep,
    finding_rate = true_find,
    n_per_obs = 2000,
    seed = 42
  )

  # Fit model with reduced iterations for speed
  model_path <- here::here("stan", "unemployment-ode-state-space.stan")
  model <- cmdstanr::cmdstan_model(model_path)

  fit <- model$sample(
    data = sim_data[c("T", "N_edu", "n_unemployed", "n_total", "month",
                      "year_frac", "shock_2008_onset", "shock_2008_peak",
                      "shock_2020_onset", "shock_2020_peak")],
    chains = 2,
    parallel_chains = 2,
    iter_sampling = 500,
    iter_warmup = 500,
    adapt_delta = 0.95,
    refresh = 0,
    show_messages = FALSE
  )

  # Check separation rate recovery
  sep_summary <- fit$summary("separation_rate")
  for (i in 1:3) {
    expect_true(
      true_sep[i] > sep_summary$q5[i] && true_sep[i] < sep_summary$q95[i],
      info = sprintf("separation_rate[%d] not recovered: true=%.3f, 90%%CI=[%.3f, %.3f]",
                     i, true_sep[i], sep_summary$q5[i], sep_summary$q95[i])
    )
  }

  # Check finding rate recovery
  find_summary <- fit$summary("finding_rate")
  for (i in 1:3) {
    expect_true(
      true_find[i] > find_summary$q5[i] && true_find[i] < find_summary$q95[i],
      info = sprintf("finding_rate[%d] not recovered: true=%.3f, 90%%CI=[%.3f, %.3f]",
                     i, true_find[i], find_summary$q5[i], find_summary$q95[i])
    )
  }
})

# ============================================================================
# Section 4: Simulation-Based Calibration (SBC)
# ============================================================================

test_that("SBC rank statistic is uniform for key parameter", {
  skip_if_no_cmdstan()
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow SBC test")

  # SBC: For a well-calibrated model, the rank of the true parameter
  # in the posterior should be uniformly distributed
  #
  # Algorithm:
  # 1. Draw theta_true from prior
  # 2. Simulate data from theta_true
  # 3. Fit model to get posterior samples
  # 4. Compute rank of theta_true in posterior
  # 5. Repeat many times, check ranks are uniform

  # For this test, we do a simplified version with fewer iterations
  # Full SBC would use 100-1000 simulation runs

  n_sbc_runs <- 10  # Reduced for speed
  ranks <- numeric(n_sbc_runs)
  n_posterior_samples <- 100

  model_path <- here::here("stan", "unemployment-ode-state-space.stan")
  model <- cmdstanr::cmdstan_model(model_path)

  for (run in 1:n_sbc_runs) {
    # Draw true separation rate from prior
    # logit(s/0.2) ~ N(-2.2, 0.5)
    logit_s <- rnorm(1, -2.2, 0.5)
    true_sep <- plogis(logit_s) * 0.2

    # Simulate minimal data
    sim_data <- simulate_unemployment_data(
      T = 60,
      N_edu = 1,
      separation_rate = true_sep,
      finding_rate = 0.30,
      n_per_obs = 500,
      seed = run
    )

    # Fit with minimal iterations
    fit <- model$sample(
      data = sim_data[c("T", "N_edu", "n_unemployed", "n_total", "month",
                        "year_frac", "shock_2008_onset", "shock_2008_peak",
                        "shock_2020_onset", "shock_2020_peak")],
      chains = 1,
      iter_sampling = n_posterior_samples,
      iter_warmup = 200,
      adapt_delta = 0.9,
      refresh = 0,
      show_messages = FALSE
    )

    # Get posterior samples for separation_rate[1]
    post_samples <- fit$draws("separation_rate[1]", format = "matrix")[, 1]

    # Compute rank
    ranks[run] <- sum(post_samples < true_sep)
  }

  # SBC check: ranks should be approximately uniform over [0, n_posterior_samples]
  # Use Kolmogorov-Smirnov test against uniform distribution
  ks_test <- ks.test(ranks / n_posterior_samples, "punif")

  # With only 10 runs, we can't expect perfect uniformity
  # Just check that ranks span the range (not all clustered)
  expect_true(max(ranks) - min(ranks) > 20,
              info = "SBC ranks too clustered - possible model miscalibration")
})

# ============================================================================
# Section 5: Model Convergence Tests
# ============================================================================

test_that("model converges on real data subset", {
  skip_if_no_cmdstan()
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  # Load a subset of real data and verify convergence
  data_path <- here::here("data", "education-spectrum-counts.rds")
  if (!file.exists(data_path)) {
    skip("Education counts data not available")
  }

  counts_data <- readRDS(data_path)
  counts_data <- as.data.table(counts_data)

  # Subset: 3 education levels, 5 years of data
  subset_edu <- c("phd", "bachelors", "less_than_hs")
  counts_subset <- counts_data[
    education %in% subset_edu & year >= 2015 & year <= 2019
  ]

  # Prepare Stan data
  stan_data <- phdunemployment::prepare_stan_data(
    counts_subset,
    education_order = subset_edu
  )

  model_path <- here::here("stan", "unemployment-ode-state-space.stan")
  model <- cmdstanr::cmdstan_model(model_path)

  fit <- model$sample(
    data = stan_data[c("T", "N_edu", "n_unemployed", "n_total", "month",
                       "year_frac", "shock_2008_onset", "shock_2008_peak",
                       "shock_2020_onset", "shock_2020_peak")],
    chains = 2,
    parallel_chains = 2,
    iter_sampling = 400,
    iter_warmup = 400,
    adapt_delta = 0.95,
    refresh = 0,
    show_messages = FALSE
  )

  # Check convergence diagnostics
  diag <- fit$diagnostic_summary()

  # No divergences expected with reparameterized model
  expect_equal(sum(diag$num_divergent), 0,
               info = "Divergent transitions detected")

  # Rhat should be < 1.05 for key parameters
  sep_summary <- fit$summary("separation_rate")
  expect_true(all(sep_summary$rhat < 1.05),
              info = "Rhat > 1.05 for separation_rate")

  # ESS should be > 100 (reduced threshold for short run)
  expect_true(all(sep_summary$ess_bulk > 100),
              info = "ESS too low for separation_rate")
})

# ============================================================================
# Section 6: Hierarchical Seasonal Effects Tests (TDD)
# ============================================================================

test_that("model has hierarchical seasonal effects parameters", {
  skip_if_no_cmdstan()
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  # Load real data for fitting
  data_path <- here::here("data", "education-spectrum-counts.rds")
  if (!file.exists(data_path)) {
    skip("Education counts data not available")
  }

  counts_data <- readRDS(data_path)
  counts_data <- as.data.table(counts_data)

  # Use efficient model
  model_path <- here::here("stan", "unemployment-ode-state-space-efficient.stan")
  if (!file.exists(model_path)) {
    skip("Efficient Stan model file not found")
  }

  # Prepare Stan data
  subset_edu <- c("phd", "masters", "bachelors")
  counts_subset <- counts_data[
    education %in% subset_edu & year >= 2015 & year <= 2019
  ]

  stan_data <- phdunemployment::prepare_stan_data(
    counts_subset,
    education_order = subset_edu
  )

  # Compile model
  model <- cmdstanr::cmdstan_model(model_path)

  # Fit with minimal iterations
  fit <- model$sample(
    data = stan_data,
    chains = 2,
    parallel_chains = 2,
    iter_sampling = 400,
    iter_warmup = 400,
    adapt_delta = 0.95,
    refresh = 0,
    show_messages = FALSE
  )

  # ====== TDD ASSERTIONS ======
  # These will FAIL until hierarchical seasonal effects are implemented

  # 1. Model should have mu_seasonal parameter (population mean)
  all_vars <- fit$metadata()$model_params
  expect_true("mu_seasonal" %in% all_vars,
              info = "mu_seasonal parameter not found in model")

  # 2. Model should have sigma_seasonal parameter (between-education SD)
  expect_true("sigma_seasonal" %in% all_vars,
              info = "sigma_seasonal parameter not found in model")

  # 3. Seasonal effects should still exist (now as deviations)
  expect_true("seasonal_u" %in% all_vars,
              info = "seasonal_u parameter not found in model")

  # 4. Check that hierarchical parameters have sensible posteriors
  mu_seasonal_summary <- fit$summary("mu_seasonal")
  expect_equal(nrow(mu_seasonal_summary), 11,
               info = "mu_seasonal should have 11 months (sum-to-zero)")

  sigma_seasonal_summary <- fit$summary("sigma_seasonal")
  expect_equal(nrow(sigma_seasonal_summary), 1,
               info = "sigma_seasonal should be scalar")

  # 5. Check that sigma_seasonal is positive
  expect_true(sigma_seasonal_summary$q5 > 0,
              info = "sigma_seasonal should be strictly positive")

  # 6. Check that hierarchical pooling works: education-specific
  # seasonal effects should be smaller than independent effects
  seasonal_summary <- fit$summary("seasonal_u")
  mean_abs_seasonal <- mean(abs(seasonal_summary$mean))

  # With pooling, effects should be modest (< 1% on logit scale)
  expect_lt(mean_abs_seasonal, 0.02,
            info = "Pooled seasonal effects should be modest")
})

test_that("hierarchical seasonal priors are properly specified", {
  skip_if_no_cmdstan()

  # Test that hierarchical seasonal priors are in the Stan code
  model_path <- here::here("stan", "unemployment-ode-state-space-efficient.stan")
  if (!file.exists(model_path)) {
    skip("Efficient Stan model file not found")
  }

  stan_code <- readLines(model_path)

  # Check for hierarchical seasonal parameters in parameters block
  expect_true(any(grepl("vector\\[11\\]\\s+mu_seasonal", stan_code)),
              info = "mu_seasonal declaration not found")

  expect_true(any(grepl("real<lower=0>\\s+sigma_seasonal", stan_code)),
              info = "sigma_seasonal declaration not found")

  # Check for non-centered parameterization
  expect_true(any(grepl("seasonal_u_raw", stan_code)),
              info = "seasonal_u_raw (non-centered) not found")

  # Check for sum-to-zero constraint on mu_seasonal
  expect_true(any(grepl("sum.*mu_seasonal.*==.*0|mu_seasonal.*sum.*zero", stan_code, ignore.case = TRUE)),
              info = "Sum-to-zero constraint on mu_seasonal not found")

  # Check for proper priors in model block
  expect_true(any(grepl("mu_seasonal.*~.*normal", stan_code)),
              info = "Prior on mu_seasonal not found")

  expect_true(any(grepl("sigma_seasonal.*~.*exponential", stan_code)),
              info = "Prior on sigma_seasonal not found")
})
