# TDD Tests for Stan reduce_sum Threading Optimization
#
# Tests verify that the threaded model (using reduce_sum) produces:
# 1. Equivalent parameter estimates to the original model
# 2. Equivalent predicted unemployment rates
# 3. Correct reduce_sum structure in Stan code
# 4. Performance improvement when using multiple threads
#
# Run with: testthat::test_file("tests/testthat/test-stan-reduce-sum.R")

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

# ============================================================================
# Section 1: Stan Code Structure Tests
# ============================================================================

test_that("threaded model file exists", {
  model_path <- here::here("stan", "unemployment-ode-state-space-threaded.stan")
  expect_true(file.exists(model_path),
              info = "unemployment-ode-state-space-threaded.stan not found")
})

test_that("threaded model uses reduce_sum for ODE computation", {
  model_path <- here::here("stan", "unemployment-ode-state-space-threaded.stan")
  if (!file.exists(model_path)) {
    skip("Threaded Stan model file not found")
  }

  stan_code <- readLines(model_path)

  # Check for reduce_sum call
  expect_true(any(grepl("reduce_sum\\(", stan_code)),
              info = "reduce_sum() call not found in threaded model")

  # Check for partial sum function definition
  # reduce_sum requires a function with signature:
  # real function_name(..., int start, int end, ...)
  # Function name can be partial_sum, partial_likelihood, etc.
  expect_true(any(grepl("real\\s+partial_\\w+\\s*\\(", stan_code)),
              info = "Partial sum function not found in threaded model")

  # Check that partial sum function has correct signature (start and end parameters)
  has_start <- any(grepl("int\\s+start", stan_code))
  has_end <- any(grepl("int\\s+end", stan_code))
  expect_true(has_start && has_end,
              info = "Partial sum function doesn't have both start and end parameters")
})

test_that("threaded model has grainsize parameter", {
  model_path <- here::here("stan", "unemployment-ode-state-space-threaded.stan")
  if (!file.exists(model_path)) {
    skip("Threaded Stan model file not found")
  }

  stan_code <- readLines(model_path)

  # Check for grainsize parameter (controls threading granularity)
  expect_true(any(grepl("int.*grainsize", stan_code, ignore.case = TRUE)),
              info = "grainsize parameter not found - needed for reduce_sum")
})

# ============================================================================
# Section 2: Model Equivalence Tests
# ============================================================================

test_that("threaded model produces equivalent parameter estimates to original", {
  skip_if_no_cmdstan()
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  # Generate simulated data with known parameters
  true_sep <- c(0.015, 0.025, 0.040)
  true_find <- c(0.35, 0.28, 0.22)

  # Use simulation function from test-stan-model.R
  source(here::here("tests", "testthat", "test-stan-model.R"), local = TRUE)
  sim_data <- simulate_unemployment_data(
    T = 60,  # Shorter for speed
    N_edu = 3,
    separation_rate = true_sep,
    finding_rate = true_find,
    n_per_obs = 2000,
    seed = 123
  )

  stan_data <- sim_data[c("T", "N_edu", "n_unemployed", "n_total", "month",
                          "year_frac", "shock_2008_onset", "shock_2008_peak",
                          "shock_2020_onset", "shock_2020_peak")]

  # Add grainsize for threading (1 = each education level is a separate task)
  stan_data$grainsize <- 1L

  # Fit original model
  original_path <- here::here("stan", "unemployment-ode-state-space.stan")
  original_model <- cmdstanr::cmdstan_model(original_path)

  original_fit <- original_model$sample(
    data = stan_data[c("T", "N_edu", "n_unemployed", "n_total", "month",
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

  # Fit threaded model (single thread first to verify equivalence)
  threaded_path <- here::here("stan", "unemployment-ode-state-space-threaded.stan")
  threaded_model <- cmdstanr::cmdstan_model(threaded_path, cpp_options = list(stan_threads = TRUE))

  threaded_fit <- threaded_model$sample(
    data = stan_data,
    chains = 2,
    parallel_chains = 2,
    threads_per_chain = 1,  # Single thread for equivalence test
    iter_sampling = 500,
    iter_warmup = 500,
    adapt_delta = 0.95,
    refresh = 0,
    show_messages = FALSE
  )

  # Compare separation rate estimates
  orig_sep <- original_fit$summary("separation_rate")
  thread_sep <- threaded_fit$summary("separation_rate")

  for (i in 1:3) {
    # Means should be very close (within 5%)
    expect_equal(orig_sep$mean[i], thread_sep$mean[i], tolerance = 0.05,
                info = sprintf("separation_rate[%d] means differ: orig=%.4f, thread=%.4f",
                              i, orig_sep$mean[i], thread_sep$mean[i]))

    # Credible intervals should overlap substantially
    orig_ci <- c(orig_sep$q5[i], orig_sep$q95[i])
    thread_ci <- c(thread_sep$q5[i], thread_sep$q95[i])

    # Check for overlap
    overlap <- max(0, min(orig_ci[2], thread_ci[2]) - max(orig_ci[1], thread_ci[1]))
    ci_width <- mean(c(orig_ci[2] - orig_ci[1], thread_ci[2] - thread_ci[1]))

    expect_gt(overlap / ci_width, 0.5,
             info = sprintf("separation_rate[%d] CIs don't overlap enough", i))
  }

  # Compare finding rate estimates
  orig_find <- original_fit$summary("finding_rate")
  thread_find <- threaded_fit$summary("finding_rate")

  for (i in 1:3) {
    expect_equal(orig_find$mean[i], thread_find$mean[i], tolerance = 0.05,
                info = sprintf("finding_rate[%d] means differ: orig=%.4f, thread=%.4f",
                              i, orig_find$mean[i], thread_find$mean[i]))
  }
})

test_that("threaded model produces equivalent predicted unemployment rates", {
  skip_if_no_cmdstan()
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  # Generate minimal test data
  true_sep <- c(0.02, 0.03)
  true_find <- c(0.30, 0.25)

  source(here::here("tests", "testthat", "test-stan-model.R"), local = TRUE)
  sim_data <- simulate_unemployment_data(
    T = 48,
    N_edu = 2,
    separation_rate = true_sep,
    finding_rate = true_find,
    n_per_obs = 1500,
    seed = 456
  )

  stan_data <- sim_data[c("T", "N_edu", "n_unemployed", "n_total", "month",
                          "year_frac", "shock_2008_onset", "shock_2008_peak",
                          "shock_2020_onset", "shock_2020_peak")]
  stan_data$grainsize <- 1L

  # Fit both models
  original_path <- here::here("stan", "unemployment-ode-state-space.stan")
  original_model <- cmdstanr::cmdstan_model(original_path)

  original_fit <- original_model$sample(
    data = stan_data[names(stan_data) != "grainsize"],
    chains = 1,
    iter_sampling = 300,
    iter_warmup = 300,
    refresh = 0,
    show_messages = FALSE
  )

  threaded_path <- here::here("stan", "unemployment-ode-state-space-threaded.stan")
  threaded_model <- cmdstanr::cmdstan_model(threaded_path, cpp_options = list(stan_threads = TRUE))

  threaded_fit <- threaded_model$sample(
    data = stan_data,
    chains = 1,
    threads_per_chain = 1,
    iter_sampling = 300,
    iter_warmup = 300,
    refresh = 0,
    show_messages = FALSE
  )

  # Compare predicted unemployment rates at several time points
  # Check t=1, t=24, t=48
  test_times <- c(1, 24, 48)
  test_edu <- c(1, 2)

  for (t in test_times) {
    for (edu in test_edu) {
      var_name <- sprintf("u[%d,%d]", t, edu)

      orig_u <- original_fit$summary(var_name)
      thread_u <- threaded_fit$summary(var_name)

      # Means should be very close
      expect_equal(orig_u$mean, thread_u$mean, tolerance = 0.02,
                  info = sprintf("u[%d,%d] means differ: orig=%.4f, thread=%.4f",
                                t, edu, orig_u$mean, thread_u$mean))
    }
  }
})

# ============================================================================
# Section 3: Threading Performance Tests
# ============================================================================

test_that("threaded model runs faster with multiple threads", {
  skip_if_no_cmdstan()
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  # Generate data with enough education levels to benefit from threading
  N_edu <- 6  # More education levels = more parallelization benefit
  true_sep <- seq(0.01, 0.05, length.out = N_edu)
  true_find <- seq(0.35, 0.20, length.out = N_edu)

  source(here::here("tests", "testthat", "test-stan-model.R"), local = TRUE)
  sim_data <- simulate_unemployment_data(
    T = 60,
    N_edu = N_edu,
    separation_rate = true_sep,
    finding_rate = true_find,
    n_per_obs = 1000,
    seed = 789
  )

  stan_data <- sim_data[c("T", "N_edu", "n_unemployed", "n_total", "month",
                          "year_frac", "shock_2008_onset", "shock_2008_peak",
                          "shock_2020_onset", "shock_2020_peak")]
  stan_data$grainsize <- 1L

  threaded_path <- here::here("stan", "unemployment-ode-state-space-threaded.stan")
  threaded_model <- cmdstanr::cmdstan_model(threaded_path, cpp_options = list(stan_threads = TRUE))

  # Benchmark single-threaded
  time_1_thread <- system.time({
    fit_1 <- threaded_model$sample(
      data = stan_data,
      chains = 1,
      threads_per_chain = 1,
      iter_sampling = 200,
      iter_warmup = 200,
      refresh = 0,
      show_messages = FALSE
    )
  })["elapsed"]

  # Benchmark multi-threaded (use 4 threads if available)
  n_threads <- min(4, parallel::detectCores() - 1, N_edu)

  time_n_threads <- system.time({
    fit_n <- threaded_model$sample(
      data = stan_data,
      chains = 1,
      threads_per_chain = n_threads,
      iter_sampling = 200,
      iter_warmup = 200,
      refresh = 0,
      show_messages = FALSE
    )
  })["elapsed"]

  # Calculate speedup
  speedup <- time_1_thread / time_n_threads

  # We should see at least 1.3x speedup with 4 threads
  # (accounting for overhead and Amdahl's law - not all code parallelizes)
  expect_gt(speedup, 1.3,
           info = sprintf("Expected speedup >1.3x, got %.2fx (1 thread: %.1fs, %d threads: %.1fs)",
                         speedup, time_1_thread, n_threads, time_n_threads))

  # Verify convergence wasn't harmed
  diag_1 <- fit_1$diagnostic_summary()
  diag_n <- fit_n$diagnostic_summary()

  expect_equal(sum(diag_1$num_divergent), 0, info = "Single-thread has divergences")
  expect_equal(sum(diag_n$num_divergent), 0, info = "Multi-thread has divergences")
})

# ============================================================================
# Section 4: Correctness Tests
# ============================================================================

test_that("threaded model has no convergence issues", {
  skip_if_no_cmdstan()
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  # Load real data subset
  data_path <- here::here("data", "education-spectrum-counts.rds")
  if (!file.exists(data_path)) {
    skip("Education counts data not available")
  }

  counts_data <- readRDS(data_path)
  counts_data <- as.data.table(counts_data)

  # Subset: 4 education levels, 3 years of data
  subset_edu <- c("phd", "masters", "bachelors", "some_college")
  counts_subset <- counts_data[
    education %in% subset_edu & year >= 2017 & year <= 2019
  ]

  # Prepare Stan data
  stan_data <- phdunemployment::prepare_stan_data(
    counts_subset,
    education_order = subset_edu
  )
  stan_data$grainsize <- 1L

  threaded_path <- here::here("stan", "unemployment-ode-state-space-threaded.stan")
  threaded_model <- cmdstanr::cmdstan_model(threaded_path, cpp_options = list(stan_threads = TRUE))

  fit <- threaded_model$sample(
    data = stan_data,
    chains = 2,
    parallel_chains = 2,
    threads_per_chain = 2,
    iter_sampling = 400,
    iter_warmup = 400,
    adapt_delta = 0.95,
    refresh = 0,
    show_messages = FALSE
  )

  # Check convergence diagnostics
  diag <- fit$diagnostic_summary()

  expect_equal(sum(diag$num_divergent), 0,
               info = "Divergent transitions detected in threaded model")

  # Rhat should be < 1.05
  sep_summary <- fit$summary("separation_rate")
  expect_true(all(sep_summary$rhat < 1.05),
              info = "Rhat > 1.05 for separation_rate in threaded model")

  # ESS should be > 100
  expect_true(all(sep_summary$ess_bulk > 100),
              info = "ESS too low for separation_rate in threaded model")
})

test_that("threaded model produces sensible unemployment predictions", {
  skip_if_no_cmdstan()
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  # Quick sanity check that threaded model produces realistic unemployment rates
  true_sep <- c(0.02, 0.04)
  true_find <- c(0.30, 0.25)

  source(here::here("tests", "testthat", "test-stan-model.R"), local = TRUE)
  sim_data <- simulate_unemployment_data(
    T = 36,
    N_edu = 2,
    separation_rate = true_sep,
    finding_rate = true_find,
    n_per_obs = 1000,
    seed = 999
  )

  stan_data <- sim_data[c("T", "N_edu", "n_unemployed", "n_total", "month",
                          "year_frac", "shock_2008_onset", "shock_2008_peak",
                          "shock_2020_onset", "shock_2020_peak")]
  stan_data$grainsize <- 1L

  threaded_path <- here::here("stan", "unemployment-ode-state-space-threaded.stan")
  threaded_model <- cmdstanr::cmdstan_model(threaded_path, cpp_options = list(stan_threads = TRUE))

  fit <- threaded_model$sample(
    data = stan_data,
    chains = 1,
    threads_per_chain = 2,
    iter_sampling = 300,
    iter_warmup = 300,
    refresh = 0,
    show_messages = FALSE
  )

  # Check that all predicted unemployment rates are in (0, 1)
  u_summary <- fit$summary(variables = "u")

  expect_true(all(u_summary$mean > 0 & u_summary$mean < 1),
              info = "Predicted unemployment rates outside (0, 1)")

  # Check that education gradient is preserved (edu 1 < edu 2)
  # Sample a few time points
  for (t in c(10, 20, 30)) {
    u1 <- fit$summary(sprintf("u[%d,1]", t))$mean
    u2 <- fit$summary(sprintf("u[%d,2]", t))$mean

    expect_lt(u1, u2,
             info = sprintf("Education gradient reversed at t=%d: u[%d,1]=%.3f >= u[%d,2]=%.3f",
                           t, t, u1, t, u2))
  }
})
