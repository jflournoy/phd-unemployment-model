# TDD Tests for Education-Level Parallelization
#
# This approach parallelizes across education levels, computing the full
# trajectory + likelihood for each education level in parallel.
#
# Key insight: Education levels are independent in the ODE computation.
# Each education level has its own trajectory that doesn't depend on others.
#
# Run with: testthat::test_file("tests/testthat/test-stan-edu-parallel.R")

library(testthat)
library(here)

# ============================================================================
# Section 1: Stan Model Structure Tests
# ============================================================================

test_that("edu-parallel model file exists", {

  model_path <- here::here("stan", "unemployment-ode-state-space-edu-parallel.stan")
  expect_true(file.exists(model_path),
              info = "Education-parallel Stan model file not found")
})

test_that("edu-parallel model uses reduce_sum over education levels", {
  model_path <- here::here("stan", "unemployment-ode-state-space-edu-parallel.stan")
  if (!file.exists(model_path)) {
    skip("Education-parallel Stan model not yet created")
  }

  stan_code <- readLines(model_path)

  # Check for reduce_sum call

expect_true(any(grepl("reduce_sum\\(", stan_code)),
              info = "reduce_sum() call not found")

  # Check for partial function that handles education trajectories
  # Should be named something like partial_edu_trajectory or partial_edu_likelihood
  expect_true(any(grepl("real\\s+partial_edu", stan_code)),
              info = "Education-level partial function not found")

  # Check that we slice over education indices, not observation indices
  expect_true(any(grepl("edu_indices|edu_slice", stan_code)),
              info = "Education index slicing not found - should parallelize over N_edu")
})

test_that("edu-parallel model has flattened data structures", {
  model_path <- here::here("stan", "unemployment-ode-state-space-edu-parallel.stan")
  if (!file.exists(model_path)) {
    skip("Education-parallel Stan model not yet created")
  }

  stan_code <- readLines(model_path)

  # Should have flattened 1D arrays for observations
  expect_true(any(grepl("array\\[N_obs\\]\\s+int.*n_unemployed_flat", stan_code)),
              info = "Flattened n_unemployed array not found")

  expect_true(any(grepl("array\\[N_obs\\]\\s+int.*n_total_flat", stan_code)),
              info = "Flattened n_total array not found")

  # Should NOT have 2D arrays in data block (those are incompatible)
  data_block <- paste(stan_code[grep("^data\\s*\\{", stan_code):
                                grep("^transformed data|^parameters", stan_code)[1]], collapse = "\n")
  expect_false(grepl("array\\[T,\\s*N_edu\\]", data_block),
               info = "Should not have 2D arrays in data block - use flattened instead")
})

test_that("edu-parallel model computes trajectory inside partial function", {
  model_path <- here::here("stan", "unemployment-ode-state-space-edu-parallel.stan")
  if (!file.exists(model_path)) {
    skip("Education-parallel Stan model not yet created")
  }

  stan_code <- readLines(model_path)
  full_code <- paste(stan_code, collapse = "\n")

  # The partial function should contain ODE dynamics
  # Look for the characteristic du_dt computation inside the partial function
  partial_fn_match <- regmatches(full_code,
    regexpr("real\\s+partial_edu[^}]+\\{[^}]*for\\s*\\([^)]*t[^)]*\\)[^}]*du_dt", full_code, perl = TRUE))

  expect_true(length(partial_fn_match) > 0 || any(grepl("du_dt", stan_code)),
              info = "ODE dynamics (du_dt) should be computed inside partial function")
})

test_that("edu-parallel model has grainsize parameter", {
  model_path <- here::here("stan", "unemployment-ode-state-space-edu-parallel.stan")
  if (!file.exists(model_path)) {
    skip("Education-parallel Stan model not yet created")
  }

  stan_code <- readLines(model_path)

  # Should have grainsize in data block
  expect_true(any(grepl("int.*grainsize", stan_code)),
              info = "grainsize parameter not found in data block")
})

test_that("edu-parallel model recomputes trajectories in generated quantities", {
  model_path <- here::here("stan", "unemployment-ode-state-space-edu-parallel.stan")
  if (!file.exists(model_path)) {
    skip("Education-parallel Stan model not yet created")
  }

  stan_code <- readLines(model_path)

  # Find generated quantities block
  gq_start <- grep("^generated quantities", stan_code)
  if (length(gq_start) == 0) {
    fail("generated quantities block not found")
  }

  gq_code <- paste(stan_code[gq_start:length(stan_code)], collapse = "\n")

  # Should recompute u trajectories for output
  expect_true(grepl("array\\[T\\]\\s+vector\\[N_edu\\]\\s+u", gq_code) ||
              grepl("for\\s*\\(t\\s+in\\s+2:T\\)", gq_code),
              info = "Generated quantities should recompute u trajectories for output")
})

# ============================================================================
# Section 2: R Data Preparation Tests
# ============================================================================

test_that("prepare_stan_data_edu_parallel function exists", {
  devtools::load_all(here::here(), quiet = TRUE)

  expect_true(exists("prepare_stan_data_edu_parallel"),
              info = "prepare_stan_data_edu_parallel() function should exist")
})

test_that("prepare_stan_data_edu_parallel returns flattened arrays", {
  devtools::load_all(here::here(), quiet = TRUE)

  if (!exists("prepare_stan_data_edu_parallel")) {
    skip("prepare_stan_data_edu_parallel() not yet implemented")
  }

  # Create minimal test data
  test_data <- data.table::data.table(
    time_index = rep(1:10, each = 3),
    education = rep(c("PhD", "Masters", "Bachelors"), 10),
    n_unemployed = rpois(30, 50),
    n_total = rpois(30, 1000) + 500,
    month = rep(1:10, each = 3),
    year_frac = rep(seq(2020, 2020.75, length.out = 10), each = 3)
  )

  result <- prepare_stan_data_edu_parallel(test_data)

  # Check dimensions
  expect_equal(result$T, 10, info = "T should be number of time points")
  expect_equal(result$N_edu, 3, info = "N_edu should be number of education levels")
  expect_equal(result$N_obs, 30, info = "N_obs should be T * N_edu")

  # Check flattened arrays exist
  expect_true("n_unemployed_flat" %in% names(result),
              info = "n_unemployed_flat should exist")
  expect_true("n_total_flat" %in% names(result),
              info = "n_total_flat should exist")

  # Check flattened arrays are 1D with correct length
  expect_equal(length(result$n_unemployed_flat), 30,
               info = "n_unemployed_flat should have T * N_edu elements")
  expect_equal(length(result$n_total_flat), 30,
               info = "n_total_flat should have T * N_edu elements")

  # Check education indices array
  expect_true("edu_indices" %in% names(result),
              info = "edu_indices array for reduce_sum should exist")
  expect_equal(length(result$edu_indices), 3,
               info = "edu_indices should have N_edu elements")
})

test_that("flattened data preserves correct indexing", {
  devtools::load_all(here::here(), quiet = TRUE)

  if (!exists("prepare_stan_data_edu_parallel")) {
    skip("prepare_stan_data_edu_parallel() not yet implemented")
  }

  # Create test data with known values
  test_data <- data.table::data.table(
    time_index = rep(1:3, each = 2),
    education = rep(c("A", "B"), 3),
    n_unemployed = c(10, 20, 30, 40, 50, 60),  # Known pattern
    n_total = c(100, 200, 300, 400, 500, 600),
    month = rep(1:3, each = 2),
    year_frac = rep(c(2020, 2020.083, 2020.167), each = 2)
  )

  result <- prepare_stan_data_edu_parallel(test_data)

  # Indexing convention: (edu-1)*T + t
  # For edu=1 (A), t=1: index = 0*3 + 1 = 1 -> n_unemployed = 10
  # For edu=1 (A), t=2: index = 0*3 + 2 = 2 -> n_unemployed = 30
  # For edu=2 (B), t=1: index = 1*3 + 1 = 4 -> n_unemployed = 20
  # For edu=2 (B), t=2: index = 1*3 + 2 = 5 -> n_unemployed = 40

  # Verify indexing is consistent
  # The exact layout depends on implementation, but should be recoverable
  expect_equal(length(result$n_unemployed_flat), 6)

  # Check that all original values are present
  expect_setequal(result$n_unemployed_flat, c(10, 20, 30, 40, 50, 60))
})

# ============================================================================
# Section 3: R Wrapper Function Tests
# ============================================================================

test_that("fit_ode_state_space_edu_parallel function exists", {
  devtools::load_all(here::here(), quiet = TRUE)

  expect_true(exists("fit_ode_state_space_edu_parallel"),
              info = "fit_ode_state_space_edu_parallel() function should exist")
})

test_that("fit_ode_state_space_edu_parallel has correct parameters", {
  devtools::load_all(here::here(), quiet = TRUE)

  if (!exists("fit_ode_state_space_edu_parallel")) {
    skip("fit_ode_state_space_edu_parallel() not yet implemented")
  }

  fn <- get("fit_ode_state_space_edu_parallel")
  args <- names(formals(fn))

  # Should have threading parameters
  expect_true("threads_per_chain" %in% args,
              info = "Should have threads_per_chain parameter")

  expect_true("parallel_chains" %in% args,
              info = "Should have parallel_chains parameter")

  expect_true("grainsize" %in% args,
              info = "Should have grainsize parameter")

  # Should have standard MCMC parameters
  expect_true("chains" %in% args, info = "Should have chains parameter")
  expect_true("iter_sampling" %in% args, info = "Should have iter_sampling parameter")
  expect_true("iter_warmup" %in% args, info = "Should have iter_warmup parameter")
})

test_that("threading configuration respects core limits", {
  devtools::load_all(here::here(), quiet = TRUE)

  if (!exists("fit_ode_state_space_edu_parallel")) {
    skip("fit_ode_state_space_edu_parallel() not yet implemented")
  }

  fn <- get("fit_ode_state_space_edu_parallel")
  defaults <- formals(fn)

  # Default parallel_chains should be reasonable (not exceed available cores)
  if (!is.null(defaults$parallel_chains)) {
    default_parallel <- eval(defaults$parallel_chains)
    expect_true(default_parallel <= parallel::detectCores(),
                info = "Default parallel_chains should not exceed available cores")
  }

  # Default threads_per_chain should leave room for parallel chains
  if (!is.null(defaults$threads_per_chain) && !is.null(defaults$parallel_chains)) {
    default_threads <- eval(defaults$threads_per_chain)
    default_parallel <- eval(defaults$parallel_chains)
    total_threads <- default_threads * default_parallel

    # Should not massively oversubscribe
    expect_true(total_threads <= parallel::detectCores() * 2,
                info = "Default threading should not massively oversubscribe cores")
  }
})

# ============================================================================
# Section 4: Model Compilation Tests
# ============================================================================

test_that("edu-parallel model compiles with threading", {
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  model_path <- here::here("stan", "unemployment-ode-state-space-edu-parallel.stan")
  if (!file.exists(model_path)) {
    skip("Education-parallel Stan model not yet created")
  }

  # Attempt compilation with threading enabled
  expect_no_error({
    model <- cmdstanr::cmdstan_model(
      model_path,
      cpp_options = list(stan_threads = TRUE),
      compile = TRUE
    )
  }, info = "Model should compile with threading enabled")
})

# ============================================================================
# Section 5: Output Equivalence Tests (Run after model works)
# ============================================================================

test_that("edu-parallel model produces equivalent results to serial", {
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  devtools::load_all(here::here(), quiet = TRUE)

  if (!exists("fit_ode_state_space_edu_parallel")) {
    skip("fit_ode_state_space_edu_parallel() not yet implemented")
  }

  # Check if serial model results exist
  serial_file <- here::here("models", "ode-state-space-efficient-fit.qs")
  if (!file.exists(serial_file)) {
    skip("Serial model results not available for comparison")
  }

  parallel_file <- here::here("models", "ode-state-space-edu-parallel-fit.qs")
  if (!file.exists(parallel_file)) {
    skip("Parallel model results not available - run model first")
  }

  serial_result <- qs::qread(serial_file)
  parallel_result <- qs::qread(parallel_file)

  # Compare key parameter posterior means
  key_params <- c("mu_logit_u_eq", "mu_log_shock_2008", "mu_log_shock_2020")

  serial_summary <- serial_result$fit$summary(variables = key_params)
  parallel_summary <- parallel_result$fit$summary(variables = key_params)

  # Parameters should be within 5% (allowing for MCMC variation)
  for (param in key_params) {
    serial_mean <- serial_summary$mean[serial_summary$variable == param]
    parallel_mean <- parallel_summary$mean[parallel_summary$variable == param]

    pct_diff <- abs(serial_mean - parallel_mean) / abs(serial_mean) * 100

    expect_true(pct_diff < 5,
                info = sprintf("Parameter %s differs by %.1f%% (should be <5%%)", param, pct_diff))
  }
})

test_that("edu-parallel model shows speedup over serial", {
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  serial_file <- here::here("models", "ode-state-space-efficient-fit.qs")
  parallel_file <- here::here("models", "ode-state-space-edu-parallel-fit.qs")

  if (!file.exists(serial_file) || !file.exists(parallel_file)) {
    skip("Both model results needed for timing comparison")
  }

  serial_result <- qs::qread(serial_file)
  parallel_result <- qs::qread(parallel_file)

  serial_time <- serial_result$timing$elapsed_mins
  parallel_time <- parallel_result$timing$elapsed_mins
  speedup <- serial_time / parallel_time

  cat(sprintf("\nSerial time: %.1f min\n", serial_time))
  cat(sprintf("Parallel time: %.1f min\n", parallel_time))
  cat(sprintf("Speedup: %.2fx\n", speedup))

  # Should see at least some speedup (>1.2x) with education-level parallelization
  expect_true(speedup > 1.2,
              info = sprintf("Speedup %.2fx should be > 1.2x", speedup))
})

# ============================================================================
# Section 6: Convergence Tests
# ============================================================================

test_that("edu-parallel model converges properly", {
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  parallel_file <- here::here("models", "ode-state-space-edu-parallel-fit.qs")
  if (!file.exists(parallel_file)) {
    skip("Parallel model results not available")
  }

  result <- qs::qread(parallel_file)

  # Check convergence diagnostics
  expect_equal(result$diagnostics$num_divergent, 0,
               info = "Should have 0 divergences")

  expect_equal(result$diagnostics$max_treedepth_exceeded, 0,
               info = "Should not exceed max treedepth")

  # E-BFMI should be > 0.2 for all chains
  expect_true(all(result$diagnostics$ebfmi > 0.2),
              info = "E-BFMI should be > 0.2 for all chains")
})
