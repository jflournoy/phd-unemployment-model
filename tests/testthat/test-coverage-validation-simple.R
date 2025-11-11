# Simpler Coverage-based Parameter Recovery Tests
# Tests whether the coverage validation function works correctly

library(here)
source(here("R", "seasonal-gam.R"))
source(here("R", "data-processing.R"))

test_that("validate_parameter_recovery_coverage function exists and works", {
  # Simulate data
  sim_data <- simulate_seasonal_unemployment(
    n_years = 10,
    baseline_rate = 0.025,
    seasonal_amplitude = 0.008,
    noise_sd = 0.002,
    seed = 999
  )

  # Fit model
  model <- fit_seasonal_gam(sim_data, k_month = 10, k_trend = 15)

  true_params <- list(
    baseline = 0.025,
    seasonal_amplitude = 0.008
  )

  # Should work with small n_sims for testing
  expect_no_error({
    result <- validate_parameter_recovery_coverage(
      model,
      sim_data,
      true_params,
      n_sims = 10  # Small for fast testing
    )
  })

  # Result should be a data frame with coverage statistics
  expect_s3_class(result, "data.frame")
  expect_true("parameter" %in% names(result))
  expect_true("coverage_rate" %in% names(result))
  expect_true("target_coverage" %in% names(result))
  expect_true("meets_target" %in% names(result))

  # Should have entries for prediction interval, baseline CI, and amplitude CI
  expect_equal(nrow(result), 3)
  expect_true("prediction_interval" %in% result$parameter)
  expect_true("baseline_CI" %in% result$parameter)
  expect_true("seasonal_amplitude_CI" %in% result$parameter)
})

test_that("coverage rates are between 0 and 1", {
  # Simulate data
  sim_data <- simulate_seasonal_unemployment(
    n_years = 10,
    baseline_rate = 0.025,
    seasonal_amplitude = 0.008,
    noise_sd = 0.002,
    seed = 123
  )

  model <- fit_seasonal_gam(sim_data, k_month = 10, k_trend = 15)
  true_params <- list(baseline = 0.025, seasonal_amplitude = 0.008)

  result <- validate_parameter_recovery_coverage(
    model, sim_data, true_params, n_sims = 10
  )

  # All coverage rates should be valid probabilities
  expect_true(all(result$coverage_rate >= 0))
  expect_true(all(result$coverage_rate <= 1))
})

test_that("coverage validation works with different parameters", {
  # Test with different true parameters
  sim_data <- simulate_seasonal_unemployment(
    n_years = 10,
    baseline_rate = 0.03,  # Different baseline
    seasonal_amplitude = 0.01,  # Different amplitude
    noise_sd = 0.003,
    seed = 456
  )

  model <- fit_seasonal_gam(sim_data, k_month = 10, k_trend = 15)
  true_params <- list(baseline = 0.03, seasonal_amplitude = 0.01)

  expect_no_error({
    result <- validate_parameter_recovery_coverage(
      model, sim_data, true_params, n_sims = 10
    )
  })

  expect_equal(nrow(result), 3)
})

test_that("tolerance parameter controls meets_target assessment", {
  # Test that tolerance parameter works
  sim_data <- simulate_seasonal_unemployment(
    n_years = 10,
    baseline_rate = 0.025,
    seasonal_amplitude = 0.008,
    noise_sd = 0.002,
    seed = 789
  )

  model <- fit_seasonal_gam(sim_data, k_month = 10, k_trend = 15)
  true_params <- list(baseline = 0.025, seasonal_amplitude = 0.008)

  # Strict tolerance
  result_strict <- validate_parameter_recovery_coverage(
    model, sim_data, true_params, n_sims = 10, tolerance = 0.05
  )

  # Lenient tolerance
  result_lenient <- validate_parameter_recovery_coverage(
    model, sim_data, true_params, n_sims = 10, tolerance = 0.20
  )

  # Should have same coverage rates but potentially different meets_target
  expect_equal(result_strict$coverage_rate, result_lenient$coverage_rate)

  # Lenient should have at least as many TRUE meets_target
  expect_gte(sum(result_lenient$meets_target), sum(result_strict$meets_target))
})
