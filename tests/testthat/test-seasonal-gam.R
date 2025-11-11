test_that("simulate_seasonal_unemployment generates valid time series", {
  # Simulate 5 years of monthly data
  sim_data <- simulate_seasonal_unemployment(
    n_years = 5,
    baseline_rate = 0.02,
    trend_slope = 0.001,
    seasonal_amplitude = 0.005,
    noise_sd = 0.002,
    seed = 123
  )

  # Check structure
  expect_s3_class(sim_data, "data.frame")
  expect_equal(nrow(sim_data), 60)  # 5 years * 12 months
  expect_true(all(c("time_index", "year", "month", "unemployment_rate") %in% names(sim_data)))

  # Check data properties
  expect_true(all(sim_data$month >= 1 & sim_data$month <= 12))
  expect_true(all(sim_data$unemployment_rate >= 0 & sim_data$unemployment_rate <= 1))
  expect_equal(min(sim_data$time_index), 1)
  expect_equal(max(sim_data$time_index), 60)
})

test_that("simulate_seasonal_unemployment has correct seasonal pattern", {
  # Simulate with strong seasonal pattern
  sim_data <- simulate_seasonal_unemployment(
    n_years = 10,
    baseline_rate = 0.02,
    trend_slope = 0,  # No trend
    seasonal_amplitude = 0.01,  # Strong seasonality
    noise_sd = 0.001,  # Low noise
    seed = 456
  )

  # Calculate mean rate by month
  monthly_means <- aggregate(unemployment_rate ~ month, data = sim_data, FUN = mean)

  # Seasonal pattern should show variation across months
  # (variance > 0, indicating seasonality is present)
  expect_gt(var(monthly_means$unemployment_rate), 0)

  # Peak should be in summer months (June-August: months 6-8)
  # due to new PhD graduates entering job market
  summer_mean <- mean(sim_data$unemployment_rate[sim_data$month %in% 6:8])
  winter_mean <- mean(sim_data$unemployment_rate[sim_data$month %in% c(12, 1, 2)])
  expect_gt(summer_mean, winter_mean)
})

test_that("fit_seasonal_gam fits model successfully", {
  # Simulate data
  sim_data <- simulate_seasonal_unemployment(
    n_years = 5,
    baseline_rate = 0.02,
    trend_slope = 0.001,
    seasonal_amplitude = 0.005,
    noise_sd = 0.002,
    seed = 789
  )

  # Fit model - this will fail because function doesn't exist
  model <- fit_seasonal_gam(sim_data)

  # Check model object
  expect_s3_class(model, "gam")
  expect_true("formula" %in% names(model))
  expect_true("fitted.values" %in% names(model))
})

test_that("fit_seasonal_gam uses cyclic spline for month", {
  sim_data <- simulate_seasonal_unemployment(n_years = 5, seed = 101)
  model <- fit_seasonal_gam(sim_data)

  # Check that model includes cyclic spline
  smooth_terms <- sapply(model$smooth, function(x) x$label)
  expect_true(any(grepl("month", smooth_terms)))

  # Check for cyclic constraint
  month_smooth <- model$smooth[[which(grepl("month", smooth_terms))]]
  expect_equal(month_smooth$bs.dim, 12)  # Should have 12 basis functions
})

test_that("fit_seasonal_gam includes smooth trend", {
  sim_data <- simulate_seasonal_unemployment(n_years = 5, seed = 102)
  model <- fit_seasonal_gam(sim_data)

  # Check that model includes smooth trend term
  smooth_terms <- sapply(model$smooth, function(x) x$label)
  expect_true(any(grepl("time_index", smooth_terms)))
})

test_that("fit_seasonal_gam predictions are reasonable", {
  sim_data <- simulate_seasonal_unemployment(
    n_years = 5,
    baseline_rate = 0.02,
    trend_slope = 0.001,
    seasonal_amplitude = 0.005,
    noise_sd = 0.002,
    seed = 103
  )

  model <- fit_seasonal_gam(sim_data)

  # Get predictions
  predictions <- predict(model, newdata = sim_data, type = "response")

  # Predictions should be in valid range [0, 1]
  expect_true(all(predictions >= 0 & predictions <= 1))

  # Predictions should be reasonably close to observed values
  # (R² should be > 0.5 for a model with clear seasonal pattern)
  rsq <- cor(sim_data$unemployment_rate, predictions)^2
  expect_gt(rsq, 0.5)
})

test_that("extract_seasonal_component extracts month effects", {
  sim_data <- simulate_seasonal_unemployment(n_years = 5, seed = 104)
  model <- fit_seasonal_gam(sim_data)

  # Extract seasonal component
  seasonal_effects <- extract_seasonal_component(model, sim_data)

  # Should return data frame with month and effect
  expect_s3_class(seasonal_effects, "data.frame")
  expect_true(all(c("month", "seasonal_effect", "se") %in% names(seasonal_effects)))
  expect_equal(nrow(seasonal_effects), 12)  # One row per month
  expect_equal(sort(unique(seasonal_effects$month)), 1:12)
})

test_that("extract_trend_component extracts time trend", {
  sim_data <- simulate_seasonal_unemployment(n_years = 5, seed = 105)
  model <- fit_seasonal_gam(sim_data)

  # Extract trend component
  trend_effects <- extract_trend_component(model, sim_data)

  # Should return data frame with time and effect
  expect_s3_class(trend_effects, "data.frame")
  expect_true(all(c("time_index", "trend_effect", "se") %in% names(trend_effects)))
  expect_equal(nrow(trend_effects), 60)  # One row per time point
})

test_that("model diagnostic checks pass", {
  sim_data <- simulate_seasonal_unemployment(n_years = 5, seed = 106)
  model <- fit_seasonal_gam(sim_data)

  # Check model convergence
  expect_true(model$converged)

  # Check effective degrees of freedom are reasonable
  # (not overfitting with too many df)
  edf <- sum(model$edf)
  expect_lt(edf, 30)  # Should use < 30 effective df for 60 observations

  # Check residuals don't have strong autocorrelation
  # (Durbin-Watson statistic should be close to 2)
  residuals <- residuals(model)
  dw_stat <- sum(diff(residuals)^2) / sum(residuals^2)
  expect_gt(dw_stat, 1.5)  # Not perfect, but reasonable
  expect_lt(dw_stat, 2.5)
})

test_that("compare_model_formulations compares different models", {
  sim_data <- simulate_seasonal_unemployment(n_years = 5, seed = 107)

  # Compare different model formulations
  comparison <- compare_model_formulations(sim_data)

  # Should return comparison table
  expect_s3_class(comparison, "data.frame")
  expect_true(all(c("model_name", "aic", "bic", "r_squared", "edf") %in% names(comparison)))
  expect_gt(nrow(comparison), 1)  # Should have multiple models

  # Best model should be identified
  expect_true("rank" %in% names(comparison))
})

test_that("plot_seasonal_decomposition creates diagnostic plot", {
  sim_data <- simulate_seasonal_unemployment(n_years = 5, seed = 108)
  model <- fit_seasonal_gam(sim_data)

  # This should create a plot (we'll just check it doesn't error)
  expect_no_error(plot_seasonal_decomposition(model, sim_data))
})

# === PARAMETER RECOVERY TESTS ===
# These tests verify the model can recover known parameters from simulated data

test_that("model recovers true baseline unemployment rate", {
  # Simulate data with known baseline, no trend, no seasonality
  true_baseline <- 0.025
  sim_data <- simulate_seasonal_unemployment(
    n_years = 10,
    baseline_rate = true_baseline,
    trend_slope = 0,  # No trend
    seasonal_amplitude = 0,  # No seasonality
    noise_sd = 0.001,  # Low noise
    seed = 201
  )

  # Fit model
  model <- fit_seasonal_gam(sim_data)

  # Extract fitted values (should be close to baseline)
  fitted_vals <- fitted(model)
  mean_fitted <- mean(fitted_vals)

  # Mean fitted value should recover true baseline within 10%
  expect_lt(abs(mean_fitted - true_baseline), 0.0025)
})

test_that("model recovers true linear trend", {
  # Simulate data with known trend, no seasonality
  true_baseline <- 0.02
  true_slope <- 0.0005  # 0.05 percentage point increase per month
  sim_data <- simulate_seasonal_unemployment(
    n_years = 10,
    baseline_rate = true_baseline,
    trend_slope = true_slope,
    seasonal_amplitude = 0,  # No seasonality
    noise_sd = 0.001,
    seed = 202
  )

  # Fit model
  model <- fit_seasonal_gam(sim_data)

  # Extract trend component
  trend <- extract_trend_component(model, sim_data)

  # Calculate estimated slope from trend
  time_points <- trend$time_index
  trend_effects <- trend$trend_effect
  estimated_slope <- lm(trend_effects ~ time_points)$coefficients[2]

  # Estimated slope should be close to true slope
  # (within 20% for nonlinear smooth, may not be exactly linear)
  expect_lt(abs(estimated_slope - true_slope), 0.0001)
})

test_that("model recovers true seasonal amplitude", {
  # Simulate data with known seasonality, no trend
  true_amplitude <- 0.008
  sim_data <- simulate_seasonal_unemployment(
    n_years = 10,
    baseline_rate = 0.02,
    trend_slope = 0,
    seasonal_amplitude = true_amplitude,
    noise_sd = 0.001,
    seed = 203
  )

  # Fit model
  model <- fit_seasonal_gam(sim_data)

  # Extract seasonal component
  seasonal <- extract_seasonal_component(model, sim_data)

  # Calculate amplitude as half the peak-to-trough range
  estimated_amplitude <- (max(seasonal$seasonal_effect) - min(seasonal$seasonal_effect)) / 2

  # Estimated amplitude should be close to true amplitude (within 20%)
  expect_lt(abs(estimated_amplitude - true_amplitude), 0.002)
})

test_that("model recovers seasonal peak timing", {
  # Simulate data with summer peak (June-July, months 6-7)
  sim_data <- simulate_seasonal_unemployment(
    n_years = 10,
    baseline_rate = 0.02,
    trend_slope = 0,
    seasonal_amplitude = 0.008,
    noise_sd = 0.001,
    seed = 204
  )

  # Fit model
  model <- fit_seasonal_gam(sim_data)

  # Extract seasonal component
  seasonal <- extract_seasonal_component(model, sim_data)

  # Find month with maximum seasonal effect
  peak_month <- seasonal$month[which.max(seasonal$seasonal_effect)]

  # Peak should be in summer months (May-August: 5-8)
  expect_true(peak_month %in% 5:8)
})

test_that("validate_parameter_recovery checks all components", {
  # Simulate data with known parameters
  true_params <- list(
    baseline = 0.025,
    trend_slope = 0.0005,
    seasonal_amplitude = 0.008
  )

  sim_data <- simulate_seasonal_unemployment(
    n_years = 10,
    baseline_rate = true_params$baseline,
    trend_slope = true_params$trend_slope,
    seasonal_amplitude = true_params$seasonal_amplitude,
    noise_sd = 0.001,
    seed = 205
  )

  # Fit model
  model <- fit_seasonal_gam(sim_data)

  # Validate recovery - this function doesn't exist yet
  recovery_check <- validate_parameter_recovery(model, sim_data, true_params)

  # Should return a data frame with validation results
  expect_s3_class(recovery_check, "data.frame")
  expect_true(all(c("parameter", "true_value", "estimated_value", "error", "recovered") %in% names(recovery_check)))

  # Most parameters should be successfully recovered
  # (allow 1 parameter to fail due to statistical variation)
  expect_gte(sum(recovery_check$recovered), 2)
})

test_that("validate_parameter_recovery detects poor recovery", {
  # Simulate complex data that may not fit linear+cyclic model well
  # (e.g., with autocorrelation or non-sinusoidal seasonality)
  sim_data <- simulate_seasonal_unemployment(
    n_years = 3,  # Too short for good estimation
    baseline_rate = 0.02,
    trend_slope = 0.001,
    seasonal_amplitude = 0.005,
    noise_sd = 0.01,  # High noise
    seed = 206
  )

  true_params <- list(
    baseline = 0.02,
    trend_slope = 0.001,
    seasonal_amplitude = 0.005
  )

  model <- fit_seasonal_gam(sim_data)
  recovery_check <- validate_parameter_recovery(model, sim_data, true_params)

  # With high noise and short series, some parameters may not recover well
  # (This test just verifies the function reports the failure)
  expect_true(any(!recovery_check$recovered))
})

test_that("parameter recovery improves with more data", {
  # Test that recovery quality improves with sample size
  true_params <- list(
    baseline = 0.025,
    trend_slope = 0.0005,
    seasonal_amplitude = 0.008
  )

  # Small dataset
  sim_small <- simulate_seasonal_unemployment(
    n_years = 3,
    baseline_rate = true_params$baseline,
    trend_slope = true_params$trend_slope,
    seasonal_amplitude = true_params$seasonal_amplitude,
    noise_sd = 0.002,
    seed = 207
  )
  model_small <- fit_seasonal_gam(sim_small)
  recovery_small <- validate_parameter_recovery(model_small, sim_small, true_params)

  # Large dataset
  sim_large <- simulate_seasonal_unemployment(
    n_years = 15,
    baseline_rate = true_params$baseline,
    trend_slope = true_params$trend_slope,
    seasonal_amplitude = true_params$seasonal_amplitude,
    noise_sd = 0.002,
    seed = 208
  )
  model_large <- fit_seasonal_gam(sim_large)
  recovery_large <- validate_parameter_recovery(model_large, sim_large, true_params)

  # Large dataset should have smaller errors (on average)
  # Use median to be robust to outliers
  median_error_small <- median(abs(recovery_small$error))
  median_error_large <- median(abs(recovery_large$error))

  # Or check that more parameters are recovered correctly
  sum_recovered_small <- sum(recovery_small$recovered)
  sum_recovered_large <- sum(recovery_large$recovered)

  # At least one metric should show improvement with more data
  improved_error <- median_error_large < median_error_small
  improved_recovery <- sum_recovered_large >= sum_recovered_small

  expect_true(improved_error || improved_recovery)
})
