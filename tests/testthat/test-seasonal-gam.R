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
