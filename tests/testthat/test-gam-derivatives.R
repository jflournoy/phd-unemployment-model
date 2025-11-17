# Tests for GAM derivative calculation and visualization functions

test_that("compute_gam_derivatives returns correct structure", {
  # Load required packages
  library(mgcv)

  # Create simple test data with known trend
  set.seed(123)
  n <- 200
  test_data <- data.frame(
    time_index = rep(1:100, times = 2),
    month = rep(6, 200),
    education = rep(c("phd", "masters"), each = 100),
    unemployment_rate = c(
      0.02 + 0.0001 * (1:100) + rnorm(100, 0, 0.002),  # PhD: slight upward trend
      0.03 + 0.0002 * (1:100) + rnorm(100, 0, 0.002)   # Masters: steeper upward trend
    )
  )
  test_data$education <- factor(test_data$education)

  # Fit simple GAM
  model <- gam(
    unemployment_rate ~ education + s(time_index, by = education, k = 10),
    data = test_data,
    method = "REML"
  )

  # Test that compute_gam_derivatives function exists and returns correct structure
  # 🔴 RED: This should fail because function doesn't exist yet
  expect_true(exists("compute_gam_derivatives"))

  # Compute derivatives
  derivs <- compute_gam_derivatives(
    model = model,
    education_levels = c("phd", "masters"),
    time_points = seq(10, 90, by = 10)
  )

  # Check output structure
  expect_s3_class(derivs, "data.frame")
  expect_true(all(c("time_index", "education", "derivative", "se", "lower", "upper", "significant") %in% names(derivs)))

  # Check data types
  expect_type(derivs$time_index, "double")
  expect_type(derivs$education, "character")
  expect_type(derivs$derivative, "double")
  expect_type(derivs$se, "double")
  expect_type(derivs$lower, "double")
  expect_type(derivs$upper, "double")
  expect_type(derivs$significant, "logical")

  # Check that we get derivatives for both education levels
  expect_equal(length(unique(derivs$education)), 2)
  expect_true("phd" %in% derivs$education)
  expect_true("masters" %in% derivs$education)

  # Check that derivatives are positive (upward trends in test data)
  expect_true(all(derivs$derivative[derivs$education == "phd"] > 0))
  expect_true(all(derivs$derivative[derivs$education == "masters"] > 0))

  # Check that masters derivatives are larger (steeper trend)
  mean_phd_deriv <- mean(derivs$derivative[derivs$education == "phd"])
  mean_masters_deriv <- mean(derivs$derivative[derivs$education == "masters"])
  expect_true(mean_masters_deriv > mean_phd_deriv)
})

test_that("compute_gam_derivatives handles edge cases", {
  library(mgcv)

  # Create test data
  set.seed(456)
  test_data <- data.frame(
    time_index = rep(1:50, times = 2),
    month = rep(6, 100),
    education = rep(c("phd", "bachelors"), each = 50),
    unemployment_rate = c(
      0.02 + rnorm(50, 0, 0.002),
      0.04 + rnorm(50, 0, 0.002)
    )
  )
  test_data$education <- factor(test_data$education)

  model <- gam(
    unemployment_rate ~ education + s(time_index, by = education, k = 8),
    data = test_data,
    method = "REML"
  )

  # 🔴 RED: Should handle NULL time_points (use all available)
  derivs_all <- compute_gam_derivatives(
    model = model,
    education_levels = c("phd"),
    time_points = NULL
  )

  expect_s3_class(derivs_all, "data.frame")
  expect_true(nrow(derivs_all) > 0)

  # 🔴 RED: Should handle single education level
  derivs_single <- compute_gam_derivatives(
    model = model,
    education_levels = "phd",
    time_points = seq(5, 45, by = 5)
  )

  expect_equal(unique(derivs_single$education), "phd")
  expect_equal(nrow(derivs_single), 9)  # 9 time points
})

test_that("plot_gam_derivatives creates valid ggplot", {
  library(mgcv)
  library(ggplot2)

  # Create test data
  set.seed(789)
  test_data <- data.frame(
    time_index = rep(1:100, times = 2),
    month = rep(6, 200),
    education = rep(c("phd", "masters"), each = 100),
    unemployment_rate = c(
      0.02 + 0.0001 * (1:100) + rnorm(100, 0, 0.002),
      0.03 - 0.0001 * (1:100) + rnorm(100, 0, 0.002)  # Masters: downward trend
    )
  )
  test_data$education <- factor(test_data$education)

  model <- gam(
    unemployment_rate ~ education + s(time_index, by = education, k = 10),
    data = test_data,
    method = "REML"
  )

  # Compute derivatives
  derivs <- compute_gam_derivatives(
    model = model,
    education_levels = c("phd", "masters"),
    time_points = seq(10, 90, by = 5)
  )

  # 🔴 RED: Test plot function
  expect_true(exists("plot_gam_derivatives"))

  p <- plot_gam_derivatives(
    derivatives = derivs,
    education_colors = c("phd" = "#000000", "masters" = "#0066CC"),
    education_labels = c("phd" = "PhD", "masters" = "Master's")
  )

  # Check it's a ggplot
  expect_s3_class(p, "ggplot")

  # Check plot has expected layers (ribbon for CI, line for derivative, hline at 0)
  expect_true(length(p$layers) >= 3)
})
