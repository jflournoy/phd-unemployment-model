test_that("quasi-binomial GAM model fits count data across education levels", {
  # Load test data
  data_file <- here::here("data", "education-spectrum-counts.rds")
  skip_if_not(file.exists(data_file), "Count data file not found")
  counts_data <- readRDS(data_file)

  # Fit quasi-binomial model with education effects and shared seasonal component
  model <- mgcv::gam(
    cbind(n_unemployed, n_employed) ~ education +
      s(time_index) +
      s(month, bs = "cc"),
    data = counts_data,
    family = quasibinomial(),
    method = "REML"
  )

  # Model should converge
  expect_true(model$converged, info = "GAM model failed to converge")

  # Model should have reasonable structure
  expect_s3_class(model, "gam")
  expect_length(model$smooth, 2)  # Two smooths: time_index and month
  expect_true(any(grepl("education", names(model$coefficients))))

  # Predictions should be in valid probability range [0, 1]
  pred_data <- data.frame(
    education = rep(unique(counts_data$education), each = 10),
    time_index = rep(seq(1, 308, length.out = 10), length(unique(counts_data$education))),
    month = 6
  )
  preds <- predict(model, newdata = pred_data, type = "response")
  expect_true(all(preds >= 0 & preds <= 1), info = "Predictions outside [0,1] range")
})

test_that("model produces education-specific unemployment estimates", {
  data_file <- here::here("data", "education-spectrum-counts.rds")
  skip_if_not(file.exists(data_file), "Count data file not found")
  counts_data <- readRDS(data_file)

  model <- mgcv::gam(
    cbind(n_unemployed, n_employed) ~ education +
      s(time_index) +
      s(month, bs = "cc"),
    data = counts_data,
    family = quasibinomial(),
    method = "REML"
  )

  # Create predictions for each education level (fixed time point)
  pred_data <- data.frame(
    education = unique(counts_data$education),
    time_index = 200,  # Mid-series
    month = 6
  )

  preds <- predict(model, newdata = pred_data, type = "response", se.fit = TRUE)

  # Each education level should have an estimate
  expect_length(preds$fit, length(unique(counts_data$education)))
  expect_length(preds$se.fit, length(unique(counts_data$education)))

  # Standard errors should be positive
  expect_true(all(preds$se.fit > 0), info = "Standard errors not positive")

  # PhDs should have lower unemployment than less_than_hs
  phd_idx <- which(unique(counts_data$education) == "phd")
  less_hs_idx <- which(unique(counts_data$education) == "less_than_hs")

  phd_unemp <- preds$fit[phd_idx]
  less_hs_unemp <- preds$fit[less_hs_idx]

  expect_lt(phd_unemp, less_hs_unemp)
})

test_that("model captures seasonal patterns", {
  data_file <- here::here("data", "education-spectrum-counts.rds")
  skip_if_not(file.exists(data_file), "Count data file not found")
  counts_data <- readRDS(data_file)

  model <- mgcv::gam(
    cbind(n_unemployed, n_employed) ~ education +
      s(time_index) +
      s(month, bs = "cc"),
    data = counts_data,
    family = quasibinomial(),
    method = "REML"
  )

  # Predictions across months should differ (seasonal effect)
  pred_data <- data.frame(
    education = "phd",
    time_index = 200,
    month = 1:12
  )

  preds <- predict(model, newdata = pred_data, type = "response")

  # Seasonal variation should be present (max - min not zero)
  seasonal_range <- max(preds) - min(preds)
  expect_gt(seasonal_range, 0)
})

test_that("model handles missing data appropriately", {
  data_file <- here::here("data", "education-spectrum-counts.rds")
  skip_if_not(file.exists(data_file), "Count data file not found")
  counts_data <- readRDS(data_file)

  # Some education levels may have missing unemployment_rate (NAs in original data)
  # Model should fit without errors
  expect_no_error({
    model <- mgcv::gam(
      cbind(n_unemployed, n_employed) ~ education +
        s(time_index) +
        s(month, bs = "cc"),
      data = counts_data,
      family = quasibinomial(),
      method = "REML"
    )
  })
})

test_that("quasi-binomial dispersion parameter is estimated", {
  data_file <- here::here("data", "education-spectrum-counts.rds")
  skip_if_not(file.exists(data_file), "Count data file not found")
  counts_data <- readRDS(data_file)

  model <- mgcv::gam(
    cbind(n_unemployed, n_employed) ~ education +
      s(time_index) +
      s(month, bs = "cc"),
    data = counts_data,
    family = quasibinomial(),
    method = "REML"
  )

  # Quasi-binomial models estimate dispersion
  dispersion <- summary(model)$dispersion

  # Dispersion should be positive and finite
  expect_gt(dispersion, 0)
  expect_true(is.finite(dispersion))

  # If dispersion >> 1, suggests overdispersion (expected for this data)
  # If dispersion ~ 1, binomial model would suffice
  # Document what we find
  cat("\nDispersion parameter:", round(dispersion, 2), "\n")
})

# =============================================================================
# TDD TESTS: Enhanced Model with Shock Dynamics (2007-2010, 2019-2021)
# =============================================================================

test_that("fit_education_binomial_gam creates shock variables correctly", {
  # SKIP: Model now uses adaptive splines instead of explicit shock terms
  skip("Model refactored to use adaptive splines - shock terms no longer used")
  library(phdunemployment)

  data_file <- here::here("data", "education-spectrum-counts.rds")
  skip_if_not(file.exists(data_file), "Count data file not found")
  counts_data <- readRDS(data_file)

  # Fit model using the function
  result <- fit_education_binomial_gam(counts_data, use_quasi = TRUE, time_k = 50)

  # GREEN: Verify shock variables are created
  expect_true("shock_2008_2009" %in% names(result$data))
  expect_true("shock_2020" %in% names(result$data))

  # Verify shock periods are extended (2007-2010 and 2019-2021)
  years_shock_2008_2009 <- unique(result$data$year[result$data$shock_2008_2009 == 1])
  years_shock_2020 <- unique(result$data$year[result$data$shock_2020 == 1])

  expect_equal(range(years_shock_2008_2009), c(2007, 2010))
  expect_equal(range(years_shock_2020), c(2019, 2021))
})

test_that("fit_education_binomial_gam uses thin plate splines", {
  # SKIP: Model now uses adaptive splines instead of thin plate splines
  skip("Model refactored to use adaptive splines - thin plate splines no longer used")
  library(phdunemployment)

  data_file <- here::here("data", "education-spectrum-counts.rds")
  skip_if_not(file.exists(data_file), "Count data file not found")
  counts_data <- readRDS(data_file)

  result <- fit_education_binomial_gam(counts_data, use_quasi = TRUE, time_k = 50)
  model <- result$model

  # Check basis types used
  smooth_labels <- sapply(model$smooth, function(x) x$label)

  # Should have thin plate splines for main trends and shock effects
  tp_smooths <- grep("tp", sapply(model$smooth, function(x) x$bs.name), value = FALSE)
  expect_true(length(tp_smooths) > 0, info = "No thin plate splines found in model")
})

test_that("fit_education_binomial_gam achieves convergence with k=150", {
  # REFACTOR: Verify model converges with full basis dimensions
  library(phdunemployment)

  data_file <- here::here("data", "education-spectrum-counts.rds")
  skip_if_not(file.exists(data_file), "Count data file not found")
  counts_data <- readRDS(data_file)

  result <- fit_education_binomial_gam(counts_data, use_quasi = TRUE, time_k = 150)

  # Model should converge
  expect_true(result$convergence_info$converged)

  # Deviance explained should be > 90%
  # Note: This function uses a different model spec than the Quarto report
  # (factor smooths bs="fs" vs adaptive splines bs="ad" with by=education)
  expect_gt(result$summary_stats$deviance_explained, 0.90)

  # Dispersion should be in reasonable range (1 < disp < 10 for this data)
  # Note: Higher dispersion tolerance for simpler factor smooth model
  expect_gt(result$summary_stats$dispersion, 1)
  expect_lt(result$summary_stats$dispersion, 10)
})

test_that("shock dynamics smooth terms are properly included", {

  # SKIP: Model now uses adaptive splines instead of explicit shock terms
  # The Quarto report fits the model inline with s(year_cont, by=education, bs="ad")
  skip("Model refactored to use adaptive splines - shock terms no longer used")
})

test_that("predictions include shock effects", {

  # SKIP: Model now uses adaptive splines instead of explicit shock terms
  # Predictions are now made with year_cont and month, not shock intensities
  skip("Model refactored to use adaptive splines - shock terms no longer used")
})

test_that("model basis dimensions match specifications", {
  # GREEN: Verify k values are correctly applied
  library(phdunemployment)

  data_file <- here::here("data", "education-spectrum-counts.rds")
  skip_if_not(file.exists(data_file), "Count data file not found")
  counts_data <- readRDS(data_file)

  result <- fit_education_binomial_gam(counts_data, use_quasi = TRUE, time_k = 80)
  model <- result$model

  # Extract basis dimensions for each smooth
  for (i in seq_along(model$smooth)) {
    smooth <- model$smooth[[i]]
    # Should have reasonable basis dimensions
    expect_true(smooth$bs.dim > 0, info = paste("Smooth", i, "has invalid basis dimension"))
    # Main time smooth should have k close to requested value
    if (grepl("time_index", smooth$label) && !grepl("shock", smooth$label)) {
      expect_true(smooth$bs.dim >= 50, info = paste("Main time smooth too small:", smooth$bs.dim))
    }
  }
})
