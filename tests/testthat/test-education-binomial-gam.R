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
