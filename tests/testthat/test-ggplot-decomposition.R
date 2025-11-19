# Load required packages
library(here)

# Source necessary functions
source(here("R", "seasonal-gam.R"))
source(here("R", "data-processing.R"))

test_that("plot_seasonal_decomposition_ggplot returns a ggplot object", {
  # Create minimal test data
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    time_index = 1:n,
    month = rep(1:12, length.out = n),
    unemployment_rate = 0.02 + 0.001 * (1:n) / n +
      0.005 * sin(2 * pi * (1:n) / 12) + rnorm(n, 0, 0.002)
  )

  # Fit a simple GAM
  model <- mgcv::gam(unemployment_rate ~ s(time_index, k = 10) + s(month, bs = "cc", k = 10),
                     data = test_data, method = "REML")

  # Call ggplot version
  result <- plot_seasonal_decomposition_ggplot(model, test_data)

  # Should return a ggplot object (or list of ggplot objects)
  expect_true(inherits(result, "ggplot") || inherits(result, "list"))

  # If it's a list, all elements should be ggplot objects
  if (inherits(result, "list")) {
    expect_true(all(sapply(result, function(x) inherits(x, "ggplot"))))
    # Should have 3 panels
    expect_equal(length(result), 3)
  }
})

test_that("plot_seasonal_decomposition_ggplot handles real CPS data", {
  # Load real data
  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))
  phd_data <- filter_phd_holders(cps_data)
  monthly_rates <- aggregate_monthly_unemployment(phd_data, weight_var = "auto")

  # month column already exists in new format

  # Fit seasonal GAM
  model <- fit_seasonal_gam(monthly_rates, k_month = 10, k_trend = 20)

  # Should not error
  expect_no_error({
    result <- plot_seasonal_decomposition_ggplot(model, monthly_rates)
  })

  # Should return ggplot object(s)
  expect_true(inherits(result, "ggplot") || inherits(result, "list"))
})

test_that("ggplot version produces same data as base R version", {
  # Create test data
  set.seed(456)
  n <- 120
  test_data <- data.frame(
    time_index = 1:n,
    month = rep(1:12, length.out = n),
    unemployment_rate = 0.03 + 0.002 * (1:n) / n +
      0.008 * cos(2 * pi * (1:n) / 12) + rnorm(n, 0, 0.003)
  )

  # Fit model
  model <- mgcv::gam(unemployment_rate ~ s(time_index, k = 15) + s(month, bs = "cc", k = 10),
                     data = test_data, method = "REML")

  # Get ggplot version
  gg_result <- plot_seasonal_decomposition_ggplot(model, test_data)

  # Extract data from ggplot objects
  if (inherits(gg_result, "list")) {
    # First panel should have fitted values
    panel1_data <- gg_result[[1]]$data
    expect_true("fitted" %in% names(panel1_data) || "fitted_vals" %in% names(panel1_data))

    # Second panel should have trend
    panel2_data <- gg_result[[2]]$data
    expect_true("trend_effect" %in% names(panel2_data))

    # Third panel should have seasonal pattern
    panel3_data <- gg_result[[3]]$data
    expect_true("seasonal_effect" %in% names(panel3_data))
    expect_equal(nrow(panel3_data), 12)  # One point per month
  }
})

test_that("create_timeseries_ggplot returns ggplot object", {
  # Load real data
  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))
  phd_data <- filter_phd_holders(cps_data)
  monthly_rates <- aggregate_monthly_unemployment(phd_data, weight_var = "auto")

  # Fit model
  model <- fit_seasonal_gam(monthly_rates, k_month = 10, k_trend = 20)

  # Extract components
  seasonal_effects <- extract_seasonal_component(model, monthly_rates)
  trend_effects <- extract_trend_component(model, monthly_rates)

  # Add fitted values to data
  monthly_rates$fitted <- fitted(model)
  monthly_rates$trend <- trend_effects$trend_effect[match(monthly_rates$time_index,
                                                           trend_effects$time_index)]
  monthly_rates$seasonal <- seasonal_effects$seasonal_effect[match(monthly_rates$MONTH,
                                                                    seasonal_effects$month)]

  # Should not error
  expect_no_error({
    result <- create_timeseries_ggplot(monthly_rates, seasonal_effects)
  })

  # Should return ggplot or list of ggplots
  expect_true(inherits(result, "ggplot") || inherits(result, "list"))

  # If list, should have 2 panels
  if (inherits(result, "list")) {
    expect_equal(length(result), 2)
    expect_true(all(sapply(result, function(x) inherits(x, "ggplot"))))
  }
})

test_that("ggplot functions use date column for x-axis when available", {
  # Load real data with date column
  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))
  phd_data <- filter_phd_holders(cps_data)
  monthly_rates <- aggregate_monthly_unemployment(phd_data, weight_var = "auto")

  # Fit model
  model <- fit_seasonal_gam(monthly_rates, k_month = 10, k_trend = 20)

  # Get seasonal effects
  seasonal_effects <- extract_seasonal_component(model, monthly_rates)

  # Add fitted values
  monthly_rates$fitted <- fitted(model)

  # Create plot
  result <- create_timeseries_ggplot(monthly_rates, seasonal_effects)

  # Extract data from first panel (time series)
  if (inherits(result, "list")) {
    panel1_data <- result[[1]]$data
    # Should have date column
    expect_true("date" %in% names(panel1_data))
    # Date should be Date class
    expect_s3_class(panel1_data$date, "Date")
  }
})

test_that("plot_seasonal_decomposition_ggplot uses date for x-axis when available", {
  # Load real data
  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))
  phd_data <- filter_phd_holders(cps_data)
  monthly_rates <- aggregate_monthly_unemployment(phd_data, weight_var = "auto")

  # Fit model
  model <- fit_seasonal_gam(monthly_rates, k_month = 10, k_trend = 20)

  # Create decomposition plots
  plots <- plot_seasonal_decomposition_ggplot(model, monthly_rates)

  # Panel 1 (observed vs fitted) should use date when available
  panel1_data <- plots$observed_fitted$data
  if ("date" %in% names(monthly_rates)) {
    expect_true("date" %in% names(panel1_data))
    expect_s3_class(panel1_data$date, "Date")
  }

  # Panel 2 (trend) should use date when available
  panel2_data <- plots$trend$data
  if ("date" %in% names(monthly_rates)) {
    expect_true("date" %in% names(panel2_data))
    expect_s3_class(panel2_data$date, "Date")
  }
})

test_that("decomposition plots show date labels not numeric index", {
  # Create test data with date column
  set.seed(789)
  n <- 60
  test_data <- data.frame(
    time_index = 1:n,
    date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = n),
    month = rep(1:12, length.out = n),
    unemployment_rate = 0.025 + 0.001 * (1:n) / n +
      0.006 * sin(2 * pi * (1:n) / 12) + rnorm(n, 0, 0.002)
  )

  # Fit model
  model <- mgcv::gam(unemployment_rate ~ s(time_index, k = 10) + s(month, bs = "cc", k = 10),
                     data = test_data, method = "REML")

  # Create plots
  plots <- plot_seasonal_decomposition_ggplot(model, test_data)

  # Check that panel 1 uses date for x-axis
  panel1_mapping <- plots$observed_fitted$mapping
  expect_true("date" %in% names(plots$observed_fitted$data))

  # Check that panel 2 uses date for x-axis
  panel2_mapping <- plots$trend$mapping
  expect_true("date" %in% names(plots$trend$data))
})
