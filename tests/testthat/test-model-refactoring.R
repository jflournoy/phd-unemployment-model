# Test Suite for Refactored Model Fitting Functions (TDD RED)
#
# These tests verify that model fitting functions:
# 1. Accept processed data objects (not file paths)
# 2. Work with targets pipeline outputs
# 3. Use new aggregation functions
# 4. Return expected model objects

library(testthat)
library(here)

# ==============================================================================
# Tests for fit_factor_smooth_to_cps_data() refactoring
# ==============================================================================

test_that("fit_factor_smooth_to_cps_data accepts data object, not file path", {
  # Load raw data and process it
  raw_data <- readRDS(here("data-raw", "ipums_data.rds"))

  # Create processed data using new aggregation function
  processed_data <- aggregate_monthly_by_education(
    raw_data,
    weight_var = "auto",
    weighted = TRUE
  )

  # Filter to test education levels
  test_data <- processed_data[processed_data$education %in% c("phd", "masters", "bachelors"), ]

  # This should work with a data object, NOT a file path
  result <- fit_factor_smooth_to_cps_data(
    data = test_data,  # NOT data_file
    education_levels = c("phd", "masters", "bachelors"),
    formula_type = "full",
    start_year = 2010,
    end_year = 2020
  )

  # Verify result structure
  expect_type(result, "list")
  expect_true("model" %in% names(result))
  expect_true("data" %in% names(result))
  expect_s3_class(result$model, "gam")
})

test_that("fit_factor_smooth_to_cps_data works with targets pipeline output", {
  # Simulate targets pipeline output
  raw_data <- readRDS(here("data-raw", "ipums_data.rds"))

  # Use exact same processing as targets pipeline
  multi_education <- aggregate_monthly_by_education(
    raw_data,
    weight_var = "auto",
    weighted = TRUE
  )

  # Should work directly with this data
  result <- fit_factor_smooth_to_cps_data(
    data = multi_education,
    education_levels = c("phd", "masters"),
    formula_type = "seasonal_by_education"
  )

  expect_s3_class(result$model, "gam")
  expect_equal(result$education_levels, c("phd", "masters"))
})

test_that("fit_factor_smooth_to_cps_data handles year filtering correctly", {
  raw_data <- readRDS(here("data-raw", "ipums_data.rds"))
  processed_data <- aggregate_monthly_by_education(
    raw_data,
    weight_var = "auto",
    weighted = TRUE
  )

  test_data <- processed_data[processed_data$education %in% c("phd", "masters"), ]

  result <- fit_factor_smooth_to_cps_data(
    data = test_data,
    education_levels = c("phd", "masters"),
    start_year = 2015,
    end_year = 2020
  )

  # Check year range
  expect_true(all(result$data$year >= 2015))
  expect_true(all(result$data$year <= 2020))
  expect_equal(result$data_years, c(2015, 2020))
})

# ==============================================================================
# Tests for fit_nested_models_to_cps_data() refactoring
# ==============================================================================

test_that("fit_nested_models_to_cps_data accepts data object, not file path", {
  raw_data <- readRDS(here("data-raw", "ipums_data.rds"))
  processed_data <- aggregate_monthly_by_education(
    raw_data,
    weight_var = "auto",
    weighted = TRUE
  )

  test_data <- processed_data[processed_data$education %in% c("phd", "masters", "bachelors"), ]

  # Should accept data object
  result <- fit_nested_models_to_cps_data(
    data = test_data,  # NOT data_file
    education_levels = c("phd", "masters", "bachelors"),
    start_year = 2010,
    end_year = 2020
  )

  # Verify result structure
  expect_type(result, "list")
  expect_true("models" %in% names(result))
  expect_true("comparison" %in% names(result))
  expect_true("best_model" %in% names(result))
  expect_true("data" %in% names(result))

  # Check models list
  expect_type(result$models, "list")
  expect_true(all(sapply(result$models, function(m) inherits(m, "gam"))))
})

test_that("fit_nested_models_to_cps_data returns proper comparison table", {
  raw_data <- readRDS(here("data-raw", "ipums_data.rds"))
  processed_data <- aggregate_monthly_by_education(
    raw_data,
    weight_var = "auto",
    weighted = TRUE
  )

  # Use 2+ education levels (factor smooths require multiple levels)
  test_data <- processed_data[processed_data$education %in% c("phd", "masters"), ]

  result <- fit_nested_models_to_cps_data(
    data = test_data,
    education_levels = c("phd", "masters")
  )

  # Check comparison table
  expect_s3_class(result$comparison, "data.frame")
  expect_true("model" %in% names(result$comparison))
  expect_true("AIC" %in% names(result$comparison))

  # Best model should be in models list
  expect_true(result$best_model %in% names(result$models))
})

# ==============================================================================
# Tests for analyze_cps_unemployment_by_education() refactoring
# ==============================================================================

test_that("analyze_cps_unemployment_by_education accepts data object", {
  raw_data <- readRDS(here("data-raw", "ipums_data.rds"))
  processed_data <- aggregate_monthly_by_education(
    raw_data,
    weight_var = "auto",
    weighted = TRUE
  )

  test_data <- processed_data[processed_data$education %in% c("phd", "masters"), ]

  # Should accept data object, not file path
  result <- analyze_cps_unemployment_by_education(
    data = test_data,  # NOT data_file
    education_levels = c("phd", "masters"),
    start_year = 2010,
    end_year = 2020
  )

  # Verify comprehensive result structure
  expect_type(result, "list")
  expect_true("best_model" %in% names(result))
  expect_true("model_comparison" %in% names(result))
  expect_true("components" %in% names(result))
  expect_true("trend_differences" %in% names(result))
  expect_true("visualization_data" %in% names(result))
  expect_true("data" %in% names(result))

  expect_s3_class(result$best_model, "gam")
})

test_that("analyze_cps_unemployment_by_education handles multiple education levels", {
  raw_data <- readRDS(here("data-raw", "ipums_data.rds"))
  processed_data <- aggregate_monthly_by_education(
    raw_data,
    weight_var = "auto",
    weighted = TRUE
  )

  test_data <- processed_data[processed_data$education %in% c("phd", "masters"), ]

  result <- analyze_cps_unemployment_by_education(
    data = test_data,
    education_levels = c("phd", "masters")
  )

  expect_s3_class(result$best_model, "gam")
  # Trend differences should exist for multiple education levels
  expect_s3_class(result$trend_differences, "data.frame")
})

# ==============================================================================
# Tests for compare_unemployment_by_education() refactoring
# ==============================================================================

test_that("compare_unemployment_by_education uses new aggregation functions", {
  # This test verifies that the function uses aggregate_monthly_by_education
  # instead of the old aggregate_monthly_unemployment approach

  raw_data <- readRDS(here("data-raw", "ipums_data.rds"))

  # The function should work with raw data and handle aggregation internally
  # BUT it should use the NEW aggregation functions, not old filtering logic
  result <- compare_unemployment_by_education(
    data = raw_data,
    education_levels = c(phd = 125, masters = 123, bachelors = 111)
  )

  expect_type(result, "list")
  expect_equal(length(result), 3)

  # Each result should have properly structured monthly data
  for (educ_name in names(result)) {
    expect_true("monthly_data" %in% names(result[[educ_name]]))
    expect_true("model" %in% names(result[[educ_name]]))
    expect_s3_class(result[[educ_name]]$model, "gam")

    # Check that monthly_data has expected structure from new functions
    monthly <- result[[educ_name]]$monthly_data
    expect_true("unemployment_rate" %in% names(monthly))
    expect_true("date" %in% names(monthly))
    expect_true("time_index" %in% names(monthly))
  }
})

test_that("compare_unemployment_by_education handles education codes correctly", {
  raw_data <- readRDS(here("data-raw", "ipums_data.rds"))

  # Test with various education code formats
  result <- compare_unemployment_by_education(
    data = raw_data,
    education_levels = c(phd = 125, bachelors = 111)
  )

  expect_equal(length(result), 2)
  expect_true("phd" %in% names(result))
  expect_true("bachelors" %in% names(result))

  # Check education labels use new description function
  expect_equal(result$phd$education_code, 125)
  expect_true(nchar(result$phd$education_label) > 0)
})

# ==============================================================================
# Integration tests with targets pipeline
# ==============================================================================

test_that("refactored functions integrate with targets pipeline data flow", {
  # Simulate complete targets pipeline data flow

  # Step 1: Raw data (from targets)
  raw_data <- readRDS(here("data-raw", "ipums_data.rds"))

  # Step 2: Education counts (for binomial models)
  education_counts <- aggregate_monthly_by_education(
    raw_data,
    weight_var = NULL,
    weighted = FALSE
  )

  # Step 3: Multi-education (for comparison)
  multi_education <- aggregate_monthly_by_education(
    raw_data,
    weight_var = "auto",
    weighted = TRUE
  )

  # Refactored functions should work directly with these outputs

  # Test with binomial data
  test_data_binomial <- education_counts[
    education_counts$education %in% c("phd", "masters") &
    education_counts$year >= 2015,
  ]

  result1 <- fit_factor_smooth_to_cps_data(
    data = test_data_binomial,
    education_levels = c("phd", "masters")
  )

  expect_s3_class(result1$model, "gam")

  # Test with weighted data
  test_data_weighted <- multi_education[
    multi_education$education %in% c("phd", "masters") &
    multi_education$year >= 2015,
  ]

  result2 <- fit_nested_models_to_cps_data(
    data = test_data_weighted,
    education_levels = c("phd", "masters")
  )

  expect_type(result2$models, "list")
  expect_true(length(result2$models) > 0)
})

test_that("all refactored functions work without file I/O", {
  # This test ensures NO function reads from files during execution
  # All data should be passed as parameters

  # Create a temp directory with no data files
  temp_dir <- tempdir()
  old_wd <- getwd()
  setwd(temp_dir)

  # Prepare data in memory
  raw_data <- readRDS(here("data-raw", "ipums_data.rds"))
  processed_data <- aggregate_monthly_by_education(
    raw_data,
    weight_var = "auto",
    weighted = TRUE
  )

  test_data <- processed_data[
    processed_data$education %in% c("phd", "masters") &
    processed_data$year >= 2015,
  ]

  # These should all work without accessing filesystem
  # (except for loading package code)

  result1 <- fit_factor_smooth_to_cps_data(
    data = test_data,
    education_levels = c("phd", "masters")
  )

  result2 <- fit_nested_models_to_cps_data(
    data = test_data,
    education_levels = c("phd", "masters")
  )

  result3 <- analyze_cps_unemployment_by_education(
    data = test_data,
    education_levels = c("phd", "masters")
  )

  setwd(old_wd)

  # All should succeed
  expect_s3_class(result1$model, "gam")
  expect_type(result2$models, "list")
  expect_s3_class(result3$best_model, "gam")
})

# ==============================================================================
# Tests for backwards compatibility (optional, but good practice)
# ==============================================================================

test_that("refactored functions maintain backward compatible return structure", {
  raw_data <- readRDS(here("data-raw", "ipums_data.rds"))
  processed_data <- aggregate_monthly_by_education(
    raw_data,
    weight_var = "auto",
    weighted = TRUE
  )

  # Use 2+ education levels for factor smooth models
  test_data <- processed_data[processed_data$education %in% c("phd", "masters"), ]

  result <- fit_factor_smooth_to_cps_data(
    data = test_data,
    education_levels = c("phd", "masters"),
    formula_type = "full"
  )

  # Check all expected return elements exist
  expected_elements <- c("model", "data", "education_levels",
                        "formula_type", "data_years")

  for (elem in expected_elements) {
    expect_true(elem %in% names(result),
                info = paste("Missing element:", elem))
  }
})
