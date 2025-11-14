# Tests for applying validated factor smooth GAM models to real CPS data
# TDD RED phase: These tests define the interface for real data analysis

library(testthat)
library(mgcv)

# ==============================================================================
# Test Data Setup
# ==============================================================================

# Create mock multi-education data file for testing
# In production, this would be real CPS data processed to have multiple education levels
setup_test_data <- function() {
  # Simulate realistic multi-education unemployment data (2010-2020)
  # Use boundary-safe parameters:
  # PhD: 0.040 - 0.005 + (-0.00005 * 132) - 3*0.002 = 0.040 - 0.005 - 0.0066 - 0.006 = 0.0224 (safe!)
  test_data <- simulate_multi_education_unemployment(
    n_years = 11,
    education_levels = c("phd", "masters", "bachelors"),
    baseline_rates = c(phd = 0.040, masters = 0.050, bachelors = 0.060),
    seasonal_amplitudes = c(phd = 0.005, masters = 0.010, bachelors = 0.015),
    trend_slopes = c(phd = -0.00005, masters = -0.0001, bachelors = -0.00015),
    noise_sd = 0.002,
    seed = 12345
  )

  # Add year column (2010-2020)
  test_data$year <- 2010 + (test_data$time_index - 1) %/% 12

  # Create temporary data file
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(test_data, temp_file)

  return(temp_file)
}

# Clean up function
teardown_test_data <- function(file_path) {
  if (file.exists(file_path)) {
    unlink(file_path)
  }
}

# ==============================================================================
# Test Suite 1: Fit Factor Smooth GAM to Real CPS Data
# ==============================================================================

test_that("fit_factor_smooth_to_cps_data loads and fits model to real unemployment data", {
  # This function should:
  # 1. Load processed CPS unemployment data
  # 2. Filter to education levels of interest (PhD, Masters, Bachelor's)
  # 3. Fit factor smooth GAM with education-specific trends and seasonality
  # 4. Return fitted model with metadata

  # Setup test data
  test_file <- setup_test_data()
  on.exit(teardown_test_data(test_file))

  # Fit model to real data
  result <- fit_factor_smooth_to_cps_data(
    data_file = test_file,
    education_levels = c("phd", "masters", "bachelors"),
    formula_type = "full",
    save_model = FALSE
  )

  # Should return a list with model and metadata
  expect_type(result, "list")
  expect_true("model" %in% names(result))
  expect_true("data" %in% names(result))
  expect_true("education_levels" %in% names(result))

  # Model should be a gam object
  expect_s3_class(result$model, "gam")

  # Data should be a data frame
  expect_s3_class(result$data, "data.frame")

  # Should have unemployment_rate, education, time_index, month columns
  required_cols <- c("unemployment_rate", "education", "time_index", "month")
  expect_true(all(required_cols %in% names(result$data)))

  # Education should be a factor with requested levels
  expect_true(is.factor(result$data$education))
  expect_equal(levels(result$data$education), c("phd", "masters", "bachelors"))

  # Model should have education-specific smooths
  expect_gt(length(result$model$smooth), 4)  # At least 6 for full model (3 educ × 2 smooths)

  # Model should converge
  expect_true(result$model$converged)

  # Should have reasonable fit
  expect_gt(summary(result$model)$r.sq, 0.3)  # R² > 0.3 for real data
})

test_that("fit_factor_smooth_to_cps_data handles date range filtering", {
  test_file <- setup_test_data()
  on.exit(teardown_test_data(test_file))

  # Should be able to fit model to subset of years
  result <- fit_factor_smooth_to_cps_data(
    data_file = test_file,
    education_levels = c("phd", "masters"),
    formula_type = "full",
    start_year = 2010,
    end_year = 2020,
    save_model = FALSE
  )

  # Data should be filtered to requested range
  expect_true(all(result$data$year >= 2010))
  expect_true(all(result$data$year <= 2020))

  # Should have correct number of months
  expected_months <- (2020 - 2010 + 1) * 12
  months_per_educ <- nrow(result$data) / 2  # 2 education levels
  expect_equal(months_per_educ, expected_months)
})

test_that("fit_factor_smooth_to_cps_data saves model with metadata when requested", {
  test_file <- setup_test_data()
  on.exit(teardown_test_data(test_file))

  # Temporary directory for test
  temp_model_dir <- tempdir()

  result <- fit_factor_smooth_to_cps_data(
    data_file = test_file,
    education_levels = c("phd", "masters", "bachelors"),
    formula_type = "full",
    save_model = TRUE,
    model_dir = temp_model_dir,
    model_name = "test_cps_factor_smooth"
  )

  # Should have saved the model
  expect_true("model_path" %in% names(result))
  expect_true(file.exists(result$model_path))

  # Load the saved model and verify it's the same
  loaded_model <- readRDS(result$model_path)
  expect_s3_class(loaded_model$model, "gam")
  expect_equal(loaded_model$education_levels, c("phd", "masters", "bachelors"))
  expect_true("fit_date" %in% names(loaded_model))
  expect_true("data_years" %in% names(loaded_model))

  # Clean up
  unlink(result$model_path)
})


# ==============================================================================
# Test Suite 2: Extract Education-Specific Components from Real Data
# ==============================================================================

test_that("extract_real_data_components extracts trends and seasonal patterns for all education levels", {
  test_file <- setup_test_data()
  on.exit(teardown_test_data(test_file))

  # First fit model
  model_result <- fit_factor_smooth_to_cps_data(
    data_file = test_file,
    education_levels = c("phd", "masters", "bachelors"),
    formula_type = "full",
    save_model = FALSE
  )

  # Extract components
  components <- extract_real_data_components(
    model = model_result$model,
    data = model_result$data,
    education_levels = c("phd", "masters", "bachelors")
  )

  # Should return list with trends and seasonal for each education level
  expect_type(components, "list")
  expect_true("trends" %in% names(components))
  expect_true("seasonal" %in% names(components))

  # Trends should have one entry per education level
  expect_equal(names(components$trends), c("phd", "masters", "bachelors"))

  # Each trend should be a data frame
  for (educ in c("phd", "masters", "bachelors")) {
    trend_df <- components$trends[[educ]]
    expect_s3_class(trend_df, "data.frame")
    expect_true(all(c("time_index", "trend_effect", "se") %in% names(trend_df)))
    expect_gt(nrow(trend_df), 100)  # Should have many time points
  }

  # Seasonal should have one entry per education level
  expect_equal(names(components$seasonal), c("phd", "masters", "bachelors"))

  # Each seasonal should be a data frame with 12 months
  for (educ in c("phd", "masters", "bachelors")) {
    seasonal_df <- components$seasonal[[educ]]
    expect_s3_class(seasonal_df, "data.frame")
    expect_true(all(c("month", "seasonal_effect", "se") %in% names(seasonal_df)))
    expect_equal(nrow(seasonal_df), 12)
  }
})


# ==============================================================================
# Test Suite 3: Compute Trend Differences for Real Data
# ==============================================================================

test_that("compute_real_data_trend_differences computes pairwise education differences over time", {
  test_file <- setup_test_data()
  on.exit(teardown_test_data(test_file))

  # Fit model
  model_result <- fit_factor_smooth_to_cps_data(
    data_file = test_file,
    education_levels = c("phd", "masters", "bachelors"),
    formula_type = "full",
    start_year = 2010,
    end_year = 2020,
    save_model = FALSE
  )

  # Compute trend differences
  diff_results <- compute_real_data_trend_differences(
    model = model_result$model,
    data = model_result$data,
    education_pairs = list(
      c("phd", "bachelors"),
      c("phd", "masters")
    ),
    time_points = seq(10, 120, by = 10),  # Every 10 months
    simultaneous = TRUE,
    alpha = 0.05
  )

  # Should return data frame
  expect_s3_class(diff_results, "data.frame")

  # Required columns
  required_cols <- c("time_index", "comparison", "difference", "se",
                     "lower", "upper", "significant", "year", "month")
  expect_true(all(required_cols %in% names(diff_results)))

  # Should have 2 comparisons × 12 time points = 24 rows
  expect_equal(nrow(diff_results), 24)

  # Comparisons should be labeled correctly
  expect_true(all(diff_results$comparison %in% c("phd - bachelors", "phd - masters")))

  # Should have year and month for interpretability
  expect_true(all(diff_results$year >= 2010))
  expect_true(all(diff_results$year <= 2020))
  expect_true(all(diff_results$month >= 1 & diff_results$month <= 12))

  # SE should be positive
  expect_true(all(diff_results$se > 0))

  # CIs should be valid
  expect_true(all(diff_results$lower <= diff_results$difference))
  expect_true(all(diff_results$upper >= diff_results$difference))
})


# ==============================================================================
# Test Suite 4: Compare Model Selection on Real Data
# ==============================================================================

test_that("fit_nested_models_to_cps_data fits and compares nested models on real data", {
  test_file <- setup_test_data()
  on.exit(teardown_test_data(test_file))

  # Fit nested sequence of models to real data
  models_result <- fit_nested_models_to_cps_data(
    data_file = test_file,
    education_levels = c("phd", "masters", "bachelors"),
    start_year = 2010,
    end_year = 2020
  )

  # Should return list with models and comparison table
  expect_type(models_result, "list")
  expect_true("models" %in% names(models_result))
  expect_true("comparison" %in% names(models_result))

  # Should have 7 models
  expect_length(models_result$models, 7)
  expect_equal(names(models_result$models), c("m0", "m1", "m2", "m3", "m4", "m5", "m6"))

  # All should be gam objects
  expect_true(all(sapply(models_result$models, function(m) inherits(m, "gam"))))

  # Comparison table should be data frame
  expect_s3_class(models_result$comparison, "data.frame")
  expect_equal(nrow(models_result$comparison), 7)

  # Required columns in comparison
  required_cols <- c("model", "AIC", "delta_AIC", "edf", "r_squared", "deviance")
  expect_true(all(required_cols %in% names(models_result$comparison)))

  # delta_AIC should be sorted (best first)
  expect_equal(models_result$comparison$delta_AIC[1], 0)

  # Best model should be identified
  expect_true("best_model" %in% names(models_result))
  expect_true(models_result$best_model %in% c("m0", "m1", "m2", "m3", "m4", "m5", "m6"))
})


# ==============================================================================
# Test Suite 5: Visualization Data Preparation
# ==============================================================================

test_that("prepare_visualization_data creates plotting-ready data for all education levels", {
  test_file <- setup_test_data()
  on.exit(teardown_test_data(test_file))

  # Fit model
  model_result <- fit_factor_smooth_to_cps_data(
    data_file = test_file,
    education_levels = c("phd", "masters", "bachelors"),
    formula_type = "full",
    start_year = 2010,
    end_year = 2020,
    save_model = FALSE
  )

  # Prepare visualization data
  viz_data <- prepare_visualization_data(
    model = model_result$model,
    data = model_result$data,
    education_levels = c("phd", "masters", "bachelors")
  )

  # Should return list with multiple data frames
  expect_type(viz_data, "list")
  expect_true("observed" %in% names(viz_data))
  expect_true("fitted" %in% names(viz_data))
  expect_true("trends" %in% names(viz_data))
  expect_true("seasonal" %in% names(viz_data))

  # Observed data should have actual unemployment rates with dates
  obs <- viz_data$observed
  expect_s3_class(obs, "data.frame")
  expect_true(all(c("date", "year", "month", "education", "unemployment_rate") %in% names(obs)))

  # Fitted data should have model predictions with confidence intervals
  fitted <- viz_data$fitted
  expect_s3_class(fitted, "data.frame")
  expect_true(all(c("date", "education", "fitted", "lower", "upper") %in% names(fitted)))

  # Trends data should be ready for trend plots
  trends <- viz_data$trends
  expect_s3_class(trends, "data.frame")
  expect_true(all(c("date", "education", "trend_effect", "se") %in% names(trends)))

  # Seasonal data should be ready for seasonal plots
  seasonal <- viz_data$seasonal
  expect_s3_class(seasonal, "data.frame")
  expect_true(all(c("month", "month_name", "education", "seasonal_effect", "se") %in% names(seasonal)))
  expect_equal(length(unique(seasonal$month)), 12)
})


# ==============================================================================
# Test Suite 6: Integration Test - Full Analysis Pipeline
# ==============================================================================

test_that("analyze_cps_unemployment_by_education runs complete analysis pipeline", {
  test_file <- setup_test_data()
  on.exit(teardown_test_data(test_file))

  # This high-level function should orchestrate the entire analysis
  analysis <- analyze_cps_unemployment_by_education(
    data_file = test_file,
    education_levels = c("phd", "masters", "bachelors"),
    start_year = 2015,
    end_year = 2020,
    save_models = FALSE,
    save_results = FALSE
  )

  # Should return comprehensive results
  expect_type(analysis, "list")

  # Core components
  expect_true("best_model" %in% names(analysis))
  expect_true("model_comparison" %in% names(analysis))
  expect_true("components" %in% names(analysis))
  expect_true("trend_differences" %in% names(analysis))
  expect_true("visualization_data" %in% names(analysis))

  # Best model should be a fitted gam
  expect_s3_class(analysis$best_model, "gam")

  # Model comparison should show all 7 models
  expect_equal(nrow(analysis$model_comparison), 7)

  # Components should have trends and seasonal for each education level
  expect_equal(length(analysis$components$trends), 3)
  expect_equal(length(analysis$components$seasonal), 3)

  # Trend differences should compare all pairs
  # 3 education levels = 3 pairwise comparisons
  unique_comparisons <- unique(analysis$trend_differences$comparison)
  expect_gte(length(unique_comparisons), 2)  # At least PhD-Masters, PhD-Bachelor's

  # Visualization data should be ready for plotting
  expect_s3_class(analysis$visualization_data$observed, "data.frame")
  expect_s3_class(analysis$visualization_data$fitted, "data.frame")
})
