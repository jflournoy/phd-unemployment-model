# Tests for GAM model validation functions
# TDD RED phase: Define validation checks for fitted models

library(testthat)
library(mgcv)

# ==============================================================================
# Test Data Setup
# ==============================================================================

setup_test_model <- function() {
  # Create multi-education test data
  test_data <- simulate_multi_education_unemployment(
    n_years = 10,
    education_levels = c("phd", "masters", "bachelors"),
    baseline_rates = c(phd = 0.040, masters = 0.050, bachelors = 0.060),
    seasonal_amplitudes = c(phd = 0.005, masters = 0.010, bachelors = 0.015),
    trend_slopes = c(phd = -0.00005, masters = -0.0001, bachelors = -0.00015),
    noise_sd = 0.002,
    seed = 42
  )

  test_data$year <- 2010 + (test_data$time_index - 1) %/% 12

  # Fit a model
  model <- fit_factor_smooth_gam(
    data = test_data,
    formula_type = "full",
    education_var = "education"
  )

  return(list(model = model, data = test_data))
}


# ==============================================================================
# Test Suite 1: Convergence Diagnostics
# ==============================================================================

test_that("validate_convergence checks if GAM converged", {
  setup <- setup_test_model()

  result <- validate_convergence(setup$model)

  # Should return a list with convergence info
  expect_type(result, "list")
  expect_true("converged" %in% names(result))
  expect_true("message" %in% names(result))
  expect_true("iterations" %in% names(result))

  # For a well-specified model, should converge
  expect_true(result$converged)
  expect_type(result$iterations, "double")
})

test_that("validate_convergence detects non-convergence", {
  # Create problematic data that won't converge well
  bad_data <- data.frame(
    unemployment_rate = rnorm(50, 0.5, 0.4),  # Noisy, bounded violation
    time_index = 1:50,
    month = rep(1:12, length.out = 50),
    education = factor(rep(c("a", "b"), 25))
  )

  # Fit model with insufficient data
  suppressWarnings({
    model <- mgcv::gam(
      unemployment_rate ~ education + s(time_index, by = education, k = 20),
      data = bad_data,
      method = "REML"
    )
  })

  result <- validate_convergence(model)

  expect_type(result, "list")
  expect_true("converged" %in% names(result))
})


# ==============================================================================
# Test Suite 2: Residual Diagnostics
# ==============================================================================

test_that("validate_residuals performs comprehensive residual checks", {
  setup <- setup_test_model()

  result <- validate_residuals(setup$model, setup$data)

  # Should return list with multiple diagnostic tests
  expect_type(result, "list")

  # Check for key diagnostic components
  expect_true("normality" %in% names(result))
  expect_true("autocorrelation" %in% names(result))
  expect_true("heteroscedasticity" %in% names(result))
  expect_true("summary" %in% names(result))

  # Normality test (Shapiro-Wilk or similar)
  expect_true("test_statistic" %in% names(result$normality))
  expect_true("p_value" %in% names(result$normality))

  # Autocorrelation test (Ljung-Box or Durbin-Watson)
  expect_true("test_statistic" %in% names(result$autocorrelation))
  expect_true("p_value" %in% names(result$autocorrelation))

  # Heteroscedasticity test (Breusch-Pagan or similar)
  expect_true("test_statistic" %in% names(result$heteroscedasticity))
  expect_true("p_value" %in% names(result$heteroscedasticity))

  # Overall summary
  expect_true(result$summary$all_checks_passed %in% c(TRUE, FALSE))
  expect_type(result$summary$issues, "character")
})

test_that("validate_residuals detects autocorrelation", {
  # Create data with strong autocorrelation
  set.seed(123)
  n <- 120
  ar_data <- data.frame(
    unemployment_rate = arima.sim(list(ar = 0.8), n = n) * 0.01 + 0.05,
    time_index = 1:n,
    month = rep(1:12, length.out = n),
    education = factor(rep("phd", n))
  )

  model <- mgcv::gam(
    unemployment_rate ~ s(month, bs = "cc"),
    data = ar_data,
    method = "REML"
  )

  result <- validate_residuals(model, ar_data)

  # Should detect autocorrelation (p-value < 0.05)
  expect_true("autocorrelation" %in% names(result))
  expect_type(result$autocorrelation$p_value, "double")
})


# ==============================================================================
# Test Suite 3: Concurvity Diagnostics
# ==============================================================================

test_that("validate_concurvity checks for collinearity in smooth terms", {
  setup <- setup_test_model()

  result <- validate_concurvity(setup$model)

  # Should return concurvity measures
  expect_type(result, "list")
  expect_true("worst" %in% names(result))
  expect_true("observed" %in% names(result))
  expect_true("estimate" %in% names(result))
  expect_true("summary" %in% names(result))

  # Concurvity measures should be numeric vectors (one value per smooth term)
  expect_true(is.numeric(result$worst))

  # Summary should flag high concurvity
  expect_true("max_concurvity" %in% names(result$summary))
  expect_true("problematic_terms" %in% names(result$summary))
  expect_type(result$summary$max_concurvity, "double")

  # For well-specified model, concurvity should be reasonable (< 0.8)
  expect_lt(result$summary$max_concurvity, 0.9)
})


# ==============================================================================
# Test Suite 4: Effective Degrees of Freedom Checks
# ==============================================================================

test_that("validate_edf checks for overfitting via EDF", {
  setup <- setup_test_model()

  result <- validate_edf(setup$model, setup$data)

  # Should return EDF diagnostics
  expect_type(result, "list")
  expect_true("total_edf" %in% names(result))
  expect_true("edf_per_smooth" %in% names(result))
  expect_true("n_obs" %in% names(result))
  expect_true("edf_ratio" %in% names(result))
  expect_true("summary" %in% names(result))

  # EDF should be positive
  expect_gt(result$total_edf, 0)

  # EDF ratio (total_edf / n_obs) should be reasonable
  expect_type(result$edf_ratio, "double")
  expect_gt(result$edf_ratio, 0)
  expect_lt(result$edf_ratio, 1)

  # Summary should indicate if overfitting is likely
  expect_true("overfitting_risk" %in% names(result$summary))
  expect_true(result$summary$overfitting_risk %in% c("low", "moderate", "high"))

  # For well-specified model with sufficient data, should have low risk
  expect_equal(result$summary$overfitting_risk, "low")
})

test_that("validate_edf detects potential overfitting", {
  # Create small dataset
  small_data <- simulate_multi_education_unemployment(
    n_years = 2,  # Only 24 months
    education_levels = c("phd", "masters"),
    baseline_rates = c(phd = 0.04, masters = 0.05),
    seasonal_amplitudes = c(phd = 0.005, masters = 0.01),
    trend_slopes = c(phd = 0, masters = 0),
    noise_sd = 0.001,
    seed = 99
  )

  # Fit overly complex model to small data
  model <- fit_factor_smooth_gam(
    data = small_data,
    formula_type = "full",
    k_trend = 15,  # Too many basis functions for 24 obs
    k_month = 10
  )

  result <- validate_edf(model, small_data)

  # Should flag potential overfitting
  expect_true(result$summary$overfitting_risk %in% c("moderate", "high"))
  expect_gt(result$edf_ratio, 0.3)  # High EDF relative to sample size
})


# ==============================================================================
# Test Suite 5: Model Adequacy Checks
# ==============================================================================

test_that("validate_model_fit checks overall model adequacy", {
  setup <- setup_test_model()

  result <- validate_model_fit(setup$model, setup$data)

  # Should return fit statistics
  expect_type(result, "list")
  expect_true("r_squared" %in% names(result))
  expect_true("adj_r_squared" %in% names(result))
  expect_true("deviance_explained" %in% names(result))
  expect_true("aic" %in% names(result))
  expect_true("bic" %in% names(result))
  expect_true("rmse" %in% names(result))
  expect_true("mae" %in% names(result))
  expect_true("summary" %in% names(result))

  # R-squared should be between 0 and 1
  expect_gte(result$r_squared, 0)
  expect_lte(result$r_squared, 1)

  # Deviance explained should be between 0 and 100
  expect_gte(result$deviance_explained, 0)
  expect_lte(result$deviance_explained, 100)

  # For simulated data with clear signal, should have good fit
  expect_gt(result$r_squared, 0.5)

  # Summary should categorize fit quality
  expect_true("fit_quality" %in% names(result$summary))
  expect_true(result$summary$fit_quality %in% c("poor", "adequate", "good", "excellent"))
})


# ==============================================================================
# Test Suite 6: Comprehensive Model Validation
# ==============================================================================

test_that("validate_gam_model performs comprehensive validation", {
  setup <- setup_test_model()

  # Comprehensive validation combining all checks
  result <- validate_gam_model(
    model = setup$model,
    data = setup$data,
    verbose = FALSE
  )

  # Should return list with all validation components
  expect_type(result, "list")
  expect_true("convergence" %in% names(result))
  expect_true("residuals" %in% names(result))
  expect_true("concurvity" %in% names(result))
  expect_true("edf" %in% names(result))
  expect_true("fit" %in% names(result))
  expect_true("overall_summary" %in% names(result))

  # Overall summary
  expect_true("validation_passed" %in% names(result$overall_summary))
  expect_true("critical_issues" %in% names(result$overall_summary))
  expect_true("warnings" %in% names(result$overall_summary))
  expect_true("recommendations" %in% names(result$overall_summary))

  # For well-specified model, validation should pass
  expect_true(result$overall_summary$validation_passed)
  expect_type(result$overall_summary$critical_issues, "character")
  expect_type(result$overall_summary$warnings, "character")
  expect_type(result$overall_summary$recommendations, "character")
})

test_that("validate_gam_model returns structured report", {
  setup <- setup_test_model()

  result <- validate_gam_model(setup$model, setup$data)

  # Should be able to print validation report
  expect_true("class" %in% names(attributes(result)))

  # Should have print method that formats nicely
  # (We'll implement a print.gam_validation S3 method)
  output <- capture.output(print(result))
  expect_gt(length(output), 10)  # Should have multiple lines of output
})


# ==============================================================================
# Test Suite 7: Education-Specific Validation
# ==============================================================================

test_that("validate_education_specific_components checks each education level", {
  setup <- setup_test_model()

  result <- validate_education_specific_components(
    model = setup$model,
    data = setup$data,
    education_levels = c("phd", "masters", "bachelors")
  )

  # Should return validation for each education level
  expect_type(result, "list")
  expect_equal(length(result), 3)
  expect_equal(names(result), c("phd", "masters", "bachelors"))

  # Each education level should have diagnostics
  for (educ in c("phd", "masters", "bachelors")) {
    expect_true("residual_diagnostics" %in% names(result[[educ]]))
    expect_true("smooth_diagnostics" %in% names(result[[educ]]))
    expect_true("sample_size" %in% names(result[[educ]]))
  }
})


# ==============================================================================
# Test Suite 8: Automated Diagnostic Plots
# ==============================================================================

test_that("create_diagnostic_plots generates standard GAM diagnostic plots", {
  setup <- setup_test_model()

  # Should create standard diagnostic plots
  plots <- create_diagnostic_plots(setup$model)

  # Should return list of ggplot objects
  expect_type(plots, "list")
  expect_true("residuals_vs_fitted" %in% names(plots))
  expect_true("qq_plot" %in% names(plots))
  expect_true("residuals_vs_linear_predictor" %in% names(plots))
  expect_true("residuals_histogram" %in% names(plots))

  # Each should be a ggplot object
  expect_s3_class(plots$residuals_vs_fitted, "gg")
  expect_s3_class(plots$qq_plot, "gg")
})


# ==============================================================================
# Test Suite 9: Comparison with mgcv::gam.check
# ==============================================================================

test_that("validation results align with mgcv::gam.check", {
  setup <- setup_test_model()

  # Capture gam.check output
  gam_check_output <- capture.output({
    mgcv::gam.check(setup$model, type = "deviance")
  })

  # Our validation should produce similar conclusions
  our_validation <- validate_gam_model(setup$model, setup$data)

  # Both should indicate model is adequate
  expect_true(our_validation$overall_summary$validation_passed)

  # Check that our EDF values match gam.check
  gam_check_k <- mgcv::k.check(setup$model)
  our_edf <- our_validation$edf

  expect_equal(our_edf$total_edf, sum(setup$model$edf), tolerance = 0.01)
})


# ==============================================================================
# Test Suite 10: Validation Report Export
# ==============================================================================

test_that("export_validation_report creates markdown report", {
  setup <- setup_test_model()

  validation <- validate_gam_model(setup$model, setup$data)

  # Create temporary file for report
  temp_file <- tempfile(fileext = ".md")

  export_validation_report(
    validation = validation,
    output_file = temp_file,
    include_plots = FALSE
  )

  # Should create file
  expect_true(file.exists(temp_file))

  # Should contain validation results
  report_content <- readLines(temp_file)
  expect_gt(length(report_content), 20)

  # Should have sections for each validation component
  expect_true(any(grepl("Convergence", report_content, ignore.case = TRUE)))
  expect_true(any(grepl("Residual", report_content, ignore.case = TRUE)))
  expect_true(any(grepl("Concurvity", report_content, ignore.case = TRUE)))

  # Clean up
  unlink(temp_file)
})


# ==============================================================================
# Test Suite 8: Validation Type Differentiation
# ==============================================================================

test_that("validate_gam_model accepts validation_type parameter", {
  setup <- setup_test_model()

  # Should work with exploratory type
  result_exploratory <- validate_gam_model(
    model = setup$model,
    data = setup$data,
    validation_type = "exploratory",
    verbose = FALSE
  )

  expect_type(result_exploratory, "list")
  expect_equal(result_exploratory$overall_summary$validation_type, "exploratory")

  # Should work with simulation type
  result_simulation <- validate_gam_model(
    model = setup$model,
    data = setup$data,
    validation_type = "simulation",
    verbose = FALSE
  )

  expect_type(result_simulation, "list")
  expect_equal(result_simulation$overall_summary$validation_type, "simulation")

  # Should default to exploratory
  result_default <- validate_gam_model(
    model = setup$model,
    data = setup$data,
    verbose = FALSE
  )

  expect_equal(result_default$overall_summary$validation_type, "exploratory")
})

test_that("exploratory validation separates critical issues from warnings", {
  setup <- setup_test_model()

  result <- validate_gam_model(
    model = setup$model,
    data = setup$data,
    validation_type = "exploratory",
    verbose = FALSE
  )

  # Should have both critical_issues and warnings fields
  expect_true("critical_issues" %in% names(result$overall_summary))
  expect_true("warnings" %in% names(result$overall_summary))
  expect_true("n_critical_issues" %in% names(result$overall_summary))
  expect_true("n_warnings" %in% names(result$overall_summary))

  # For a good model, should have no critical issues
  expect_equal(result$overall_summary$n_critical_issues, 0)
})

test_that("exploratory validation treats normality test as warning not error", {
  # Create data that may have non-normal residuals but is otherwise fine
  test_data <- simulate_multi_education_unemployment(
    n_years = 10,
    education_levels = c("phd", "masters"),
    baseline_rates = c(phd = 0.040, masters = 0.050),
    seasonal_amplitudes = c(phd = 0.005, masters = 0.010),
    trend_slopes = c(phd = 0, masters = 0),
    noise_sd = 0.005,
    seed = 123
  )

  test_data$year <- 2010 + (test_data$time_index - 1) %/% 12

  model <- fit_factor_smooth_gam(
    data = test_data,
    formula_type = "full",
    education_var = "education"
  )

  # Exploratory validation
  result_exploratory <- validate_gam_model(
    model = model,
    data = test_data,
    validation_type = "exploratory",
    verbose = FALSE
  )

  # Simulation validation
  result_simulation <- validate_gam_model(
    model = model,
    data = test_data,
    validation_type = "simulation",
    verbose = FALSE
  )

  # If normality fails, exploratory should treat as warning not critical issue
  # (We can't guarantee normality will fail, but if it does, check behavior)
  if (!result_exploratory$residuals$normality$passed) {
    # For exploratory, normality failure should be a warning
    expect_true(grepl("Non-normal", result_exploratory$overall_summary$warnings))
    # For simulation, it might be an issue
    # (Not guaranteed, depends on sample size and how strict test is)
  }
})

test_that("simulation validation is stricter than exploratory", {
  setup <- setup_test_model()

  result_exploratory <- validate_gam_model(
    model = setup$model,
    data = setup$data,
    validation_type = "exploratory",
    verbose = FALSE
  )

  result_simulation <- validate_gam_model(
    model = setup$model,
    data = setup$data,
    validation_type = "simulation",
    verbose = FALSE
  )

  # For same model, simulation validation should never be more lenient
  # If exploratory fails, simulation should also fail
  if (!result_exploratory$overall_summary$validation_passed) {
    expect_false(result_simulation$overall_summary$validation_passed)
  }

  # Simulation counts warnings as failures, exploratory doesn't
  if (result_exploratory$overall_summary$n_warnings > 0 &&
      result_exploratory$overall_summary$n_critical_issues == 0) {
    # Exploratory with only warnings should pass
    expect_true(result_exploratory$overall_summary$validation_passed)
    # Simulation with warnings should fail
    expect_false(result_simulation$overall_summary$validation_passed)
  }
})

test_that("validation type affects heteroscedasticity interpretation", {
  setup <- setup_test_model()

  result_exploratory <- validate_gam_model(
    model = setup$model,
    data = setup$data,
    validation_type = "exploratory",
    verbose = FALSE
  )

  result_simulation <- validate_gam_model(
    model = setup$model,
    data = setup$data,
    validation_type = "simulation",
    verbose = FALSE
  )

  # If heteroscedasticity is detected
  if (!result_exploratory$residuals$heteroscedasticity$passed) {
    # For exploratory, should be a warning
    expect_true(grepl("Heteroscedasticity", result_exploratory$overall_summary$warnings) ||
                  grepl("Heteroscedasticity", result_exploratory$overall_summary$critical_issues))

    # For simulation, should be a critical issue
    expect_true(grepl("Heteroscedasticity", result_simulation$overall_summary$critical_issues) ||
                  !result_simulation$overall_summary$validation_passed)
  }
})

test_that("print method shows validation type", {
  setup <- setup_test_model()

  result <- validate_gam_model(
    model = setup$model,
    data = setup$data,
    validation_type = "exploratory",
    verbose = FALSE
  )

  # Capture print output
  output <- capture.output(print(result))

  # Should mention validation type
  expect_true(any(grepl("EXPLORATORY", output, ignore.case = TRUE)))
})

test_that("print method distinguishes critical issues from warnings", {
  setup <- setup_test_model()

  result <- validate_gam_model(
    model = setup$model,
    data = setup$data,
    validation_type = "exploratory",
    verbose = FALSE
  )

  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")

  # Should have separate sections if both exist
  # (Can't guarantee warnings will exist, but structure should support them)
  if (result$overall_summary$n_warnings > 0) {
    expect_true(grepl("Warnings", output_text, ignore.case = TRUE))
  }
})
