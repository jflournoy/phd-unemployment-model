# Tests for factor smooth GAM models (joint modeling of multiple education levels)
# Following TDD RED-GREEN-REFACTOR pattern

library(testthat)
library(mgcv)

# Helper function to create multi-education simulated data
create_multi_education_sim_data <- function(n_months = 120,
                                            baseline_rates = c(phd = 0.025, masters = 0.035, bachelors = 0.045),
                                            seasonal_amplitudes = c(phd = 0.005, masters = 0.010, bachelors = 0.015),
                                            trend_slopes = c(phd = -0.0001, masters = -0.0002, bachelors = -0.0003)) {

  education_levels <- names(baseline_rates)

  # Generate data for each education level
  data_list <- lapply(education_levels, function(educ) {
    # Create base data
    time_index <- seq_len(n_months)
    month <- ((time_index - 1) %% 12) + 1

    # Seasonal component (sinusoidal)
    seasonal_effect <- seasonal_amplitudes[[educ]] * sin(2 * pi * month / 12)

    # Trend component
    trend_effect <- trend_slopes[[educ]] * time_index

    # Combined rate
    unemployment_rate <- baseline_rates[[educ]] + seasonal_effect + trend_effect

    # Add noise
    unemployment_rate <- unemployment_rate + rnorm(n_months, 0, 0.002)

    data.frame(
      time_index = time_index,
      month = month,
      unemployment_rate = unemployment_rate,
      education = educ,
      stringsAsFactors = FALSE
    )
  })

  # Combine all education levels
  combined_data <- do.call(rbind, data_list)
  combined_data$education <- factor(combined_data$education, levels = education_levels)

  # Add metadata as attributes
  attr(combined_data, 'true_baseline') <- baseline_rates
  attr(combined_data, 'true_seasonal_amplitude') <- seasonal_amplitudes
  attr(combined_data, 'true_trend_slope') <- trend_slopes

  return(combined_data)
}


# ==============================================================================
# Test Suite 1: Model Fitting & Structure
# ==============================================================================

test_that("fit_factor_smooth_gam fits joint model for all education levels", {
  # Create multi-education data
  sim_data <- create_multi_education_sim_data(n_months = 120)

  # Fit factor smooth model with full specification
  model <- fit_factor_smooth_gam(
    sim_data,
    formula_type = "full",
    education_var = "education"
  )

  # Model should be a gam object
  expect_s3_class(model, "gam")

  # Should have 'by' variable smooths (6 total: 3 education × 2 smooth types)
  expect_equal(length(model$smooth), 6)

  # Check that formula includes by= argument
  expect_true(any(grepl("by", as.character(model$formula))))

  # Model should have stored metadata
  expect_equal(attr(model, "formula_type"), "full")
  expect_equal(attr(model, "education_var"), "education")
})

test_that("fit_factor_smooth_gam works with different formula types", {
  sim_data <- create_multi_education_sim_data(n_months = 100)

  # Test each formula type
  formula_types <- c("shared", "seasonal_by_education", "trend_by_education", "full")

  for (ftype in formula_types) {
    model <- fit_factor_smooth_gam(sim_data, formula_type = ftype)

    expect_s3_class(model, "gam")
    expect_equal(attr(model, "formula_type"), ftype)
  }
})

test_that("fit_factor_smooth_gam validates education variable is factor", {
  sim_data <- create_multi_education_sim_data(n_months = 100)

  # Convert education to character
  sim_data$education <- as.character(sim_data$education)

  # Should still work (function converts to factor)
  model <- fit_factor_smooth_gam(sim_data, formula_type = "full")

  expect_s3_class(model, "gam")
  expect_true(is.factor(model$model$education))
})


# ==============================================================================
# Test Suite 2: Nested Model Comparisons
# ==============================================================================

test_that("can fit nested sequence of joint models", {
  sim_data <- create_multi_education_sim_data(n_months = 120)

  # Fit sequence of nested models
  models <- fit_nested_model_sequence(sim_data)

  # Should return 7 models
  expect_length(models, 7)
  expect_equal(names(models), c("m0", "m1", "m2", "m3", "m4", "m5", "m6"))

  # All should be gam objects
  expect_true(all(sapply(models, function(m) inherits(m, "gam"))))

  # All models should fit to same data (same number of observations)
  n_obs <- sapply(models, function(m) nrow(model.frame(m)))
  expect_true(all(n_obs == n_obs[1]))

  # m0 should have fewest parameters, m6 should have most
  edfs <- sapply(models, function(m) sum(m$edf))
  expect_lt(edfs["m0"], edfs["m6"])
  expect_gt(edfs["m6"], 10)  # Full model should have substantial EDF
})

test_that("nested models can be compared via AIC", {
  sim_data <- create_multi_education_sim_data(n_months = 120)
  models <- fit_nested_model_sequence(sim_data)

  # Get AIC comparison table
  aic_table <- compare_nested_models(models)

  # Should return a data frame
  expect_s3_class(aic_table, "data.frame")
  expect_equal(nrow(aic_table), 7)

  # Required columns
  required_cols <- c("model", "AIC", "deviance", "df_residual", "edf", "r_squared", "delta_AIC")
  expect_true(all(required_cols %in% names(aic_table)))

  # delta_AIC should be sorted (best model first)
  expect_equal(aic_table$delta_AIC[1], 0)
  expect_true(all(diff(aic_table$delta_AIC) >= 0))

  # More complex models should explain more deviance
  # (though may not have better AIC due to penalty)
  deviances <- sapply(models, deviance)
  expect_gte(deviances["m0"], deviances["m6"])
})

test_that("AIC comparison identifies parsimonious model when appropriate", {
  # Simulate data with no patterns (just noise around constant rate)
  sim_data <- create_multi_education_sim_data(
    n_months = 150,
    baseline_rates = c(phd = 0.03, masters = 0.03, bachelors = 0.03),  # Same baseline
    seasonal_amplitudes = c(phd = 0, masters = 0, bachelors = 0),  # No seasonality
    trend_slopes = c(phd = 0, masters = 0, bachelors = 0)  # No trends
  )

  models <- fit_nested_model_sequence(sim_data)
  aic_table <- compare_nested_models(models)

  # Simplest models should be competitive when data has no structure
  # m0 or m1 should be in top 3
  top_3_models <- aic_table$model[1:3]
  expect_true("m0" %in% top_3_models || "m1" %in% top_3_models)
})


# ==============================================================================
# Test Suite 3: Parameter Recovery
# ==============================================================================

test_that("factor smooth recovers education-specific seasonal patterns", {
  # Simulate with known seasonal differences
  sim_data <- create_multi_education_sim_data(
    n_months = 200,
    seasonal_amplitudes = c(phd = 0.005, masters = 0.015, bachelors = 0.025)
  )

  true_amplitudes <- attr(sim_data, 'true_seasonal_amplitude')

  # Fit full model
  model <- fit_factor_smooth_gam(sim_data, formula_type = "full")

  # Extract seasonal components for each education level
  seasonal_phd <- extract_education_specific_seasonal(model, "phd")
  seasonal_masters <- extract_education_specific_seasonal(model, "masters")
  seasonal_bachelors <- extract_education_specific_seasonal(model, "bachelors")

  # Each should have 12 months
  expect_equal(nrow(seasonal_phd), 12)
  expect_equal(nrow(seasonal_masters), 12)
  expect_equal(nrow(seasonal_bachelors), 12)

  # Check required columns
  expect_true(all(c("month", "seasonal_effect", "se") %in% names(seasonal_phd)))

  # Compute estimated amplitudes (peak-to-trough / 2)
  amp_phd <- (max(seasonal_phd$seasonal_effect) - min(seasonal_phd$seasonal_effect)) / 2
  amp_masters <- (max(seasonal_masters$seasonal_effect) - min(seasonal_masters$seasonal_effect)) / 2
  amp_bachelors <- (max(seasonal_bachelors$seasonal_effect) - min(seasonal_bachelors$seasonal_effect)) / 2

  # Should recover true amplitudes within generous tolerance
  # (Factor smooths pool information, may not recover exact values)
  expect_lte(abs(amp_phd - true_amplitudes["phd"]), 0.015)
  expect_lte(abs(amp_masters - true_amplitudes["masters"]), 0.015)
  expect_lte(abs(amp_bachelors - true_amplitudes["bachelors"]), 0.025)

  # Ordering should be approximately preserved (within noise)
  # Just check phd <= bachelors + tolerance (strongest contrast)
  # Using expect_true with <= to handle edge cases
  expect_true(amp_phd <= amp_bachelors + 0.005)
})

test_that("factor smooth recovers education-specific trends", {
  # Simulate with known trend differences
  sim_data <- create_multi_education_sim_data(
    n_months = 200,
    trend_slopes = c(phd = -0.0002, masters = -0.0004, bachelors = -0.0006)
  )

  true_slopes <- attr(sim_data, 'true_trend_slope')

  # Fit full model
  model <- fit_factor_smooth_gam(sim_data, formula_type = "full")

  # Extract trend components
  trend_phd <- extract_education_specific_trend(model, "phd")
  trend_masters <- extract_education_specific_trend(model, "masters")

  # Should have one value per time point
  expect_gt(nrow(trend_phd), 100)

  # Check required columns
  expect_true(all(c("time_index", "trend_effect", "se") %in% names(trend_phd)))

  # Estimate slopes from first and last 20 points
  estimate_slope <- function(trend_df) {
    early <- mean(trend_df$trend_effect[1:20])
    late <- mean(trend_df$trend_effect[(nrow(trend_df)-19):nrow(trend_df)])
    time_diff <- mean(trend_df$time_index[(nrow(trend_df)-19):nrow(trend_df)]) -
                 mean(trend_df$time_index[1:20])
    (late - early) / time_diff
  }

  slope_phd <- estimate_slope(trend_phd)
  slope_masters <- estimate_slope(trend_masters)

  # Should recover true slopes within generous tolerance
  # Smoothing and pooling can affect recovery
  expect_lt(abs(slope_phd - true_slopes["phd"]), 0.0005)
  expect_lt(abs(slope_masters - true_slopes["masters"]), 0.0005)

  # Ordering: more negative for masters than phd (approximately)
  expect_lt(slope_masters, slope_phd + 0.0003)
})


# ==============================================================================
# Test Suite 4: Trend Difference Inference
# ==============================================================================

test_that("can compute trend differences with proper uncertainty", {
  sim_data <- create_multi_education_sim_data(n_months = 150)
  model <- fit_factor_smooth_gam(sim_data, formula_type = "full")

  # Compute pairwise differences
  diff_results <- compute_trend_differences(
    model,
    education_pairs = list(c("phd", "bachelors"), c("masters", "bachelors")),
    time_points = seq(1, 150, by = 10)
  )

  # Should return a data frame
  expect_s3_class(diff_results, "data.frame")

  # Required columns
  required_cols <- c("time_index", "comparison", "difference", "se",
                     "lower", "upper", "significant")
  expect_true(all(required_cols %in% names(diff_results)))

  # Should have 2 comparisons × 15 time points = 30 rows
  expect_equal(nrow(diff_results), 30)

  # SE should be positive
  expect_true(all(diff_results$se > 0))

  # Confidence intervals should be valid
  expect_true(all(diff_results$lower <= diff_results$difference))
  expect_true(all(diff_results$upper >= diff_results$difference))
  expect_true(all(diff_results$lower < diff_results$upper))
})

test_that("trend differences use proper variance-covariance matrix", {
  sim_data <- create_multi_education_sim_data(n_months = 120)
  model <- fit_factor_smooth_gam(sim_data, formula_type = "full")

  # Compute differences
  diff_results <- compute_trend_differences(
    model,
    education_pairs = list(c("phd", "masters")),
    time_points = c(50, 100)
  )

  # SE should be smaller than naive sqrt(se1^2 + se2^2)
  # because of positive correlation
  # (Can't test this directly without implementing naive version,
  #  but we can check SE is reasonable)

  expect_true(all(diff_results$se > 0))
  expect_true(all(diff_results$se < 0.01))  # Should be small for simulated data
})

test_that("simultaneous confidence bands are wider than pointwise", {
  sim_data <- create_multi_education_sim_data(n_months = 120)
  model <- fit_factor_smooth_gam(sim_data, formula_type = "full")

  time_pts <- seq(1, 120, by = 5)

  # Pointwise bands
  diff_pointwise <- compute_trend_differences(
    model,
    education_pairs = list(c("phd", "bachelors")),
    time_points = time_pts,
    simultaneous = FALSE,
    alpha = 0.05
  )

  # Simultaneous bands
  diff_simultaneous <- compute_trend_differences(
    model,
    education_pairs = list(c("phd", "bachelors")),
    time_points = time_pts,
    simultaneous = TRUE,
    alpha = 0.05
  )

  # Simultaneous bands should be wider
  ci_width_pointwise <- diff_pointwise$upper - diff_pointwise$lower
  ci_width_simultaneous <- diff_simultaneous$upper - diff_simultaneous$lower

  expect_true(all(ci_width_simultaneous >= ci_width_pointwise))
  expect_gt(mean(ci_width_simultaneous), mean(ci_width_pointwise))
})

test_that("simultaneous confidence bands control family-wise error rate", {
  skip_if(getRversion() < "4.0", "Test requires R >= 4.0")

  # Simulate data with NO true differences between education levels
  # (same parameters for all)
  sim_data <- create_multi_education_sim_data(
    n_months = 120,
    baseline_rates = c(phd = 0.03, masters = 0.03, bachelors = 0.03),
    seasonal_amplitudes = c(phd = 0.01, masters = 0.01, bachelors = 0.01),
    trend_slopes = c(phd = 0, masters = 0, bachelors = 0)
  )

  model <- fit_factor_smooth_gam(sim_data, formula_type = "full")

  diff_results <- compute_trend_differences(
    model,
    education_pairs = list(c("phd", "masters"), c("phd", "bachelors")),
    time_points = seq(1, 120, by = 5),
    simultaneous = TRUE,
    alpha = 0.05
  )

  # With no true differences and FWER control, should have few significant results
  prop_sig <- mean(diff_results$significant)

  # Allow for some variability, but should be much less than without correction
  # With 2 comparisons × 24 time points = 48 tests, pointwise would give ~2.4 false positives
  # Simultaneous should give closer to 0.05 × 2 comparisons = 0.1 or less
  expect_lte(prop_sig, 0.15)
})


# ==============================================================================
# Test Suite 5: Model Selection
# ==============================================================================

test_that("model selection identifies correct structure - seasonal only", {
  # Simulate data with ONLY seasonal differences (shared trend)
  sim_data <- create_multi_education_sim_data(
    n_months = 200,
    baseline_rates = c(phd = 0.03, masters = 0.035, bachelors = 0.04),
    seasonal_amplitudes = c(phd = 0.005, masters = 0.015, bachelors = 0.025),
    trend_slopes = c(phd = 0, masters = 0, bachelors = 0)  # Same trend (none)!
  )

  models <- fit_nested_model_sequence(sim_data)
  aic_table <- compare_nested_models(models)

  # Model 5 (shared trend + education-specific seasonality) should be best
  # or at least better than model 4 (education-specific trend + shared seasonality)
  aic_m4 <- aic_table$AIC[aic_table$model == "m4"]
  aic_m5 <- aic_table$AIC[aic_table$model == "m5"]

  # m5 should have lower AIC than m4 (or within 2 AIC units)
  expect_lte(aic_m5, aic_m4 + 2)
})

test_that("model selection identifies correct structure - trend only", {
  # Simulate data with ONLY trend differences (shared seasonality)
  sim_data <- create_multi_education_sim_data(
    n_months = 200,
    baseline_rates = c(phd = 0.03, masters = 0.035, bachelors = 0.04),
    seasonal_amplitudes = c(phd = 0.01, masters = 0.01, bachelors = 0.01),  # Same!
    trend_slopes = c(phd = -0.0001, masters = -0.0003, bachelors = -0.0005)
  )

  models <- fit_nested_model_sequence(sim_data)
  aic_table <- compare_nested_models(models)

  # Model 4 (education-specific trend + shared seasonality) should be best
  # or at least better than model 5
  aic_m4 <- aic_table$AIC[aic_table$model == "m4"]
  aic_m5 <- aic_table$AIC[aic_table$model == "m5"]

  expect_lte(aic_m4, aic_m5 + 2)
})

test_that("model selection prefers full model when both differ", {
  # Simulate data with both seasonal AND trend differences
  sim_data <- create_multi_education_sim_data(
    n_months = 200,
    seasonal_amplitudes = c(phd = 0.005, masters = 0.015, bachelors = 0.025),
    trend_slopes = c(phd = -0.0001, masters = -0.0003, bachelors = -0.0005)
  )

  models <- fit_nested_model_sequence(sim_data)
  aic_table <- compare_nested_models(models)

  # Full model (m6) should be competitive (within top 2)
  best_model <- aic_table$model[1]
  second_best <- aic_table$model[2]

  expect_true("m6" %in% c(best_model, second_best))
})


# ==============================================================================
# Test Suite 6: Multi-Education Simulation Function
# ==============================================================================

test_that("simulate_multi_education_unemployment is exported from package", {
  # Should be accessible as exported function
  expect_true(exists("simulate_multi_education_unemployment"))
  expect_type(simulate_multi_education_unemployment, "closure")
})

test_that("simulate_multi_education_unemployment generates correct data structure", {
  # Generate data for 3 education levels
  sim_data <- simulate_multi_education_unemployment(
    n_years = 5,
    education_levels = c("phd", "masters", "bachelors"),
    baseline_rates = c(phd = 0.025, masters = 0.035, bachelors = 0.045),
    seasonal_amplitudes = c(phd = 0.005, masters = 0.010, bachelors = 0.015),
    trend_slopes = c(phd = -0.0001, masters = -0.0002, bachelors = -0.0003),
    seed = 123
  )

  # Should return a data frame
  expect_s3_class(sim_data, "data.frame")

  # Should have correct number of rows (3 education levels × 5 years × 12 months)
  expect_equal(nrow(sim_data), 3 * 5 * 12)

  # Should have required columns
  required_cols <- c("time_index", "month", "unemployment_rate", "education")
  expect_true(all(required_cols %in% names(sim_data)))

  # education should be a factor
  expect_true(is.factor(sim_data$education))
  expect_equal(levels(sim_data$education), c("phd", "masters", "bachelors"))

  # time_index should run from 1 to 60 for each education level
  expect_true(all(sim_data$time_index >= 1 & sim_data$time_index <= 60))

  # month should be 1-12
  expect_true(all(sim_data$month >= 1 & sim_data$month <= 12))
})

test_that("simulate_multi_education_unemployment stores true parameters as attributes", {
  true_baseline <- c(phd = 0.03, masters = 0.04, bachelors = 0.05)
  true_amplitude <- c(phd = 0.008, masters = 0.012, bachelors = 0.016)
  true_slope <- c(phd = -0.0002, masters = -0.0003, bachelors = -0.0004)

  sim_data <- simulate_multi_education_unemployment(
    n_years = 3,
    education_levels = names(true_baseline),
    baseline_rates = true_baseline,
    seasonal_amplitudes = true_amplitude,
    trend_slopes = true_slope
  )

  # Should store true parameters as attributes
  expect_equal(attr(sim_data, "true_baseline"), true_baseline)
  expect_equal(attr(sim_data, "true_seasonal_amplitude"), true_amplitude)
  expect_equal(attr(sim_data, "true_trend_slope"), true_slope)
})

test_that("simulate_multi_education_unemployment respects seed for reproducibility", {
  params <- list(
    n_years = 3,
    education_levels = c("phd", "masters"),
    baseline_rates = c(phd = 0.03, masters = 0.04),
    seasonal_amplitudes = c(phd = 0.01, masters = 0.015),
    trend_slopes = c(phd = 0, masters = -0.0001)
  )

  # Generate twice with same seed
  sim1 <- simulate_multi_education_unemployment(seed = 42, n_years = params$n_years,
                                                education_levels = params$education_levels,
                                                baseline_rates = params$baseline_rates,
                                                seasonal_amplitudes = params$seasonal_amplitudes,
                                                trend_slopes = params$trend_slopes)
  sim2 <- simulate_multi_education_unemployment(seed = 42, n_years = params$n_years,
                                                education_levels = params$education_levels,
                                                baseline_rates = params$baseline_rates,
                                                seasonal_amplitudes = params$seasonal_amplitudes,
                                                trend_slopes = params$trend_slopes)

  # Should be identical
  expect_equal(sim1$unemployment_rate, sim2$unemployment_rate)

  # Different seed should give different results
  sim3 <- simulate_multi_education_unemployment(seed = 999, n_years = params$n_years,
                                                education_levels = params$education_levels,
                                                baseline_rates = params$baseline_rates,
                                                seasonal_amplitudes = params$seasonal_amplitudes,
                                                trend_slopes = params$trend_slopes)

  expect_false(identical(sim1$unemployment_rate, sim3$unemployment_rate))
})

test_that("simulate_multi_education_unemployment validates parameter vectors", {
  # Should error if parameter vectors don't match education_levels
  expect_error(
    simulate_multi_education_unemployment(
      n_years = 3,
      education_levels = c("phd", "masters", "bachelors"),
      baseline_rates = c(phd = 0.03, masters = 0.04),  # Missing bachelors!
      seasonal_amplitudes = c(phd = 0.01, masters = 0.015, bachelors = 0.02),
      trend_slopes = c(phd = 0, masters = -0.0001, bachelors = -0.0002)
    ),
    "missing values for: bachelors"
  )

  # Should error if names don't match
  expect_error(
    simulate_multi_education_unemployment(
      n_years = 3,
      education_levels = c("phd", "masters"),
      baseline_rates = c(doctoral = 0.03, masters = 0.04),  # Wrong name!
      seasonal_amplitudes = c(phd = 0.01, masters = 0.015),
      trend_slopes = c(phd = 0, masters = -0.0001)
    ),
    "missing values for: phd"
  )
})

test_that("simulate_multi_education_unemployment generates education-specific patterns", {
  # Simulate with very different parameters
  sim_data <- simulate_multi_education_unemployment(
    n_years = 15,
    education_levels = c("low", "high"),
    baseline_rates = c(low = 0.08, high = 0.02),  # Large difference
    seasonal_amplitudes = c(low = 0.002, high = 0.030),  # Large difference
    trend_slopes = c(low = 0.0005, high = -0.001),  # Opposite directions
    noise_sd = 0.001,  # Low noise to see patterns clearly
    seed = 123
  )

  # Split by education
  low_data <- sim_data[sim_data$education == "low", ]
  high_data <- sim_data[sim_data$education == "high", ]

  # Check that baselines are approximately correct
  # (mean over all time should be close to baseline + half the trend effect)
  expect_gt(mean(low_data$unemployment_rate), mean(high_data$unemployment_rate))

  # Check seasonality differences using detrended data
  # Fit linear trend and extract residuals for seasonal analysis
  low_lm <- lm(unemployment_rate ~ time_index, data = low_data)
  high_lm <- lm(unemployment_rate ~ time_index, data = high_data)

  low_data$detrended <- residuals(low_lm) + mean(low_data$unemployment_rate)
  high_data$detrended <- residuals(high_lm) + mean(high_data$unemployment_rate)

  # Compute range (peak-to-trough) for each education level from detrended data
  low_monthly <- aggregate(detrended ~ month, data = low_data, FUN = mean)
  high_monthly <- aggregate(detrended ~ month, data = high_data, FUN = mean)

  low_range <- max(low_monthly$detrended) - min(low_monthly$detrended)
  high_range <- max(high_monthly$detrended) - min(high_monthly$detrended)

  # high should have much larger seasonal range (15x larger by design)
  # Allow for noise affecting estimates
  expect_gt(high_range, low_range)  # Should be larger

  # Check trend directions
  # Use the fitted slopes from linear models
  low_slope <- coef(low_lm)["time_index"]
  high_slope <- coef(high_lm)["time_index"]

  # low should have positive slope, high should have negative slope
  expect_gt(low_slope, 0)
  expect_lt(high_slope, 0)
})


# ==============================================================================
# Test Suite 7: Integration with Existing Functions
# ==============================================================================

test_that("compare_unemployment_factor_smooth integrates with data processing", {
  skip("Requires real CPS data - implement after GREEN phase")

  # This test will use actual CPS data to ensure integration works
  # Will test after implementing the function
})
