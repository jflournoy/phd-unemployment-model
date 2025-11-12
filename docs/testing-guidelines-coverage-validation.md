# Coverage Validation Testing Guidelines

## Overview

This document describes the correct methodology for **coverage-based validation** of statistical models. Coverage validation verifies that confidence intervals and prediction intervals are properly calibrated - i.e., that 95% CIs contain true values approximately 95% of the time.

**Last Updated:** 2025-11-12
**Status:** Production guidelines based on bug fixes in parameter recovery validation

## Two Types of Coverage Tests

### 1. Prediction Interval Coverage

**What it tests:** Do the model's prediction intervals correctly capture new observations from the same data-generating process?

**Methodology:**
1. Fit ONE reference model to reference dataset
2. Calculate prediction intervals from the reference model (once)
3. Generate MANY new datasets from the same DGP
4. Check if reference model's PIs contain observations from new datasets
5. Compute overall coverage rate across all new observations

**Expected result:** ~95% of new observations should fall within the 95% prediction intervals

**Example from test-seasonal-gam.R:**
```r
# Calculate PIs from reference model (once)
pred_ref <- predict(model, se.fit = TRUE)
pred_sd <- sqrt(pred_ref$se.fit^2 + noise_sd^2)
lower_pi <- pred_ref$fit - 1.96 * pred_sd
upper_pi <- pred_ref$fit + 1.96 * pred_sd

# Test on many new datasets
for (i in 1:n_sims) {
  # Generate NEW dataset from same DGP
  new_data <- simulate_seasonal_unemployment(
    n_years = nrow(data) / 12,
    baseline_rate = baseline_rate,
    trend_slope = trend_slope,
    seasonal_amplitude = seasonal_amplitude,
    noise_sd = noise_sd,
    seed = i * 12345
  )

  # Check if reference PIs contain new observations
  within_interval <- (new_data$unemployment_rate >= lower_pi) &
                     (new_data$unemployment_rate <= upper_pi)
  total_coverage_count <- total_coverage_count + sum(within_interval)
  total_observations <- total_observations + nrow(new_data)
}

coverage_rate <- total_coverage_count / total_observations
```

**COMMON MISTAKE TO AVOID:**
Do NOT fit a new model to each dataset and check against its own training data. This gives artificially perfect 100% coverage because you're testing the model on the data it was fit to.

### 2. Parameter CI Coverage

**What it tests:** Do the model's confidence intervals for parameters correctly capture the true parameter values?

**Methodology:**
1. For each of many simulations (typically 100):
   - Generate NEW dataset from DGP with known parameters
   - Fit NEW model to this dataset
   - Extract parameter estimate and CI from this model
   - Check if the CI contains the true parameter value
2. Calculate proportion of CIs that contain true value

**Expected result:** ~95% of the CIs should contain the true parameter value

**Example from test-seasonal-gam.R:**
```r
baseline_coverage_count <- 0

for (i in 1:n_sims) {
  # Generate NEW dataset
  sim_data <- simulate_seasonal_unemployment(
    n_years = nrow(data) / 12,
    baseline_rate = baseline_rate,
    trend_slope = trend_slope,
    seasonal_amplitude = seasonal_amplitude,
    noise_sd = noise_sd,
    seed = i * 123
  )

  # Fit NEW model
  sim_model <- fit_seasonal_gam(sim_data, k_month = 10, k_trend = 15)

  # Extract baseline estimate and CI from THIS model
  baseline_est <- estimate_baseline(sim_model, sim_data)
  baseline_ci <- baseline_confidence_interval(sim_model, sim_data)

  # Check if CI contains TRUE baseline
  if (baseline_ci$lower <= baseline_rate && baseline_ci$upper >= baseline_rate) {
    baseline_coverage_count <- baseline_coverage_count + 1
  }
}

coverage_rate <- baseline_coverage_count / n_sims
```

## Critical Requirement: DGP Consistency

**The data-generating process (DGP) used for reference data MUST exactly match the DGP used in coverage validation.**

Any parameter mismatch creates systematic bias and miscalibrated coverage.

### Example of DGP Mismatch (WRONG)

```r
# Reference data simulated WITH trend
sim_data <- simulate_seasonal_unemployment(
  n_years = 10,
  baseline_rate = 0.025,
  trend_slope = 0.0003,  # Reference has trend
  seasonal_amplitude = 0.008,
  noise_sd = 0.002
)

model <- fit_seasonal_gam(sim_data)

# But true_params doesn't include trend_slope
true_params <- list(
  baseline = 0.025,
  seasonal_amplitude = 0.008
  # Missing: trend_slope
)

# Validation function will default to trend_slope = 0
# This creates MISMATCH: reference has trend, validation data has no trend
result <- validate_parameter_recovery_coverage(model, sim_data, true_params)
# Result: Poor baseline coverage (~73% instead of ~95%)
```

### Example of DGP Match (CORRECT)

```r
# Reference data simulated WITHOUT trend
sim_data <- simulate_seasonal_unemployment(
  n_years = 10,
  baseline_rate = 0.025,
  trend_slope = 0,  # No trend
  seasonal_amplitude = 0.008,
  noise_sd = 0.002
)

model <- fit_seasonal_gam(sim_data)

# true_params matches DGP (no trend_slope = defaults to 0)
true_params <- list(
  baseline = 0.025,
  seasonal_amplitude = 0.008
)

# Validation uses same DGP
result <- validate_parameter_recovery_coverage(model, sim_data, true_params)
# Result: Good baseline coverage (~94-97%)
```

## Implementation Checklist

When implementing coverage validation for a new model:

- [ ] **Define the DGP explicitly** - Document all parameters used to generate reference data
- [ ] **Match DGP in validation** - Ensure validation function uses identical parameters
- [ ] **Separate the two test types:**
  - [ ] Prediction intervals: ONE model, MANY datasets
  - [ ] Parameter CIs: MANY models, each with its own dataset
- [ ] **Use adequate sample size:**
  - [ ] n_sims >= 100 for production validation
  - [ ] n_sims >= 30 for unit tests (faster)
- [ ] **Set appropriate tolerance:**
  - [ ] Production: tolerance = 0.05 (90-100% coverage acceptable)
  - [ ] Testing: tolerance = 0.15 (80-100% coverage acceptable)
- [ ] **Document default parameters** - If validation function has defaults, document them clearly

## Test Structure Examples

### Basic Coverage Validation Test

```r
test_that("comprehensive coverage validation passes all three tests", {
  # Simulate reference data
  sim_data <- simulate_data(
    param1 = value1,
    param2 = value2,
    seed = 999
  )

  # Fit model
  model <- fit_model(sim_data)

  # Define true parameters (matching DGP exactly)
  true_params <- list(
    param1 = value1,
    param2 = value2
  )

  # Run coverage validation
  coverage_results <- validate_coverage(
    model,
    sim_data,
    true_params,
    n_sims = 30
  )

  # Check all tests included
  expect_equal(nrow(coverage_results), 3)

  # Check coverage rates are valid
  expect_true(all(coverage_results$coverage_rate >= 0))
  expect_true(all(coverage_results$coverage_rate <= 1))

  # Most should pass (allow for statistical variation)
  expect_gte(sum(coverage_results$meets_target), 2)
})
```

### DGP Mismatch Detection Test

```r
test_that("coverage validation detects DGP mismatch", {
  # Simulate reference data WITH feature
  sim_data <- simulate_data(
    param1 = value1,
    param2 = value2,
    feature = TRUE,  # Include feature
    seed = 888
  )

  model <- fit_model(sim_data)

  # Define true_params WITHOUT feature (creates mismatch)
  true_params <- list(
    param1 = value1,
    param2 = value2
    # Note: feature intentionally omitted
  )

  # Run coverage validation (will use feature=FALSE default)
  coverage_results <- validate_coverage(model, sim_data, true_params)

  # With DGP mismatch, coverage should be poor
  expect_lt(coverage_results$coverage_rate[1], 0.80)
})
```

### DGP Match Success Test

```r
test_that("coverage validation with matched DGP succeeds", {
  # Simulate reference data WITHOUT feature
  sim_data <- simulate_data(
    param1 = value1,
    param2 = value2,
    feature = FALSE,  # Explicitly no feature
    seed = 777
  )

  model <- fit_model(sim_data)

  # Define true_params (matches validation default)
  true_params <- list(
    param1 = value1,
    param2 = value2
  )

  # Run coverage validation (will use feature=FALSE - matches!)
  coverage_results <- validate_coverage(model, sim_data, true_params)

  # With matched DGP, coverage should be good
  expect_gte(coverage_results$coverage_rate[1], 0.80)
})
```

## References

### Project Files
- [R/seasonal-gam.R](../R/seasonal-gam.R) - Contains `validate_parameter_recovery_coverage()` function
- [tests/testthat/test-seasonal-gam.R](../tests/testthat/test-seasonal-gam.R) - Comprehensive coverage validation tests
- [tests/testthat/test-coverage-validation-simple.R](../tests/testthat/test-coverage-validation-simple.R) - Basic function tests
- [reports/parameter-recovery-validation.qmd](../reports/parameter-recovery-validation.qmd) - Full validation report with examples

### Key Learnings
See [LEARNINGS.md](../LEARNINGS.md) entries:
- "Coverage validation requires different approaches for prediction intervals vs parameter CIs"
- "DGP consistency is critical for accurate coverage validation"
- "Default parameters in validation functions must be explicitly documented"

### Bug Fixes
- **2025-11-12:** Fixed baseline CI coverage failure (73% → 94%) by ensuring DGP consistency
- **2025-11-12:** Fixed prediction interval validation bug (100% → ~95%) by using correct Test 1 methodology

## Summary

**Key Principles:**
1. **Two distinct methodologies** - Prediction intervals use ONE model on MANY datasets; parameter CIs use MANY models
2. **DGP consistency is critical** - Reference data and validation data must use identical parameters
3. **Document defaults** - Validation functions with default parameters must document them clearly
4. **Test both success and failure** - Include tests that verify DGP mismatches are detected

**Common Pitfalls:**
- Testing models on their own training data (gives 100% coverage)
- DGP parameter mismatches between reference and validation
- Insufficient sample size (use n_sims >= 100 for production)
- Not documenting default parameter values
