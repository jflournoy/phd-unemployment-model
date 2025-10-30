# TDD Workflow for Statistical Modeling

## Adapting TDD to Statistical Analysis

Test-Driven Development can be adapted for statistical modeling by focusing on:

1. **Data validation** - Tests for data quality and structure
2. **Function behavior** - Tests for data processing functions
3. **Model properties** - Tests for model convergence and diagnostics
4. **Statistical properties** - Tests for expected parameter ranges

## TDD Cycle for Statistical Functions

### Red Phase: Write Failing Test

Example: Testing a function to calculate unemployment rate

```r
# tests/testthat/test-unemployment-rate.R
test_that("calculate_unemployment_rate handles valid input", {
  # Create test data
  test_data <- tibble(
    employed = 90,
    unemployed = 10,
    not_in_labor_force = 50
  )

  result <- calculate_unemployment_rate(test_data)

  expect_equal(result, 0.1)  # 10 / (90 + 10) = 0.1
})

test_that("calculate_unemployment_rate rejects invalid data", {
  invalid_data <- tibble(employed = -5, unemployed = 10)

  expect_error(calculate_unemployment_rate(invalid_data))
})
```

### Green Phase: Write Minimal Code

```r
# R/unemployment-rate.R
#' Calculate unemployment rate
#'
#' @param data A tibble with employed and unemployed counts
#' @return Unemployment rate (proportion)
#' @export
calculate_unemployment_rate <- function(data) {
  # Validation
  if (any(data$employed < 0) || any(data$unemployed < 0)) {
    stop("Employment counts cannot be negative")
  }

  # Calculate rate
  labor_force <- data$employed + data$unemployed
  rate <- data$unemployed / labor_force

  return(rate)
}
```

### Refactor Phase: Improve Code

- Add vectorization
- Handle edge cases (zero labor force)
- Add documentation
- Add type checking

## TDD for Model Development

### Test Model Convergence

```r
test_that("baseline model converges", {
  # Use small test dataset
  test_data <- generate_test_timeseries(n = 100)

  model <- fit_baseline_model(test_data,
                               chains = 2,
                               iter = 1000,
                               seed = 123)

  # Check convergence
  expect_true(all(rhat(model) < 1.01))
  expect_true(all(neff_ratio(model) > 0.1))
})
```

### Test Prior Predictive Checks

```r
test_that("priors generate reasonable predictions", {
  prior_samples <- sample_prior(baseline_model_formula,
                                data = test_data,
                                prior = model_priors)

  # Check that prior predictions are in plausible range
  pred_mean <- mean(prior_samples$unemployment_rate)

  expect_true(pred_mean >= 0 && pred_mean <= 1)
})
```

### Test Posterior Predictive Checks

```r
test_that("model recovers known parameters", {
  # Simulate data with known parameters
  sim_data <- simulate_unemployment_data(
    trend = 0.05,
    seasonal_amplitude = 0.02,
    n_obs = 200,
    seed = 456
  )

  # Fit model
  fit <- fit_model(sim_data)

  # Check parameter recovery
  trend_post <- posterior_summary(fit, variable = "trend")

  expect_true(trend_post["mean"] > 0.04 && trend_post["mean"] < 0.06)
})
```

## Testing Data Processing Pipelines

```r
test_that("IPUMS data processing pipeline produces valid output", {
  # Use small test file
  test_file <- "tests/testthat/fixtures/test_ipums_data.csv"

  processed <- process_ipums_data(test_file)

  # Structure tests
  expect_s3_class(processed, "tbl_df")
  expect_named(processed, c("date", "phd_unemp_rate", "gen_unemp_rate"))

  # Data validity tests
  expect_true(all(processed$phd_unemp_rate >= 0))
  expect_true(all(processed$phd_unemp_rate <= 1))
  expect_true(all(!is.na(processed$date)))

  # Temporal ordering
  expect_true(all(diff(processed$date) > 0))
})
```

## Running Tests

```bash
# Run all tests
npm run test:r

# Run specific test file
Rscript -e 'testthat::test_file("tests/testthat/test-unemployment-rate.R")'

# Run with coverage
npm run test:r:coverage
```

## Test Organization

- `tests/testthat/test-*.R` - Main test files
- `tests/testthat/fixtures/` - Test data files
- `tests/testthat/helper-*.R` - Test helper functions
- `tests/testthat/setup.R` - Global test setup

## Best Practices

1. **Keep tests fast** - Use small datasets, cache expensive operations
2. **Test in isolation** - Each test should be independent
3. **Use fixtures** - Store test data in fixtures directory
4. **Mock expensive operations** - Don't fit full Stan models in every test
5. **Test error conditions** - Verify functions fail gracefully
6. **Document test intent** - Use descriptive test names
