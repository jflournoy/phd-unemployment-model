# Testing Strategy for Report Generation

## Lessons Learned from Recent Bugs

### Bug: Centered Effects Plotted Instead of Predictions

**What happened**: The factor smooth report plotted `trend_effect` (centered smooth effects) instead of actual predicted unemployment rates with intercept. This made PhD unemployment appear to converge toward "Less than HS" unemployment near 0%, which was economically nonsensical.

**Root cause**: Confusion between two types of model outputs:
- `predict(model, type="terms")`: Returns **centered** smooth effects (mean ≈ 0, can be negative)
- `predict(model, type="response")`: Returns **predictions** with intercept (actual unemployment rates, always positive)

**Why it's insidious**: The code ran without errors, produced smooth plots with confidence intervals, and looked plausible at first glance. Only domain knowledge (PhD unemployment should be low, not converging to "Less than HS") revealed the bug.

---

## Test Categories to Prevent Report Bugs

### 1. **Range Validation Tests**

These catch the most obvious symptom: unemployment rates outside [0, 1].

```r
test_that("predicted unemployment rates are in valid range", {
  # Get predictions
  preds <- predict(model, newdata = data, type = "response")

  # CRITICAL: Must be in [0, 1]
  expect_true(all(preds >= 0, na.rm = TRUE),
              info = "Predictions should be >= 0 (not centered effects!)")
  expect_true(all(preds <= 1, na.rm = TRUE),
              info = "Predictions should be <= 1 (unemployment is proportion)")

  # Should be in realistic range
  expect_true(all(preds >= 0.005 & preds <= 0.15, na.rm = TRUE),
              info = "Predictions unrealistic (<0.5% or >15%)")
})
```

**What this catches**:
- Using centered effects (which can be negative)
- Predictions on wrong scale (e.g., percentages instead of proportions)
- Model misspecification producing nonsensical values

---

### 2. **Domain Knowledge Tests**

These encode economic/substantive relationships that must hold.

```r
test_that("PhD unemployment is lower than less educated groups", {
  # Get predictions for all education levels
  mean_rates <- tapply(predictions, education, mean)

  # CRITICAL: Education gradient must be correct
  expect_true(mean_rates["phd"] < mean_rates["less_than_hs"],
              info = "PhD unemployment should be < Less than HS")
  expect_true(mean_rates["phd"] < mean_rates["bachelors"],
              info = "PhD unemployment should be < Bachelors")

  # Specific ranges based on economic data
  expect_true(mean_rates["phd"] >= 0.01 && mean_rates["phd"] <= 0.04,
              info = sprintf("PhD rate should be 1-4%%, got %.1f%%",
                           mean_rates["phd"] * 100))
})
```

**What this catches**:
- The original bug (PhD converging to "Less than HS")
- Model misspecification (wrong smooths, interactions)
- Data errors (wrong education coding)

---

### 3. **Data Structure Validation Tests**

These prevent errors from assuming wrong object structure.

```r
test_that("analysis object has expected structure", {
  analysis <- analyze_cps_unemployment_by_education(...)

  # Must have best_model (not models$m6!)
  expect_true("best_model" %in% names(analysis),
              info = "analysis should have 'best_model' element")
  expect_s3_class(analysis$best_model, "gam")

  # visualization_data should have expected components
  viz_data <- analysis$visualization_data
  expect_true("fitted" %in% names(viz_data))
  expect_true("trends" %in% names(viz_data))
  expect_true("date" %in% names(viz_data$fitted))
})
```

**What this catches**:
- Trying to access `analysis$models$m6` when it's `analysis$best_model`
- Missing variables in scope (`viz_data` not extracted)
- API changes breaking reports

---

### 4. **Documentation Tests**

These document expected behavior to prevent confusion.

```r
test_that("visualization data trends are centered effects, not predictions", {
  viz_data <- analysis$visualization_data
  phd_trends <- viz_data$trends[viz_data$trends$education == "phd", ]

  # EXPECTED: trend_effect should be centered
  expect_true(abs(mean(phd_trends$trend_effect)) < 0.02,
              info = "trend_effect should be centered (mean near 0)")

  # EXPECTED: trend_effect can be negative
  expect_true(any(phd_trends$trend_effect < 0),
              info = "trend_effect can be negative (it's centered)")

  # WARNING in test output:
  # Do NOT use trend_effect directly as unemployment rate!
})
```

**What this catches**:
- Future developers using `trend_effect` incorrectly
- Changes to `extract_education_specific_trend()` breaking assumptions

---

### 5. **Method Validation Tests**

These ensure statistical methods work as intended.

```r
test_that("fixed month predictions remove seasonality", {
  # Predict at June
  pred_june <- predict(model, newdata = data.frame(
    time_index = 100:120,
    month = 6,  # Fixed month
    education = "phd"
  ))

  # Predict at December
  pred_dec <- predict(model, newdata = data.frame(
    time_index = 100:120,
    month = 12,
    education = "phd"
  ))

  # Should differ (seasonality)
  expect_false(isTRUE(all.equal(pred_june, pred_dec)),
               info = "Predictions should differ by month")

  # Fixed month should have smooth trend
  expect_true(sd(pred_june) < sd(c(pred_june, pred_dec)) / 2,
              info = "Fixed month predictions have less variance")
})
```

**What this catches**:
- Seasonal component not being removed correctly
- Predictions not varying by month (seasonal smooth not working)

---

### 6. **Confidence Interval Tests**

These catch issues with uncertainty quantification.

```r
test_that("confidence intervals are reasonable width", {
  preds <- predict(model, newdata = data, type = "response", se.fit = TRUE)

  # SE should be positive
  expect_true(all(preds$se.fit > 0))

  # SE in realistic range (0.01% to 2% for unemployment)
  expect_true(all(preds$se.fit < 0.02 & preds$se.fit > 0.0001))

  # 95% CI should be reasonable width
  ci_width <- 1.96 * 2 * preds$se.fit
  expect_true(all(ci_width < 0.08),
              info = "95% CI narrower than 8 percentage points")
})
```

**What this catches**:
- Wrong SE calculation
- Using SE from wrong smooth component
- Variance inflation issues

---

## Testing Workflow for Reports

### Before Rendering

1. **Run unit tests** on all visualization functions:
   ```r
   devtools::test(filter = "report-validation")
   ```

2. **Smoke test** with recent data:
   ```r
   # Quick analysis on recent period only
   analysis <- analyze_cps_unemployment_by_education(
     data_file,
     education_levels = c("phd", "masters", "bachelors"),
     start_year = 2020,  # Recent only for speed
     end_year = 2024
   )

   # Validate structure
   expect_s3_class(analysis$best_model, "gam")
   expect_true("visualization_data" %in% names(analysis))
   ```

### During Development

1. **Interactive validation** in the report:
   ```r
   # In QMD chunk:
   preds <- predict(best_model, newdata = pred_data, type = "response")

   # Sanity check before plotting
   stopifnot(all(preds >= 0 & preds <= 1))
   stopifnot(mean(preds[education == "phd"]) <
             mean(preds[education == "less_than_hs"]))

   # Print diagnostics
   cat("Prediction ranges by education:\n")
   print(tapply(preds, pred_data$education, range))
   ```

2. **Visual inspection** of plots:
   - Do y-axis ranges make sense? (e.g., 0-10% unemployment, not -5% to +5%)
   - Do education gradients go the right direction?
   - Do confidence intervals have reasonable width?

### After Rendering

1. **Automated checks** on rendered HTML:
   ```r
   # Extract numbers from report
   html <- rvest::read_html("report.html")
   unemployment_values <- html |>
     rvest::html_nodes(".unemployment-rate") |>
     rvest::html_text() |>
     as.numeric()

   # Validate
   expect_true(all(unemployment_values >= 0 & unemployment_values <= 100))
   ```

2. **Manual review**:
   - Do the results tell a coherent story?
   - Do trend lines match your economic intuition?
   - Are there any surprising jumps or discontinuities?

---

## Principles for Robust Reports

### 1. **Always Use `type="response"` for Plots**

```r
# CORRECT: Actual unemployment predictions
preds <- predict(model, type = "response")  # Includes intercept

# WRONG: Centered smooth effects
effects <- predict(model, type = "terms")   # No intercept, can be negative
```

### 2. **Remove Seasonality by Fixing Month, Not Extracting Component**

```r
# CORRECT: Predict at fixed month
trend_preds <- predict(model, newdata = data.frame(
  time_index = 1:300,
  month = 6,  # Fixed month removes seasonality
  education = "phd"
), type = "response")

# WRONG: Extract trend component separately
# (This gives centered effect without intercept)
trend_component <- extract_education_specific_trend(model, "phd")
```

### 3. **Validate Outputs Before Plotting**

```r
# Add assertions
stopifnot(
  all(predictions >= 0),
  all(predictions <= 1),
  mean(predictions[education == "phd"]) < mean(predictions[education == "bachelors"])
)

# Print diagnostics
cat(sprintf("PhD mean: %.2f%%, Bachelors mean: %.2f%%\n",
            mean(predictions[education == "phd"]) * 100,
            mean(predictions[education == "bachelors"]) * 100))
```

### 4. **Document Data Structures**

```r
# GOOD: Document what viz_data$trends contains
#' @field trends Data frame with centered trend effects (NOT predictions!)
#'   - trend_effect: Centered smooth component (mean ≈ 0)
#'   - se: Standard error of trend effect
#'   - DO NOT use trend_effect directly as unemployment rate!
```

---

## Future Enhancements

### 1. **Golden Output Tests**

Save "known good" outputs and test against them:

```r
test_that("predictions match golden output", {
  current <- analyze_cps_unemployment_by_education(data_file, ...)
  golden <- readRDS("tests/fixtures/golden_predictions.rds")

  # Allow small numerical differences
  expect_equal(current$predictions, golden$predictions,
               tolerance = 1e-4)
})
```

### 2. **Visual Regression Tests**

Automatically compare plot outputs:

```r
test_that("trend plot hasn't changed unexpectedly", {
  vdiffr::expect_doppelganger(
    "phd-vs-bachelors-trend",
    trend_comparison_plot(analysis, "phd", "bachelors")
  )
})
```

### 3. **Property-Based Tests**

Test invariants across random inputs:

```r
test_that("predictions monotonic in education (stochastic)", {
  for (i in 1:100) {
    # Generate random prediction grid
    test_data <- generate_random_prediction_grid()
    preds <- predict(model, newdata = test_data)

    # Education gradient should hold
    expect_true(mean(preds[edu == "phd"]) <
                mean(preds[edu == "bachelors"]))
  }
})
```

---

## Summary

The key insight is that **reports need domain-specific validation**, not just code correctness. Tests should encode:

1. **Mathematical constraints**: Values in valid ranges
2. **Domain knowledge**: Economic relationships that must hold
3. **Method properties**: Statistical procedures working correctly
4. **API contracts**: Data structures as expected
5. **Behavioral documentation**: What different outputs mean

These tests would have caught the centered effects bug **before** it reached the rendered report, saving debugging time and preventing incorrect conclusions.
