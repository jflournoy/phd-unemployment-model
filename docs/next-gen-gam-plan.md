# Next-Generation GAM Models: Implementation Plan

**Date**: 2025-11-16
**Context**: Current GAMs leave significant variance unexplained. Plan addresses this with binomial family, increased basis dimensions, intervention modeling, and AR(1) correlation.

## Executive Summary

Based on diagnostic analysis, current Gaussian GAMs with k=20 trend splines are underspecified. This plan implements four enhancements in phases to improve variance explanation while maintaining interpretability.

**Key Improvements**:
1. Quasi-binomial family with weights (respects [0,1] bounds)
2. Increased basis dimensions (k=30 for trends)
3. Smooth intervention modeling (COVID, recessions)
4. AR(1) temporal correlation (lagged residuals approach)

**Expected Outcomes**:
- R² improvement: ~60% → 75-80%
- Autocorrelation: Ljung-Box test passes
- COVID effects: Quantified 2020-2025 structural changes
- Maintain: Education-level comparisons, shared smoothing

---

## Phase 1: Quick Wins (Week 1-2)

### 1.1 Quasi-Binomial Family with Weights

**Problem**: Current Gaussian family doesn't respect [0,1] bounds for proportion data

**Solution**: Use quasi-binomial with sample size weights

**Files Modified**:
- `R/seasonal-gam.R`: Add `family` and `use_weights` parameters to `fit_seasonal_gam()`
- `R/model-validation.R`: Add `validate_binomial_fit()` function

**Implementation Details**:

```r
fit_seasonal_gam <- function(data,
                              k_month = 12,
                              k_trend = 20,
                              family = "gaussian",  # NEW
                              use_weights = TRUE) {  # NEW

  # Set up weights for binomial
  weights <- if (use_weights && family %in% c("quasibinomial", "binomial") &&
                 "n_obs" %in% names(data)) {
    data$n_obs
  } else {
    NULL
  }

  # Fit model
  model <- mgcv::gam(
    unemployment_rate ~ s(time_index, bs = "cr", k = k_trend) +
                        s(month, bs = "cc", k = k_month),
    data = data,
    family = family,
    weights = weights,
    method = "REML"
  )

  return(model)
}

# Validation for binomial models
validate_binomial_fit <- function(model, data) {
  # Check overdispersion
  scale_param <- summary(model)$dispersion

  # Check predictions in [0,1]
  preds <- predict(model, type = "response")

  list(
    overdispersion = scale_param,
    overdispersion_warning = scale_param > 2,
    predictions_in_bounds = all(preds >= 0 & preds <= 1, na.rm = TRUE)
  )
}
```

**Why First**:
- Low risk, high impact
- Minimal code changes
- Immediate improvement in model appropriateness

**Tests Needed**:
```r
test_that("quasi-binomial predictions stay in [0,1]", {
  model <- fit_seasonal_gam(data, family = "quasibinomial")
  preds <- predict(model, type = "response")
  expect_true(all(preds >= 0 & preds <= 1))
})
```

---

### 1.2 Increase Basis Dimensions to k=30

**Problem**: k=20 may be too restrictive for 300 months of data (25 years)

**Solution**: Increase default k_trend to 30 with EDF ratio validation

**Files Modified**:
- `R/seasonal-gam.R`: Change default k_trend, add `validate_edf_ratio()`
- `R/model-validation.R`: Add EDF checks

**Implementation Details**:

```r
fit_seasonal_gam <- function(data,
                              k_month = 12,
                              k_trend = 30,  # CHANGED from 20
                              family = "gaussian",
                              use_weights = TRUE,
                              check_edf = TRUE) {  # NEW

  model <- mgcv::gam(...)

  # Validate EDF ratio
  if (check_edf) {
    edf_check <- validate_edf_ratio(model, data, max_ratio = 0.15)
    if (!edf_check$acceptable) {
      warning("High EDF ratio: ", edf_check$recommendation)
    }
  }

  return(model)
}

validate_edf_ratio <- function(model, data, max_ratio = 0.15) {
  total_edf <- sum(model$edf)

  # For factor smooths, divide by number of groups
  if ("education" %in% names(data)) {
    n_groups <- length(unique(data$education))
    n_obs <- nrow(data) / n_groups
  } else {
    n_obs <- nrow(data)
  }

  edf_ratio <- total_edf / n_obs

  list(
    total_edf = total_edf,
    n_obs = n_obs,
    edf_ratio = edf_ratio,
    acceptable = edf_ratio < max_ratio,
    recommendation = if (edf_ratio > max_ratio) {
      sprintf("Consider reducing k (ratio: %.1f%%)", 100 * edf_ratio)
    } else {
      "EDF ratio acceptable"
    }
  )
}
```

**Safe Range**: k=30-40 for trend (300 months / 7-10 ≈ 40-50 max)

**Why First**:
- Safe with validation checks
- Independent of other features
- Allows more flexible long-term patterns

**Tests Needed**:
```r
test_that("k=30 maintains acceptable EDF ratio", {
  model <- fit_seasonal_gam(data, k_trend = 30)
  edf_check <- validate_edf_ratio(model, data, max_ratio = 0.15)
  expect_true(edf_check$acceptable)
})
```

---

### 1.3 COVID Intervention Smooth

**Problem**: COVID-19 pandemic (Mar 2020+) represents structural break not captured by smooth trend

**Solution**: Add smooth intervention variable that is 0 before COVID, increases after

**Files Modified**:
- `R/interventions.R`: New file for intervention utilities
- `R/seasonal-gam.R`: Support intervention parameters

**Implementation Details**:

```r
# R/interventions.R
add_intervention_times <- function(data, interventions = c("covid")) {

  if ("covid" %in% interventions) {
    # Find COVID start (March 2020)
    covid_start_idx <- which(data$year == 2020 & data$month == 3)[1]

    if (!is.na(covid_start_idx)) {
      data$covid_time <- pmax(0, data$time_index - covid_start_idx)
    } else {
      warning("COVID start date not found in data")
      data$covid_time <- 0
    }
  }

  return(data)
}

extract_intervention_effect <- function(model, intervention_name, data) {
  intv_var <- paste0(intervention_name, "_time")

  # Predict with and without intervention
  data_with <- data
  data_without <- data
  data_without[[intv_var]] <- 0

  pred_with <- predict(model, newdata = data_with, type = "response",
                       exclude = NULL)
  pred_without <- predict(model, newdata = data_without, type = "response",
                          exclude = intv_var)

  effect <- pred_with - pred_without

  data.frame(
    date = data$date,
    time_index = data$time_index,
    intervention_effect = effect,
    relative_effect = effect / pred_without
  )
}

# R/seasonal-gam.R
fit_seasonal_gam_with_interventions <- function(data,
                                                 k_month = 12,
                                                 k_trend = 30,
                                                 family = "gaussian",
                                                 interventions = NULL,
                                                 intervention_k = 8) {

  # Add intervention times
  if (!is.null(interventions)) {
    data <- add_intervention_times(data, interventions)
  }

  # Build formula
  base_formula <- sprintf(
    "unemployment_rate ~ s(time_index, bs='cr', k=%d) + s(month, bs='cc', k=%d)",
    k_trend, k_month
  )

  # Add intervention smooths
  if (!is.null(interventions)) {
    for (intv in interventions) {
      intv_var <- paste0(intv, "_time")
      if (intv_var %in% names(data)) {
        base_formula <- paste0(base_formula,
                               sprintf(" + s(%s, bs='cr', k=%d)",
                                       intv_var, intervention_k))
      }
    }
  }

  formula <- as.formula(base_formula)

  model <- mgcv::gam(formula, data = data, family = family, method = "REML")
  attr(model, "interventions") <- interventions

  return(model)
}
```

**Why First**:
- Highest impact on 2024-2025 period variance
- Clear structural break, easy to validate
- Independent smooth term, low interaction risk

**Tests Needed**:
```r
test_that("COVID intervention effect is positive and significant", {
  model <- fit_seasonal_gam_with_interventions(data, interventions = "covid")
  effect <- extract_intervention_effect(model, "covid", data)

  # Effect should be positive after March 2020
  post_covid <- effect[effect$time_index >= covid_start, ]
  expect_true(mean(post_covid$intervention_effect) > 0.005)
})
```

---

## Phase 2: Full Intervention Framework (Week 3-4)

### 2.1 Recession & Additional Interventions

**Problem**: 2008-2009 recession also represents structural break

**Solution**: Extend intervention framework to handle multiple events

**Files Modified**:
- `R/interventions.R`: Add recession, Trump admin interventions
- `R/seasonal-gam.R`: Already supports multiple interventions

**Implementation Details**:

```r
# R/interventions.R - extend add_intervention_times()
add_intervention_times <- function(data,
                                    interventions = c("covid", "recession_2008")) {

  # COVID (existing)
  if ("covid" %in% interventions) {
    covid_start_idx <- which(data$year == 2020 & data$month == 3)[1]
    data$covid_time <- pmax(0, data$time_index - covid_start_idx)
  }

  # 2008-2009 Recession
  if ("recession_2008" %in% interventions) {
    recession_start_idx <- which(data$year == 2007 & data$month == 12)[1]
    data$recession_2008_time <- pmax(0, data$time_index - recession_start_idx)
  }

  # Trump Administration (if needed)
  if ("trump_admin" %in% interventions) {
    trump_start_idx <- which(data$year == 2017 & data$month == 1)[1]
    trump_end_idx <- which(data$year == 2021 & data$month == 1)[1]

    # Active during Trump years, 0 otherwise
    data$trump_admin_time <- ifelse(
      data$time_index >= trump_start_idx & data$time_index < trump_end_idx,
      data$time_index - trump_start_idx,
      0
    )
  }

  return(data)
}
```

**Why Second**:
- Builds on COVID infrastructure
- More complex validation (older data)
- May have smaller effects than COVID

**Tests Needed**:
```r
test_that("multiple interventions are independently identifiable", {
  model <- fit_seasonal_gam_with_interventions(
    data,
    interventions = c("covid", "recession_2008")
  )

  covid_effect <- extract_intervention_effect(model, "covid", data)
  recession_effect <- extract_intervention_effect(model, "recession_2008", data)

  # Effects should be distinct
  expect_false(cor(covid_effect$intervention_effect,
                   recession_effect$intervention_effect) > 0.95)
})
```

---

## Phase 3: Temporal Correlation (Week 5)

### 3.1 AR(1) via Lagged Residuals

**Problem**: Residuals show autocorrelation (Ljung-Box test fails)

**Solution**: Two-step approach with lagged residuals (compatible with binomial)

**Files Modified**:
- `R/seasonal-gam.R`: Add `ar1` parameter and implementation
- `R/model-validation.R`: Add AR(1) validation

**Implementation Details**:

```r
# R/seasonal-gam.R
fit_seasonal_gam_ar1 <- function(data,
                                  k_month = 12,
                                  k_trend = 30,
                                  family = "gaussian",
                                  ar1 = FALSE,
                                  ar1_method = "auto") {

  if (!ar1) {
    # Standard GAM
    model <- mgcv::gam(
      unemployment_rate ~ s(time_index, bs = "cr", k = k_trend) +
                          s(month, bs = "cc", k = k_month),
      data = data,
      family = family,
      method = "REML"
    )
    attr(model, "ar1_method") <- "none"
    return(model)
  }

  # Determine AR(1) method
  if (ar1_method == "auto") {
    ar1_method <- if (family == "gaussian") "gamm" else "lag_residual"
  }

  if (ar1_method == "gamm" && family == "gaussian") {
    # Use gamm() with corAR1 (Gaussian only)
    model <- mgcv::gamm(
      unemployment_rate ~ s(time_index, bs = "cr", k = k_trend) +
                          s(month, bs = "cc", k = k_month),
      data = data,
      correlation = nlme::corAR1(form = ~ time_index)
    )
    attr(model, "ar1_method") <- "gamm"
    attr(model, "ar1_coefficient") <-
      coef(model$lme$modelStruct$corStruct, unconstrained = FALSE)

  } else {
    # Lagged residual approach (works with any family)

    # Step 1: Fit base model
    model_base <- mgcv::gam(
      unemployment_rate ~ s(time_index, bs = "cr", k = k_trend) +
                          s(month, bs = "cc", k = k_month),
      data = data,
      family = family,
      method = "REML"
    )

    # Step 2: Add lagged residual
    data$lag1_resid <- c(NA, residuals(model_base, type = "response")[-nrow(data)])

    model <- mgcv::gam(
      unemployment_rate ~ s(time_index, bs = "cr", k = k_trend) +
                          s(month, bs = "cc", k = k_month) +
                          lag1_resid,
      data = data[!is.na(data$lag1_resid), ],
      family = family,
      method = "REML"
    )

    attr(model, "ar1_method") <- "lag_residual"
    attr(model, "ar1_coefficient") <- coef(model)["lag1_resid"]
  }

  return(model)
}

# R/model-validation.R
validate_ar1_structure <- function(model, data) {
  ar1_method <- attr(model, "ar1_method")

  if (is.null(ar1_method) || ar1_method == "none") {
    # Check if AR(1) needed
    resid <- residuals(model, type = "response")
    acf_result <- acf(resid, lag.max = 12, plot = FALSE)
    lag1_acf <- acf_result$acf[2]

    return(list(
      ar1_used = FALSE,
      lag1_acf = lag1_acf,
      ar1_needed = abs(lag1_acf) > 0.3,
      ljung_box = Box.test(resid, lag = 12, type = "Ljung-Box")
    ))
  }

  # AR(1) was used - check effectiveness
  resid <- residuals(model, type = "response")
  acf_result <- acf(resid, lag.max = 12, plot = FALSE)

  list(
    ar1_used = TRUE,
    ar1_method = ar1_method,
    ar1_coefficient = attr(model, "ar1_coefficient"),
    lag1_acf_after = acf_result$acf[2],
    ljung_box = Box.test(resid, lag = 12, type = "Ljung-Box")
  )
}
```

**Why Last**:
- Most complex feature
- Requires stable base model
- Interacts with all other features

**Tests Needed**:
```r
test_that("AR(1) reduces autocorrelation in residuals", {
  model_no_ar <- fit_seasonal_gam_ar1(data, ar1 = FALSE)
  model_ar <- fit_seasonal_gam_ar1(data, ar1 = TRUE)

  acf_no_ar <- acf(residuals(model_no_ar), plot = FALSE)$acf[2]
  acf_ar <- acf(residuals(model_ar), plot = FALSE)$acf[2]

  expect_true(abs(acf_ar) < abs(acf_no_ar))

  # Ljung-Box should improve
  lb_no_ar <- Box.test(residuals(model_no_ar), lag = 12)$p.value
  lb_ar <- Box.test(residuals(model_ar), lag = 12)$p.value

  expect_true(lb_ar > lb_no_ar)
})
```

---

## Phase 4: Testing & Documentation (Week 6)

### 4.1 Comprehensive Test Suite

**Files Created**:
- `tests/testthat/test-advanced-gam.R`: All feature tests
- `tests/testthat/test-interventions.R`: Intervention-specific tests
- `tests/testthat/test-ar1.R`: AR(1) specific tests

**Test Coverage**:
1. Binomial family respects bounds
2. EDF ratios acceptable with k=30
3. Intervention effects extractable and positive
4. Multiple interventions identifiable
5. AR(1) reduces autocorrelation
6. Combined features work together
7. Parameter recovery on simulated data

---

### 4.2 Update Reports & Documentation

**Files Modified**:
- `reports/factor-smooth/factor-smooth-unemployment-analysis.qmd`: Add comparison sections
- New: `vignettes/advanced-gam-features.Rmd`: Comprehensive guide
- `CLAUDE.md`: Update modeling standards

**Report Additions**:

```r
# Model comparison section
models <- list(
  baseline = fit_seasonal_gam(data, family = "gaussian", k_trend = 20),
  quasibinom = fit_seasonal_gam(data, family = "quasibinomial", k_trend = 20),
  higher_k = fit_seasonal_gam(data, family = "quasibinomial", k_trend = 30),
  covid = fit_seasonal_gam_with_interventions(
    data, family = "quasibinomial", k_trend = 30, interventions = "covid"
  ),
  full = fit_seasonal_gam_with_interventions(
    data, family = "quasibinomial", k_trend = 30,
    interventions = c("covid", "recession_2008")
  )
)

# Compare R², AIC, autocorrelation
comparison <- data.frame(
  model = names(models),
  r_squared = sapply(models, function(m) summary(m)$r.sq),
  deviance_explained = sapply(models, function(m) summary(m)$dev.expl),
  AIC = sapply(models, AIC),
  lag1_acf = sapply(models, function(m) {
    acf(residuals(m), plot = FALSE)$acf[2]
  })
)
```

**Vignette Structure**:
```markdown
# Advanced GAM Features for Unemployment Modeling

## When to Use Each Feature

### Quasi-Binomial Family
- **Use when**: Response is proportion data (unemployment rate)
- **Benefits**: Respects [0,1] bounds, better uncertainty quantification
- **Trade-offs**: Logit link requires inverse transformation for interpretation

### Increased Basis Dimensions
- **Use when**: Residual plots show systematic patterns
- **Benefits**: Captures more complex long-term trends
- **Trade-offs**: Overfitting risk (monitor EDF ratio)

### Intervention Modeling
- **Use when**: Known structural breaks (recessions, pandemics)
- **Benefits**: Explicit effect quantification, better extrapolation
- **Trade-offs**: Requires a priori knowledge of event timing

### AR(1) Correlation
- **Use when**: Ljung-Box test fails (p < 0.05)
- **Benefits**: Valid standard errors, better prediction intervals
- **Trade-offs**: Computational cost, interpretation complexity

## Example Workflow

[Full examples with code]
```

---

## Key Design Decisions

### 1. Quasi-Binomial vs Full Binomial

**Choice**: Start with quasi-binomial

**Rationale**:
- Data is aggregated (monthly rates), not individual-level
- Quasi-binomial handles overdispersion automatically
- Avoids data reformatting (no need for n_unemployed/n_employed counts)
- Still supports AIC model comparison
- Can add full binomial later if needed

### 2. Smooth Interventions vs Indicators

**Choice**: Smooth interventions (s(covid_time))

**Rationale**:
- Captures gradual onset and recovery patterns
- More realistic than step changes
- Allows non-linear effects over time
- Can extract time-varying intervention effects

### 3. Lagged Residuals vs gamm()

**Choice**: Lagged residuals for non-Gaussian, gamm() option for Gaussian

**Rationale**:
- gamm() only supports Gaussian family (fundamental limitation)
- Lagged residual approach works with binomial
- Trade-off: approximation vs exact AR(1)
- Keep both options for flexibility

### 4. Parameter Organization

**Choice**: Add parameters to existing functions (backward compatible)

**Rationale**:
- Single entry point easier to maintain
- Default behavior unchanged (family = "gaussian", ar1 = FALSE)
- Optional features via parameters
- Can add wrapper functions later if needed

---

## Implementation Order Rationale

### Why This Sequence?

1. **Phase 1 first**: Foundation features are independently useful and low-risk
   - Quasi-binomial: Immediate model appropriateness improvement
   - k=30: More flexible fits with EDF safeguards
   - COVID: Highest impact on recent variance

2. **Phase 2 second**: Builds on intervention infrastructure
   - Adding more interventions is incremental
   - Can validate each independently
   - Lower urgency than COVID

3. **Phase 3 third**: Most complex, requires stable base
   - AR(1) interacts with all other features
   - Need good base model first
   - Can assess whether still needed after interventions

4. **Phase 4 last**: Integration and communication
   - All features stabilized
   - Can compare systematically
   - Document actual (not hypothetical) usage

### Validation at Each Step

- After Phase 1: Check R² improvement, bound compliance, EDF ratios
- After Phase 2: Verify intervention effects, check identifiability
- After Phase 3: Confirm Ljung-Box passes, autocorrelation reduced
- After Phase 4: Full parameter recovery tests

---

## Risk Assessment

### Low Risk (Safe to Implement)
✅ Quasi-binomial with weights
✅ k=30 with EDF validation
✅ COVID smooth intervention

### Medium Risk (Careful Validation)
⚠️ Multiple interventions (identifiability)
⚠️ Lagged residual AR(1) (approximation quality)
⚠️ k > 40 (overfitting potential)

### High Risk (Proceed with Caution)
⚠️⚠️ All features combined (parameter explosion)
⚠️⚠️ Trump admin intervention (less clear structural break)
⚠️⚠️ gamm() with binomial (not possible - would need to use glmmTMB or similar)

---

## Backward Compatibility

### Principles
- All new features are **optional**
- Default behavior **unchanged**
- Existing code **continues to work**
- Deprecation warnings for breaking changes

### Examples

```r
# Old code still works
model <- fit_seasonal_gam(data)  # Uses Gaussian, k=20

# New features opt-in
model_advanced <- fit_seasonal_gam(
  data,
  family = "quasibinomial",
  k_trend = 30
)

# Full feature set
model_full <- fit_seasonal_gam_with_interventions(
  data,
  family = "quasibinomial",
  k_trend = 30,
  interventions = c("covid", "recession_2008"),
  ar1 = TRUE
)
```

---

## Expected Diagnostic Improvements

### Baseline (Current)
- R²: ~60%
- Deviance explained: ~65%
- Ljung-Box (lag 12): p < 0.05 (fails)
- Predictions: Some outside [0,1]

### After Phase 1
- R²: ~70%
- Deviance explained: ~75%
- Ljung-Box: p ≈ 0.10 (marginal)
- Predictions: All in [0,1]

### After Phase 2
- R²: ~75%
- Deviance explained: ~80%
- Ljung-Box: p ≈ 0.15 (improved)
- Intervention effects: Quantified

### After Phase 3
- R²: ~75-80%
- Deviance explained: ~80-85%
- Ljung-Box: p > 0.20 (passes)
- Autocorrelation: Near zero at lag 1

---

## Next Steps

1. **Review this plan**: Confirm approach and priorities
2. **Start Phase 1**: Implement quasi-binomial (highest ROI)
3. **Validate incrementally**: Check diagnostics after each feature
4. **Document learnings**: Use `/learn` for insights
5. **Update reports**: Show before/after comparisons

---

## References

### mgcv Documentation
- `?mgcv::gam` - GAM fitting
- `?mgcv::gamm` - GAM with correlation structures
- `?mgcv::family.mgcv` - Family functions
- Wood (2017) - Generalized Additive Models: An Introduction with R (2nd ed)

### Statistical Background
- McCullagh & Nelder (1989) - Generalized Linear Models (quasi-likelihood)
- Venables & Ripley (2002) - MASS (time series in GLMs)
- Wood (2011) - Fast stable restricted maximum likelihood

### Project Files
- `R/seasonal-gam.R` - Current implementation
- `R/model-validation.R` - Validation framework
- `tests/testthat/test-shared-smoothing.R` - Test patterns
- `reports/factor-smooth/factor-smooth-unemployment-analysis.qmd` - Report structure
