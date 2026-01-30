# Model Audit: Shock and Seasonal Effects Capture

**Date:** 2025-12-20
**Model:** Quasi-binomial GAM with education-specific trends and shock dynamics
**Status:** Critical issues identified requiring model restructuring

---

## Executive Summary

The current model has two critical structural problems:

1. **Shock effects are being penalized to near-zero** (EDF ≈ 1 for all shock smooths)
2. **Seasonal effects are under-captured** for some education levels

The root cause is that the flexible main time trends (501 total EDF) absorb ALL time-varying variance before shock terms can capture anything. The shock smooths are effectively fitting straight lines despite having k=40 basis dimensions.

---

## Critical Findings

### Issue 1: Shock Smooths Penalized to Linear

**Observed:**
```
2008-2009 shock smooths (shock=1 only): EDFs = 1, 1, 1, 1, 1, 1, 1
2020 shock smooths (shock=1 only):      EDFs = 1, 1, 1, 1, 1, 1, 1
```

**Expected:** With k=40 and 48 months of crisis data, EDFs should be 10-30 to capture nonlinear dynamics.

**Impact:** Shock curves appear flat because they can only fit straight lines.

### Issue 2: Time Trends Absorb Shock Variance

**Observed:**
```
Main time trend EDFs by education:
  bachelors:    93.0
  high_school:  110.5
  less_than_hs: 35.9
  masters:      72.7
  phd:          45.3
  professional: 44.3
  some_college: 99.3
  --------------------------------
  TOTAL:        501.0 EDF
```

**Problem:** The 501 EDF for time trends captures ALL time-varying signal including the crisis spikes, leaving nothing for shock terms to model.

### Issue 3: Counter-intuitive Shock Coefficients

**Observed:**
```
shock_2008_2009 coefficient: -0.1467 (NEGATIVE!)
shock_2020 coefficient:       1.2443 (positive)
```

**Problem:** The 2008-2009 shock coefficient is negative, suggesting the financial crisis *reduced* unemployment. This is nonsensical and indicates model misspecification.

**Empirical reality:**
```
PhD Unemployment:
  2005 (baseline): 1.61%
  2008-2009:       2.15% (+0.54 pp increase)
  2020:            2.35% (+0.74 pp increase)

High School:
  2005 (baseline): 5.54%
  2008-2009:       8.54% (+3.0 pp increase)
  2020:            9.48% (+3.94 pp increase)
```

### Issue 4: Seasonal Effects Under-captured

**Observed:**
```
Seasonal smooth EDFs:
  s(month) shared:      6.41 (using ~half of k=12)
  s(month):bachelors    6.12
  s(month):high_school  6.99
  s(month):less_than_hs 2.03 (heavily penalized!)
  s(month):masters      4.79
  s(month):phd          2.73 (nearly linear!)
  s(month):professional 2.66
  s(month):some_college 6.49
```

**Problem:** PhD seasonal deviation has EDF = 2.73, meaning it's nearly flat. This misses genuine education-specific hiring cycles (academic calendar effects).

### Issue 5: Marginal Effects Extraction Broken

**Observed:** The `extract_shock_effects()` function fails with error:
```
Error in x$qr: lm object does not have a proper 'qr' component.
```

**Cause:** Using `bam()` with `discrete=TRUE` creates a model object incompatible with standard `predict()` calls.

---

## Root Cause Analysis

### Why Shock Effects Fail

The model structure creates a **variance competition** between components:

```r
formula <- cbind(n_unemployed, n_employed) ~ education +
  shock_2008_2009 + shock_2020 +
  s(time_index, k=150, by=education, bs="tp", id=1) +           # FLEXIBLE (150 df)
  s(time_index, k=40, by=interaction(education, shock), ...)  # PENALIZED
```

1. The education-specific time trends fit with k=150 basis functions
2. These trends can capture ANY time-varying pattern, including crisis spikes
3. REML penalization then shrinks shock smooths because variance is already explained
4. Result: shock smooths collapse to linear (EDF ≈ 1)

### Why Seasonal Effects are Weak

1. The shared smoothing parameter constraint (`id=4`) forces all education groups to similar wiggliness
2. Groups with genuinely different seasonal patterns (PhD with academic cycles) get averaged toward the pooled pattern
3. Low EDF indicates the penalty is too aggressive

---

## Proposed Solutions

### Solution A: Separate Time Trends for Shock vs Non-Shock Periods

**Approach:** Restructure the model to prevent time trends from absorbing shock variance.

```r
# Create period indicator
data$period <- ifelse(data$shock_2008_2009 | data$shock_2020, "crisis", "normal")

formula <- cbind(n_unemployed, n_employed) ~ education +
  # Non-shock period time trends (flexible)
  s(time_index, k=100, by=interaction(education, period=="normal"), bs="tp") +
  # Shock period time trends (separate, can capture crisis dynamics)
  s(time_index, k=40, by=interaction(education, shock_2008_2009), bs="tp") +
  s(time_index, k=40, by=interaction(education, shock_2020), bs="tp") +
  # Seasonal
  s(month, k=12, bs="cc") +
  s(month, k=12, bs="cc", by=education)
```

**Pros:** Clean separation of variance sources
**Cons:** May create discontinuities at period boundaries

### Solution B: Use Unpenalized Shock Smooths

**Approach:** Make shock smooths unpenalized during crisis periods using `fx=TRUE`.

```r
s(time_index, k=20, by=interaction(education, shock_2008_2009), bs="tp", fx=TRUE)
s(time_index, k=20, by=interaction(education, shock_2020), bs="tp", fx=TRUE)
```

**Pros:** Forces model to use available basis functions
**Cons:** May overfit, requires careful k selection

### Solution C: Reduce Time Trend Flexibility

**Approach:** Use smaller k for main time trends to leave variance for shocks.

```r
s(time_index, k=50, by=education, bs="tp", id=1)  # Reduced from k=150
```

**Pros:** Simple change
**Cons:** May underfit long-term trends

### Solution D: Tensor Product for Shock × Time Interaction (Recommended)

**Approach:** Use proper tensor product smooths that model shock effects as deviations from baseline.

```r
formula <- cbind(n_unemployed, n_employed) ~ education +
  # Baseline time trend (reference)
  s(time_index, k=100, by=education, bs="tp") +
  # Shock DEVIATIONS from baseline (centered at zero outside shock periods)
  ti(time_index, shock_2008_2009, k=c(20,1), bs=c("tp","re")) +
  ti(time_index, shock_2020, k=c(20,1), bs=c("tp","re")) +
  # Seasonal
  s(month, k=12, bs="cc") +
  s(month, k=12, bs="cc", by=education)
```

**Pros:** Cleanly separates baseline from shock effects
**Cons:** More complex interpretation

### Solution E: Remove Shock Smooths, Use Difference Model

**Approach:** Fit two models (with/without shock indicators) and compute differences.

```r
# Model 1: Full model with shock indicators as linear terms
model_full <- bam(... ~ education + shock_2008_2009 + shock_2020 +
                  s(time_index, k=150, by=education) + s(month, k=12, bs="cc"))

# Shock effect = prediction(shock=1) - prediction(shock=0)
# at each time point during crisis periods
```

**Pros:** Simple, no penalization issues
**Cons:** Loses nonlinear shock dynamics

### Solution for Seasonal Effects

**Option 1:** Increase seasonal k from 12 to 18
```r
s(month, k=18, bs="cc") +
s(month, k=18, bs="cc", by=education)
```

**Option 2:** Remove id constraint for education-specific seasonals
```r
s(month, k=12, bs="cc") +
s(month, k=12, bs="cc", by=education)  # No id=4
```

**Option 3:** Use factor smooth for seasonality
```r
s(month, bs="cc", k=12) +
s(month, education, bs="fs", k=12)  # Factor smooth
```

---

## Recommended Action Plan

### Priority 1: Fix Shock Effects (Critical)

1. **Implement Solution D** (tensor product for shock × time)
2. Remove the `by=interaction(education, shock)` structure
3. Use `ti()` to model shock as deviation from baseline
4. Verify shock smooths have EDF > 5 after refitting

### Priority 2: Fix Seasonal Effects (High)

1. Remove `id=4` constraint from education-specific seasonals
2. Increase k from 12 to 18 for better resolution
3. Verify PhD seasonal has EDF > 5 after refitting

### Priority 3: Fix Marginal Effects Extraction (Medium)

1. Update `extract_shock_effects()` to handle bam() models with discrete=TRUE
2. Use `predict.bam()` explicitly or set `discrete=FALSE` during prediction
3. Add error handling for edge cases

### Priority 4: Update Report Documentation

1. Update formula in report to match actual fitted model
2. Add diagnostic plots showing shock and seasonal EDF
3. Include raw vs. fitted comparison for crisis periods

---

## Validation Criteria

After implementing fixes, the model should pass these tests:

1. **Shock EDF Test:** `EDF(shock_smooth) > 5` for all education levels
2. **Shock Coefficient Test:** Both shock coefficients should be positive
3. **Seasonal EDF Test:** `EDF(seasonal_by_education) > 3` for all levels
4. **Empirical Match Test:** Predicted crisis peaks match observed data within 0.5 pp
5. **Marginal Effects Test:** All 4 plots render without errors

---

## Files to Modify

1. `R/education-binomial-gam.R` - Model formula restructuring
2. `R/marginal-effects.R` - Fix extraction functions for bam()
3. `_targets.R` - Update model specifications
4. `reports/education-binomial-analysis.qmd` - Update formula documentation
5. `tests/testthat/test-education-binomial-gam.R` - Add EDF validation tests
