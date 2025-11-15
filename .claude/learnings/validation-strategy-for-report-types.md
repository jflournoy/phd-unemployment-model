# Validation Strategy for Different Report Types

**Date**: 2025-11-15
**Context**: Determining appropriate statistical validation checks for exploratory/modeling reports vs parameter recovery validation reports

## Report Types and Their Validation Needs

### 1. Parameter Recovery Validation Reports

**Purpose**: Validate that a statistical method can recover known parameters from simulated data

**Appropriate Validation Checks**:
- ✅ **Coverage validation**: Do 95% CIs contain true value in ~95% of simulations?
- ✅ **Bias quantification**: Mean estimation error near zero?
- ✅ **False positive control**: Type I error rate ≤5%?
- ✅ **DGP consistency**: Reference data matches validation data-generating process?
- ✅ **Simulation sample size**: Sufficient runs (n≥200 for coverage)?

**Why These Matter**: These checks directly test whether the method works as intended. Failure means the method itself is broken.

**Example**: `reports/factor-smooth/factor-smooth-parameter-recovery.html`

### 2. Exploratory/Modeling Reports on Real Data

**Purpose**: Apply validated methods to answer substantive research questions about real phenomena

**Appropriate Validation Checks**:
- ✅ **Model convergence**: Did optimization succeed?
- ✅ **Model fit diagnostics**: Residual patterns, Q-Q plots, scale-location plots
- ✅ **Concurvity**: Are smooth terms confounded?
- ✅ **EDF (effective degrees of freedom)**: Is model complexity reasonable?
- ✅ **Model comparison**: AIC/BIC for nested models
- ⚠️ **Residual tests**: Use with caution (see below)
- ❌ **Coverage validation**: NOT APPLICABLE (no true values to compare against)
- ❌ **Bias quantification**: NOT APPLICABLE (true parameters unknown)
- ❌ **False positive control via simulation**: NOT APPLICABLE (use actual statistical tests instead)

**Why Coverage/Bias Don't Apply**: In real data analysis, we don't know the "true" unemployment rate trend - that's what we're trying to estimate! These validation checks only make sense when we have ground truth (simulated data).

**Example**: `reports/factor-smooth/factor-smooth-unemployment-analysis.html`

## Specific Validation Check Appropriateness

### Convergence (`validate_convergence`)
- **Exploratory reports**: ✅ CRITICAL - Must check
- **Validation reports**: ✅ CRITICAL - Must check
- **Rationale**: Non-convergence means results are unreliable in any context

### Residual Diagnostics (`validate_residuals`)

#### Normality Test (Shapiro-Wilk)
- **Exploratory reports**: ⚠️ USE WITH CAUTION
- **Validation reports**: ⚠️ USE WITH CAUTION
- **Issues**:
  - GAMs don't require normal residuals, only independence
  - With large samples (n>5000), test rejects even trivial deviations
  - Visual Q-Q plots more informative than p-values
- **Recommendation**: Report Q-Q plot, don't fail validation on Shapiro p-value alone

#### Autocorrelation Test (Ljung-Box)
- **Exploratory reports (time series data)**: ✅ IMPORTANT
- **Validation reports**: ✅ IMPORTANT
- **Rationale**: Time series GAMs should capture temporal correlation; residual autocorrelation suggests model misspecification
- **Action if fails**: Consider adding AR terms, using `gamm()`, or temporal smooth

#### Heteroscedasticity Test (Breusch-Pagan)
- **Exploratory reports**: ⚠️ INFORMATIVE but not necessarily a failure
- **Validation reports**: ⚠️ INFORMATIVE
- **Issues**:
  - Some heteroscedasticity is common in real data (unemployment varies more during recessions)
  - GAMs are robust to moderate heteroscedasticity
  - Can address with variance modeling or robust SEs
- **Recommendation**: Report it, consider addressing if severe, but don't automatically fail validation

### Concurvity (`validate_concurvity`)
- **Exploratory reports**: ✅ IMPORTANT
- **Validation reports**: ✅ IMPORTANT
- **Rationale**: High concurvity means smooths are confounded, estimates unreliable
- **Threshold**: Worry if >0.8 (same as multicollinearity VIF>10)

### EDF Validation (`validate_edf`)
- **Exploratory reports**: ✅ IMPORTANT
- **Validation reports**: ✅ IMPORTANT
- **Rationale**: EDF ≈ k (basis dimension) suggests smooth is too constrained; very high EDF suggests overfitting
- **Use**: Diagnostic, not pass/fail criterion

### Model Fit (`validate_model_fit`)
- **Exploratory reports**: ✅ INFORMATIVE
- **Validation reports**: ❌ NOT APPLICABLE (simulations are perfect)
- **Metrics**: R², deviance explained, AIC, BIC, RMSE, MAE
- **Rationale**: Tells us how well model captures patterns, but "good" thresholds are context-dependent

## Recommendations by Report Type

### For Parameter Recovery Validation Reports

**MUST include**:
1. Coverage validation (fit MANY models on NEW data, check CI contains true value)
2. Bias quantification (mean error, SD, RMSE)
3. False positive control (null simulations)
4. DGP consistency documentation
5. Sufficient simulation runs (n≥200)

**SHOULD include** (model diagnostics):
6. Convergence checks
7. Residual diagnostic plots (Q-Q, residuals vs fitted)
8. Concurvity checks
9. EDF summaries

**DON'T include**:
- Strict pass/fail based on Shapiro-Wilk p-value
- Model fit R² thresholds (simulations can be perfect or deliberately noisy)

### For Exploratory/Modeling Reports on Real Data

**MUST include**:
1. Model convergence verification
2. Residual diagnostic plots (Q-Q, scale-location, residuals vs fitted, ACF)
3. Concurvity assessment
4. EDF summary
5. Model comparison (AIC/BIC for nested models)

**SHOULD include**:
6. Autocorrelation test for time series data
7. Model fit statistics (R², deviance explained, AIC)
8. Heteroscedasticity assessment (report, don't necessarily fail on)
9. Sensitivity analyses (different basis dimensions, smoothing parameters)

**DON'T include**:
- Coverage validation (no true values)
- Bias quantification (no ground truth)
- False positive control via simulation (use actual hypothesis tests instead)
- Strict pass/fail on normality test (use plots, large-sample tests over-reject)

## Current Implementation Issues

### `validate_gam_model()` function

**Problem**: Uses same validation criteria for both report types

**Current behavior**:
```r
if (normality_test$p.value < 0.05) {
  issues <- c(issues, "Non-normal residuals detected")
}
if (autocorr_test$p.value < 0.05) {
  issues <- c(issues, "Significant autocorrelation detected")
}
if (bp_pval < 0.05) {
  issues <- c(issues, "Heteroscedasticity detected")
}
```

**Issues**:
1. **Normality**: With n=9.5M (high school CPS data), Shapiro-Wilk will reject even if residuals are nearly perfect
2. **Autocorrelation**: Good check for time series, but must be interpreted correctly
3. **Heteroscedasticity**: Often present in real data, not necessarily a failure

**Proposed fix**: Add `validation_type` parameter:

```r
validate_gam_model <- function(model, data,
                               validation_type = c("exploratory", "simulation"),
                               verbose = TRUE) {
  validation_type <- match.arg(validation_type)

  # Run diagnostics
  convergence <- validate_convergence(model)
  residuals_check <- validate_residuals(model, data,
                                       type = validation_type)
  concurvity <- validate_concurvity(model)
  edf <- validate_edf(model, data)
  fit <- validate_model_fit(model, data)

  # Different pass/fail criteria by type
  if (validation_type == "exploratory") {
    # More lenient on normality, focus on plots
    # Autocorrelation is important for time series
    # Heteroscedasticity is informative but not fatal
  } else if (validation_type == "simulation") {
    # Stricter convergence requirements
    # Diagnostic plots matter more than tests
  }
}
```

## Summary

**Key Insight**: Validation checks that test "does the method work?" (coverage, bias, false positives) only apply to simulated data where we know ground truth. Diagnostic checks that test "did the model fit correctly?" (convergence, residuals, concurvity) apply to both simulated and real data.

**Action Items**:
1. ✅ Keep current `validate_gam_model()` for model diagnostics
2. ⚠️ Add `validation_type` parameter to adjust pass/fail thresholds
3. ⚠️ For real data: Report Shapiro-Wilk but don't fail on it alone; use Q-Q plots
4. ✅ For time series: Autocorrelation test is important
5. ✅ Document that coverage/bias/false-positive checks are validation-report-only
6. ⚠️ Consider separating model diagnostics from validation-specific checks

**For the current report** (`factor-smooth-unemployment-analysis.html`):
- The model diagnostics (convergence, residuals plots, concurvity, EDF) are **appropriate**
- The strict p-value thresholds might be **too stringent** for real data with n=9.5M observations
- Should interpret diagnostic checks in context, not as hard pass/fail
