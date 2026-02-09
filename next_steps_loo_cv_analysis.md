# Next Steps for LOO-CV Analysis

## Current Status
The education-trend Stan model has been successfully implemented with full targets pipeline integration. However, we cannot perform LOO-CV comparison because:

1. **Existing model fits don't have `log_lik` matrices** - The `compute_loo()` function requires these
2. **Models need to be refitted** - This takes several hours per model
3. **Education-trend model run was incomplete** - Only 2 of 4 chains completed

## Immediate Actions Required

### 1. Refit Models with `log_lik` Extraction
Both models need to be refitted using the updated functions that extract and store `log_lik` matrices.

**Option A: Full refit (recommended for final analysis)**
```r
# Run in separate R sessions or overnight
library(targets)

# 1. Refit edu-parallel model (~99 minutes)
tar_make('model_ode_state_space_edu_parallel')

# 2. Refit education-trend model (~355 minutes)
tar_make('model_ode_state_space_education_trend')
```

**Option B: Quick test with minimal iterations**
```r
# For testing the pipeline only
quick_fit <- fit_ode_state_space_education_trend(
  education_counts,
  chains = 2,
  iter_sampling = 100,
  iter_warmup = 100,
  parallel_chains = 1,
  threads_per_chain = 1
)

# Verify log_lik is stored
!is.null(quick_fit$log_lik)  # Should be TRUE
```

### 2. Verify LOO-CV Pipeline
Once models are refitted, test the complete pipeline:

```r
library(targets)

# 1. Compute LOO-CV for both models
tar_make(c('loo_edu_parallel', 'loo_education_trend'))

# 2. Compare models
tar_make('loo_comparison')

# 3. View results
comparison <- tar_read('loo_comparison')
print(comparison$loo_compare)
print(comparison$summary)
```

## Expected Results

### LOO-CV Comparison Metrics
The `loo_comparison` target will produce:
- `elpd_diff`: Difference in expected log predictive density
- `se_diff`: Standard error of the difference
- Model ranking based on predictive performance

### Interpretation
- **Positive `elpd_diff`**: Education-trend model has better predictive performance
- **Negative `elpd_diff`**: Edu-parallel model is better (trends don't help)
- **`|elpd_diff| < 2*se_diff`**: Models are not significantly different

## Analysis Workflow

### Phase 1: Model Comparison
1. **Extract LOO-CV results** from `loo_comparison` target
2. **Compare model fit** using `loo::loo_compare()`
3. **Check Pareto k diagnostics** for problematic observations

### Phase 2: Parameter Analysis
1. **Extract beta_* parameters** (education trends)
2. **Compute credible intervals** for each trend
3. **Visualize trends** with education rank on x-axis

### Phase 3: Scientific Interpretation
1. **Answer key questions**:
   - Does unemployment decrease with education? (beta_logit_u_eq)
   - Do educated workers recover faster? (beta_decay_*)
   - Are educated workers less shock-sensitive? (beta_log_shock_*)
2. **Compare with theoretical expectations**
3. **Discuss policy implications**

## Scripts to Use

### Existing Scripts
- `test_education_trend_quick.R` - Quick model check
- `analyze_existing_models.R` - Model diagnostics
- `run_loo_cv.R` - LOO-CV analysis (if exists)

### Scripts to Create
1. **`extract_education_trends.R`** - Extract and visualize beta parameters
2. **`create_model_comparison_report.R`** - Generate comprehensive report
3. **`run_quick_validation.R`** - Quick test with minimal iterations

## Time Estimates

### Model Fitting
- **Edu-parallel**: ~1.5 hours (5925 seconds historically)
- **Education-trend**: ~6 hours (21291 seconds historically)
- **Total**: ~7.5 hours (overnight run recommended)

### Analysis
- **LOO-CV computation**: ~5-10 minutes per model
- **Trend extraction**: ~15 minutes
- **Visualization**: ~30 minutes
- **Report generation**: ~1 hour

## Quality Checks

### Before Final Analysis
1. [ ] Verify both models have `log_lik` matrices
2. [ ] Check model convergence (Rhat < 1.05, ESS > 400)
3. [ ] Validate no divergent transitions
4. [ ] Confirm identical data preparation for both models

### After LOO-CV
1. [ ] Check Pareto k values (< 0.7 acceptable)
2. [ ] Verify LOO-CV computation succeeded
3. [ ] Validate model comparison results
4. [ ] Cross-check with alternative comparison methods

## Contingency Plans

### If Models Fail to Converge
1. **Increase iterations**: 2000 warmup + 2000 sampling
2. **Adjust adapt_delta**: Increase to 0.99
3. **Simplify model**: Remove non-significant trends
4. **Use stronger priors**: Informative priors for beta parameters

### If LOO-CV Fails
1. **Check `log_lik` dimensions**: Must match observations
2. **Verify Stan model**: `log_lik` must be in generated quantities
3. **Try alternative**: Use WAIC or k-fold CV instead

## Final Outputs

### Required
1. **LOO-CV comparison table** with elpd_diff and SE
2. **Education trend plots** with credible intervals
3. **Model diagnostics** (convergence, Pareto k)

### Recommended
1. **Scientific interpretation** of trends
2. **Model comparison discussion**
3. **Limitations and future work**

## Summary
The education-trend model implementation is complete and ready for analysis. The main remaining task is refitting models to obtain `log_lik` matrices for LOO-CV comparison. Once this is done, the full analysis pipeline will provide insights into how labor market dynamics vary systematically with education level.