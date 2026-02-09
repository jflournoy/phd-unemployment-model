# Model Refitting Plan for LOO-CV Analysis

## Current Status
- ✅ Education-trend Stan model implemented
- ✅ R functions for data preparation, fitting, and initialization
- ✅ Targets pipeline with LOO-CV comparison targets
- ✅ Stan model computes `log_lik` in generated quantities
- ✅ R function extracts `log_lik` from Stan fit
- ❌ Existing model fits don't have `log_lik` matrices
- ❌ Education-trend model run was incomplete (2 of 4 chains)

## Refitting Strategy

### Option 1: Quick Test (Recommended First Step)
Run minimal iterations to verify the complete pipeline works:
- Chains: 2
- Warmup: 100 iterations
- Sampling: 100 iterations
- Threads: 1 per chain (simpler debugging)
- Expected runtime: ~10-20 minutes

**Purpose**: Verify that:
1. Models compile and run without errors
2. `log_lik` matrices are properly extracted and stored
3. LOO-CV computation works
4. Model comparison produces valid results

### Option 2: Overnight Full Refit
Once quick test passes, run full models:
- **Edu-parallel model**: 4 chains, 1500 warmup + 1500 sampling, 7 threads/chain
- **Education-trend model**: 4 chains, 1500 warmup + 1500 sampling, 7 threads/chain
- **Expected runtime**: ~7.5 hours total
- **When to run**: Overnight or when computer won't be used

## Step-by-Step Plan

### Phase 1: Quick Test Verification
1. **Test education-trend model** with minimal iterations
   ```r
   quick_fit <- fit_ode_state_space_education_trend(
     education_counts,
     chains = 2,
     iter_sampling = 100,
     iter_warmup = 100,
     parallel_chains = 1,
     threads_per_chain = 1
   )
   ```

2. **Verify `log_lik` extraction**
   ```r
   !is.null(quick_fit$log_lik)  # Should be TRUE
   dim(quick_fit$log_lik)       # Should be [iterations × observations]
   ```

3. **Test LOO-CV computation**
   ```r
   quick_loo <- compute_loo(quick_fit)
   print(quick_loo$estimates)
   ```

### Phase 2: Full Model Refitting
1. **Refit edu-parallel model** (using targets pipeline)
   ```r
   library(targets)
   tar_make('model_ode_state_space_edu_parallel')
   ```

2. **Refit education-trend model**
   ```r
   tar_make('model_ode_state_space_education_trend')
   ```

3. **Monitor progress** - check for convergence issues

### Phase 3: LOO-CV Analysis
1. **Compute LOO-CV for both models**
   ```r
   tar_make(c('loo_edu_parallel', 'loo_education_trend'))
   ```

2. **Compare models**
   ```r
   tar_make('loo_comparison')
   comparison <- tar_read('loo_comparison')
   print(comparison$loo_compare)
   ```

### Phase 4: Trend Analysis
1. **Extract beta parameters** (education trends)
2. **Compute credible intervals** for each trend
3. **Visualize trends** with education rank on x-axis
4. **Interpret scientific findings**

## Quality Checks

### Before Refitting
- [ ] Verify data preparation is identical for both models
- [ ] Check Stan model files have valid syntax
- [ ] Ensure `log_lik` is computed in generated quantities
- [ ] Verify R functions properly extract `log_lik`

### During Refitting
- [ ] Monitor for divergent transitions
- [ ] Check Rhat < 1.05 for key parameters
- [ ] Verify ESS > 400 for key parameters
- [ ] Ensure no chains get stuck

### After Refitting
- [ ] Confirm `log_lik` matrices exist in both model fits
- [ ] Check dimensions match expectations
- [ ] Verify LOO-CV computation succeeds
- [ ] Validate model comparison results

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

## Expected Outputs

1. **Refitted model files** with `log_lik` matrices
2. **LOO-CV comparison results** (elpd_diff, SE, model ranking)
3. **Education trend estimates** with credible intervals
4. **Visualizations** of trends across education levels
5. **Scientific interpretation** of findings

## Timeline Estimate

- **Quick test**: 30 minutes (including verification)
- **Full refitting**: 7.5 hours (overnight)
- **LOO-CV analysis**: 30 minutes
- **Trend analysis and visualization**: 1 hour
- **Report generation**: 1 hour

**Total**: ~10.5 hours (spread over 2 days)

## Next Immediate Action
Run the quick test to verify the pipeline works before committing to full refitting.