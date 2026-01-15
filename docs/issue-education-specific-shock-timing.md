# Feature Request: Education-Specific Shock Peak Timing

**Issue Type**: Enhancement
**Labels**: `enhancement`, `hierarchical-modeling`, `shock-dynamics`, `research-question`
**Created**: 2026-01-15

## Current State

Shock timing is currently **fixed** based on economic history:

```r
shock_2008_onset = 2007.75,  # Subprime crisis begins (Q4 2007)
shock_2008_peak = 2009.50,   # Unemployment peaks (mid-2009)
shock_2020_onset = 2020.17,  # COVID March 2020
shock_2020_peak = 2020.33,   # April 2020 peak
```

While shock **magnitudes** and **recovery rates** are hierarchically estimated across education levels, the **timing** is the same for all groups.

## Proposed Enhancement

Allow **education-specific shock peak timing** for the 2008 financial crisis, while keeping the aggregate timing anchored to economic reality.

### Rationale

1. **Education-specific impacts**: Different industries and sectors were affected at different times during the Great Recession
   - Finance/real estate (hit early): likely more educated workers
   - Manufacturing/construction (hit mid-crisis): likely less educated workers
   - Services (more resilient): mixed educational composition

2. **Labor market frictions**: PhDs in academia may have faced delayed impacts compared to construction workers

3. **Data-driven timing**: Let unemployment patterns identify optimal shock timing for each education level

### Implementation Approach

**For 2008 shock** (more uncertainty in timing):
```stan
parameters {
  // Population-level peak (anchored to economic history)
  real<lower=2008.5, upper=2010.0> shock_2008_peak_mean;

  // Education-specific offsets with tight hierarchical prior
  real<lower=0> sigma_peak_offset;  // Between-education SD
  vector[N_edu] peak_offset_raw;    // Non-centered deviations
}

transformed parameters {
  // Education-specific peak timing: within ±3-6 months of aggregate peak
  vector[N_edu] shock_2008_peak;
  for (i in 1:N_edu) {
    shock_2008_peak[i] = shock_2008_peak_mean + sigma_peak_offset * peak_offset_raw[i];
  }
}

model {
  shock_2008_peak_mean ~ normal(2009.5, 0.25);  // Anchor to mid-2009
  sigma_peak_offset ~ exponential(4);            // Small variation (mean=0.25 years = 3 months)
  peak_offset_raw ~ std_normal();                // Non-centered deviations
}
```

**For 2020 shock** (timing well-known):
- Keep **fixed** at March 2020 onset, April 2020 peak
- COVID shutdowns were simultaneous across all sectors

### Benefits

1. **Better fit**: If timing differs by education, model captures this
2. **Economic insight**: Reveals which groups were hit first/last in recession
3. **Maintained interpretability**: Tight priors keep timing near historical dates
4. **Hierarchical pooling**: Small sample sizes don't lead to wild timing estimates

### Considerations

1. **Identification**: Timing vs magnitude trade-off
   - Peak timing and shock magnitude are partially confounded
   - Tight priors on timing offsets help identification

2. **Decay rate confounding**: Earlier peak → longer recovery time observed
   - Need to ensure decay rates remain identifiable

3. **Computational cost**: Adds 3 parameters (mean, sigma, offsets vector)
   - Minimal impact on runtime

4. **Onset timing**: Consider whether to estimate onset timing as well
   - Probably not necessary - onset is better documented than peak

## Questions to Resolve

1. Should we estimate peak timing only, or both onset and peak?
   - **Recommendation**: Peak only (onset better documented)

2. What prior SD for peak offsets? (σ_peak_offset)
   - **Option 1**: exponential(4) → mean 3 months variation
   - **Option 2**: exponential(2) → mean 6 months variation

3. Should shock_2008_peak_mean be estimated or fixed?
   - **Option A**: Estimate with tight prior centered at 2009.5
   - **Option B**: Fix at 2009.5, only estimate education-specific offsets

4. Do we need transformed data changes?
   - Yes: shock timing calculations need to use education-specific peaks
   - Need to modify shock intensity computation in transformed parameters

## Implementation Steps

1. **TDD RED**: Add failing tests for education-specific shock peak parameters
2. **TDD GREEN**:
   - Add parameters to Stan model
   - Update transformed data/parameters for education-specific timing
   - Update R init function
   - Compile and verify convergence
3. **Analysis**: Compare education-specific timing estimates
4. **Documentation**: Update report with findings

## Expected Results

After implementation, we should see:
- Education-specific peak timing estimates (e.g., PhDs peak at 2009.3, less-than-HS peak at 2009.7)
- Posterior for σ_peak_offset reveals whether timing differences are meaningful
- Model comparison (LOO-CV) shows whether flexible timing improves fit

## References

- Labor market dynamics by education: [Hoynes et al. (2012)](https://www.aeaweb.org/articles?id=10.1257/jep.26.3.27) - "Who Suffers During Recessions?"
- Great Recession timeline: NBER recession dating (Dec 2007 - June 2009)
- Current model: `stan/unemployment-ode-state-space-efficient.stan`

## Related Files

- `stan/unemployment-ode-state-space-efficient.stan` - Main Stan model
- `R/ode-state-space.R:84-87` - Where shock timing is currently set
- `tests/testthat/test-stan-model.R` - Add TDD tests here

## Priority

**Medium** - This is a valuable research question but not critical for core functionality. The current fixed-timing model already achieves excellent convergence.

Consider implementing after:
- [ ] Hierarchical spline smoothness (if pursued)
- [ ] Full model validation on real data
- [ ] Initial results writeup

## Notes

- This feature would make a nice contribution to labor economics literature
- Could be part of a "model comparison" section in dissertation/paper
- Consider mentioning in future grant applications (methodological innovation)
