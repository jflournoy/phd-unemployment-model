# CPS Survey Weights: Critical Implementation Notes

## Current Implementation Status: ✅ CORRECTLY WEIGHTED

Our unemployment rate calculations properly use CPS person weights:

- **Regular months (Jan, Feb, Apr-Dec)**: `WTFINL`
- **March ASEC supplement**: `ASECWT`

## Why Weights Matter

CPS uses complex survey design with stratification and clustering. Weights are essential for:

1. **Population representativeness**: Sample is not random - certain groups oversampled
2. **Unbiased estimates**: Without weights, estimates are biased
3. **Correct standard errors**: Variance must account for survey design

## Weight Variables in CPS

### WTFINL (Final Person Weight)
- Used for regular monthly CPS samples
- Represents number of persons in civilian noninstitutional population
- Scaled so weighted sample equals population

### ASECWT (ASEC Weight)
- Used for March ASEC supplement only
- Different weighting scheme for income/demographic questions
- **Critical**: March data has `WTFINL = NA`, must use `ASECWT`

## Our Implementation

### Unemployment Rate Calculation

```r
calculate_unemployment_rate <- function(data) {
  # Auto-detects weight variable
  if ("ASECWT" %in% names(data) && any(!is.na(data$ASECWT))) {
    weight_var <- "ASECWT"
  } else {
    weight_var <- "WTFINL"
  }

  # Properly weighted rate
  unemployment_rate = sum(weights[unemployed]) / sum(weights[in_labor_force])
}
```

**Result**: Population-representative unemployment rates, not sample proportions.

## Important for Future Statistical Modeling

### Stan/brms Models

When implementing Bayesian models, we need to account for:

1. **Survey design effects**:
   - Complex standard errors
   - Design effects (deff) typically 1.5-3.0 for CPS
   - Clustered observations

2. **Weight incorporation options**:

   **Option A: Pre-aggregate to weighted estimates** (RECOMMENDED for this project)
   ```r
   # Current approach - already doing this
   monthly_rates <- calculate_monthly_unemployment(phd_data)
   # Now monthly_rates contains weighted estimates
   # Model these estimates directly
   ```

   **Option B: Individual-level modeling with weights**
   ```stan
   // Would need to incorporate weights in likelihood
   // More complex, may not be necessary for our purposes
   for (i in 1:N) {
     target += weights[i] * bernoulli_lpmf(unemployed[i] | theta);
   }
   ```

3. **Standard errors**:
   - Our monthly estimates have uncertainty from sampling
   - Larger PhD sample sizes (~1,700/month) → smaller SEs
   - March ASEC has larger sample (~2,200) → smaller SE
   - Stan models will naturally quantify uncertainty in trend/seasonal components

### Survey Package Integration (if needed)

If we need design-based standard errors for comparison:

```r
library(survey)

# Define survey design
cps_design <- svydesign(
  ids = ~1,           # No cluster variable available in public CPS
  weights = ~WTFINL,  # Or ~ASECWT for March
  data = phd_data
)

# Get design-based unemployment rate
svymean(~unemployed, cps_design)  # Includes correct SE
```

## Validation

### Check our weighted estimates match BLS

For validation, we could compare our weighted PhD unemployment rates against:
- BLS published unemployment rates for doctorate holders
- Known unemployment patterns by education level

### Weight distribution checks

```r
# Weights should be positive
all(phd_data$WTFINL > 0, na.rm = TRUE)  # ✓

# Weights should sum to approximate population
sum(phd_data$WTFINL)  # Should be ~millions (US population size)

# Weight variability (design effect indicator)
sd(phd_data$WTFINL) / mean(phd_data$WTFINL)  # CV of weights
```

## Key Takeaway for Statistical Modeling

✅ **Our current pipeline already produces correctly weighted estimates**

When we build Stan/brms models for:
- Seasonal adjustment
- Trend decomposition
- Forecasting
- Comparisons across groups

We will model the **weighted monthly unemployment rates** from `calculate_monthly_unemployment()`, not individual observations.

This is appropriate because:
1. We've already correctly aggregated with weights
2. Monthly rates are our actual data points of interest
3. Reduces computational burden (12-312 observations vs. millions)
4. Stan excels at modeling time series of aggregated rates

## References

- [IPUMS CPS Weight Documentation](https://cps.ipums.org/cps/weights.shtml)
- [CPS Survey Design](https://www.census.gov/programs-surveys/cps/technical-documentation/methodology.html)
- Lumley, T. (2010). *Complex Surveys: A Guide to Analysis Using R*
