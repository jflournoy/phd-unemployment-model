# Parameter Recovery Report Generator

You are helping create a parameter recovery validation report. Follow these steps:

## Step 1: Identify the Model Family

Ask the user:
- What model family are they validating? (Gaussian GAM, Binomial GAM, etc.)
- What parameters do they want to recover?

**CRITICAL CHECKS**:
- ❌ **STOP** if model family is quasi-binomial, quasi-poisson, or any "quasi-" family
  - These are variance adjustment methods, NOT generative models
  - Cannot do parameter recovery for quasi-families
  - Suggest robustness testing instead (see Section 8 of report-evaluation.md)

- ❌ **STOP** if they want to "recover dispersion from beta-binomial data using quasi-binomial"
  - Beta-binomial ≠ quasi-binomial (different dispersion measures)
  - This is model misspecification testing, not parameter recovery
  - Use quasi-binomial validation template instead

## Step 2: Design the Data Generating Process (DGP)

For valid parameter recovery, the DGP must:
1. **Match the model family exactly**
   - Gaussian GAM → Generate from Gaussian with smooth mean
   - Binomial GAM → Generate from Binomial with smooth probability
   - Negative Binomial GAM → Generate from NegBin with smooth mean

2. **Have known, recoverable parameters**
   - Smooth functions (trend, seasonality) with specified basis
   - Variance/dispersion parameters
   - Random effects (if applicable)

3. **Example for Gaussian GAM**:
```r
generate_gaussian_gam_data <- function(n_months = 60,
                                        baseline = 0.05,
                                        trend_slope = 0.0003,
                                        seasonal_amp = 0.01,
                                        sigma = 0.01) {
  time_index <- 1:n_months
  month <- rep(1:12, length.out = n_months)

  # True smooth mean (what we want to recover)
  true_mean <- baseline + trend_slope * time_index +
    seasonal_amp * sin(2 * pi * month / 12)

  # Generate Gaussian data
  y <- rnorm(n_months, mean = true_mean, sd = sigma)

  data.frame(
    time_index = time_index,
    month = month,
    y = y,
    true_mean = true_mean,  # For coverage checking
    true_sigma = rep(sigma, n_months)
  )
}
```

## Step 3: Fit the Model

The model family must match the DGP:
```r
# Fit GAM with same structure as DGP
model <- gam(y ~ s(time_index, k = 8) + s(month, bs = "cc", k = 6),
             family = gaussian(),  # Matches DGP
             data = train_data,
             method = "REML")
```

## Step 4: Test Coverage (NOT Point Estimates!)

**CRITICAL**: Test interval coverage, not bias/RMSE

```r
n_sims <- 200
coverage_results <- numeric(n_sims)

for (i in 1:n_sims) {
  # Generate new dataset
  sim_data <- generate_gaussian_gam_data()

  # Fit model
  model <- gam(y ~ s(time_index, k = 8) + s(month, bs = "cc", k = 6),
               family = gaussian(),
               data = sim_data,
               method = "REML")

  # Get predictions with CIs
  pred <- predict(model, type = "link", se.fit = TRUE)

  # Check coverage for EACH observation
  ci_lower <- pred$fit - 1.96 * pred$se.fit
  ci_upper <- pred$fit + 1.96 * pred$se.fit

  coverage_vector <- (ci_lower <= sim_data$true_mean) &
                     (sim_data$true_mean <= ci_upper)

  # Coverage rate for this simulation
  coverage_results[i] <- mean(coverage_vector)
}

# Should be ~0.95 across simulations
mean(coverage_results)
```

## Step 5: Test Differences (If Applicable)

If comparing groups (e.g., PhD vs HS unemployment):

```r
# Test coverage of DIFFERENCES, not individual groups
diff_phd_hs <- predict(model, newdata = phd_data) -
               predict(model, newdata = hs_data)

# CI for difference (accounting for covariance!)
V <- vcov(model)
se_diff <- sqrt(V[1,1] + V[2,2] - 2*V[1,2])

ci_diff <- diff_phd_hs + c(-1.96, 1.96) * se_diff

# Check coverage
covered <- (ci_diff[1] <= true_diff) & (true_diff <= ci_diff[2])
```

## Step 6: Report Structure

Create report with:

1. **Executive Summary**
   - What model family (must match DGP!)
   - What parameters being recovered
   - Coverage results (target: 93-97%)

2. **Data Generation**
   - Document exact DGP
   - Show parameter values
   - Explain why this DGP matches the model

3. **Coverage Tests**
   - Observation-level coverage (not global mean!)
   - Multiple simulations (≥200)
   - Coverage plots by parameter

4. **Conclusions**
   - Did model recover parameters? (coverage ≈ 95%)
   - If not, diagnose: wrong k? wrong family? model misspecification?

## Common Mistakes to Avoid

❌ **Testing quasi-families** - No generative model!
❌ **DGP ≠ model family** - Not parameter recovery, it's robustness testing
❌ **Global mean coverage** - Too lenient, test observation-level
❌ **Testing bias instead of coverage** - Coverage is what matters
❌ **Comparing CI to PI** - Know which you're testing

## When NOT to Do Parameter Recovery

Use robustness testing instead if:
- Model family is quasi-* (no generative model)
- Testing model misspecification (DGP ≠ model)
- Exploring model behavior under violations
- Comparing model families on same data

For these cases, see:
- `docs/report-evaluation.md` Section 8 (quasi-binomial)
- `reports/quasi-binomial-validation/` (example robustness test)

---

Now ask the user to confirm:
1. What model family?
2. What parameters to recover?
3. Confirm DGP matches model family

Then generate the appropriate template!
