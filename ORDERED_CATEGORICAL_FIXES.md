# Ordered Categorical Model - Issues and Fixes

**Date**: 2026-02-10
**Purpose**: Document specification issues and fixes before refitting with higher adapt_delta

## Summary of Issues

### 1. ✅ Scale Parameters (No Change Needed)

**Current Specification**:
```stan
real sigma_log_sigma_spline;  // Can be positive or negative
// ...
sigma_log_sigma_spline ~ normal(0, 0.5);
```

**Analysis**:
- Allowing negative sigma is INTENTIONAL - permits trends to go either direction
- Prior `normal(0, 0.5)` is weakly informative and centered at 0
- ✅ This is correct as-is

### 2. ❌ Initialization Not Centered at Prior Means

**Current Initialization** (R/ode-state-space.R:1908-1924):
```r
theta_log_sigma_spline_raw <- sort(rnorm(N_edu, 0, 0.1))
# ...
mu_log_sigma_spline = rnorm(1, -0.22, 0.1),
sigma_log_sigma_spline = rnorm(1, 0, 0.1),  # Should be exactly 0!
```

**Problems**:
- Hierarchical means init with noise around prior mean (should be exact)
- Scale parameters init near 0 with noise (should be exactly 0)
- Ordered vectors use ad-hoc sorted normals (should be evenly spaced)

**Fix**: Use exact prior means (like monotonic spline model does):
```r
mu_log_sigma_spline = -0.22,  # Exact prior mean
sigma_log_sigma_spline = 0,   # Exact prior mean (centered)
theta_log_sigma_spline_raw = seq(-1, 1, length.out = N_edu)  # Evenly spaced
```

### 3. ⚠️ Prior on Ordered Vectors (Acceptable but Could Be Better)

**Current Specification** (stan file, lines 398-405):
```stan
theta_log_sigma_spline_raw ~ std_normal();
theta_logit_u_eq_raw ~ std_normal();
// ... etc for all theta_* parameters
```

**Analysis**:
- This is **technically valid** but creates implicit preference for larger spreads
- The `ordered[N]` constraint truncates the distribution
- Better: explicit prior on differences (random walk)

**Alternative (optional improvement)**:
```stan
// Option 1: Random walk prior (more explicit control)
theta_raw[1] ~ normal(0, 1);
theta_raw[2:N_edu] - theta_raw[1:(N_edu-1)] ~ exponential(1);

// Option 2: Stick with std_normal (simpler, current approach)
theta_raw ~ std_normal();  // Current approach - acceptable
```

**Decision**: Keep current approach for now (simpler), monitor diagnostics

### 4. ✅ Hierarchical Mean Priors (Already Good)

**Current Specification** (stan file, lines 417-437):
```stan
mu_logit_u_eq ~ normal(-3.3, 0.15);   // Tight, weakly informative
mu_log_adj_speed ~ normal(2.3, 0.25);  // Tight, weakly informative
mu_log_shock_2008 ~ normal(-2, 0.8);
mu_log_shock_2020 ~ normal(-1.5, 0.8);
mu_decay_2008 ~ normal(0, 0.5);
mu_decay_2020 ~ normal(0, 0.5);
mu_log_sigma_spline ~ normal(-0.22, 0.4);
```

**Analysis**: ✅ All weakly informative and reasonable

## Required Fixes

### Fix 1: Update Initialization Function

**File**: `R/ode-state-space.R`, function `make_init_at_prior_ordered_categorical`

**Changes**:
1. Initialize all `mu_*` parameters at exact prior means (no noise)
2. Initialize all `sigma_*` parameters at exactly 0 (prior center)
3. Initialize ordered vectors with evenly spaced sequence

### Fix 2: Increase Sampler Settings

After fixing initialization, refit with:
- `adapt_delta = 0.98` (was 0.95)
- `max_treedepth = 14` (was 12)

## Implementation

See updated R function below.
