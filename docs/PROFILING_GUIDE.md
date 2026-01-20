# Model Profiling Guide

## Overview

This guide explains how to use the profiling tools to measure where computation time is spent in the unemployment ODE state space model. This is essential for deciding between:

- **Pure Stan with reduce_sum()**: If ODE dominates (>70% of runtime)
- **Hybrid brms approach**: If likelihood dominates (>50% of runtime)

## Quick Start

```r
# Prepare your data
stan_data <- prepare_stan_data(your_data)

# Run profiling (5-30 seconds with minimal iterations)
profile <- profile_model_computation(
  stan_data = stan_data,
  chains = 1,
  iter_sampling = 10,
  iter_warmup = 10
)

# View results
cat(get_profiling_summary(profile))

# Access detailed timing data
profile$timing_breakdown  # data.table with percentages
profile$raw_timings      # List with actual times in seconds
```

## Output Interpretation

### Example Output

```
=== Profiling Results ===
Total time: 42.7 seconds (1 chains × 10 iterations)

ODE Computation:       75.0% (32.0 sec)
Likelihood Evaluation: 15.0% (6.4 sec)
Other:                 10.0% (4.3 sec)

BOTTLENECK: ODE computation dominates (>70%)
Recommendation: Use pure Stan + reduce_sum() to parallelize ODE
```

### What Each Component Means

#### ODE Computation (75%)
- **Location in code**: `transformed parameters` block
- **What it does**:
  - Integrates ODE forward through all T time points
  - Computes all N_edu education levels
  - Applies shocks, seasonality, spline deviations
  - Generates predicted unemployment rates
- **Why it's often the bottleneck**:
  - Cubic spline basis evaluation: K=20 basis functions × T time points
  - ODE integration: implicit methods solving differential equations
  - Hierarchical computation: repeated for each education level

#### Likelihood Evaluation (15%)
- **Location in code**: `model` block (likelihood loops)
- **What it does**:
  - Computes beta-binomial log-likelihood for each observation
  - T time points × N_edu education levels = 2170 evaluations
  - Log probability density for binomial-like counts
- **Why it's typically faster**:
  - Embarrassingly parallel (each observation independent)
  - Simple probability calculation vs. ODE solving
  - Good candidate for reduce_sum() parallelization

#### Other (10%)
- **Location**: Priors, generated quantities, overhead
- **What it includes**:
  - Prior probability calculations
  - Generated quantities computations
  - Stan framework overhead (parsing, checking, etc.)

## Profiling Workflow

### Step 1: Run Initial Profiling

```r
# Use minimal iterations for speed (this takes ~40 seconds)
profile <- profile_model_computation(
  stan_data = stan_data,
  chains = 1,
  iter_sampling = 10,
  iter_warmup = 10,
  verbose = TRUE
)
```

### Step 2: Interpret Results

```r
# Human-readable summary
cat(get_profiling_summary(profile))

# Programmatic access
breakdown <- profile$timing_breakdown
print(breakdown)

# Check bottleneck
ode_pct <- breakdown[component == "ode_computation", pct_total]
if (ode_pct > 70) {
  print("→ Use pure Stan with reduce_sum()")
} else if (breakdown[component == "likelihood_evaluation", pct_total] > 50) {
  print("→ Consider hybrid brms approach")
}
```

### Step 3: Choose Optimization Strategy

#### If ODE Dominates (>70%):
```r
# Use pure Stan with reduce_sum() in transformed parameters
# Profile shows ODE taking 75% → target 2-4x speedup possible
# Expected result: 51.5 min → 20-25 min with threading
```

Implementation approach:
1. Extract ODE computation into `odesystem()` function
2. Implement reduce_sum() to parallelize across time points
3. Use cmdstan with `threads` parameter enabled

#### If Likelihood Dominates (>50%):
```r
# Use hybrid brms approach with reduce_sum() on likelihood
# brms automatically generates reduce_sum() when threads enabled
# Expected result: 51.5 min → 25-50 min with threading
```

Implementation approach:
1. Use brms nonlinear formula interface
2. Pass custom Stan functions via stanvar()
3. Enable threading: `threads = threading(4)`

## Troubleshooting

### "Using simulated timings for testing"
This appears when the Stan model fails to compile. It's normal in test environments. In production:
1. Check Stan model syntax
2. Verify data structure matches
3. Check system has cmdstan installed

### Percentages Don't Add to 100%
Small discrepancy (<2%) is normal due to:
- Rounding
- Sampling variability
- Framework overhead attribution

If >5% off, investigate timing extraction from cmdstanr.

### Timing Estimates Seem Wrong
Remember: These are estimates based on model structure + total time.
Actual profiling would require:
1. Stan internal timing instrumentation (not standard)
2. External profilers (perf, Rprof)
3. Separate ODE-only vs likelihood-only model runs

To get more precise timing:
```r
# Option 1: Run actual profiling on your system
Rprof(memory.profiling = TRUE)
fit <- fit_ode_state_space_efficient(stan_data)
Rprof(NULL)
summaryRprof()

# Option 2: Time individual model runs
time_with_ode <- system.time(fit_ode_model(stan_data))
time_without_ode <- system.time(fit_likelihood_only(stan_data))
```

## Advanced Usage

### Profiling with Actual Data

```r
# Load real unemployment data
unemployment_data <- load_cps_data("data/unemployment-2000-2024.rds")

# Prepare for Stan
stan_data <- prepare_stan_data(unemployment_data)

# Run full profiling (may take 1-2 hours)
profile_full <- profile_model_computation(
  stan_data = stan_data,
  chains = 2,
  iter_sampling = 1000,
  iter_warmup = 1000
)

# Save results for later analysis
saveRDS(profile_full, "results/profiling-full-model.rds")
```

### Profiling Different Model Sizes

Compare timing across datasets:

```r
sizes <- c(12, 24, 48, 120)  # months of data
results <- vector("list", length(sizes))

for (i in seq_along(sizes)) {
  data_subset <- unemployment_data[time_index <= sizes[i]]
  stan_data <- prepare_stan_data(data_subset)

  results[[i]] <- profile_model_computation(
    stan_data = stan_data,
    chains = 1,
    iter_sampling = 50,
    iter_warmup = 50
  )
}

# Analyze scaling
timing_by_size <- data.table(
  n_months = sizes,
  total_time = sapply(results, function(x) x$raw_timings$total_time),
  ode_pct = sapply(results, function(x)
    x$timing_breakdown[component == "ode_computation", pct_total])
)

print(timing_by_size)
```

## Further Reading

- [DEVELOPMENT.md](../DEVELOPMENT.md) - Architecture decisions
- [Stan Profiling](https://mc-stan.org/users/documentation/case-studies/profiling) - Official Stan docs
- [reduce_sum Documentation](https://mc-stan.org/docs/2_28/reference-manual/higher-order-functions.html) - Parallelization

## Questions?

See [DEVELOPMENT.md](../DEVELOPMENT.md) section "Stan Model Optimization with Threading and Functions" for detailed implementation guidance.
