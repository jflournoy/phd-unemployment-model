# Development Roadmap

## Completed Features

### Hierarchical Variance Analysis (✅ Completed)
- **Date**: 2026-01-15
- **Commit**: c606c68
- **Description**: Added comprehensive analysis examining between-education variance (σ) across all 8 hierarchical parameter groups in the Stan unemployment ODE state space model
- **Deliverables**:
  - `scripts/analyze-hierarchical-variance.R` - Analysis script with 413 lines covering all hierarchical parameters
  - Updated `reports/state-space-comparison.qmd` - Integrated variance analysis with visualizations
  - Generated figures: `hierarchical-variance-comparison.png`, `education-specific-parameters.png`
  - Saved results: `results/hierarchical-variance-summary.rds`

**Key Findings**:
- Most education-specific variation: 2020 decay rate (σ = 5.503) - weak pooling
- Least education-specific variation: Seasonal effects (σ = 0.051) - very tight pooling
- Parameters categorized by pooling strength: tight (<0.15), moderate (0.15-0.4), weak (≥0.4)

---

## Next Steps (Backlog)

### 1. Stan Model Optimization with Threading and Functions ⚠️ (Priority: HIGH)
- **Description**: Refactor the ODE state space Stan model to use Stan functions and multi-threaded likelihood evaluation for improved sampling efficiency
- **Approach**: Pure Stan (not brms hybrid) to achieve full reduce_sum() performance benefits
- **Scope**:
  - Extract ODE solver logic into Stan functions (currently inline in `transformed parameters` block)
  - Extract likelihood computation into reusable Stan functions
  - Implement `reduce_sum()` for parallel likelihood evaluation across time points and education levels in model block
  - Optimize hierarchical parameter transformations with functions
  - Enable threading via cmdstan `threads` parameter
- **Benefits**:
  - Faster model sampling through parallelized likelihood computation
  - Improved code maintainability through functions block isolation
  - Better testability (functions can be tested independently)
  - Foundation for scaling to larger datasets and models
  - **Target: 2-4x speedup** (51.5 minutes → 20-25 minutes on 8-core system)
- **Estimated Effort**: High (8-12 hours, requires careful testing)
- **Dependencies**: Completion of hierarchical variance analysis

**Why Pure Stan (Not brms Hybrid)**:
- brms's nonlinear formula interface can't override the model block
- brms's `threads` parameter only provides algorithmic threading (~50% speedup)
- Pure Stan with reduce_sum() + threads gives full performance gains (2-4x speedup)
- Functions block still provides code quality benefits even in pure Stan approach

**Proposed Architecture**:
```stan
// New functions in unemployment-ode-state-space-efficient.stan

// ODE solver wrapper function
vector ode_unemployment_dynamics(
  vector y_init,
  vector u_eq,
  vector adj_speed,
  vector shock_intensity,
  vector decay,
  vector seasonal,
  // ... other parameters
) {
  // Encapsulate ODE solving logic
}

// Likelihood function for time point and education combination
real partial_log_likelihood(
  int start_idx,
  int end_idx,
  data array[] int n_unemployed,
  data array[] int n_total,
  vector u,
  real phi
) {
  // Compute log-likelihood for subset of data (for reduce_sum)
}

// In model block: use reduce_sum for parallelization
target += reduce_sum(partial_log_likelihood, 1, grainsize,
                     n_unemployed, n_total, u, phi);
```

**Testing Strategy**:
- Verify results match current implementation
- Benchmark sampling time improvement
- Check gradient computation correctness
- Validate on 4+ core systems

---

### 2. Decay Rate Deep-Dive Analysis (Priority: HIGH)
- **Description**: Comprehensive analysis of why 2020 decay rate shows the highest education-specific variation
- **Scope**:
  - Extract posterior draws for decay_2020 by education level
  - Visualize posteriors to understand distributional differences
  - Investigate correlation with other parameters
  - Document policy/economic interpretation of education-specific recovery speeds
- **Estimated Effort**: Low-Medium (2-3 hours)

---

### 3. Shock Response Comparison Visualizations (Priority: MEDIUM)
- **Description**: Create detailed comparisons of 2008 vs 2020 shock effects across education levels
- **Scope**:
  - Compare shock magnitude (2008 vs 2020) by education
  - Compare decay speed (how quickly unemployment returned to baseline)
  - Visualize combined shock + decay trajectory for key education groups
- **Estimated Effort**: Medium (3-4 hours)

---

### 4. Model Validation & Sensitivity Analysis (Priority: MEDIUM)
- **Description**: Verify model robustness through prior sensitivity and LOO-CV cross-validation
- **Scope**:
  - Prior predictive checks for hierarchical parameters
  - Posterior predictive checks for overall fit
  - Leave-One-Out Cross-Validation (LOO-CV) by education level
  - Sensitivity analysis: how much do results change with different priors?
- **Estimated Effort**: Medium-High (4-6 hours)

---

### 5. Publication-Ready Write-Up (Priority: MEDIUM)
- **Description**: Prepare manuscript-quality summary of findings for academic presentation
- **Scope**:
  - Abstract summarizing key findings
  - Methods section explaining hierarchical ODE state space model
  - Results section with interpretation of pooling strength patterns
  - Discussion of implications for PhD unemployment understanding
  - High-quality figures optimized for publication
- **Estimated Effort**: Medium (4-5 hours)

---

## Architecture Notes

### Current Stan Model Implementation
- **Language**: Stan (probabilistic programming)
- **File**: `stan/unemployment-ode-state-space-efficient.stan` (~470 lines)
- **Sampling**: Single-threaded likelihood evaluation
- **ODE Logic**: Inline in `transformed parameters` block
- **Likelihood**: Loop-based computation in `model` block
- **Runtime**: ~51.5 minutes for 4 chains × 3000 iterations (2170 data rows)

### Current Limitations
1. **Sequential Likelihood**: No parallelization across time points or education levels
2. **Code Duplication**: ODE solving and likelihood logic not factored into functions
3. **Scalability**: Difficult to extend to larger datasets or more complex models
4. **Maintainability**: Inline logic makes the model harder to read and debug

### Proposed Stan Improvements
1. **Functions Block**: Extract reusable components
   - `ode_unemployment_dynamics()` - Encapsulate ODE system
   - `compute_shock_intensity()` - Calculate exponential decay
   - `partial_log_likelihood()` - Likelihood for data subset (reduce_sum compatible)

2. **Parallelization**: Use `reduce_sum()` for efficiency
   - Partition likelihood computation across cores
   - Typical targets: 100-500 observations per thread

3. **Numerical Stability**: Improved computation patterns
   - Log-space computations where appropriate
   - Better handling of extreme parameter values

4. **Testing**: Validation scripts
   - Compare old vs new model outputs (should match exactly)
   - Benchmark on various core counts
   - Verify gradient computations

---

## Testing Standards
- All new functions must have unit tests in `tests/testthat/`
- Use mock data for rapid iteration, real model output for validation
- Test execution time target: <30 seconds for full suite

---

## Performance Targets

### Stan Model Optimization (Primary Focus)
- **Current sampling time**: 51.5 minutes (4 chains × 3000 iter on 8-core system)
- **Target with threading**: 20-25 minutes (50% reduction)
- **Speedup mechanism**: `reduce_sum()` parallelization across likelihood evaluations
- **Expected benefit**: 2-4x reduction in likelihood computation time

### Analysis and Reporting
- Hierarchical variance analysis runtime: <10 seconds (currently ~15s)
- Memory usage: <500MB for full model analysis
- Report rendering time: <2 minutes including all analyses

### Validation Criteria
- ✓ Output numerically identical to sequential version (diff < 1e-10)
- ✓ Gradient checks pass (numerical gradient matches autodiff)
- ✓ Effective sample size improves or stays constant
- ✓ Rhat remains < 1.01 for all parameters
- ✓ No divergences introduced

---

## Architectural Alternatives

### Alternative: Nonlinear brms Model

**Description**: Instead of a custom Stan ODE state space model, use brms' nonlinear formula interface to specify unemployment dynamics as a formula-based system.

**Potential Approach**:
```r
# Conceptual brms specification
brms::brm(
  n_unemployed | trials(n_total) ~
    # Nonlinear formula for unemployment rate
    (u_eq + shock_2008 * exp(-decay_2008 * t_since_2008) +
     shock_2020 * exp(-decay_2020 * t_since_2020) +
     seasonal + spline_smooth)(education),
  # Beta-binomial family for count data
  family = beta_binomial(),
  # Hierarchical structure via brms syntax
  ...
)
```

**Pros**:
- Easier to understand and modify for non-Stan experts
- Leverages brms' extensive infrastructure (priors, diagnostics, post-processing)
- Familiar tidyverse-like syntax
- Lower barrier to entry for new team members

**Cons**:
- Less efficient for complex dynamics (no ODE solver integration)
- Harder to enforce structural constraints (e.g., ODE consistency)
- Limited control over sampling algorithm compared to raw Stan
- Would require approximating discrete shock dynamics in formula space
- May struggle with identifiability of non-linear shock recovery curves
- Loss of transparency in model internals

**When to Consider**:
- ✗ If performance is critical (we're already 51.5 min → targeting 20-25 min with threading)
- ✓ If expanding team and onboarding is a bottleneck
- ✓ If model is too complex for brms (paradoxically, simpler models may fit brms better)
- ✗ If we need to publish methods paper (custom Stan shows more technical rigor)

**Current Assessment**: The custom Stan approach is more appropriate because:
1. ODE dynamics are fundamental to the model (not incidental)
2. We're already optimizing performance (threading + functions)
3. The model structure is well-documented and understood
4. Beta-binomial likelihood is properly handled in Stan
5. Shock dynamics with exponential decay are core scientific claims

**Recommendation**: Keep the custom Stan approach and focus on threading optimization. Consider brms as a fallback if:
- Stan model becomes unmaintainable
- Team grows and needs higher-level abstraction
- Need to rapidly test many model variants

---

### Hybrid Approach: Custom Stan Functions + brms Nonlinear Formulas

**Description**: Implement the ODE dynamics as custom Stan functions (with threading), then expose them through brms' nonlinear formula interface.

**Architecture**:
```r
# Step 1: Create custom Stan functions file with threading
stanvars <- stanvar(
  block = "functions",
  scode = read_file("stan/unemployment-functions.stan")
)

# Step 2: Use brms nonlinear formula interface
brms::brm(
  n_unemployed | trials(n_total) ~
    unemployment_rate(
      u_eq[education],
      adj_speed[education],
      shock_2008_effect[education],
      shock_2020_effect[education],
      decay_2008[education],
      decay_2020[education],
      seasonal[education, month],
      time_since_2008 = t - shock_2008_peak,
      time_since_2020 = t - shock_2020_peak
    ),
  family = beta_binomial(),
  prior = c(
    prior(normal(-3.3, 0.3), class = "b", nlpar = "u_eq"),
    # ... other priors
  ),
  stanvars = stanvars,
  cores = 4,
  threads = threading(2)  # Threads per chain
)
```

**Stan Functions File** (`stan/unemployment-functions.stan`):
```stan
functions {
  // Encapsulated ODE dynamics with reduce_sum parallelization
  real unemployment_rate(
    real u_eq,
    real adj_speed,
    real shock_2008_effect,
    real shock_2020_effect,
    real decay_2008,
    real decay_2020,
    real seasonal_effect,
    real t_since_2008,
    real t_since_2020
  ) {
    // ODE + shocks + seasonal
    // ...implementation...
  }

  // Partial likelihood for reduce_sum (parallelized)
  real partial_log_likelihood(
    int start_idx, int end_idx,
    data array[] int n_unemployed,
    data array[] int n_total,
    vector u,
    real phi
  ) {
    // Subset of likelihood computation
  }
}
```

**Pros**:
- ✓ Combines ODE efficiency with brms usability
- ✓ Custom functions with threading optimization
- ✓ brms handles prior specification, diagnostics, post-processing
- ✓ Cleaner separation between model logic (functions) and specification (formula)
- ✓ Easier to test functions independently
- ✓ Scales well if team grows
- ✓ brms' posterior processing tools work seamlessly

**Cons**:
- Intermediate complexity (more moving parts than standalone Stan)
- Requires brms >= 2.14.0 for reduce_sum threading support (cmdstanr backend)
- **With nonlinear models, threading only parallelizes likelihood computation, NOT the predictor computation**
  - If ODE computation (predictor) dominates runtime, threading benefit is limited
  - Pure Stan can parallelize both predictor and likelihood
- Debugging can be trickier (brms-generated Stan code + custom functions)
- Less direct control over exact Stan code generation
- May need workarounds for advanced features (LOO-CV, custom generated quantities)

**Threading Capability Clarified**:
```
With brms + custom functions (via threads = threading()):
├─ threads parameter → Generates reduce_sum() for likelihood (DATA-level parallelization! ✓)
├─ Parallelizes: Likelihood evaluation (beta-binomial loop)
└─ Does NOT parallelize: ODE computation for unemployment_rate() predictor

With pure Stan + reduce_sum():
├─ threads parameter → Algorithmic threading + reduce_sum()
├─ Parallelizes: Both likelihood AND user-designed predictor computation
└─ Full flexibility to parallelize ODE computation if it's the bottleneck
```

**Performance Implication**:
- If ODE computation is ~80% of runtime and likelihood is ~20%:
  - brms threading: ~50% speedup (parallelizes 20% of work)
  - Pure Stan reduce_sum: up to ~4x speedup (can parallelize the ODE)
- If ODE computation is ~50% and likelihood is ~50%:
  - brms threading: ~2x speedup (parallelizes 50% of work)
  - Pure Stan reduce_sum: similar or better (depends on implementation)

**When This Makes Sense**:
- ✓ ODE is complex and benefits from isolation (this project!)
- ✓ brms infrastructure value is high (model comparison, priors, diagnostics)
- ✓ Team will grow and brms syntax is helpful for onboarding
- ✓ Want threading but also want formula-based specification
- ✗ If debugging Stan is a primary activity

**Implementation Path**:
1. Extract ODE + reduce_sum logic into `stan/unemployment-functions.stan`
2. Create wrapper R function that builds brms call with stanvars
3. Test that outputs match current custom Stan model exactly
4. Benchmark threading effectiveness
5. Gradually migrate priors and inference to brms infrastructure

**Current Assessment**: This hybrid approach is **actually quite attractive** because:
1. **Best of both worlds**: ODE efficiency + brms usability
2. **Cleaner code**: Functions block is isolated and testable
3. **Threading integrated**: reduce_sum works within brms pipeline
4. **Scalable**: Easier to onboard researchers unfamiliar with raw Stan
5. **Future-proof**: Can gradually leverage more brms features

**Recommendation**: Depends on where the computational bottleneck is located.

**Key Question**: Is the bottleneck the **ODE computation** or the **likelihood evaluation**?

- **If ODE computation dominates** (likely):
  - brms + threads: Limited speedup (~30-50%)
  - Pure Stan + reduce_sum(): Full speedup (can parallelize ODE)
  - → **Use pure Stan approach**

- **If likelihood evaluation dominates** (less likely given 2170 observations):
  - brms + threads: ~2-4x speedup (reduces 51.5 min → 25-50 min)
  - Pure Stan + reduce_sum(): Similar or better
  - → **Hybrid approach becomes attractive** (easier code + good performance)

**Decision Path**:
1. **FIRST**: Profile current model to identify bottleneck
   - Time ODE computation separately
   - Time likelihood evaluation separately
   - Determine % of total runtime for each

2. **IF ODE > 70% of runtime**:
   - Use **pure Stan + reduce_sum()** for full control
   - Extract ODE into functions for modularity
   - Target: 2-4x speedup

3. **IF likelihood > 50% of runtime**:
   - Use **hybrid approach** (brms + custom functions)
   - brms's reduce_sum() parallelizes likelihood
   - Easier to maintain and extend
   - Target: 2x speedup on likelihood portion

**Recommendation Before Profiling**:
Given the model structure (many time points, education levels, spline basis functions), the **ODE computation likely dominates**. Therefore:
- **Use pure Stan approach** as the primary path
- After achieving speedup, can reconsider hybrid approach if team needs brms features

**Proposal**: Profile the current model first to make final decision.

