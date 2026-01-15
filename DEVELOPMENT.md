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
- **Scope**:
  - Extract ODE solver logic into Stan functions (currently inline in model block)
  - Extract likelihood computation into reusable Stan functions
  - Implement `reduce_sum()` for parallel likelihood evaluation across time points and education levels
  - Optimize hierarchical parameter transformations with functions
- **Benefits**:
  - Faster model sampling through parallelized likelihood computation
  - Improved code maintainability and readability
  - Foundation for scaling to larger datasets and models
  - Potential 2-4x speedup on multi-core systems
- **Estimated Effort**: High (8-12 hours, requires careful testing)
- **Dependencies**: Completion of hierarchical variance analysis

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

