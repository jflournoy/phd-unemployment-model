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

### 1. Multi-threaded Refactoring of Hierarchical Variance Analysis ⚠️ (Priority: HIGH)
- **Description**: Refactor `scripts/analyze-hierarchical-variance.R` to use functional programming patterns and parallel processing for improved performance and maintainability
- **Scope**:
  - Extract helper functions into modular, reusable components in `R/`
  - Implement parallel processing for parameter extraction across education levels
  - Create wrapper functions for visualization generation
  - Consider `parallel::mclapply()` or `furrr::future_map()` for concurrent operations
- **Benefits**:
  - Faster analysis execution on multi-core systems
  - Improved code reusability across other analysis scripts
  - Better testability via functional decomposition
  - Foundation for scaling to larger hierarchical models
- **Estimated Effort**: Medium (4-6 refactoring iterations)
- **Dependencies**: Completion of hierarchical variance analysis

**Proposed Architecture**:
```r
# New functions in R/hierarchical-analysis.R
extract_hierarchical_param_info()      # Extract σ from model output
interpret_pooling_strength()           # Categorize by σ threshold
compare_pooling_across_parameters()    # Generate comparison table
create_variance_visualization()        # Generate publication-quality plots
create_education_comparison_plots()    # Generate faceted comparisons
```

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

### Current Implementation
- Single-threaded R script (`scripts/analyze-hierarchical-variance.R`)
- Direct computation without intermediate caching
- Monolithic 413-line script with embedded visualizations

### Proposed Improvements
1. **Modularization**: Break into testable functions in `R/` package
2. **Parallelization**: Use `furrr` or `parallel` for multi-threaded execution
3. **Caching**: Cache intermediate results for faster re-analysis
4. **Testing**: Add unit tests for each analytical function
5. **Documentation**: Roxygen2 comments for function discoverability

---

## Testing Standards
- All new functions must have unit tests in `tests/testthat/`
- Use mock data for rapid iteration, real model output for validation
- Test execution time target: <30 seconds for full suite

---

## Performance Targets
- Hierarchical variance analysis runtime: <10 seconds (currently ~15s)
- Memory usage: <500MB for full model analysis
- Report rendering time: <2 minutes including all analyses

