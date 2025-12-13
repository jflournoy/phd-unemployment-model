# Binomial/Quasi-Binomial Analysis Architecture

## Overview

This directory contains validation and comparison analyses for binomial vs quasi-binomial GAM models applied to PhD unemployment data.

## Targets Pipeline Integration

The analysis follows the **targets** pipeline pattern for reproducibility and efficient caching:

```
Raw Data → Processed Data → Models → Comparison → Report
```

### Pipeline Dependency Graph

```
ipums_data.rds (raw file)
    ↓
education_counts (target: aggregated monthly counts)
    ↓
model_comparison (target: fits both models and compares)
    ├→ model_binomial (extracted from comparison)
    ├→ model_quasibinomial (extracted from comparison)
    └→ summary, fitted_values, comparison_table
         ↓
report_binomial_quasi (target: Quarto report)
```

## Key Design Decisions

### 1. Single Comparison Target

Instead of fitting models separately, we use a single `model_comparison` target that:
- Fits both binomial and quasi-binomial models
- Computes all comparison metrics
- Returns a comprehensive comparison object

**Benefits**:
- Models fit with identical data
- Comparison metrics guaranteed to match
- Single cache invalidation point

### 2. Report as Pure Consumer

The [binomial-comparison-real-data.qmd](binomial-comparison-real-data.qmd) report:
- Does **NOT** fit models
- Does **NOT** process data
- **ONLY** visualizes and interprets results from targets

**Benefits**:
- Fast re-rendering (no model refitting)
- Separation of concerns (computation vs presentation)
- Easy to update visualizations without re-running expensive computations

### 3. Model Extraction Targets

`model_binomial` and `model_quasibinomial` are separate targets that extract models from `model_comparison`:

```r
tar_target(
  model_binomial,
  model_comparison$binomial_model
)
```

**Benefits**:
- Individual models available for other analyses
- Clear dependency tracking
- Minimal overhead (extraction is cheap)

## Running the Analysis

### Build Everything

```r
targets::tar_make()
```

This builds:
1. `education_counts` (if data changed)
2. `model_comparison` (if data or code changed)
3. `model_binomial`, `model_quasibinomial` (extracted)
4. `report_binomial_quasi` (rendered)

### Rebuild Just the Report

```r
targets::tar_make(report_binomial_quasi)
```

If models haven't changed, this just re-renders the report (fast).

### Check What's Outdated

```r
targets::tar_outdated()
targets::tar_visnetwork()
```

### Load Results Interactively

```r
# Load comparison
comparison <- targets::tar_read(model_comparison)
print(comparison$summary)

# Load individual models
model_b <- targets::tar_read(model_binomial)
model_q <- targets::tar_read(model_quasibinomial)

# Compare smoothing parameters
mean(model_b$sp)
mean(model_q$sp)
```

## File Structure

```
reports/quasi-binomial-validation/
├── ARCHITECTURE.md                      # This file
├── README.md                            # High-level overview
├── quasi-binomial-validation.qmd        # Simulation-based validation
├── quasi-binomial-validation.html
├── binomial-comparison-real-data.qmd    # Real CPS data comparison
└── binomial-comparison-real-data.html
```

## Report Differences

### quasi-binomial-validation.qmd

- **Data**: Simulated beta-binomial data
- **Goal**: Test model robustness to overdispersion
- **Method**: Prediction interval coverage on holdout data
- **Pattern**: Self-contained (generates its own data)

### binomial-comparison-real-data.qmd

- **Data**: Real CPS unemployment counts from targets
- **Goal**: Compare binomial vs quasi-binomial on actual data
- **Method**: Model comparison metrics + visualization
- **Pattern**: Pure consumer (loads from targets)

## Targets Pipeline Definition

See `_targets.R` for the full pipeline definition:

```r
list(
  # ... other targets ...

  tar_target(
    model_comparison,
    {
      compare_binomial_quasibinomial(
        data = education_counts,
        formula_type = "full",
        education_var = "education",
        success_col = "n_unemployed",
        total_col = "n_total"
      )
    }
  ),

  tar_target(
    model_binomial,
    model_comparison$binomial_model
  ),

  tar_target(
    model_quasibinomial,
    model_comparison$quasibinomial_model
  ),

  tar_quarto(
    report_binomial_quasi,
    path = "reports/quasi-binomial-validation"
  )
)
```

## Benefits of This Architecture

1. **Reproducibility**: Exact same data and models every time
2. **Efficiency**: Intelligent caching (only recompute what changed)
3. **Clarity**: Clear separation between computation and presentation
4. **Flexibility**: Easy to add new reports or analyses
5. **Verification**: Can inspect intermediate results (`tar_read()`)

## Future Extensions

Potential additions that would slot cleanly into this architecture:

- `model_beta_binomial`: Fit beta-binomial GAM for comparison
- `model_validation`: Holdout validation metrics
- `report_model_selection`: Extended model selection report
- `sensitivity_analysis`: Sensitivity to hyperparameters

Each would be a new target that depends on existing targets, maintaining the dependency graph.
