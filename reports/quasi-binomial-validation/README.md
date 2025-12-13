# Quasi-Binomial GAM Validation Report

**IMPORTANT**: This is NOT a parameter recovery validation. Quasi-binomial has no generative model.

This report tests **model robustness to overdispersion** using beta-binomial data.

## What This Report Shows

1. **Model comparison under overdispersion**: How quasi-binomial vs binomial GAMs perform with beta-binomial data
2. **Different model fits**: Quasi-binomial fits smoother trends due to REML smoothing parameter differences
3. **Prediction interval coverage**: Tests whether quasi-binomial provides better-calibrated PIs than binomial
4. **Realistic overdispersion**: Beta-binomial mimics CPS data where rates vary by unmodeled factors

## Key Findings

- **Quasi-binomial vs binomial**: Different model fits (not just SE adjustment)
  - Quasi-binomial λ ~64,000× larger → smoother fits
  - Expected REML behavior based on dispersion parameter

- **Prediction interval coverage on beta-binomial data**:
  - Under-coverage expected (PIs don't capture all variance sources)
  - Quasi-binomial performs better than binomial (wider, more realistic PIs)
  - Neither achieves 95% coverage (DGP mismatch is expected)

- **Two variance sources in beta-binomial data**:
  1. Beta-binomial heterogeneity: Each obs has random p ~ Beta(α, β)
  2. Binomial sampling: Given p, count ~ Binomial(n, p)
  - Standard PIs only account for (2), not (1)

## Important Distinctions

This report tests **model robustness to overdispersion**, NOT:
- Parameter recovery (quasi-binomial has no parameters to recover)
- Confidence intervals on smooth mean function (different question)
- Performance on real CPS data (use real data validation for that)

## Files

### Simulation-Based Validation

- `quasi-binomial-validation.qmd`: Simulation-based validation using beta-binomial data
- `quasi-binomial-validation.html`: Rendered HTML report (simulation-based)

### Real CPS Data Comparison

- `binomial-comparison-real-data.qmd`: Binomial vs quasi-binomial comparison on real CPS data
- `binomial-comparison-real-data.html`: Rendered HTML report (real data)

**Difference between reports**:
- `quasi-binomial-validation`: Tests model behavior under simulated overdispersion (beta-binomial DGP)
- `binomial-comparison-real-data`: Compares binomial vs quasi-binomial on actual PhD/Masters/Bachelors unemployment data from CPS

## Recommendations

For CPS unemployment modeling:

1. Use quasi-binomial when overdispersion present (φ > 2)
2. Consider alternatives for extreme overdispersion (β-binomial GAM, hierarchical models)
3. **Always validate on real data**, not just simulations
4. Simulation validation only tests that specific DGP!

## Background Research

Supporting scripts in `scripts/`:
- `debug-quasi-binomial.R`: Investigates SE calculation and model differences
- `investigate-quasi-vs-binomial.R`: Tests why binomial and quasi-binomial fits differ

Key insight: Quasi-binomial uses `scale = φ` in REML, leading to much larger smoothing parameters and smoother fits.
