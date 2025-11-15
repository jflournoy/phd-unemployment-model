# Statistical Analysis Reports

This directory contains Quarto reports organized by modeling approach and analysis type.

## Directory Structure

### `basic-spline/`

Basic GAM models with simple spline smooths for time trends and seasonal patterns.

- **parameter-recovery-validation** - Validates basic seasonal GAM using coverage-based testing
  - Tests prediction interval and confidence interval calibration
  - Uses coverage probability rather than point estimate accuracy
  - 95% intervals should contain 95% of targets (tolerance: 85%-100%)

### `factor-smooth/`

Factor smooth GAM models allowing education-specific effects (PhD vs other degrees).

- **factor-smooth-parameter-recovery** - Validates factor smooth GAMs with education-specific parameters
  - Tests recovery of education-specific seasonal patterns, trends, and baselines
  - Validates CI coverage for pairwise differences (PhD vs HS, PhD vs BA, etc.)
  - Tests false positive control and model selection via AIC
- **factor-smooth-unemployment-analysis** - Main analysis applying validated factor smooth model to real 2000-2025 CPS data
  - Education spectrum analysis: High School, Some College, Bachelor's, Master's, Professional, PhD
  - Seasonal patterns by education level
  - Long-term trends and comparisons

### `exploratory/`

Exploratory analyses and preliminary investigations.

- **education-comparison-2000-2025** - Cross-education comparisons and exploratory visualizations
- **exploratory-2024** - Initial exploratory analysis of 2024 data
- **phd-annual-trend.pdf** - Annual trend visualizations
- **phd-monthly-timeseries.pdf** - Monthly time series plots
- **phd-monthly-seasonal-decomposition.pdf** - Seasonal decomposition visualizations

### `data-quality/`

Data quality assessments and completeness reports.

- **data-completeness-2024** - Quality checks for 2024 CPS data
- **data-completeness-2000-2025** - Comprehensive quality assessment for full 2000-2025 dataset
  - Sample size validation
  - Variable completeness
  - Temporal coverage

## Modeling Workflow

1. **Data Quality** (`data-quality/`) - Verify data completeness and quality
2. **Exploration** (`exploratory/`) - Initial exploratory analysis
3. **Parameter Recovery** - Validate models before applying to real data
   - `basic-spline/parameter-recovery-validation` for simple models
   - `factor-smooth/factor-smooth-parameter-recovery` for education-specific models
4. **Main Analysis** - Apply validated models to real data
   - `factor-smooth/factor-smooth-unemployment-analysis` for final results

## Key Methodological Standards

All parameter recovery reports follow project standards:

- **Coverage-based validation** - Test CI/PI calibration, not point estimates
- **Difference testing** - For comparisons (PhD vs HS), validate coverage of *differences*
- **False positive control** - Test Type I error rates under null hypothesis
- **Model selection** - Use AIC for complexity-accuracy tradeoff
- **Adequate simulations** - Minimum 200 simulations for stable coverage estimates

See [LEARNINGS.md](../.claude/learnings/) for detailed methodological insights.
