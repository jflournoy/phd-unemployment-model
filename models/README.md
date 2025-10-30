# Statistical Models Directory

This directory contains fitted statistical models and Stan model code.

## Directory Structure

- `.stan` files - Stan model code (if writing custom models)
- `.rds` files - Fitted model objects (ignored by git due to size)
- Model metadata files documenting model specifications

## Model Naming Convention

Use descriptive names indicating model type and features:

```
baseline_linear_trend_2025-10-30.rds
gam_seasonal_annual_2025-10-30.rds
gp_full_model_2025-11-01.rds
```

## Model Documentation

Each model should have an accompanying documentation file:

```
model_name_YYYY-MM-DD.md
```

Including:

- Model formula
- Prior specifications
- Convergence diagnostics
- Model comparison metrics (LOO-CV)
- Date fitted
- R/Stan versions
- Key findings

## Example Model Documentation

```markdown
# GAM with Annual Seasonality

**Date**: 2025-10-30
**File**: gam_seasonal_annual_2025-10-30.rds

## Model Formula

```r
unemp_rate ~ s(time_index) + s(month, bs = "cc")
```

## Priors

- Default brms priors
- Student t(3, 0, 2.5) for intercept

## Diagnostics

- Rhat: max 1.00
- ESS bulk: min 1500
- ESS tail: min 1200

## Model Comparison

- LOO-CV ELPD: -150.2 (SE 15.3)
- Preferred over linear model (ΔELPD: 25.1, SE: 8.2)

## Notes

Model captures annual seasonality well. Consider adding economic cycle component.
```

## Saving Models

Always save models with metadata:

```r
# Save model with metadata
model_metadata <- list(
  model = fitted_model,
  date = Sys.Date(),
  formula = formula(fitted_model),
  data_file = "data/phd_unemployment.rds",
  convergence = summary(fitted_model)$fixed,
  r_version = R.version.string,
  brms_version = packageVersion("brms")
)

saveRDS(model_metadata, "models/gam_seasonal_annual_2025-10-30.rds")
```

## Model Comparison

Keep a `model-comparison.csv` file tracking all models:

| Model                  | Date       | ELPD   | SE    | Notes                |
| ---------------------- | ---------- | ------ | ----- | -------------------- |
| baseline_linear        | 2025-10-30 | -175.3 | 16.1  | Simple trend         |
| gam_seasonal_annual    | 2025-10-30 | -150.2 | 15.3  | Best so far          |
| gp_full                | 2025-11-01 | -148.5 | 15.8  | Flexible but complex |
