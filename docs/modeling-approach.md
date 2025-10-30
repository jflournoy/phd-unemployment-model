# Statistical Modeling Approach

## Modeling Philosophy

Follow the principle of **parsimony**: Start with the simplest model that captures key features, then add complexity only as justified by the data and research questions.

## Model Development Sequence

### Phase 1: Baseline Models

1. **Descriptive statistics**
   - Raw unemployment rates over time
   - Basic visualizations
   - Identify key patterns (trends, seasonality, anomalies)

2. **Simple time series models**
   - Linear trends
   - Basic seasonal decomposition
   - Identify autocorrelation structure

### Phase 2: Comparative Models

1. **Multiple time series**
   - PhD vs general unemployment
   - PhD vs Master's/Bachelor's
   - Identify differential patterns

2. **Relative risk models**
   - Ratio of PhD to baseline unemployment
   - Log-linear models for rate ratios

### Phase 3: Advanced Decomposition

1. **Generalized Additive Models (GAMs)**
   - Smooth trends
   - Multiple seasonal components (annual, other periodicities)
   - Non-linear relationships

2. **Gaussian Process Models**
   - Flexible covariance structures
   - Handle irregular spacing
   - Quantify uncertainty

3. **Autoregressive models**
   - AR, ARMA structures
   - Account for temporal dependence
   - Forecast future values

## Model Fitting with brms/Stan

### Prior Selection

- Use **weakly informative priors** as defaults
- Document prior choices
- Run **prior predictive checks**
- Adjust based on domain knowledge

### Model Diagnostics

Required for all models:

- **Convergence**: Rhat < 1.01
- **Effective sample size**: ESS > 400 (bulk and tail)
- **Trace plots**: Visual inspection
- **Posterior predictive checks**: Compare to observed data
- **LOO-CV**: Model comparison and outlier detection

### Model Comparison

- Use **LOO-CV** (leave-one-out cross-validation) for model selection
- Report ELPD differences and standard errors
- Check for influential observations
- Consider **model stacking** if multiple models perform similarly

## Seasonal Decomposition Strategy

### Multiple Seasonal Components

1. **Annual cycle**: 12-month periodicity
2. **Economic cycles**: Identify from data (e.g., recession periods)
3. **Structural breaks**: Major events (COVID-19, policy changes)

### Implementation

Use Fourier basis functions or splines to capture seasonal patterns:

```stan
// Example: Annual seasonality with Fourier terms
real seasonal_effect;
seasonal_effect = 0;
for (k in 1:K) {
  seasonal_effect += beta_cos[k] * cos(2 * pi() * k * month / 12) +
                     beta_sin[k] * sin(2 * pi() * k * month / 12);
}
```

## Visualization Standards

All models should include:

1. **Time series plots**: Observed vs predicted
2. **Posterior intervals**: 50%, 80%, 95% credible intervals
3. **Residual diagnostics**: Check model assumptions
4. **Component plots**: Separate trend, seasonal, residual components
5. **Comparative plots**: PhD vs baseline rates

## Reproducibility Requirements

- Save model objects with metadata
- Document priors and model structure
- Include convergence diagnostics in reports
- Version control for model code
- Use `set.seed()` for reproducibility
