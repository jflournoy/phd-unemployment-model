// Unemployment ODE State Space Model
//
// Models unemployment dynamics as a discretized ODE system with:
// - Education-specific separation and finding rates
// - Economic shock effects (2008, 2020)
// - Seasonal patterns
// - Hierarchical structure across education levels
// - Beta-binomial observation model for overdispersion
//
// Author: Claude Code
// Date: 2025-12-28

functions {
  // Fuzzy impulse function for shock intensity
  // Gradual onset, peak, and exponential decay
  real shock_impulse(real t, real onset, real peak, real decay_rate) {
    if (t < onset) {
      return 0;
    } else if (t <= peak) {
      // Linear rise from onset to peak
      return (t - onset) / (peak - onset);
    } else {
      // Exponential decay from peak
      return exp(-decay_rate * (t - peak));
    }
  }
}

data {
  int<lower=1> T;                           // Number of time points
  int<lower=1> N_edu;                       // Number of education levels
  array[T, N_edu] int<lower=0> n_unemployed; // Unemployment counts
  array[T, N_edu] int<lower=0> n_total;      // Total labor force counts
  array[T] int<lower=1, upper=12> month;     // Month indicator (1-12)
  array[T] real<lower=0> year_frac;          // Year as continuous (e.g., 2020.5)

  // Shock timing parameters (fixed based on economic history)
  real shock_2008_onset;    // ~2007.75
  real shock_2008_peak;     // ~2009.5
  real shock_2020_onset;    // ~2020.17
  real shock_2020_peak;     // ~2020.33
}

transformed data {
  // Compute shock rise phase (before decay is applied)
  // Decay will be applied in transformed parameters with education-specific rates
  array[T] real shock_2008_rise;  // 0 before onset, rises to 1 at peak, stays 1 after
  array[T] real shock_2020_rise;
  array[T] real time_since_2008_peak;  // For decay calculation
  array[T] real time_since_2020_peak;

  for (t in 1:T) {
    // 2008 shock rise
    if (year_frac[t] < shock_2008_onset) {
      shock_2008_rise[t] = 0;
      time_since_2008_peak[t] = 0;
    } else if (year_frac[t] <= shock_2008_peak) {
      shock_2008_rise[t] = (year_frac[t] - shock_2008_onset) / (shock_2008_peak - shock_2008_onset);
      time_since_2008_peak[t] = 0;
    } else {
      shock_2008_rise[t] = 1;
      time_since_2008_peak[t] = year_frac[t] - shock_2008_peak;
    }

    // 2020 shock rise
    if (year_frac[t] < shock_2020_onset) {
      shock_2020_rise[t] = 0;
      time_since_2020_peak[t] = 0;
    } else if (year_frac[t] <= shock_2020_peak) {
      shock_2020_rise[t] = (year_frac[t] - shock_2020_onset) / (shock_2020_peak - shock_2020_onset);
      time_since_2020_peak[t] = 0;
    } else {
      shock_2020_rise[t] = 1;
      time_since_2020_peak[t] = year_frac[t] - shock_2020_peak;
    }
  }
}

parameters {
  // Initial latent unemployment rates (logit scale)
  vector[N_edu] logit_u_init;

  // Latent unemployment rate innovations (logit scale)
  array[T-1] vector[N_edu] logit_u_innov;

  // Hierarchical separation rates (monthly probability of job loss)
  real<lower=0> mu_separation;
  real<lower=0> sigma_separation;
  vector<lower=0, upper=0.2>[N_edu] separation_rate;  // Max 20%/month

  // Hierarchical finding rates (monthly probability of finding job)
  real<lower=0> mu_finding;
  real<lower=0> sigma_finding;
  vector<lower=0, upper=1>[N_edu] finding_rate;  // Max 100%/month

  // Shock effects (increase in separation rate during shocks)
  vector<lower=0>[N_edu] shock_2008_effect;
  vector<lower=0>[N_edu] shock_2020_effect;

  // Shock decay rates (education-specific: different groups recover differently)
  vector<lower=0.1, upper=5>[N_edu] decay_2008;
  vector<lower=0.1, upper=5>[N_edu] decay_2020;

  // Seasonal effects on finding rate (sum-to-zero constraint)
  // These affect the rate at which unemployed find jobs
  matrix[11, N_edu] seasonal_finding_raw;

  // Direct seasonal effects on unemployment (sum-to-zero constraint)
  // These capture observed seasonal patterns directly (e.g., academic calendar)
  matrix[11, N_edu] seasonal_u_raw;

  // State evolution noise (logit scale)
  real<lower=0> sigma_state;

  // Beta-binomial dispersion parameter (larger = less overdispersion)
  real<lower=1> phi;
}

transformed parameters {
  // Latent unemployment rates on probability scale
  array[T] vector<lower=0, upper=1>[N_edu] u;

  // Seasonal effects on finding rate with sum-to-zero
  matrix[12, N_edu] seasonal_finding;

  // Direct seasonal effects on unemployment (logit scale) with sum-to-zero
  matrix[12, N_edu] seasonal_u;

  // Latent logit unemployment rates
  array[T] vector[N_edu] logit_u;

  // Education-specific shock intensities (computed from decay rates)
  array[T] vector[N_edu] shock_2008_intensity;
  array[T] vector[N_edu] shock_2020_intensity;

  // Build seasonal effects with sum-to-zero constraint
  for (i in 1:N_edu) {
    // Finding rate seasonality
    seasonal_finding[1:11, i] = seasonal_finding_raw[, i];
    seasonal_finding[12, i] = -sum(seasonal_finding_raw[, i]);

    // Direct unemployment seasonality
    seasonal_u[1:11, i] = seasonal_u_raw[, i];
    seasonal_u[12, i] = -sum(seasonal_u_raw[, i]);
  }

  // Compute education-specific shock intensities
  for (t in 1:T) {
    for (i in 1:N_edu) {
      // Apply education-specific decay after peak
      shock_2008_intensity[t][i] = shock_2008_rise[t] *
        exp(-decay_2008[i] * time_since_2008_peak[t]);
      shock_2020_intensity[t][i] = shock_2020_rise[t] *
        exp(-decay_2020[i] * time_since_2020_peak[t]);
    }
  }

  // Initialize first time point
  logit_u[1] = logit_u_init;
  u[1] = inv_logit(logit_u[1]);

  // State evolution: discretized ODE with shocks and seasonality
  for (t in 2:T) {
    for (i in 1:N_edu) {
      // Effective separation rate (baseline + education-specific shock effects)
      real s_eff = separation_rate[i]
                   + shock_2008_intensity[t][i] * shock_2008_effect[i]
                   + shock_2020_intensity[t][i] * shock_2020_effect[i];

      // Effective finding rate (baseline + finding rate seasonality)
      real f_eff = finding_rate[i] * (1 + seasonal_finding[month[t], i]);

      // Discretized ODE: dU/dt = s*(1-U) - f*U
      real du_dt = s_eff * (1 - u[t-1][i]) - f_eff * u[t-1][i];

      // State evolution on logit scale with:
      // - ODE dynamics (du_dt)
      // - Direct seasonal effect on unemployment
      // - Stochastic innovation
      logit_u[t][i] = logit_u[t-1][i] + du_dt + seasonal_u[month[t], i] + logit_u_innov[t-1][i];
    }
    u[t] = inv_logit(logit_u[t]);
  }
}

model {
  // === Priors ===

  // Separation rate priors (informed by labor economics literature)
  // Monthly job separation ~1-3% on average
  mu_separation ~ normal(0.02, 0.01);
  sigma_separation ~ exponential(50);
  separation_rate ~ normal(mu_separation, sigma_separation);

  // Finding rate priors
  // Monthly job finding rate ~20-40% for unemployed
  mu_finding ~ normal(0.30, 0.10);
  sigma_finding ~ exponential(5);
  finding_rate ~ normal(mu_finding, sigma_finding);

  // Shock effect priors (positive: shocks increase separation)
  shock_2008_effect ~ normal(0.02, 0.01);
  shock_2020_effect ~ normal(0.03, 0.015);

  // Decay rate priors (half-life of 6-18 months typical)
  // Education-specific: allows different recovery speeds
  decay_2008 ~ normal(0.5, 0.3);
  decay_2020 ~ normal(1.0, 0.5);

  // Seasonal effects on finding rate (modest - this is multiplicative)
  to_vector(seasonal_finding_raw) ~ normal(0, 0.10);

  // Direct seasonal effects on unemployment (logit scale)
  // This allows substantial seasonal patterns to be captured directly
  // Prior of 0.05 on logit scale ≈ ±0.5 percentage points at 3% unemployment
  to_vector(seasonal_u_raw) ~ normal(0, 0.05);

  // Initial states (informed by typical unemployment levels)
  logit_u_init ~ normal(-3.5, 0.5);  // ~3% unemployment

  // State noise
  sigma_state ~ exponential(20);

  // State innovations (non-centered parameterization)
  for (t in 1:(T-1)) {
    logit_u_innov[t] ~ normal(0, sigma_state);
  }

  // Overdispersion parameter
  phi ~ exponential(0.05);

  // === Likelihood ===

  // Beta-binomial observation model
  // Use fmin/fmax to keep u strictly in (eps, 1-eps) for numerical stability
  for (t in 1:T) {
    for (i in 1:N_edu) {
      if (n_total[t, i] > 0) {
        real u_safe = fmin(fmax(u[t][i], 1e-6), 1 - 1e-6);
        real alpha = u_safe * phi;
        real beta_param = (1 - u_safe) * phi;
        n_unemployed[t, i] ~ beta_binomial(n_total[t, i], alpha, beta_param);
      }
    }
  }
}

generated quantities {
  // Equilibrium unemployment rates (steady state of ODE)
  vector[N_edu] u_equilibrium;
  for (i in 1:N_edu) {
    u_equilibrium[i] = separation_rate[i] / (separation_rate[i] + finding_rate[i]);
  }

  // Education-specific shock half-lives (years)
  vector[N_edu] halflife_2008;
  vector[N_edu] halflife_2020;
  for (i in 1:N_edu) {
    halflife_2008[i] = log(2) / decay_2008[i];
    halflife_2020[i] = log(2) / decay_2020[i];
  }

  // Non-seasonal trend: unemployment driven by baseline rates + shocks only
  // This removes the seasonal modulation of the finding rate
  // Uses SAME innovations as full model for comparable trajectory
  array[T] vector[N_edu] u_trend;
  {
    array[T] vector[N_edu] logit_u_trend;

    // Initialize at same point as full model
    logit_u_trend[1] = logit_u_init;
    u_trend[1] = inv_logit(logit_u_trend[1]);

    // Evolve using baseline finding rate (no seasonal)
    for (t in 2:T) {
      for (i in 1:N_edu) {
        // Effective separation rate (baseline + education-specific shock effects)
        real s_eff = separation_rate[i]
                     + shock_2008_intensity[t][i] * shock_2008_effect[i]
                     + shock_2020_intensity[t][i] * shock_2020_effect[i];

        // Baseline finding rate (NO seasonal adjustment)
        real f_base = finding_rate[i];

        // Discretized ODE without seasonal
        real du_dt = s_eff * (1 - u_trend[t-1][i]) - f_base * u_trend[t-1][i];

        // Same innovations as full model to maintain comparable trajectory
        logit_u_trend[t][i] = logit_u_trend[t-1][i] + du_dt + logit_u_innov[t-1][i];
      }
      u_trend[t] = inv_logit(logit_u_trend[t]);
    }
  }

  // Pure ODE trajectory: NO innovations, NO seasonal
  // Shows what the structural ODE dynamics alone would predict
  // This is useful for comparing against the full model to see stochastic component
  array[T] vector[N_edu] u_ode_pure;
  {
    array[T] vector[N_edu] logit_u_ode;

    // Initialize at same point
    logit_u_ode[1] = logit_u_init;
    u_ode_pure[1] = inv_logit(logit_u_ode[1]);

    // Evolve using ONLY ODE dynamics (no innovations, no seasonal)
    for (t in 2:T) {
      for (i in 1:N_edu) {
        // Effective separation rate with shocks
        real s_eff = separation_rate[i]
                     + shock_2008_intensity[t][i] * shock_2008_effect[i]
                     + shock_2020_intensity[t][i] * shock_2020_effect[i];

        // Baseline finding rate only
        real f_base = finding_rate[i];

        // Pure ODE dynamics
        real du_dt = s_eff * (1 - u_ode_pure[t-1][i]) - f_base * u_ode_pure[t-1][i];

        // NO innovation term - pure structural dynamics
        logit_u_ode[t][i] = logit_u_ode[t-1][i] + du_dt;
      }
      u_ode_pure[t] = inv_logit(logit_u_ode[t]);
    }
  }

  // Seasonal effect magnitude: difference between full model and trend
  // This directly shows the seasonal oscillation at each time point
  array[T] vector[N_edu] seasonal_effect;
  for (t in 1:T) {
    seasonal_effect[t] = u[t] - u_trend[t];
  }

  // Log-likelihood for LOO-CV
  array[T, N_edu] real log_lik;
  for (t in 1:T) {
    for (i in 1:N_edu) {
      if (n_total[t, i] > 0) {
        real u_safe = fmin(fmax(u[t][i], 1e-6), 1 - 1e-6);
        real alpha = u_safe * phi;
        real beta_param = (1 - u_safe) * phi;
        log_lik[t, i] = beta_binomial_lpmf(n_unemployed[t, i] | n_total[t, i],
                                           alpha, beta_param);
      } else {
        log_lik[t, i] = 0;
      }
    }
  }

  // Posterior predictive checks
  array[T, N_edu] int n_unemployed_rep;
  for (t in 1:T) {
    for (i in 1:N_edu) {
      if (n_total[t, i] > 0) {
        real u_safe = fmin(fmax(u[t][i], 1e-6), 1 - 1e-6);
        real alpha = u_safe * phi;
        real beta_param = (1 - u_safe) * phi;
        n_unemployed_rep[t, i] = beta_binomial_rng(n_total[t, i], alpha, beta_param);
      } else {
        n_unemployed_rep[t, i] = 0;
      }
    }
  }
}
