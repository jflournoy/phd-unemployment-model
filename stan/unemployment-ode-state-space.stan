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
  // Compute shock intensities at each time point
  array[T] real shock_2008;
  array[T] real shock_2020;

  for (t in 1:T) {
    shock_2008[t] = shock_impulse(year_frac[t], shock_2008_onset, shock_2008_peak, 0.5);
    shock_2020[t] = shock_impulse(year_frac[t], shock_2020_onset, shock_2020_peak, 1.0);
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

  // Shock decay rates (how quickly shock effects fade)
  real<lower=0.1, upper=5> decay_2008;
  real<lower=0.1, upper=5> decay_2020;

  // Seasonal effects on finding rate (sum-to-zero constraint)
  matrix[11, N_edu] seasonal_raw;

  // State evolution noise (logit scale)
  real<lower=0> sigma_state;

  // Beta-binomial dispersion parameter (larger = less overdispersion)
  real<lower=1> phi;
}

transformed parameters {
  // Latent unemployment rates on probability scale
  array[T] vector<lower=0, upper=1>[N_edu] u;

  // Full seasonal effects with sum-to-zero
  matrix[12, N_edu] seasonal;

  // Latent logit unemployment rates
  array[T] vector[N_edu] logit_u;

  // Build seasonal effects with sum-to-zero constraint
  for (i in 1:N_edu) {
    seasonal[1:11, i] = seasonal_raw[, i];
    seasonal[12, i] = -sum(seasonal_raw[, i]);
  }

  // Initialize first time point
  logit_u[1] = logit_u_init;
  u[1] = inv_logit(logit_u[1]);

  // State evolution: discretized ODE with shocks and seasonality
  for (t in 2:T) {
    for (i in 1:N_edu) {
      // Effective separation rate (baseline + shock effects)
      real s_eff = separation_rate[i]
                   + shock_2008[t] * shock_2008_effect[i]
                   + shock_2020[t] * shock_2020_effect[i];

      // Effective finding rate (baseline + seasonal)
      real f_eff = finding_rate[i] * (1 + seasonal[month[t], i]);

      // Discretized ODE: dU/dt = s*(1-U) - f*U
      real du_dt = s_eff * (1 - u[t-1][i]) - f_eff * u[t-1][i];

      // State evolution on logit scale with innovation
      logit_u[t][i] = logit_u[t-1][i] + du_dt + logit_u_innov[t-1][i];
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
  decay_2008 ~ normal(0.5, 0.2);
  decay_2020 ~ normal(1.0, 0.3);

  // Seasonal effects (modest amplitude)
  to_vector(seasonal_raw) ~ normal(0, 0.05);

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

  // Shock half-lives (years)
  real halflife_2008 = log(2) / decay_2008;
  real halflife_2020 = log(2) / decay_2020;

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
