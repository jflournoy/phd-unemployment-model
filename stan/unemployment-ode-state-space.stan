// Unemployment ODE State Space Model
//
// Models unemployment dynamics as a discretized ODE system with:
// - Education-specific separation and finding rates
// - Economic shock effects (2008, 2020) with education-specific decay
// - Direct seasonal effects on unemployment (logit scale)
// - Hierarchical structure across education levels
// - Beta-binomial observation model for overdispersion
//
// Key design choices for efficient sampling:
// - NON-CENTERED PARAMETERIZATION for hierarchical parameters (avoids funnels)
// - UNBOUNDED PARAMETERIZATION via log/logit transforms (avoids hard boundaries)
// - All bounded parameters transformed: separation_rate, finding_rate, decay, phi
//
// Author: Claude Code
// Date: 2025-12-28
// Updated: 2026-01-08 (reparameterization for better sampling)

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

  // === NON-CENTERED HIERARCHICAL PARAMETERS ===
  // Using non-centered parameterization to avoid funnel geometry
  // Raw parameters are standard normal, transformed in transformed parameters

  // Hierarchical separation rates (non-centered, logit scale)
  // separation_rate in [0, 0.2] -> logit(rate/0.2) is unbounded
  real mu_logit_separation;           // Mean on logit(rate/0.2) scale
  real<lower=0> sigma_logit_separation;  // SD on logit scale
  vector[N_edu] separation_raw;       // N(0,1) raw values (non-centered)

  // Hierarchical finding rates (non-centered, logit scale)
  // finding_rate in [0, 1] -> logit(rate) is unbounded
  real mu_logit_finding;              // Mean on logit scale
  real<lower=0> sigma_logit_finding;  // SD on logit scale
  vector[N_edu] finding_raw;          // N(0,1) raw values (non-centered)

  // Shock effects (log scale to ensure positivity)
  // Using log-normal to avoid hard boundary at 0
  vector[N_edu] log_shock_2008_effect;
  vector[N_edu] log_shock_2020_effect;

  // Shock decay rates (log scale for [0.1, 5] -> unconstrained)
  // decay = 0.1 + 4.9 * inv_logit(decay_raw) maps (-inf, inf) -> (0.1, 5)
  vector[N_edu] decay_2008_raw;
  vector[N_edu] decay_2020_raw;

  // Direct seasonal effects on unemployment (sum-to-zero constraint)
  matrix[11, N_edu] seasonal_u_raw;

  // State evolution noise (log scale)
  real log_sigma_state;

  // Beta-binomial dispersion (log scale, phi > 1)
  real log_phi_minus_1;  // log(phi - 1), so phi = 1 + exp(log_phi_minus_1) > 1
}

transformed parameters {
  // === TRANSFORM RAW PARAMETERS TO CONSTRAINED SCALE ===

  // Separation rates: non-centered, then transform from logit scale
  // separation_rate = 0.2 * inv_logit(logit_separation)
  vector[N_edu] logit_separation;
  vector<lower=0, upper=0.2>[N_edu] separation_rate;
  for (i in 1:N_edu) {
    logit_separation[i] = mu_logit_separation + sigma_logit_separation * separation_raw[i];
    separation_rate[i] = 0.2 * inv_logit(logit_separation[i]);
  }

  // Finding rates: non-centered, then transform from logit scale
  vector[N_edu] logit_finding;
  vector<lower=0, upper=1>[N_edu] finding_rate;
  for (i in 1:N_edu) {
    logit_finding[i] = mu_logit_finding + sigma_logit_finding * finding_raw[i];
    finding_rate[i] = inv_logit(logit_finding[i]);
  }

  // Shock effects: exp transform (log-normal)
  vector<lower=0>[N_edu] shock_2008_effect = exp(log_shock_2008_effect);
  vector<lower=0>[N_edu] shock_2020_effect = exp(log_shock_2020_effect);

  // Decay rates: transform from unbounded to [0.1, 5]
  vector<lower=0.1, upper=5>[N_edu] decay_2008;
  vector<lower=0.1, upper=5>[N_edu] decay_2020;
  for (i in 1:N_edu) {
    decay_2008[i] = 0.1 + 4.9 * inv_logit(decay_2008_raw[i]);
    decay_2020[i] = 0.1 + 4.9 * inv_logit(decay_2020_raw[i]);
  }

  // State noise and dispersion
  real<lower=0> sigma_state = exp(log_sigma_state);
  real<lower=1> phi = 1 + exp(log_phi_minus_1);

  // === LATENT STATE DYNAMICS ===

  // Latent unemployment rates on probability scale
  array[T] vector<lower=0, upper=1>[N_edu] u;

  // Direct seasonal effects on unemployment (logit scale) with sum-to-zero
  matrix[12, N_edu] seasonal_u;

  // Latent logit unemployment rates
  array[T] vector[N_edu] logit_u;

  // Education-specific shock intensities (computed from decay rates)
  array[T] vector[N_edu] shock_2008_intensity;
  array[T] vector[N_edu] shock_2020_intensity;

  // Build seasonal effects with sum-to-zero constraint
  for (i in 1:N_edu) {
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

      // Discretized ODE: dU/dt = s*(1-U) - f*U
      real du_dt = s_eff * (1 - u[t-1][i]) - finding_rate[i] * u[t-1][i];

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
  // === Priors (Non-Centered Parameterization) ===

  // Separation rate hierarchical priors (on logit scale)
  // Monthly job separation ~1-3% => logit(0.02/0.2) ≈ -2.2
  mu_logit_separation ~ normal(-2.2, 0.5);  // Centers around 2% separation
  sigma_logit_separation ~ exponential(2);  // Allows moderate variation
  separation_raw ~ std_normal();  // Non-centered: raw ~ N(0,1)

  // Finding rate hierarchical priors (on logit scale)
  // Monthly job finding rate ~30% => logit(0.30) ≈ -0.85
  mu_logit_finding ~ normal(-0.85, 0.5);  // Centers around 30% finding
  sigma_logit_finding ~ exponential(2);   // Allows moderate variation
  finding_raw ~ std_normal();  // Non-centered: raw ~ N(0,1)

  // Shock effect priors (log scale for log-normal)
  // E[shock] ≈ 0.02 => log(0.02) ≈ -3.9
  log_shock_2008_effect ~ normal(-3.9, 0.5);  // ~0.02 with factor ~1.6 variation
  log_shock_2020_effect ~ normal(-3.5, 0.5);  // ~0.03 with factor ~1.6 variation

  // Decay rate priors (on transformed scale)
  // decay ~ 0.5-1.0 typical => inv_logit maps 0 to 0.5
  decay_2008_raw ~ normal(0, 1);  // Centered on mid-range
  decay_2020_raw ~ normal(0.5, 1);  // Slightly higher for COVID

  // Direct seasonal effects on unemployment (logit scale)
  // Prior of 0.05 on logit scale ≈ ±0.5 percentage points at 3% unemployment
  to_vector(seasonal_u_raw) ~ normal(0, 0.05);

  // Initial states (informed by typical unemployment levels)
  logit_u_init ~ normal(-3.5, 0.5);  // ~3% unemployment

  // State noise (log scale)
  // sigma_state ~ 0.05 => log(0.05) ≈ -3
  log_sigma_state ~ normal(-3, 1);

  // State innovations
  for (t in 1:(T-1)) {
    logit_u_innov[t] ~ normal(0, sigma_state);
  }

  // Overdispersion parameter (log scale)
  // phi ~ 20 => log(phi-1) ≈ log(19) ≈ 3
  log_phi_minus_1 ~ normal(3, 1);

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
  // This removes the direct seasonal effect on unemployment
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
