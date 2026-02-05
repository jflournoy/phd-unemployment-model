// Unemployment ODE State Space Model - Education-Level Parallelization
//
// PARALLELIZATION STRATEGY:
// - Parallelize across EDUCATION LEVELS (N_edu = 7), not time points
// - Each thread computes the complete trajectory + likelihood for one education level
// - Time evolution is sequential within each education level (required by ODE)
// - Education levels are independent (no cross-education dependencies in ODE)
//
// WHY THIS WORKS:
// - reduce_sum receives 1D arrays (compatible with Stan's type constraints)
// - Each education level's trajectory is computed entirely within the partial function
// - Hierarchical parameters are shared but fixed during trajectory computation
//
// EXPECTED SPEEDUP:
// - With 7 education levels and 4 threads: ~1.5-1.7x speedup
// - ODE+Likelihood (~90% of runtime) now parallelized across education levels
//
// DATA LAYOUT:
// - Observations flattened to 1D: index = (edu-1)*T + t
// - edu_indices = [1, 2, 3, ..., N_edu] for reduce_sum slicing
//
// Author: Claude Code (TDD implementation)
// Date: 2026-01-21
// Based on: unemployment-ode-state-space-efficient.stan

functions {
  // B-spline basis function (order 4 = cubic)
  real bspline_basis(real t, array[] real knots, int k, int order) {
    if (order == 1) {
      if (t >= knots[k] && t < knots[k + 1]) {
        return 1.0;
      } else {
        return 0.0;
      }
    } else {
      real w1 = 0;
      real w2 = 0;
      real denom1 = knots[k + order - 1] - knots[k];
      real denom2 = knots[k + order] - knots[k + 1];

      if (denom1 > 0) {
        w1 = (t - knots[k]) / denom1;
      }
      if (denom2 > 0) {
        w2 = (knots[k + order] - t) / denom2;
      }

      return w1 * bspline_basis(t, knots, k, order - 1) +
             w2 * bspline_basis(t, knots, k + 1, order - 1);
    }
  }

  // =========================================================================
  // EDUCATION-LEVEL PARTIAL FUNCTION
  // =========================================================================
  // Computes the complete trajectory + log-likelihood for one or more
  // education levels. The ODE is computed inside this function.
  //
  // Arguments:
  //   edu_slice: array of education indices to process (1-indexed)
  //   start, end: slice bounds (standard reduce_sum signature)
  //   All other args are shared data/parameters passed to all threads
  //
  real partial_edu_trajectory(
    array[] int edu_slice,    // Education indices to process
    int start,                // Start index in slice (inclusive, 1-indexed)
    int end,                  // End index in slice (inclusive, 1-indexed)
    // Flattened observation data (1D arrays - reduce_sum compatible!)
    array[] int n_unemployed_flat,
    array[] int n_total_flat,
    // Time series data
    array[] int month,
    array[] real year_frac,
    // Pre-computed shock timing (1D arrays)
    array[] real shock_2008_rise,
    array[] real shock_2020_rise,
    array[] real time_since_2008_peak,
    array[] real time_since_2020_peak,
    // Pre-computed spline basis
    matrix B_spline,
    // Model parameters (vectors - reduce_sum compatible!)
    vector separation_rate,
    vector finding_rate,
    vector shock_2008_effect,
    vector shock_2020_effect,
    vector decay_2008,
    vector decay_2020,
    vector logit_u_init,
    matrix spline_deviation,  // [T, N_edu] - matrix is compatible
    matrix seasonal_u,        // [12, N_edu] - matrix is compatible
    real phi,
    int T
  ) {
    real lp = 0;

    // Process each education level in the slice
    // NOTE: start/end are indices into the ORIGINAL array, not the slice
    // The slice contains elements x[start:end], so we iterate over the slice directly
    for (slice_idx in 1:size(edu_slice)) {
      int edu = edu_slice[slice_idx];  // 1-indexed education level

      // Compute full trajectory for this education level
      real logit_u_curr = logit_u_init[edu];
      real u_curr = inv_logit(logit_u_curr);

      // First time point
      {
        int obs_idx = (edu - 1) * T + 1;  // Flattened index
        if (n_total_flat[obs_idx] > 0) {
          real u_safe = fmin(fmax(u_curr, 1e-6), 1 - 1e-6);
          real alpha = u_safe * phi;
          real beta_param = (1 - u_safe) * phi;
          lp += beta_binomial_lpmf(n_unemployed_flat[obs_idx] | n_total_flat[obs_idx],
                                   alpha, beta_param);
        }
      }

      // Time evolution for this education level
      for (t in 2:T) {
        // Compute shock intensities at time t
        real shock_2008_intensity = shock_2008_rise[t] *
          exp(-decay_2008[edu] * time_since_2008_peak[t]);
        real shock_2020_intensity = shock_2020_rise[t] *
          exp(-decay_2020[edu] * time_since_2020_peak[t]);

        // Effective separation rate with shocks
        real s_eff = separation_rate[edu]
                     + shock_2008_intensity * shock_2008_effect[edu]
                     + shock_2020_intensity * shock_2020_effect[edu];

        // ODE dynamics
        real du_dt = s_eff * (1 - u_curr) - finding_rate[edu] * u_curr;

        // State evolution: ODE + seasonal + spline deviation
        logit_u_curr = logit_u_curr + du_dt + seasonal_u[month[t], edu] +
                       spline_deviation[t, edu] - spline_deviation[t-1, edu];
        u_curr = inv_logit(logit_u_curr);

        // Add likelihood for this (t, edu) observation
        int obs_idx = (edu - 1) * T + t;  // Flattened index
        if (n_total_flat[obs_idx] > 0) {
          real u_safe = fmin(fmax(u_curr, 1e-6), 1 - 1e-6);
          real alpha = u_safe * phi;
          real beta_param = (1 - u_safe) * phi;
          lp += beta_binomial_lpmf(n_unemployed_flat[obs_idx] | n_total_flat[obs_idx],
                                   alpha, beta_param);
        }
      }
    }

    return lp;
  }
}

data {
  int<lower=1> T;                             // Number of time points
  int<lower=1> N_edu;                         // Number of education levels
  int<lower=1> N_obs;                         // Total observations = T * N_edu

  // FLATTENED observation data (1D arrays for reduce_sum compatibility)
  // Indexing: (edu-1)*T + t where edu in 1:N_edu, t in 1:T
  array[N_obs] int<lower=0> n_unemployed_flat;
  array[N_obs] int<lower=0> n_total_flat;

  // Time series data
  array[T] int<lower=1, upper=12> month;
  array[T] real<lower=0> year_frac;

  // Shock timing parameters
  real shock_2008_onset;
  real shock_2008_peak;
  real shock_2020_onset;
  real shock_2020_peak;

  // Spline configuration
  int<lower=4> K_spline;

  // Threading control
  int<lower=1> grainsize;  // Education levels per thread chunk (typically 1-2)
}

transformed data {
  // Pre-compute shock timing arrays
  array[T] real shock_2008_rise;
  array[T] real shock_2020_rise;
  array[T] real time_since_2008_peak;
  array[T] real time_since_2020_peak;

  // Time range for spline knots
  real t_min = min(year_frac);
  real t_max = max(year_frac);

  // Create knot sequence for B-splines
  int n_internal_knots = K_spline - 4;
  int n_knots = K_spline + 4;
  array[n_knots] real knots;

  // Boundary knots
  for (i in 1:4) {
    knots[i] = t_min;
    knots[n_knots - 4 + i] = t_max;
  }

  // Internal knots
  for (i in 1:n_internal_knots) {
    knots[4 + i] = t_min + i * (t_max - t_min) / (n_internal_knots + 1);
  }

  // Pre-compute spline basis matrix
  matrix[T, K_spline] B_spline;
  for (t in 1:T) {
    for (k in 1:K_spline) {
      B_spline[t, k] = bspline_basis(year_frac[t], knots, k, 4);
    }
  }

  // Shock timing computation
  for (t in 1:T) {
    if (year_frac[t] < shock_2008_onset) {
      shock_2008_rise[t] = 0;
      time_since_2008_peak[t] = 0;
    } else if (year_frac[t] <= shock_2008_peak) {
      shock_2008_rise[t] = (year_frac[t] - shock_2008_onset) /
                           (shock_2008_peak - shock_2008_onset);
      time_since_2008_peak[t] = 0;
    } else {
      shock_2008_rise[t] = 1;
      time_since_2008_peak[t] = year_frac[t] - shock_2008_peak;
    }

    if (year_frac[t] < shock_2020_onset) {
      shock_2020_rise[t] = 0;
      time_since_2020_peak[t] = 0;
    } else if (year_frac[t] <= shock_2020_peak) {
      shock_2020_rise[t] = (year_frac[t] - shock_2020_onset) /
                           (shock_2020_peak - shock_2020_onset);
      time_since_2020_peak[t] = 0;
    } else {
      shock_2020_rise[t] = 1;
      time_since_2020_peak[t] = year_frac[t] - shock_2020_peak;
    }
  }

  // Education indices for reduce_sum (1 to N_edu)
  array[N_edu] int edu_indices;
  for (i in 1:N_edu) {
    edu_indices[i] = i;
  }
}

parameters {
  // === SPLINE COEFFICIENTS FOR DEVIATIONS ===
  matrix[K_spline, N_edu] spline_coef_raw;

  // === HIERARCHICAL SPLINE SMOOTHNESS (NON-CENTERED) ===
  real mu_log_sigma_spline;
  real<lower=0> sigma_log_sigma_spline;
  vector[N_edu] sigma_spline_raw;

  // === NON-CENTERED HIERARCHICAL PARAMETERS ===
  // Equilibrium unemployment rates
  real mu_logit_u_eq;  // Use tight prior instead of hard bounds
  real<lower=0> sigma_logit_u_eq;
  vector[N_edu] u_eq_raw;

  // Adjustment speeds (s+f)
  real mu_log_adj_speed;  // Use tight prior instead of hard bounds
  real<lower=0> sigma_log_adj_speed;
  vector[N_edu] adj_speed_raw;

  // === HIERARCHICAL SHOCK PARAMETERS (NON-CENTERED) ===
  real mu_log_shock_2008;
  real<lower=0> sigma_log_shock_2008;
  vector[N_edu] shock_2008_raw;

  real mu_log_shock_2020;
  real<lower=0> sigma_log_shock_2020;
  vector[N_edu] shock_2020_raw;

  // === HIERARCHICAL DECAY RATES (NON-CENTERED) ===
  real mu_decay_2008;
  real<lower=0> sigma_decay_2008;
  vector[N_edu] decay_2008_raw;

  real mu_decay_2020;
  real<lower=0> sigma_decay_2020;
  vector[N_edu] decay_2020_raw;

  // === HIERARCHICAL SEASONAL EFFECTS (NON-CENTERED) ===
  vector[11] mu_seasonal;
  real<lower=0> sigma_seasonal;
  matrix[11, N_edu] seasonal_u_raw;

  // Initial states
  vector[N_edu] logit_u_init;

  // Dispersion
  real log_phi_minus_1;
}

transformed parameters {
  // === TRANSFORM PARAMETERS ===

  // Equilibrium unemployment rates
  vector[N_edu] logit_u_eq;
  vector<lower=0, upper=1>[N_edu] u_eq;
  for (i in 1:N_edu) {
    logit_u_eq[i] = mu_logit_u_eq + sigma_logit_u_eq * u_eq_raw[i];
    u_eq[i] = inv_logit(logit_u_eq[i]);
  }

  // Adjustment speeds
  vector[N_edu] log_adj_speed;
  vector<lower=0>[N_edu] adj_speed;
  for (i in 1:N_edu) {
    log_adj_speed[i] = mu_log_adj_speed + sigma_log_adj_speed * adj_speed_raw[i];
    adj_speed[i] = exp(log_adj_speed[i]);
  }

  // Derived flow rates with numerical safeguards
  vector<lower=0>[N_edu] separation_rate;
  vector<lower=0>[N_edu] finding_rate;
  for (i in 1:N_edu) {
    // Ensure u_eq is in valid range to prevent numerical issues
    real u_eq_safe = fmin(fmax(u_eq[i], 1e-6), 1 - 1e-6);
    separation_rate[i] = u_eq_safe * adj_speed[i];
    finding_rate[i] = (1 - u_eq_safe) * adj_speed[i];
  }

  // Shock parameters
  vector<lower=0>[N_edu] shock_2008_effect;
  vector<lower=0>[N_edu] shock_2020_effect;
  for (i in 1:N_edu) {
    shock_2008_effect[i] = exp(mu_log_shock_2008 + sigma_log_shock_2008 * shock_2008_raw[i]);
    shock_2020_effect[i] = exp(mu_log_shock_2020 + sigma_log_shock_2020 * shock_2020_raw[i]);
  }

  // Spline smoothness
  vector<lower=0>[N_edu] sigma_spline;
  for (i in 1:N_edu) {
    sigma_spline[i] = exp(mu_log_sigma_spline + sigma_log_sigma_spline * sigma_spline_raw[i]);
  }

  // Decay rates
  vector<lower=0.1, upper=5>[N_edu] decay_2008;
  vector<lower=0.1, upper=5>[N_edu] decay_2020;
  for (i in 1:N_edu) {
    decay_2008[i] = 0.1 + 4.9 * inv_logit(mu_decay_2008 + sigma_decay_2008 * decay_2008_raw[i]);
    decay_2020[i] = 0.1 + 4.9 * inv_logit(mu_decay_2020 + sigma_decay_2020 * decay_2020_raw[i]);
  }

  // Dispersion
  real<lower=1> phi = 1 + exp(log_phi_minus_1);

  // === SPLINE DEVIATIONS ===
  matrix[K_spline, N_edu] spline_coef;
  matrix[T, N_edu] spline_deviation;

  for (i in 1:N_edu) {
    spline_coef[, i] = sigma_spline[i] * spline_coef_raw[, i];
    vector[T] raw_dev = B_spline * spline_coef[, i];
    for (t in 1:T) {
      spline_deviation[t, i] = fmax(-1.0, fmin(1.0, raw_dev[t]));
    }
  }

  // === SEASONAL EFFECTS ===
  matrix[12, N_edu] seasonal_u;
  for (i in 1:N_edu) {
    seasonal_u[1:11, i] = mu_seasonal + sigma_seasonal * seasonal_u_raw[, i];
    seasonal_u[12, i] = -sum(seasonal_u[1:11, i]);
  }
}

model {
  // === PRIORS ===

  // Hierarchical equilibrium unemployment rates - tighter priors to prevent extreme values
  mu_logit_u_eq ~ normal(-3.3, 0.15);  // Tighter prior (was 0.3)
  sigma_logit_u_eq ~ exponential(4);   // Tighter prior (was 2) - smaller variance across education levels
  u_eq_raw ~ std_normal();

  // Hierarchical adjustment speeds - tighter priors to prevent extreme values
  mu_log_adj_speed ~ normal(2.3, 0.25);  // Tighter prior (was 0.5)
  sigma_log_adj_speed ~ exponential(2);  // Tighter prior (was 1) - smaller variance across education levels
  adj_speed_raw ~ std_normal();

  // Hierarchical shock parameters
  mu_log_shock_2008 ~ normal(-2, 0.8);
  sigma_log_shock_2008 ~ exponential(1);
  shock_2008_raw ~ std_normal();

  mu_log_shock_2020 ~ normal(-1.5, 0.8);
  sigma_log_shock_2020 ~ exponential(1);
  shock_2020_raw ~ std_normal();

  // Hierarchical decay rates
  mu_decay_2008 ~ normal(0, 0.5);
  sigma_decay_2008 ~ exponential(1);
  decay_2008_raw ~ std_normal();

  mu_decay_2020 ~ normal(0, 0.5);
  sigma_decay_2020 ~ exponential(1);
  decay_2020_raw ~ std_normal();

  // Hierarchical seasonal effects
  mu_seasonal ~ normal(0, 0.03);
  sum(mu_seasonal) ~ normal(0, 0.001);
  sigma_seasonal ~ exponential(10);
  to_vector(seasonal_u_raw) ~ std_normal();

  // Initial states
  logit_u_init ~ normal(-3.0, 0.5);

  // Spline coefficients
  to_vector(spline_coef_raw) ~ std_normal();

  // Hierarchical spline smoothness
  mu_log_sigma_spline ~ normal(-0.22, 0.4);
  sigma_log_sigma_spline ~ exponential(2);
  sigma_spline_raw ~ std_normal();

  // Overdispersion
  log_phi_minus_1 ~ normal(8.5, 0.5);

  // === LIKELIHOOD (parallelized across education levels) ===
  target += reduce_sum(
    partial_edu_trajectory,
    edu_indices,              // Array of education indices to split
    grainsize,                // Education levels per thread chunk
    n_unemployed_flat, n_total_flat,
    month, year_frac,
    shock_2008_rise, shock_2020_rise,
    time_since_2008_peak, time_since_2020_peak,
    B_spline,
    separation_rate, finding_rate,
    shock_2008_effect, shock_2020_effect,
    decay_2008, decay_2020,
    logit_u_init,
    spline_deviation, seasonal_u,
    phi, T
  );
}

generated quantities {
  // Equilibrium unemployment rates
  vector[N_edu] u_equilibrium = u_eq;

  // Shock half-lives
  vector[N_edu] halflife_2008;
  vector[N_edu] halflife_2020;
  for (i in 1:N_edu) {
    halflife_2008[i] = log(2) / decay_2008[i];
    halflife_2020[i] = log(2) / decay_2020[i];
  }

  // === RECOMPUTE TRAJECTORIES FOR OUTPUT ===
  // (Since u was computed inside reduce_sum, we need to recompute for output)
  array[T] vector[N_edu] u;
  array[T] vector[N_edu] logit_u;
  array[T] vector[N_edu] shock_2008_intensity;
  array[T] vector[N_edu] shock_2020_intensity;

  // Compute shock intensities
  for (t in 1:T) {
    for (i in 1:N_edu) {
      shock_2008_intensity[t][i] = shock_2008_rise[t] *
        exp(-decay_2008[i] * time_since_2008_peak[t]);
      shock_2020_intensity[t][i] = shock_2020_rise[t] *
        exp(-decay_2020[i] * time_since_2020_peak[t]);
    }
  }

  // Initialize
  logit_u[1] = logit_u_init;
  u[1] = inv_logit(logit_u[1]);

  // State evolution
  for (t in 2:T) {
    for (i in 1:N_edu) {
      real s_eff = separation_rate[i]
                   + shock_2008_intensity[t][i] * shock_2008_effect[i]
                   + shock_2020_intensity[t][i] * shock_2020_effect[i];
      real du_dt = s_eff * (1 - u[t-1][i]) - finding_rate[i] * u[t-1][i];
      logit_u[t][i] = logit_u[t-1][i] + du_dt + seasonal_u[month[t], i] +
                       spline_deviation[t, i] - spline_deviation[t-1, i];
    }
    u[t] = inv_logit(logit_u[t]);
  }

  // Trend (without seasonal)
  array[T] vector[N_edu] u_trend;
  {
    array[T] vector[N_edu] logit_u_trend;
    logit_u_trend[1] = logit_u_init;
    u_trend[1] = inv_logit(logit_u_trend[1]);

    for (t in 2:T) {
      for (i in 1:N_edu) {
        real s_eff = separation_rate[i]
                     + shock_2008_intensity[t][i] * shock_2008_effect[i]
                     + shock_2020_intensity[t][i] * shock_2020_effect[i];
        real du_dt = s_eff * (1 - u_trend[t-1][i]) - finding_rate[i] * u_trend[t-1][i];
        logit_u_trend[t][i] = logit_u_trend[t-1][i] + du_dt +
                               spline_deviation[t, i] - spline_deviation[t-1, i];
      }
      u_trend[t] = inv_logit(logit_u_trend[t]);
    }
  }

  // Seasonal effect
  array[T] vector[N_edu] seasonal_effect;
  for (t in 1:T) {
    seasonal_effect[t] = u[t] - u_trend[t];
  }

  // Pure ODE trajectory
  array[T] vector[N_edu] u_ode_pure;
  {
    array[T] vector[N_edu] logit_u_ode;
    logit_u_ode[1] = logit_u_init;
    u_ode_pure[1] = inv_logit(logit_u_ode[1]);

    for (t in 2:T) {
      for (i in 1:N_edu) {
        real s_eff = separation_rate[i]
                     + shock_2008_intensity[t][i] * shock_2008_effect[i]
                     + shock_2020_intensity[t][i] * shock_2020_effect[i];
        real du_dt = s_eff * (1 - u_ode_pure[t-1][i]) - finding_rate[i] * u_ode_pure[t-1][i];
        logit_u_ode[t][i] = logit_u_ode[t-1][i] + du_dt;
      }
      u_ode_pure[t] = inv_logit(logit_u_ode[t]);
    }
  }

  // Log-likelihood for LOO-CV
  array[T, N_edu] real log_lik;
  for (t in 1:T) {
    for (i in 1:N_edu) {
      int obs_idx = (i - 1) * T + t;
      if (n_total_flat[obs_idx] > 0) {
        real u_safe = fmin(fmax(u[t][i], 1e-6), 1 - 1e-6);
        real alpha = u_safe * phi;
        real beta_param = (1 - u_safe) * phi;
        log_lik[t, i] = beta_binomial_lpmf(n_unemployed_flat[obs_idx] | n_total_flat[obs_idx],
                                           alpha, beta_param);
      } else {
        log_lik[t, i] = 0;
      }
    }
  }

  // Posterior predictive
  array[T, N_edu] int n_unemployed_rep;
  for (t in 1:T) {
    for (i in 1:N_edu) {
      int obs_idx = (i - 1) * T + t;
      if (n_total_flat[obs_idx] > 0) {
        real u_safe = fmin(fmax(u[t][i], 1e-6), 1 - 1e-6);
        n_unemployed_rep[t, i] = beta_binomial_rng(n_total_flat[obs_idx],
                                                    u_safe * phi,
                                                    (1 - u_safe) * phi);
      } else {
        n_unemployed_rep[t, i] = 0;
      }
    }
  }
}
