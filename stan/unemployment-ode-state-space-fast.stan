// Fast Unemployment ODE State Space Model (Multithreaded)
//
// Optimized version of the unemployment ODE model with:
// - reduce_sum parallelization for likelihood calculation
// - Vectorized operations where possible
// - Pre-computed transformed data to minimize redundant calculations
// - Efficient memory layout
//
// Requires compilation with STAN_THREADS=true and running with threads_per_chain
//
// Author: Claude Code
// Date: 2026-01-08

functions {
  // Partial sum for likelihood calculation (for reduce_sum parallelization)
  // This function computes the log-likelihood for a slice of time points
  real partial_sum_lpmf(
    array[] int slice_n_unemp,    // Slice of flattened unemployment counts
    int start,                     // Starting index (1-indexed)
    int end,                       // Ending index (1-indexed)
    array[] int n_total_flat,      // Flattened total counts
    array[] real u_flat,           // Flattened latent unemployment rates
    real phi,                      // Beta-binomial dispersion
    int N_edu                      // Number of education levels
  ) {
    real lp = 0;

    for (idx in start:end) {
      if (n_total_flat[idx] > 0) {
        real u_safe = fmin(fmax(u_flat[idx], 1e-6), 1 - 1e-6);
        real alpha = u_safe * phi;
        real beta_param = (1 - u_safe) * phi;
        lp += beta_binomial_lpmf(slice_n_unemp[idx - start + 1] |
                                  n_total_flat[idx], alpha, beta_param);
      }
    }
    return lp;
  }

  // Vectorized inv_logit for transformed parameters
  vector vec_inv_logit(vector x) {
    return inv_logit(x);
  }

  // Soft exponential decay with numerical stability
  real stable_exp_decay(real decay_rate, real time_since_peak) {
    if (time_since_peak <= 0) return 1.0;
    if (decay_rate * time_since_peak > 20) return 0.0;  // Prevent underflow
    return exp(-decay_rate * time_since_peak);
  }
}

data {
  int<lower=1> T;                           // Number of time points
  int<lower=1> N_edu;                       // Number of education levels
  array[T, N_edu] int<lower=0> n_unemployed; // Unemployment counts
  array[T, N_edu] int<lower=0> n_total;      // Total labor force counts
  array[T] int<lower=1, upper=12> month;     // Month indicator (1-12)
  array[T] real<lower=0> year_frac;          // Year as continuous

  // Shock timing parameters
  real shock_2008_onset;
  real shock_2008_peak;
  real shock_2020_onset;
  real shock_2020_peak;

  // Multithreading control
  int<lower=1> grainsize;  // Grainsize for reduce_sum (typically T*N_edu / n_threads)
}

transformed data {
  // Total number of observations for flattened arrays
  int N_obs = T * N_edu;

  // Flatten observation arrays for efficient parallel access
  array[N_obs] int n_unemployed_flat;
  array[N_obs] int n_total_flat;

  // Pre-compute shock rise phase and time since peak (expensive, only compute once)
  array[T] real shock_2008_rise;
  array[T] real shock_2020_rise;
  array[T] real time_since_2008_peak;
  array[T] real time_since_2020_peak;

  // Pre-compute inverse transforms for seasonal constraint
  // (avoid recomputation in model block)

  // Flatten arrays column-major (education-major): [t=1,edu=1], [t=1,edu=2], ...
  for (t in 1:T) {
    for (i in 1:N_edu) {
      int idx = (t - 1) * N_edu + i;
      n_unemployed_flat[idx] = n_unemployed[t, i];
      n_total_flat[idx] = n_total[t, i];
    }
  }

  // Compute shock timing (vectorized where possible)
  for (t in 1:T) {
    // 2008 shock
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

    // 2020 shock
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
}

parameters {
  // Initial latent states
  vector[N_edu] logit_u_init;

  // State innovations (using matrix for memory efficiency)
  matrix[T-1, N_edu] logit_u_innov_mat;

  // === HIERARCHICAL RATES (Non-Centered) ===
  real mu_logit_separation;
  real<lower=0> sigma_logit_separation;
  vector[N_edu] separation_raw;

  real mu_logit_finding;
  real<lower=0> sigma_logit_finding;
  vector[N_edu] finding_raw;

  // Shock effects (log scale)
  vector[N_edu] log_shock_2008_effect;
  vector[N_edu] log_shock_2020_effect;

  // Decay rates (unbounded, transform to [0.1, 5])
  vector[N_edu] decay_2008_raw;
  vector[N_edu] decay_2020_raw;

  // Seasonal (11 free parameters per education, 12th = -sum)
  matrix[11, N_edu] seasonal_u_raw;

  // Noise and dispersion (log scale)
  real log_sigma_state;
  real log_phi_minus_1;
}

transformed parameters {
  // === TRANSFORM PARAMETERS (Vectorized) ===

  // Separation rates: non-centered transform
  vector[N_edu] logit_separation = mu_logit_separation +
                                    sigma_logit_separation * separation_raw;
  vector[N_edu] separation_rate = 0.2 * inv_logit(logit_separation);

  // Finding rates: non-centered transform
  vector[N_edu] logit_finding = mu_logit_finding +
                                 sigma_logit_finding * finding_raw;
  vector[N_edu] finding_rate = inv_logit(logit_finding);

  // Shock effects (log-normal)
  vector[N_edu] shock_2008_effect = exp(log_shock_2008_effect);
  vector[N_edu] shock_2020_effect = exp(log_shock_2020_effect);

  // Decay rates: [0.1, 5] range
  vector[N_edu] decay_2008 = 0.1 + 4.9 * inv_logit(decay_2008_raw);
  vector[N_edu] decay_2020 = 0.1 + 4.9 * inv_logit(decay_2020_raw);

  // State noise and dispersion
  real sigma_state = exp(log_sigma_state);
  real phi = 1 + exp(log_phi_minus_1);

  // Seasonal with sum-to-zero (using matrix operations)
  matrix[12, N_edu] seasonal_u;
  seasonal_u[1:11, ] = seasonal_u_raw;
  for (i in 1:N_edu) {
    seasonal_u[12, i] = -sum(seasonal_u_raw[, i]);
  }

  // === STATE DYNAMICS ===
  // Store latent rates and logit scale
  matrix[T, N_edu] logit_u_mat;
  matrix[T, N_edu] u_mat;

  // Flattened u for likelihood (matches n_unemployed_flat layout)
  array[T * N_edu] real u_flat;

  // Initialize first time point
  logit_u_mat[1, ] = logit_u_init';
  u_mat[1, ] = inv_logit(logit_u_mat[1, ]);

  // State evolution with vectorization where possible
  for (t in 2:T) {
    // Compute shock intensities for this time point (vectorized over education)
    vector[N_edu] shock_2008_int;
    vector[N_edu] shock_2020_int;

    for (i in 1:N_edu) {
      shock_2008_int[i] = shock_2008_rise[t] *
                          stable_exp_decay(decay_2008[i], time_since_2008_peak[t]);
      shock_2020_int[i] = shock_2020_rise[t] *
                          stable_exp_decay(decay_2020[i], time_since_2020_peak[t]);
    }

    // Effective separation rate (vectorized)
    vector[N_edu] s_eff = separation_rate +
                           shock_2008_int .* shock_2008_effect +
                           shock_2020_int .* shock_2020_effect;

    // ODE dynamics (vectorized)
    vector[N_edu] u_prev = to_vector(u_mat[t-1, ]);
    vector[N_edu] du_dt = s_eff .* (1 - u_prev) - finding_rate .* u_prev;

    // Seasonal effects for this month
    vector[N_edu] seasonal_t = to_vector(seasonal_u[month[t], ]);

    // State evolution (vectorized)
    logit_u_mat[t, ] = (to_vector(logit_u_mat[t-1, ]) + du_dt + seasonal_t +
                        to_vector(logit_u_innov_mat[t-1, ]))';
    u_mat[t, ] = inv_logit(logit_u_mat[t, ]);
  }

  // Flatten u_mat for parallel likelihood (column-major to match n_unemployed_flat)
  for (t in 1:T) {
    for (i in 1:N_edu) {
      u_flat[(t-1) * N_edu + i] = u_mat[t, i];
    }
  }
}

model {
  // === PRIORS ===

  // Hierarchical separation rate
  mu_logit_separation ~ normal(-2.2, 0.5);
  sigma_logit_separation ~ exponential(2);
  separation_raw ~ std_normal();

  // Hierarchical finding rate
  mu_logit_finding ~ normal(-0.85, 0.5);
  sigma_logit_finding ~ exponential(2);
  finding_raw ~ std_normal();

  // Shock effects (log-normal priors)
  log_shock_2008_effect ~ normal(-3.9, 0.5);
  log_shock_2020_effect ~ normal(-3.5, 0.5);

  // Decay rates
  decay_2008_raw ~ normal(0, 1);
  decay_2020_raw ~ normal(0.5, 1);

  // Seasonal effects
  to_vector(seasonal_u_raw) ~ normal(0, 0.05);

  // Initial states
  logit_u_init ~ normal(-3.5, 0.5);

  // State noise
  log_sigma_state ~ normal(-3, 1);

  // State innovations (vectorized)
  to_vector(logit_u_innov_mat) ~ normal(0, sigma_state);

  // Dispersion
  log_phi_minus_1 ~ normal(3, 1);

  // === LIKELIHOOD (Parallelized) ===
  target += reduce_sum(partial_sum_lpmf, n_unemployed_flat, grainsize,
                       n_total_flat, u_flat, phi, N_edu);
}

generated quantities {
  // Equilibrium unemployment rates
  vector[N_edu] u_equilibrium;
  for (i in 1:N_edu) {
    u_equilibrium[i] = separation_rate[i] / (separation_rate[i] + finding_rate[i]);
  }

  // Shock half-lives
  vector[N_edu] halflife_2008 = log(2.0) ./ decay_2008;
  vector[N_edu] halflife_2020 = log(2.0) ./ decay_2020;

  // Non-seasonal trend (same logic but vectorized)
  matrix[T, N_edu] u_trend_mat;
  {
    matrix[T, N_edu] logit_u_trend;
    logit_u_trend[1, ] = logit_u_init';
    u_trend_mat[1, ] = inv_logit(logit_u_trend[1, ]);

    for (t in 2:T) {
      vector[N_edu] shock_2008_int;
      vector[N_edu] shock_2020_int;

      for (i in 1:N_edu) {
        shock_2008_int[i] = shock_2008_rise[t] *
                            stable_exp_decay(decay_2008[i], time_since_2008_peak[t]);
        shock_2020_int[i] = shock_2020_rise[t] *
                            stable_exp_decay(decay_2020[i], time_since_2020_peak[t]);
      }

      vector[N_edu] s_eff = separation_rate +
                             shock_2008_int .* shock_2008_effect +
                             shock_2020_int .* shock_2020_effect;

      vector[N_edu] u_prev = to_vector(u_trend_mat[t-1, ]);
      vector[N_edu] du_dt = s_eff .* (1 - u_prev) - finding_rate .* u_prev;

      // NO seasonal, but SAME innovations
      logit_u_trend[t, ] = (to_vector(logit_u_trend[t-1, ]) + du_dt +
                            to_vector(logit_u_innov_mat[t-1, ]))';
      u_trend_mat[t, ] = inv_logit(logit_u_trend[t, ]);
    }
  }

  // Seasonal effect magnitude
  matrix[T, N_edu] seasonal_effect_mat = u_mat - u_trend_mat;

  // Log-likelihood (for LOO-CV, must match flattened layout)
  array[T, N_edu] real log_lik;
  for (t in 1:T) {
    for (i in 1:N_edu) {
      if (n_total[t, i] > 0) {
        real u_safe = fmin(fmax(u_mat[t, i], 1e-6), 1 - 1e-6);
        real alpha = u_safe * phi;
        real beta_param = (1 - u_safe) * phi;
        log_lik[t, i] = beta_binomial_lpmf(n_unemployed[t, i] | n_total[t, i],
                                           alpha, beta_param);
      } else {
        log_lik[t, i] = 0;
      }
    }
  }

  // Posterior predictive samples
  array[T, N_edu] int n_unemployed_rep;
  for (t in 1:T) {
    for (i in 1:N_edu) {
      if (n_total[t, i] > 0) {
        real u_safe = fmin(fmax(u_mat[t, i], 1e-6), 1 - 1e-6);
        real alpha = u_safe * phi;
        real beta_param = (1 - u_safe) * phi;
        n_unemployed_rep[t, i] = beta_binomial_rng(n_total[t, i], alpha, beta_param);
      } else {
        n_unemployed_rep[t, i] = 0;
      }
    }
  }
}
