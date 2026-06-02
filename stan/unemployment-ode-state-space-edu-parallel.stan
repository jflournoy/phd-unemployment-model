// Unemployment ODE State Space Model - Education-Level Parallelization
// TIME-VARYING EQUILIBRIUM VERSION
//
// Architecture:
//   u_eq[t] is a smooth time-varying equilibrium (B-spline × coefs)
//   ODE: du/dt = s_eff*(1-u) - f[t]*u
//     where s_eff = s[t] + shocks
//           s[t] = u_eq[t] * adj_speed
//           f[t] = (1 - u_eq[t]) * adj_speed
//   The ODE pulls u toward u_eq[t], shocks create transitory deviations,
//   seasonal adds monthly patterns.
//
// KEY INSIGHT: u_eq[t] absorbs slow structural trends. No separate spline
// kludge. Shocks are cleanly identified as temporary deviations from
// the time-varying baseline.
//
// PARALLELIZATION: across education levels via reduce_sum
//
// Author: Claude Code
// Date: 2026-06-02

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
  real partial_edu_trajectory(
    array[] int edu_slice,
    int start,
    int end,
    // Flattened observation data
    array[] int n_unemployed_flat,
    array[] int n_total_flat,
    // Time series data
    array[] int month,
    array[] real year_frac,
    // Pre-computed shock timing
    array[] real shock_2008_rise,
    array[] real shock_2020_rise,
    array[] real time_since_2008_peak,
    array[] real time_since_2020_peak,
    // Time-varying equilibrium (pre-computed in transformed parameters)
    matrix logit_u_eq,       // [T, N_edu]
    // Per-education parameters
    vector adj_speed,
    vector shock_2008_effect,
    vector shock_2020_effect,
    vector decay_2008,
    vector decay_2020,
    vector logit_u_init,
    matrix seasonal_u,       // [12, N_edu]
    real phi,
    int T
  ) {
    real lp = 0;

    for (slice_idx in 1:size(edu_slice)) {
      int edu = edu_slice[slice_idx];

      real logit_u_curr = logit_u_init[edu];
      real u_curr = inv_logit(logit_u_curr);

      // First time point
      {
        int obs_idx = (edu - 1) * T + 1;
        if (n_total_flat[obs_idx] > 0) {
          real u_safe = fmin(fmax(u_curr, 1e-6), 1 - 1e-6);
          real alpha = u_safe * phi;
          real beta_param = (1 - u_safe) * phi;
          lp += beta_binomial_lpmf(n_unemployed_flat[obs_idx] | n_total_flat[obs_idx],
                                   alpha, beta_param);
        }
      }

      // Time evolution
      for (t in 2:T) {
        // Time-varying equilibrium at time t
        real u_eq_t = inv_logit(logit_u_eq[t, edu]);
        real u_eq_safe = fmin(fmax(u_eq_t, 1e-6), 1 - 1e-6);

        // Flow rates from time-varying equilibrium
        real s_base = u_eq_safe * adj_speed[edu];
        real f_base = (1 - u_eq_safe) * adj_speed[edu];

        // Shock intensities
        real shock_2008_intensity = shock_2008_rise[t] *
          exp(-decay_2008[edu] * time_since_2008_peak[t]);
        real shock_2020_intensity = shock_2020_rise[t] *
          exp(-decay_2020[edu] * time_since_2020_peak[t]);

        // Effective separation rate
        real s_eff = s_base
                     + shock_2008_intensity * shock_2008_effect[edu]
                     + shock_2020_intensity * shock_2020_effect[edu];

        // ODE: du/dt = s_eff*(1-u) - f*u
        real du_dt = s_eff * (1 - u_curr) - f_base * u_curr;

        // State evolution: ODE + seasonal (no separate spline!)
        logit_u_curr = logit_u_curr + du_dt + seasonal_u[month[t], edu];
        u_curr = inv_logit(logit_u_curr);

        // Likelihood
        int obs_idx = (edu - 1) * T + t;
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
  int<lower=1> T;
  int<lower=1> N_edu;
  int<lower=1> N_obs;

  array[N_obs] int<lower=0> n_unemployed_flat;
  array[N_obs] int<lower=0> n_total_flat;

  array[T] int<lower=1, upper=12> month;
  array[T] real<lower=0> year_frac;

  real shock_2008_onset;
  real shock_2008_peak;
  real shock_2020_onset;
  real shock_2020_peak;

  int<lower=4> K_spline;
  int<lower=1> grainsize;
}

transformed data {
  // Pre-compute shock timing arrays
  array[T] real shock_2008_rise;
  array[T] real shock_2020_rise;
  array[T] real time_since_2008_peak;
  array[T] real time_since_2020_peak;

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

  // Pre-compute B-spline basis matrix
  real t_min = min(year_frac);
  real t_max = max(year_frac) + 1e-6;
  int n_internal_knots = K_spline - 4;
  int n_knots = K_spline + 4;
  array[n_knots] real knots;

  for (i in 1:4) {
    knots[i] = t_min;
    knots[n_knots - 4 + i] = t_max;
  }
  for (i in 1:n_internal_knots) {
    knots[4 + i] = t_min + i * (t_max - t_min) / (n_internal_knots + 1);
  }

  matrix[T, K_spline] B_spline;
  for (t in 1:T) {
    for (k in 1:K_spline) {
      B_spline[t, k] = bspline_basis(year_frac[t], knots, k, 4);
    }
  }

  // Education indices for reduce_sum
  array[N_edu] int edu_indices;
  for (i in 1:N_edu) {
    edu_indices[i] = i;
  }
}

parameters {
  // === TIME-VARYING EQUILIBRIUM (SPLINE COEFFICIENTS) ===
  matrix[K_spline, N_edu] u_eq_coef_raw;
  vector<lower=0>[N_edu] sigma_u_eq_spline;  // Smoothness per edu

  // === HIERARCHICAL ADJUSTMENT SPEEDS (NON-CENTERED) ===
  real mu_log_adj_speed;
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
  // === TIME-VARYING EQUILIBRIUM ===
  matrix[T, N_edu] logit_u_eq;
  matrix[T, N_edu] u_eq;
  for (i in 1:N_edu) {
    logit_u_eq[, i] = B_spline * u_eq_coef_raw[, i];
    u_eq[, i] = inv_logit(logit_u_eq[, i]);
  }

  // === ADJUSTMENT SPEEDS ===
  vector[N_edu] log_adj_speed;
  vector<lower=0>[N_edu] adj_speed;
  for (i in 1:N_edu) {
    log_adj_speed[i] = mu_log_adj_speed + sigma_log_adj_speed * adj_speed_raw[i];
    adj_speed[i] = exp(log_adj_speed[i]);
  }

  // === SHOCK PARAMETERS ===
  vector<lower=0>[N_edu] shock_2008_effect;
  vector<lower=0>[N_edu] shock_2020_effect;
  for (i in 1:N_edu) {
    shock_2008_effect[i] = exp(mu_log_shock_2008 + sigma_log_shock_2008 * shock_2008_raw[i]);
    shock_2020_effect[i] = exp(mu_log_shock_2020 + sigma_log_shock_2020 * shock_2020_raw[i]);
  }

  // === DECAY RATES ===
  vector<lower=0.1, upper=5>[N_edu] decay_2008;
  vector<lower=0.1, upper=5>[N_edu] decay_2020;
  for (i in 1:N_edu) {
    decay_2008[i] = 0.1 + 4.9 * inv_logit(mu_decay_2008 + sigma_decay_2008 * decay_2008_raw[i]);
    decay_2020[i] = 0.1 + 4.9 * inv_logit(mu_decay_2020 + sigma_decay_2020 * decay_2020_raw[i]);
  }

  // === DISPERSION ===
  real<lower=1> phi = 1 + exp(log_phi_minus_1);

  // === SEASONAL EFFECTS ===
  matrix[12, N_edu] seasonal_u;
  for (i in 1:N_edu) {
    seasonal_u[1:11, i] = mu_seasonal + sigma_seasonal * seasonal_u_raw[, i];
    seasonal_u[12, i] = -sum(seasonal_u[1:11, i]);
  }
}

model {
  // === PRIORS ===

  // Time-varying equilibrium spline: RW prior for smoothness
  for (i in 1:N_edu) {
    u_eq_coef_raw[1, i] ~ normal(-3.0, 1.0);  // Initial level centered at ~5%
    for (k in 2:K_spline) {
      u_eq_coef_raw[k, i] ~ normal(u_eq_coef_raw[k-1, i], sigma_u_eq_spline[i]);
    }
  }
  sigma_u_eq_spline ~ exponential(3);  // Moderate smoothness

  // Adjustment speeds
  mu_log_adj_speed ~ normal(2.3, 0.25);
  sigma_log_adj_speed ~ exponential(20);
  adj_speed_raw ~ std_normal();

  // Shock effects
  mu_log_shock_2008 ~ normal(-2, 0.8);
  sigma_log_shock_2008 ~ exponential(20);
  shock_2008_raw ~ std_normal();

  mu_log_shock_2020 ~ normal(-1.5, 0.8);
  sigma_log_shock_2020 ~ exponential(20);
  shock_2020_raw ~ std_normal();

  // Decay rates
  mu_decay_2008 ~ normal(0, 0.5);
  sigma_decay_2008 ~ exponential(5);
  decay_2008_raw ~ std_normal();

  mu_decay_2020 ~ normal(0, 0.5);
  sigma_decay_2020 ~ exponential(5);
  decay_2020_raw ~ std_normal();

  // Seasonal effects
  mu_seasonal ~ normal(0, 0.03);
  sum(mu_seasonal) ~ normal(0, 0.001);
  sigma_seasonal ~ exponential(10);
  to_vector(seasonal_u_raw) ~ std_normal();

  // Initial states
  logit_u_init ~ normal(-3.0, 0.5);

  // Dispersion
  log_phi_minus_1 ~ normal(8.5, 0.5);

  // === LIKELIHOOD (parallelized across education levels) ===
  target += reduce_sum(
    partial_edu_trajectory,
    edu_indices,
    grainsize,
    n_unemployed_flat, n_total_flat,
    month, year_frac,
    shock_2008_rise, shock_2020_rise,
    time_since_2008_peak, time_since_2020_peak,
    logit_u_eq,
    adj_speed,
    shock_2008_effect, shock_2020_effect,
    decay_2008, decay_2020,
    logit_u_init,
    seasonal_u,
    phi, T
  );
}

generated quantities {
  // Mean equilibrium over time (summary for reporting)
  vector[N_edu] u_eq_mean;
  for (i in 1:N_edu) {
    u_eq_mean[i] = mean(u_eq[, i]);
  }

  // Shock half-lives
  vector[N_edu] halflife_2008;
  vector[N_edu] halflife_2020;
  for (i in 1:N_edu) {
    halflife_2008[i] = log(2) / decay_2008[i];
    halflife_2020[i] = log(2) / decay_2020[i];
  }

  // === RECOMPUTE TRAJECTORIES ===
  array[T] vector[N_edu] u;
  array[T] vector[N_edu] logit_u;
  array[T] vector[N_edu] shock_2008_intensity;
  array[T] vector[N_edu] shock_2020_intensity;

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

  // State evolution (matching partial function exactly)
  for (t in 2:T) {
    for (i in 1:N_edu) {
      real u_eq_t = u_eq[t, i];
      real u_eq_safe = fmin(fmax(u_eq_t, 1e-6), 1 - 1e-6);
      real s_base = u_eq_safe * adj_speed[i];
      real f_base = (1 - u_eq_safe) * adj_speed[i];
      real s_eff = s_base
                   + shock_2008_intensity[t][i] * shock_2008_effect[i]
                   + shock_2020_intensity[t][i] * shock_2020_effect[i];
      real du_dt = s_eff * (1 - u[t-1][i]) - f_base * u[t-1][i];
      logit_u[t][i] = logit_u[t-1][i] + du_dt + seasonal_u[month[t], i];
    }
    u[t] = inv_logit(logit_u[t]);
  }

  // Trend (equilibrium + shocks, no seasonal)
  array[T] vector[N_edu] u_trend;
  {
    array[T] vector[N_edu] logit_u_trend;
    logit_u_trend[1] = logit_u_init;
    u_trend[1] = inv_logit(logit_u_trend[1]);

    for (t in 2:T) {
      for (i in 1:N_edu) {
        real u_eq_t = u_eq[t, i];
        real u_eq_safe = fmin(fmax(u_eq_t, 1e-6), 1 - 1e-6);
        real s_base = u_eq_safe * adj_speed[i];
        real f_base = (1 - u_eq_safe) * adj_speed[i];
        real s_eff = s_base
                     + shock_2008_intensity[t][i] * shock_2008_effect[i]
                     + shock_2020_intensity[t][i] * shock_2020_effect[i];
        real du_dt = s_eff * (1 - u_trend[t-1][i]) - f_base * u_trend[t-1][i];
        logit_u_trend[t][i] = logit_u_trend[t-1][i] + du_dt;
      }
      u_trend[t] = inv_logit(logit_u_trend[t]);
    }
  }

  // Seasonal effect
  array[T] vector[N_edu] seasonal_effect;
  for (t in 1:T) {
    seasonal_effect[t] = u[t] - u_trend[t];
  }

  // Log-likelihood for LOO-CV
  array[T, N_edu] real log_lik;
  for (t in 1:T) {
    for (i in 1:N_edu) {
      int obs_idx = (i - 1) * T + t;
      if (n_total_flat[obs_idx] > 0) {
        real u_safe = fmin(fmax(u[t][i], 1e-6), 1 - 1e-6);
        log_lik[t, i] = beta_binomial_lpmf(n_unemployed_flat[obs_idx] | n_total_flat[obs_idx],
                                           u_safe * phi, (1 - u_safe) * phi);
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
                                                    u_safe * phi, (1 - u_safe) * phi);
      } else {
        n_unemployed_rep[t, i] = 0;
      }
    }
  }
}
