// Unemployment ODE State Space Model - Efficient Version
//
// This version addresses the max treedepth issue by:
// 1. Using SPLINE BASIS for latent deviations instead of per-timepoint innovations
// 2. Dramatically reducing dimensionality: K_spline << T parameters per education level
// 3. The ODE dynamics + shocks + seasonality provide the main structure
// 4. Spline deviations capture residual low-frequency variation
//
// Key insight: Most variation is explained by the structural model (ODE + shocks).
// Residual variation is smooth, so K=20 spline basis functions capture it well.
//
// Author: Claude Code
// Date: 2026-01-09

functions {
  // B-spline basis function (order 4 = cubic)
  // t: evaluation point, knots: knot sequence, k: basis index
  real bspline_basis(real t, array[] real knots, int k, int order) {
    if (order == 1) {
      // Base case: indicator function
      if (t >= knots[k] && t < knots[k + 1]) {
        return 1.0;
      } else {
        return 0.0;
      }
    } else {
      // Recursive formula
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
}

data {
  int<lower=1> T;                           // Number of time points
  int<lower=1> N_edu;                       // Number of education levels
  array[T, N_edu] int<lower=0> n_unemployed; // Unemployment counts
  array[T, N_edu] int<lower=0> n_total;      // Total labor force counts
  array[T] int<lower=1, upper=12> month;     // Month indicator (1-12)
  array[T] real<lower=0> year_frac;          // Year as continuous (e.g., 2020.5)

  // Shock timing parameters
  real shock_2008_onset;
  real shock_2008_peak;
  real shock_2020_onset;
  real shock_2020_peak;

  // Spline configuration
  int<lower=4> K_spline;   // Number of spline basis functions (e.g., 20)
}

transformed data {
  // Pre-compute shock timing
  array[T] real shock_2008_rise;
  array[T] real shock_2020_rise;
  array[T] real time_since_2008_peak;
  array[T] real time_since_2020_peak;

  // Time range for spline knots
  real t_min = min(year_frac);
  real t_max = max(year_frac);

  // Create knot sequence for B-splines (uniform, with boundary padding)
  int n_internal_knots = K_spline - 4;  // For cubic splines
  int n_knots = K_spline + 4;  // Total knots including boundaries
  array[n_knots] real knots;

  // Boundary knots (repeated for cubic spline)
  for (i in 1:4) {
    knots[i] = t_min;
    knots[n_knots - 4 + i] = t_max;
  }

  // Internal knots (evenly spaced)
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
  // === SPLINE COEFFICIENTS FOR DEVIATIONS ===
  // K_spline coefficients per education level (much smaller than T-1 innovations!)
  matrix[K_spline, N_edu] spline_coef_raw;  // Standard normal raw coefficients

  // === HIERARCHICAL SPLINE SMOOTHNESS (NON-CENTERED) ===
  // Pool spline smoothness across education levels
  real mu_log_sigma_spline;                 // Population mean on log scale
  real<lower=0> sigma_log_sigma_spline;     // Between-education SD
  vector[N_edu] sigma_spline_raw;           // Non-centered deviations

  // === NON-CENTERED HIERARCHICAL PARAMETERS ===
  //
  // REPARAMETERIZATION FOR IDENTIFICATION:
  // Instead of separate separation (s) and finding (f) rates, we estimate:
  //   u_eq = equilibrium unemployment = s/(s+f)  [well-identified from data]
  //   adj_speed = adjustment speed = s+f         [affects dynamics]
  // Then derive: s = u_eq * adj_speed, f = (1-u_eq) * adj_speed

  // Hierarchical equilibrium unemployment rates
  real mu_logit_u_eq;                   // Population mean on logit scale
  real<lower=0> sigma_logit_u_eq;       // Between-education SD
  vector[N_edu] u_eq_raw;               // Non-centered deviations

  // Hierarchical adjustment speeds (s+f)
  real mu_log_adj_speed;                // Population mean on log scale
  real<lower=0> sigma_log_adj_speed;    // Between-education SD
  vector[N_edu] adj_speed_raw;          // Non-centered deviations

  // === HIERARCHICAL SHOCK PARAMETERS (NON-CENTERED) ===
  // Pooling shock magnitudes across education levels within each shock event
  // Use unbounded parameterization for better MCMC geometry

  // 2008 shock magnitude (hierarchical)
  real mu_log_shock_2008;                 // Population mean on log scale
  real<lower=0> sigma_log_shock_2008;     // Between-education SD
  vector[N_edu] shock_2008_raw;           // Non-centered deviations

  // 2020 shock magnitude (hierarchical)
  real mu_log_shock_2020;                 // Population mean on log scale
  real<lower=0> sigma_log_shock_2020;     // Between-education SD
  vector[N_edu] shock_2020_raw;           // Non-centered deviations

  // === HIERARCHICAL DECAY RATES (NON-CENTERED) ===
  // Pool recovery dynamics across education levels
  real mu_decay_2008;                     // Population mean on logit scale
  real<lower=0> sigma_decay_2008;         // Between-education SD
  vector[N_edu] decay_2008_raw;           // Non-centered deviations

  real mu_decay_2020;                     // Population mean on logit scale
  real<lower=0> sigma_decay_2020;         // Between-education SD
  vector[N_edu] decay_2020_raw;           // Non-centered deviations

  // === HIERARCHICAL SEASONAL EFFECTS (NON-CENTERED) ===
  // Pool seasonal patterns across education levels
  // Sum-to-zero constraint: only model 11 months, 12th is -sum(first 11)
  vector[11] mu_seasonal;                 // Population-level mean seasonal pattern
  real<lower=0> sigma_seasonal;           // Between-education SD for seasonal effects
  matrix[11, N_edu] seasonal_u_raw;       // Non-centered deviations

  // Initial latent unemployment rates (not hierarchical - equilibrium handles this)
  vector[N_edu] logit_u_init;

  // Beta-binomial dispersion
  real log_phi_minus_1;
}

transformed parameters {
  // === TRANSFORM PARAMETERS ===

  // Equilibrium unemployment rates (the key identifiable quantity)
  vector[N_edu] logit_u_eq;
  vector<lower=0, upper=1>[N_edu] u_eq;
  for (i in 1:N_edu) {
    logit_u_eq[i] = mu_logit_u_eq + sigma_logit_u_eq * u_eq_raw[i];
    u_eq[i] = inv_logit(logit_u_eq[i]);
  }

  // Adjustment speeds (s+f) - how fast the system returns to equilibrium
  vector[N_edu] log_adj_speed;
  vector<lower=0>[N_edu] adj_speed;
  for (i in 1:N_edu) {
    log_adj_speed[i] = mu_log_adj_speed + sigma_log_adj_speed * adj_speed_raw[i];
    adj_speed[i] = exp(log_adj_speed[i]);
  }

  // DERIVED flow rates (internally consistent by construction)
  vector<lower=0>[N_edu] separation_rate;
  vector<lower=0>[N_edu] finding_rate;
  for (i in 1:N_edu) {
    separation_rate[i] = u_eq[i] * adj_speed[i];
    finding_rate[i] = (1 - u_eq[i]) * adj_speed[i];
  }

  // Transform shock parameters (non-centered hierarchical)
  vector[N_edu] log_shock_2008_effect;
  vector[N_edu] log_shock_2020_effect;
  vector<lower=0>[N_edu] shock_2008_effect;
  vector<lower=0>[N_edu] shock_2020_effect;
  for (i in 1:N_edu) {
    log_shock_2008_effect[i] = mu_log_shock_2008 + sigma_log_shock_2008 * shock_2008_raw[i];
    log_shock_2020_effect[i] = mu_log_shock_2020 + sigma_log_shock_2020 * shock_2020_raw[i];
    shock_2008_effect[i] = exp(log_shock_2008_effect[i]);
    shock_2020_effect[i] = exp(log_shock_2020_effect[i]);
  }

  // Spline smoothness: transform with hierarchical pooling
  vector<lower=0>[N_edu] sigma_spline;
  for (i in 1:N_edu) {
    // Non-centered hierarchical: exp(mu + sigma * raw)
    sigma_spline[i] = exp(mu_log_sigma_spline + sigma_log_sigma_spline * sigma_spline_raw[i]);
  }

  // Decay rates: transform from unbounded to [0.1, 5] with hierarchical pooling
  vector<lower=0.1, upper=5>[N_edu] decay_2008;
  vector<lower=0.1, upper=5>[N_edu] decay_2020;
  for (i in 1:N_edu) {
    // Non-centered hierarchical: mu + sigma * raw
    decay_2008[i] = 0.1 + 4.9 * inv_logit(mu_decay_2008 + sigma_decay_2008 * decay_2008_raw[i]);
    decay_2020[i] = 0.1 + 4.9 * inv_logit(mu_decay_2020 + sigma_decay_2020 * decay_2020_raw[i]);
  }

  // Dispersion
  real<lower=1> phi = 1 + exp(log_phi_minus_1);

  // === SPLINE DEVIATIONS ===
  // Smooth deviations from ODE trajectory
  // Bounded to prevent extreme values that cause numerical overflow
  matrix[K_spline, N_edu] spline_coef;
  matrix[T, N_edu] spline_deviation;

  for (i in 1:N_edu) {
    spline_coef[, i] = sigma_spline[i] * spline_coef_raw[, i];
    // Compute spline deviation and bound to prevent extreme values
    vector[T] raw_dev = B_spline * spline_coef[, i];
    for (t in 1:T) {
      // Bound deviations to ±1 on logit scale (prevents extreme u values)
      spline_deviation[t, i] = fmax(-1.0, fmin(1.0, raw_dev[t]));
    }
  }

  // === LATENT STATE DYNAMICS ===

  array[T] vector<lower=0, upper=1>[N_edu] u;
  matrix[12, N_edu] seasonal_u;
  array[T] vector[N_edu] logit_u;
  array[T] vector[N_edu] shock_2008_intensity;
  array[T] vector[N_edu] shock_2020_intensity;

  // Hierarchical seasonal effects with sum-to-zero
  // Non-centered parameterization: seasonal = mu + sigma * raw
  for (i in 1:N_edu) {
    seasonal_u[1:11, i] = mu_seasonal + sigma_seasonal * seasonal_u_raw[, i];
    seasonal_u[12, i] = -sum(seasonal_u[1:11, i]);
  }

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

  // State evolution with spline deviations
  for (t in 2:T) {
    for (i in 1:N_edu) {
      // Effective separation rate
      real s_eff = separation_rate[i]
                   + shock_2008_intensity[t][i] * shock_2008_effect[i]
                   + shock_2020_intensity[t][i] * shock_2020_effect[i];

      // ODE dynamics
      real du_dt = s_eff * (1 - u[t-1][i]) - finding_rate[i] * u[t-1][i];

      // State evolution: ODE + seasonal + smooth spline deviation
      logit_u[t][i] = logit_u[t-1][i] + du_dt + seasonal_u[month[t], i] +
                       spline_deviation[t, i] - spline_deviation[t-1, i];
    }
    u[t] = inv_logit(logit_u[t]);
  }
}

model {
  // === INFORMATIVE PRIORS ===
  //
  // REPARAMETERIZED MODEL: We estimate u_eq and adj_speed, not s and f directly
  //
  // Equilibrium unemployment by education (historical averages):
  //   - PhD: ~1.5% → logit(0.015) ≈ -4.2
  //   - Professional: ~1.5%
  //   - Masters: ~2.5% → logit(0.025) ≈ -3.7
  //   - Bachelors: ~3% → logit(0.03) ≈ -3.5
  //   - Some College: ~4% → logit(0.04) ≈ -3.2
  //   - High School: ~5% → logit(0.05) ≈ -2.9
  //   - Less than HS: ~7% → logit(0.07) ≈ -2.6
  //
  // Adjustment speed (s+f): Controls how fast unemployment responds
  //   - Typical monthly s ≈ 2%, f ≈ 30%, so s+f ≈ 0.32
  //   - log(0.32) ≈ -1.1

  // Hierarchical equilibrium unemployment rates
  // Prior centers on ~3.5% average equilibrium (logit ≈ -3.3)
  mu_logit_u_eq ~ normal(-3.3, 0.3);
  sigma_logit_u_eq ~ exponential(2);  // Allow education differences
  u_eq_raw ~ std_normal();

  // Hierarchical adjustment speeds
  // DATA-INFORMED: Posterior shows adj_speed 2-30, median ~10
  // Note: rates are implicitly scaled by 1/(u*(1-u)) ≈ 30 due to logit dynamics
  // Prior: exp(2.3) ≈ 10, range [3, 30] at 95%
  mu_log_adj_speed ~ normal(2.3, 0.5);
  sigma_log_adj_speed ~ exponential(1);  // Allow substantial education variation
  adj_speed_raw ~ std_normal();

  // === HIERARCHICAL SHOCK PARAMETERS ===
  // Pooling shock magnitudes across education levels
  // Updated priors based on posterior from initial fit

  // 2008 shock: exp(-2) ≈ 0.14 (14% increase in separation rate)
  mu_log_shock_2008 ~ normal(-2, 0.8);         // Population mean (data-informed)
  sigma_log_shock_2008 ~ exponential(1);       // Between-education SD (allow ~1)
  shock_2008_raw ~ std_normal();               // Non-centered deviations

  // 2020 shock: exp(-1.3) ≈ 0.27 (27% increase in separation rate)
  mu_log_shock_2020 ~ normal(-1.5, 0.8);       // Population mean (data-informed)
  sigma_log_shock_2020 ~ exponential(1);       // Between-education SD (allow ~1)
  shock_2020_raw ~ std_normal();               // Non-centered deviations

  // === HIERARCHICAL DECAY RATES ===
  // Pool recovery dynamics across education levels
  // mu=0 on logit scale → decay ≈ 2.55 (moderate recovery)

  // 2008 decay: population mean and between-education variation
  mu_decay_2008 ~ normal(0, 0.5);              // Center on moderate recovery
  sigma_decay_2008 ~ exponential(1);           // Allow education variation
  decay_2008_raw ~ std_normal();               // Non-centered deviations

  // 2020 decay: population mean and between-education variation
  mu_decay_2020 ~ normal(0, 0.5);              // Center on moderate recovery
  sigma_decay_2020 ~ exponential(1);           // Allow education variation
  decay_2020_raw ~ std_normal();               // Non-centered deviations

  // === HIERARCHICAL SEASONAL EFFECTS ===
  // Pool seasonal patterns across education levels
  // Seasonal unemployment typically shows 1-3% variation on logit scale

  // Population-level mean seasonal pattern (11 months, 12th derived for sum-to-zero)
  mu_seasonal ~ normal(0, 0.03);                // Modest seasonal effects
  sum(mu_seasonal) ~ normal(0, 0.001);          // Soft sum-to-zero constraint

  // Between-education variation in seasonal patterns
  sigma_seasonal ~ exponential(10);             // Prior: small variation (mean=0.1)

  // Education-specific deviations (non-centered)
  to_vector(seasonal_u_raw) ~ std_normal();     // Standard normal deviations

  // Initial states - start near equilibrium (not hierarchical)
  logit_u_init ~ normal(-3.0, 0.5);

  // Spline coefficients (non-centered)
  // Allow more flexibility with more basis functions
  to_vector(spline_coef_raw) ~ std_normal();

  // === HIERARCHICAL SPLINE SMOOTHNESS ===
  // Pool spline smoothness across education levels
  // DATA-INFORMED: posterior shows sigma_spline ≈ 0.8 → log(0.8) ≈ -0.22
  mu_log_sigma_spline ~ normal(-0.22, 0.4);    // Center on exp(-0.22) ≈ 0.8
  sigma_log_sigma_spline ~ exponential(2);     // Allow modest education variation
  sigma_spline_raw ~ std_normal();             // Non-centered deviations

  // Overdispersion - DATA-INFORMED: posterior shows phi ≈ 5000-6000
  // log(5000-1) ≈ 8.5, so center there
  log_phi_minus_1 ~ normal(8.5, 0.5);  // Centers on phi ≈ 5000

  // === Likelihood ===

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
  // Equilibrium unemployment rates (directly from u_eq, no computation needed)
  // Note: u_equilibrium = u_eq by construction in this reparameterized model
  vector[N_edu] u_equilibrium = u_eq;

  // Shock half-lives
  vector[N_edu] halflife_2008;
  vector[N_edu] halflife_2020;
  for (i in 1:N_edu) {
    halflife_2008[i] = log(2) / decay_2008[i];
    halflife_2020[i] = log(2) / decay_2020[i];
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

  // Pure ODE trajectory: NO spline deviations, NO seasonal
  // Shows what the structural ODE dynamics alone would predict
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
        // Pure ODE: no spline deviation, no seasonal
        logit_u_ode[t][i] = logit_u_ode[t-1][i] + du_dt;
      }
      u_ode_pure[t] = inv_logit(logit_u_ode[t]);
    }
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

  // Posterior predictive
  array[T, N_edu] int n_unemployed_rep;
  for (t in 1:T) {
    for (i in 1:N_edu) {
      if (n_total[t, i] > 0) {
        real u_safe = fmin(fmax(u[t][i], 1e-6), 1 - 1e-6);
        n_unemployed_rep[t, i] = beta_binomial_rng(n_total[t, i],
                                                    u_safe * phi,
                                                    (1 - u_safe) * phi);
      } else {
        n_unemployed_rep[t, i] = 0;
      }
    }
  }
}
