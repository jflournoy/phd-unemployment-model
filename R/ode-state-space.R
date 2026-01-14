#' ODE State Space Model for Unemployment Dynamics
#'
#' Full Bayesian ODE-based state space model for unemployment dynamics
#' across education levels. Uses Stan for MCMC inference.
#'
#' @name ode-state-space
#' @docType package
NULL

#' Prepare Data for Stan State Space Model
#'
#' Converts the education-spectrum count data into the format required
#' by the Stan model.
#'
#' @param data Data frame with columns: n_unemployed, n_employed,
#'   time_index, month, education, year
#' @param education_order Optional character vector specifying education
#'   level order. If NULL, uses factor levels or alphabetical.
#'
#' @return List suitable for Stan model input
#' @export
prepare_stan_data <- function(data, education_order = NULL) {
  # Ensure data.table
  if (!inherits(data, "data.table")) {
    data <- data.table::as.data.table(data)
  }


  # Get education levels
  if (!is.null(education_order)) {
    edu_levels <- education_order
  } else if (is.factor(data$education)) {
    edu_levels <- levels(data$education)
  } else {
    edu_levels <- sort(unique(data$education))
  }
  N_edu <- length(edu_levels)

  # Get unique time points
  time_points <- sort(unique(data$time_index))
  T_len <- length(time_points)

  # Create mapping from time_index to sequential t
  time_map <- data.table::data.table(
    time_index = time_points,
    t = seq_along(time_points)
  )

  # Merge time mapping
  data <- merge(data, time_map, by = "time_index")

  # Create arrays
  n_unemployed <- matrix(0L, nrow = T_len, ncol = N_edu)
  n_total <- matrix(0L, nrow = T_len, ncol = N_edu)
  month_vec <- integer(T_len)
  year_frac <- numeric(T_len)

  # Fill arrays
  for (i in seq_along(edu_levels)) {
    edu <- edu_levels[i]
    edu_data <- data[data$education == edu, ]

    for (row_idx in seq_len(nrow(edu_data))) {
      t_idx <- edu_data$t[row_idx]
      n_unemployed[t_idx, i] <- as.integer(edu_data$n_unemployed[row_idx])
      n_total[t_idx, i] <- as.integer(edu_data$n_unemployed[row_idx] +
                                       edu_data$n_employed[row_idx])
      if (month_vec[t_idx] == 0) {
        month_vec[t_idx] <- as.integer(edu_data$month[row_idx])
        year_frac[t_idx] <- edu_data$year[row_idx] +
          (edu_data$month[row_idx] - 0.5) / 12
      }
    }
  }

  list(
    T = T_len,
    N_edu = N_edu,
    n_unemployed = n_unemployed,
    n_total = n_total,
    month = month_vec,
    year_frac = year_frac,
    # Shock timing (based on economic history)
    shock_2008_onset = 2007.75,  # Subprime crisis begins
    shock_2008_peak = 2009.50,   # Unemployment peaks
    shock_2020_onset = 2020.17,  # COVID March 2020
    shock_2020_peak = 2020.33,   # April 2020 peak
    # Metadata
    education_levels = edu_levels,
    time_points = time_points
  )
}


#' Fit ODE State Space Model
#'
#' Fits the full Bayesian ODE-based state space model for unemployment
#' dynamics across education levels using Stan.
#'
#' @param data Count data with n_unemployed, n_employed, time_index,
#'   month, education, year
#' @param chains Number of MCMC chains (default 4)
#' @param iter_sampling Number of sampling iterations per chain (default 1000)
#' @param iter_warmup Number of warmup iterations per chain (default 1000)
#' @param adapt_delta Target average proposal acceptance probability
#'   (default 0.99, higher to avoid divergences)
#' @param max_treedepth Maximum tree depth for NUTS sampler (default 12)
#' @param parallel_chains Number of chains to run in parallel
#' @param refresh How often to print progress (default 100)
#' @param stan_file Path to Stan model file (default uses package file)
#'
#' @return List with:
#'   \item{fit}{CmdStanMCMC object with posterior samples}
#'   \item{stan_data}{Prepared Stan data list}
#'   \item{diagnostics}{Convergence diagnostics summary}
#'
#' @export
fit_ode_state_space <- function(data,
                                 chains = 4,
                                 iter_sampling = 1000,
                                 iter_warmup = 1000,
                                 adapt_delta = 0.99,
                                 max_treedepth = 12,
                                 parallel_chains = 4,
                                 refresh = 100,
                                 stan_file = NULL) {

  # Prepare data
  stan_data <- prepare_stan_data(data)

  # Find Stan file

  if (is.null(stan_file)) {
    stan_file <- here::here("stan", "unemployment-ode-state-space.stan")
  }

  if (!file.exists(stan_file)) {
    stop("Stan model file not found: ", stan_file)
  }

  # Compile model
  message("Compiling Stan model...")
  model <- cmdstanr::cmdstan_model(stan_file)

  # Fit model
  message("Fitting ODE state space model...")
  message(sprintf("  Time points: %d", stan_data$T))
  message(sprintf("  Education levels: %d", stan_data$N_edu))
  message(sprintf("  Chains: %d, Iterations: %d warmup + %d sampling",
                  chains, iter_warmup, iter_sampling))

  fit <- model$sample(
    data = stan_data[c("T", "N_edu", "n_unemployed", "n_total", "month",
                       "year_frac", "shock_2008_onset", "shock_2008_peak",
                       "shock_2020_onset", "shock_2020_peak")],
    chains = chains,
    parallel_chains = parallel_chains,
    iter_sampling = iter_sampling,
    iter_warmup = iter_warmup,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    refresh = refresh
  )

  # Get diagnostics
  diag_summary <- fit$diagnostic_summary()
  diagnostics <- list(
    num_divergent = sum(diag_summary$num_divergent),
    max_treedepth_exceeded = sum(diag_summary$num_max_treedepth),
    ebfmi = diag_summary$ebfmi
  )

  message("Model fitting complete.")
  if (diagnostics$num_divergent > 0) {
    warning(sprintf("%d divergent transitions detected!",
                    diagnostics$num_divergent))
  }

  list(
    fit = fit,
    stan_data = stan_data,
    diagnostics = diagnostics
  )
}


#' Extract Economic Parameters from Fitted Model
#'
#' Extracts interpretable economic parameters from the fitted state
#' space model, including separation rates, finding rates, shock effects,
#' and equilibrium unemployment rates.
#'
#' @param result Result from fit_ode_state_space()
#' @param prob Probability mass for credible intervals (default 0.95)
#'
#' @return List with data frames for each parameter type:
#'   \item{separation_rates}{Monthly job separation rates by education}
#'   \item{finding_rates}{Monthly job finding rates by education}
#'   \item{shock_2008_effects}{2008 shock effects by education}
#'   \item{shock_2020_effects}{2020 shock effects by education}
#'   \item{equilibrium_rates}{Steady-state unemployment rates}
#'   \item{shock_halflives}{Shock persistence (years)}
#'   \item{overdispersion}{Beta-binomial dispersion parameter}
#'
#' @export
extract_economic_params <- function(result, prob = 0.95) {
  fit <- result$fit
  edu_levels <- result$stan_data$education_levels
  N_edu <- length(edu_levels)

  # Helper to extract and label
  extract_param <- function(param_name, labels = edu_levels) {
    summary_df <- fit$summary(param_name)
    if (!is.null(labels) && nrow(summary_df) == length(labels)) {
      summary_df$education <- labels
    }
    summary_df
  }

  # Try to get state noise parameter (different names in different models)
  state_noise <- tryCatch(
    fit$summary("sigma_state"),
    error = function(e) {
      # Efficient model uses sigma_spline instead
      tryCatch(
        fit$summary("sigma_spline"),
        error = function(e2) NULL
      )
    }
  )

  # Try to get reparameterized model parameters
  u_eq <- tryCatch(extract_param("u_eq"), error = function(e) NULL)
  adj_speed <- tryCatch(extract_param("adj_speed"), error = function(e) NULL)

  # Try to extract hierarchical shock/decay parameters (may not exist in older models)
  mu_shock_2008 <- tryCatch(fit$summary("mu_log_shock_2008"), error = function(e) NULL)
  mu_shock_2020 <- tryCatch(fit$summary("mu_log_shock_2020"), error = function(e) NULL)
  mu_decay_2008 <- tryCatch(fit$summary("mu_log_decay_2008"), error = function(e) NULL)
  mu_decay_2020 <- tryCatch(fit$summary("mu_log_decay_2020"), error = function(e) NULL)
  sigma_shock_2008 <- tryCatch(fit$summary("sigma_log_shock_2008"), error = function(e) NULL)
  sigma_shock_2020 <- tryCatch(fit$summary("sigma_log_shock_2020"), error = function(e) NULL)
  sigma_decay_2008 <- tryCatch(fit$summary("sigma_log_decay_2008"), error = function(e) NULL)
  sigma_decay_2020 <- tryCatch(fit$summary("sigma_log_decay_2020"), error = function(e) NULL)

  list(
    # Primary identifiable parameters (reparameterized model)
    u_eq = u_eq,
    adj_speed = adj_speed,
    # Derived flow rates (for interpretation)
    separation_rates = extract_param("separation_rate"),
    finding_rates = extract_param("finding_rate"),
    # Shock parameters (education-specific)
    shock_2008_effects = extract_param("shock_2008_effect"),
    shock_2020_effects = extract_param("shock_2020_effect"),
    decay_2008 = extract_param("decay_2008"),
    decay_2020 = extract_param("decay_2020"),
    # Hierarchical shock/decay parameters (population-level, may be NULL for older models)
    mu_shock_2008 = mu_shock_2008,
    mu_shock_2020 = mu_shock_2020,
    mu_decay_2008 = mu_decay_2008,
    mu_decay_2020 = mu_decay_2020,
    sigma_shock_2008 = sigma_shock_2008,
    sigma_shock_2020 = sigma_shock_2020,
    sigma_decay_2008 = sigma_decay_2008,
    sigma_decay_2020 = sigma_decay_2020,
    # Equilibrium and dynamics
    equilibrium_rates = extract_param("u_equilibrium"),
    halflife_2008 = extract_param("halflife_2008"),
    halflife_2020 = extract_param("halflife_2020"),
    # Nuisance parameters
    overdispersion = fit$summary("phi"),
    state_noise = state_noise
  )
}


#' Extract Latent Unemployment Rates
#'
#' Extracts the posterior distribution of latent unemployment rates
#' from the fitted model.
#'
#' @param result Result from fit_ode_state_space()
#' @param summary If TRUE, return summary statistics. If FALSE, return
#'   full posterior draws.
#'
#' @return Data frame with unemployment rate estimates by time and education
#' @export
extract_latent_rates <- function(result, summary = TRUE) {
  fit <- result$fit
  stan_data <- result$stan_data

  if (summary) {
    # Get summary for all u parameters
    u_summary <- fit$summary(variables = "u")

    # Parse parameter names to get indices
    u_summary$time_index <- as.integer(
      gsub("u\\[(\\d+),\\d+\\]", "\\1", u_summary$variable)
    )
    u_summary$edu_index <- as.integer(
      gsub("u\\[\\d+,(\\d+)\\]", "\\1", u_summary$variable)
    )

    # Add labels
    u_summary$time_point <- stan_data$time_points[u_summary$time_index]
    u_summary$education <- stan_data$education_levels[u_summary$edu_index]

    # Add year_frac for plotting
    u_summary$year_frac <- stan_data$year_frac[u_summary$time_index]

    u_summary
  } else {
    # Return full draws
    fit$draws(variables = "u", format = "draws_df")
  }
}


#' Extract Non-Seasonal Trend
#'
#' Extracts the unemployment trajectory without seasonal effects.
#' This shows the underlying dynamics driven by separation/finding rates
#' and economic shocks.
#'
#' @param result Result from fit_ode_state_space()
#' @param summary If TRUE, return summary statistics. If FALSE, return
#'   full posterior draws.
#'
#' @return Data frame with trend estimates by time and education
#' @export
extract_trend <- function(result, summary = TRUE) {
  fit <- result$fit
  stan_data <- result$stan_data

  if (summary) {
    # Get summary for all u_trend parameters
    trend_summary <- fit$summary(variables = "u_trend")

    # Parse parameter names to get indices
    trend_summary$time_index <- as.integer(
      gsub("u_trend\\[(\\d+),\\d+\\]", "\\1", trend_summary$variable)
    )
    trend_summary$edu_index <- as.integer(
      gsub("u_trend\\[\\d+,(\\d+)\\]", "\\1", trend_summary$variable)
    )

    # Add labels
    trend_summary$time_point <- stan_data$time_points[trend_summary$time_index]
    trend_summary$education <- stan_data$education_levels[trend_summary$edu_index]

    # Add year_frac for plotting
    trend_summary$year_frac <- stan_data$year_frac[trend_summary$time_index]

    trend_summary
  } else {
    # Return full draws
    fit$draws(variables = "u_trend", format = "draws_df")
  }
}


#' Extract Pure ODE Trajectory
#'
#' Extracts the unemployment trajectory driven purely by ODE dynamics
#' without stochastic innovations or seasonal effects. This shows what
#' the structural model predicts based on separation/finding rates
#' and shock effects alone.
#'
#' @param result Result from fit_ode_state_space()
#' @param summary If TRUE, return summary statistics. If FALSE, return
#'   full posterior draws.
#'
#' @return Data frame with pure ODE trajectory estimates by time and education
#' @export
extract_pure_ode <- function(result, summary = TRUE) {
  fit <- result$fit
  stan_data <- result$stan_data

  if (summary) {
    # Get summary for all u_ode_pure parameters
    ode_summary <- fit$summary(variables = "u_ode_pure")

    # Parse parameter names to get indices
    ode_summary$time_index <- as.integer(
      gsub("u_ode_pure\\[(\\d+),\\d+\\]", "\\1", ode_summary$variable)
    )
    ode_summary$edu_index <- as.integer(
      gsub("u_ode_pure\\[\\d+,(\\d+)\\]", "\\1", ode_summary$variable)
    )

    # Add labels
    ode_summary$time_point <- stan_data$time_points[ode_summary$time_index]
    ode_summary$education <- stan_data$education_levels[ode_summary$edu_index]

    # Add year_frac for plotting
    ode_summary$year_frac <- stan_data$year_frac[ode_summary$time_index]

    ode_summary
  } else {
    # Return full draws
    fit$draws(variables = "u_ode_pure", format = "draws_df")
  }
}


#' Extract Seasonal Effects
#'
#' Extracts the computed seasonal effect at each time point, representing
#' the difference between the full model (with seasonality) and the
#' trend (without seasonality).
#'
#' @param result Result from fit_ode_state_space()
#' @param summary If TRUE, return summary statistics. If FALSE, return
#'   full posterior draws.
#'
#' @return Data frame with seasonal effect estimates by time and education
#' @export
extract_seasonal_effects <- function(result, summary = TRUE) {
  fit <- result$fit
  stan_data <- result$stan_data

  if (summary) {
    # Get summary for all seasonal_effect parameters
    seas_summary <- fit$summary(variables = "seasonal_effect")

    # Parse parameter names to get indices
    seas_summary$time_index <- as.integer(
      gsub("seasonal_effect\\[(\\d+),\\d+\\]", "\\1", seas_summary$variable)
    )
    seas_summary$edu_index <- as.integer(
      gsub("seasonal_effect\\[\\d+,(\\d+)\\]", "\\1", seas_summary$variable)
    )

    # Add labels
    seas_summary$time_point <- stan_data$time_points[seas_summary$time_index]
    seas_summary$education <- stan_data$education_levels[seas_summary$edu_index]

    # Add year_frac and month for plotting
    seas_summary$year_frac <- stan_data$year_frac[seas_summary$time_index]
    seas_summary$month <- stan_data$month[seas_summary$time_index]

    seas_summary
  } else {
    # Return full draws
    fit$draws(variables = "seasonal_effect", format = "draws_df")
  }
}


#' Posterior Predictive Check Data
#'
#' Extracts simulated data from the posterior for comparison with
#' observed data.
#'
#' @param result Result from fit_ode_state_space()
#'
#' @return Data frame with observed and replicated counts
#' @export
extract_ppc_data <- function(result) {
  fit <- result$fit
  stan_data <- result$stan_data

  # Get posterior predictive samples
  ppc_draws <- fit$draws(variables = "n_unemployed_rep", format = "matrix")

  # Compute summary statistics
  ppc_mean <- colMeans(ppc_draws)
  ppc_q025 <- apply(ppc_draws, 2, quantile, probs = 0.025)
  ppc_q975 <- apply(ppc_draws, 2, quantile, probs = 0.975)

  # Create data frame
  T_len <- stan_data$T
  N_edu <- stan_data$N_edu

  ppc_df <- data.frame(
    time_index = rep(seq_len(T_len), N_edu),
    edu_index = rep(seq_len(N_edu), each = T_len),
    observed = as.vector(stan_data$n_unemployed),
    n_total = as.vector(stan_data$n_total),
    predicted_mean = ppc_mean,
    predicted_lower = ppc_q025,
    predicted_upper = ppc_q975
  )

  # Add labels
  ppc_df$education <- stan_data$education_levels[ppc_df$edu_index]
  ppc_df$year_frac <- stan_data$year_frac[ppc_df$time_index]

  # Compute rates
  ppc_df$observed_rate <- ppc_df$observed / ppc_df$n_total
  ppc_df$predicted_rate <- ppc_df$predicted_mean / ppc_df$n_total
  ppc_df$predicted_rate_lower <- ppc_df$predicted_lower / ppc_df$n_total
  ppc_df$predicted_rate_upper <- ppc_df$predicted_upper / ppc_df$n_total

  ppc_df
}


#' LOO Cross-Validation for State Space Model
#'
#' Computes leave-one-out cross-validation statistics for model comparison.
#'
#' @param result Result from fit_ode_state_space()
#'
#' @return loo object from the loo package
#' @export
compute_loo <- function(result) {
  fit <- result$fit

  # Extract log-likelihood
  log_lik <- fit$draws(variables = "log_lik", format = "matrix")

  # Compute LOO
  loo::loo(log_lik, cores = 4)
}


#' Fit Fast ODE State Space Model (Multithreaded)
#'
#' Fits the optimized Bayesian ODE-based state space model with
#' multithreading support using Stan's reduce_sum for parallel
#' likelihood evaluation.
#'
#' @param data Count data with n_unemployed, n_employed, time_index,
#'   month, education, year
#' @param chains Number of MCMC chains (default 4)
#' @param iter_sampling Number of sampling iterations per chain (default 1000)
#' @param iter_warmup Number of warmup iterations per chain (default 1000)
#' @param adapt_delta Target average proposal acceptance probability
#'   (default 0.99)
#' @param max_treedepth Maximum tree depth for NUTS sampler (default 12)
#' @param parallel_chains Number of chains to run in parallel
#' @param threads_per_chain Number of threads per chain for reduce_sum
#' @param grainsize Grainsize for reduce_sum parallelization (default auto)
#' @param refresh How often to print progress (default 100)
#' @param stan_file Path to Stan model file (default uses fast version)
#'
#' @return List with:
#'   \item{fit}{CmdStanMCMC object with posterior samples}
#'   \item{stan_data}{Prepared Stan data list}
#'   \item{diagnostics}{Convergence diagnostics summary}
#'   \item{timing}{Sampling time statistics}
#'
#' @details
#' The fast model uses several optimizations:
#' \itemize{
#'   \item \code{reduce_sum} for parallel likelihood calculation
#'   \item Vectorized operations in transformed parameters
#'   \item Matrix storage for better memory layout
#'   \item Pre-computed shock timing in transformed data
#' }
#'
#' For optimal performance:
#' \itemize{
#'   \item Set \code{threads_per_chain} to number of cores / chains
#'   \item Use \code{parallel_chains = 1} if using many threads per chain
#'   \item Adjust grainsize if T * N_edu is small (< 100)
#' }
#'
#' @export
fit_ode_state_space_fast <- function(data,
                                      chains = 4,
                                      iter_sampling = 1000,
                                      iter_warmup = 1000,
                                      adapt_delta = 0.99,
                                      max_treedepth = 12,
                                      parallel_chains = 4,
                                      threads_per_chain = 1,
                                      grainsize = NULL,
                                      refresh = 100,
                                      stan_file = NULL) {

  # Prepare data
  stan_data <- prepare_stan_data(data)

  # Auto-calculate grainsize if not specified
  # Optimal grainsize is typically N_obs / (threads_per_chain * 2-4)
  if (is.null(grainsize)) {
    n_obs <- stan_data$T * stan_data$N_edu
    grainsize <- max(1, floor(n_obs / (threads_per_chain * 4)))
  }
  stan_data$grainsize <- as.integer(grainsize)

  # Find Stan file
  if (is.null(stan_file)) {
    stan_file <- here::here("stan", "unemployment-ode-state-space-fast.stan")
  }

  if (!file.exists(stan_file)) {
    stop("Stan model file not found: ", stan_file)
  }

  # Compile model with threading support
  message("Compiling fast Stan model with threading support...")
  model <- cmdstanr::cmdstan_model(
    stan_file,
    cpp_options = list(stan_threads = TRUE)
  )

  # Fit model
  message("Fitting fast ODE state space model...")
  message(sprintf("  Time points: %d", stan_data$T))
  message(sprintf("  Education levels: %d", stan_data$N_edu))
  message(sprintf("  Observations: %d", stan_data$T * stan_data$N_edu))
  message(sprintf("  Chains: %d, Parallel chains: %d", chains, parallel_chains))
  message(sprintf("  Threads per chain: %d", threads_per_chain))
  message(sprintf("  Grainsize: %d", grainsize))
  message(sprintf("  Iterations: %d warmup + %d sampling", iter_warmup, iter_sampling))

  start_time <- Sys.time()

  fit <- model$sample(
    data = stan_data[c("T", "N_edu", "n_unemployed", "n_total", "month",
                       "year_frac", "shock_2008_onset", "shock_2008_peak",
                       "shock_2020_onset", "shock_2020_peak", "grainsize")],
    chains = chains,
    parallel_chains = parallel_chains,
    threads_per_chain = threads_per_chain,
    iter_sampling = iter_sampling,
    iter_warmup = iter_warmup,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    refresh = refresh
  )

  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "mins"))

  # Get diagnostics
  diag_summary <- fit$diagnostic_summary()
  diagnostics <- list(
    num_divergent = sum(diag_summary$num_divergent),
    max_treedepth_exceeded = sum(diag_summary$num_max_treedepth),
    ebfmi = diag_summary$ebfmi
  )

  message(sprintf("Model fitting complete in %.1f minutes.", elapsed))
  if (diagnostics$num_divergent > 0) {
    warning(sprintf("%d divergent transitions detected!",
                    diagnostics$num_divergent))
  }

  list(
    fit = fit,
    stan_data = stan_data,
    diagnostics = diagnostics,
    timing = list(
      elapsed_mins = elapsed,
      chains = chains,
      threads_per_chain = threads_per_chain,
      parallel_chains = parallel_chains
    )
  )
}


#' Extract Trend from Fast Model
#'
#' Extracts the unemployment trajectory without seasonal effects from
#' the fast model. Uses matrix output format.
#'
#' @param result Result from fit_ode_state_space_fast()
#' @param summary If TRUE, return summary statistics.
#'
#' @return Data frame with trend estimates by time and education
#' @export
extract_trend_fast <- function(result, summary = TRUE) {
  fit <- result$fit
  stan_data <- result$stan_data

  if (summary) {
    # Get summary for trend parameters (matrix format in fast model)
    trend_summary <- fit$summary(variables = "u_trend_mat")

    # Parse parameter names to get indices
    trend_summary$time_index <- as.integer(
      gsub("u_trend_mat\\[(\\d+),\\d+\\]", "\\1", trend_summary$variable)
    )
    trend_summary$edu_index <- as.integer(
      gsub("u_trend_mat\\[\\d+,(\\d+)\\]", "\\1", trend_summary$variable)
    )

    # Add labels
    trend_summary$time_point <- stan_data$time_points[trend_summary$time_index]
    trend_summary$education <- stan_data$education_levels[trend_summary$edu_index]
    trend_summary$year_frac <- stan_data$year_frac[trend_summary$time_index]

    trend_summary
  } else {
    fit$draws(variables = "u_trend_mat", format = "draws_df")
  }
}


#' Extract Latent Rates from Fast Model
#'
#' Extracts the posterior distribution of latent unemployment rates
#' from the fast model. Uses matrix output format.
#'
#' @param result Result from fit_ode_state_space_fast()
#' @param summary If TRUE, return summary statistics.
#'
#' @return Data frame with unemployment rate estimates by time and education
#' @export
extract_latent_rates_fast <- function(result, summary = TRUE) {
  fit <- result$fit
  stan_data <- result$stan_data

  if (summary) {
    # Get summary for u_mat parameters
    u_summary <- fit$summary(variables = "u_mat")

    # Parse parameter names to get indices
    u_summary$time_index <- as.integer(
      gsub("u_mat\\[(\\d+),\\d+\\]", "\\1", u_summary$variable)
    )
    u_summary$edu_index <- as.integer(
      gsub("u_mat\\[\\d+,(\\d+)\\]", "\\1", u_summary$variable)
    )

    # Add labels
    u_summary$time_point <- stan_data$time_points[u_summary$time_index]
    u_summary$education <- stan_data$education_levels[u_summary$edu_index]
    u_summary$year_frac <- stan_data$year_frac[u_summary$time_index]

    u_summary
  } else {
    fit$draws(variables = "u_mat", format = "draws_df")
  }
}


#' Generate Initial Values at Prior Centers
#'
#' Creates a function that generates initial values centered on data-informed
#' priors for the efficient ODE state space model. This avoids multimodal
#' posterior issues from random initialization.
#'
#' @param stan_data Prepared Stan data list from prepare_stan_data()
#'
#' @return A function that returns a list of initial values when called
#'
#' @details
#' The init function centers each parameter at data-informed values:
#' \itemize{
#'   \item mu_logit_u_eq: -3.3 (equilibrium ~3.5%)
#'   \item mu_log_adj_speed: 2.3 (adj_speed ~ 10, scaled for logit dynamics)
#'   \item mu_log_shock_2008: -2 (effect ~0.14)
#'   \item mu_log_shock_2020: -1.5 (effect ~0.22)
#'   \item mu_decay_2008, mu_decay_2020: 0 (decay ~ 2.55 on [0.1, 5])
#'   \item sigma_spline: 0.8 (data-informed)
#'   \item log_phi_minus_1: 8.5 (phi ~ 5000)
#'   \item All _raw parameters: 0 (non-centered at population mean)
#'   \item All sigma (population SD) parameters: 0.5-0.8 (prior centers)
#' }
#'
#' @export
make_init_at_prior <- function(stan_data) {
  N_edu <- stan_data$N_edu
  K_spline <- stan_data$K_spline

  # Return a function that generates inits (cmdstanr interface)
  # ALL VALUES ARE DATA-INFORMED from prior model fits
  function() {
    list(
      # Spline coefficients (centered at 0, the prior mean)
      spline_coef_raw = matrix(0, nrow = K_spline, ncol = N_edu),
      sigma_spline = 0.8,  # DATA-INFORMED: posterior ~0.8

      # Hierarchical equilibrium unemployment (non-centered)
      mu_logit_u_eq = -3.3,         # Prior: normal(-3.3, 0.3) → ~3.5%
      sigma_logit_u_eq = 0.5,       # Prior: exponential(2), mean = 0.5
      u_eq_raw = rep(0, N_edu),     # Non-centered: start at population mean

      # Hierarchical adjustment speeds (non-centered, DATA-INFORMED)
      # Note: rates scaled by ~30x due to logit dynamics
      mu_log_adj_speed = 2.3,       # DATA-INFORMED: exp(2.3) ≈ 10
      sigma_log_adj_speed = 1.0,    # DATA-INFORMED: substantial education variation
      adj_speed_raw = rep(0, N_edu), # Non-centered: start at population mean

      # Hierarchical shock effects (non-centered, data-informed)
      mu_log_shock_2008 = -2,       # Prior: normal(-2, 0.8) → effect ~0.14
      sigma_log_shock_2008 = 0.8,   # Prior: exponential(1), expect ~1
      shock_2008_raw = rep(0, N_edu), # Non-centered: start at population mean

      mu_log_shock_2020 = -1.5,     # Prior: normal(-1.5, 0.8) → effect ~0.22
      sigma_log_shock_2020 = 0.8,   # Prior: exponential(1), expect ~1
      shock_2020_raw = rep(0, N_edu), # Non-centered: start at population mean

      # Hierarchical decay rates (non-centered, data-informed)
      # mu=0 on logit scale → decay ≈ 2.55 (moderate recovery)
      mu_decay_2008 = 0,             # Prior: normal(0, 0.5)
      sigma_decay_2008 = 0.5,        # Prior: exponential(1), mean = 1
      decay_2008_raw = rep(0, N_edu), # Non-centered: start at population mean

      mu_decay_2020 = 0,             # Prior: normal(0, 0.5)
      sigma_decay_2020 = 0.5,        # Prior: exponential(1), mean = 1
      decay_2020_raw = rep(0, N_edu), # Non-centered: start at population mean

      # Hierarchical seasonal effects (non-centered)
      # Population-level mean pattern (prior: normal(0, 0.03) with sum ≈ 0)
      mu_seasonal = rep(0, 11),      # Start at zero (prior mean)
      sigma_seasonal = 0.1,          # Prior: exponential(10), mean = 0.1
      seasonal_u_raw = matrix(0, nrow = 11, ncol = N_edu), # Non-centered deviations

      # Initial states (prior: normal(-3.0, 0.5))
      logit_u_init = rep(-3.0, N_edu),

      # Overdispersion - DATA-INFORMED: posterior ~5000
      log_phi_minus_1 = 8.5
    )
  }
}


#' Fit Efficient ODE State Space Model (Spline Basis)
#'
#' Fits the efficient Bayesian ODE-based state space model that uses
#' a spline basis for latent state deviations instead of per-timepoint
#' innovations. This dramatically reduces dimensionality and improves
#' sampling efficiency.
#'
#' @param data Count data with n_unemployed, n_employed, time_index,
#'   month, education, year
#' @param K_spline Number of spline basis functions (default 25). Higher values
#'   give more flexibility but increase computation time.
#' @param chains Number of MCMC chains (default 4)
#' @param iter_sampling Number of sampling iterations per chain (default 1500)
#' @param iter_warmup Number of warmup iterations per chain (default 1000)
#' @param adapt_delta Target average proposal acceptance probability
#'   (default 0.95)
#' @param max_treedepth Maximum tree depth for NUTS sampler (default 12)
#' @param parallel_chains Number of chains to run in parallel
#' @param refresh How often to print progress (default 100)
#' @param stan_file Path to Stan model file
#'
#' @return List with:
#'   \item{fit}{CmdStanMCMC object with posterior samples}
#'   \item{stan_data}{Prepared Stan data list}
#'   \item{diagnostics}{Convergence diagnostics summary}
#'
#' @details
#' This model addresses the max treedepth issue in the full state space
#' model by using K_spline << T spline basis functions to capture smooth
#' deviations from the ODE trajectory. For T=310 time points, this reduces
#' from ~2100 latent parameters to ~140 (K_spline * N_edu).
#'
#' The ODE dynamics + shocks + seasonality provide the main structure,
#' while the spline deviations capture residual low-frequency variation.
#'
#' @export
fit_ode_state_space_efficient <- function(data,
                                           K_spline = 25,
                                           chains = 4,
                                           iter_sampling = 1500,
                                           iter_warmup = 1000,
                                           adapt_delta = 0.95,
                                           max_treedepth = 12,
                                           parallel_chains = 4,
                                           refresh = 100,
                                           stan_file = NULL) {

  # Prepare data
  stan_data <- prepare_stan_data(data)

  # Add spline configuration
  stan_data$K_spline <- as.integer(K_spline)

  # Find Stan file
  if (is.null(stan_file)) {
    stan_file <- here::here("stan", "unemployment-ode-state-space-efficient.stan")
  }

  if (!file.exists(stan_file)) {
    stop("Stan model file not found: ", stan_file)
  }

  # Compile model
  message("Compiling efficient Stan model...")
  model <- cmdstanr::cmdstan_model(stan_file)

  # Fit model
  n_latent_params <- K_spline * stan_data$N_edu
  n_full_params <- (stan_data$T - 1) * stan_data$N_edu

  message("Fitting efficient ODE state space model...")
  message(sprintf("  Time points: %d", stan_data$T))
  message(sprintf("  Education levels: %d", stan_data$N_edu))
  message(sprintf("  Spline basis functions: %d", K_spline))
  message(sprintf("  Latent parameters: %d (vs %d in full model, %.1fx reduction)",
                  n_latent_params, n_full_params, n_full_params / n_latent_params))
  message(sprintf("  Chains: %d, Iterations: %d warmup + %d sampling",
                  chains, iter_warmup, iter_sampling))

  start_time <- Sys.time()

  # Generate init function at prior centers
  init_fn <- make_init_at_prior(stan_data)

  fit <- model$sample(
    data = stan_data[c("T", "N_edu", "n_unemployed", "n_total", "month",
                       "year_frac", "shock_2008_onset", "shock_2008_peak",
                       "shock_2020_onset", "shock_2020_peak", "K_spline")],
    chains = chains,
    parallel_chains = parallel_chains,
    iter_sampling = iter_sampling,
    iter_warmup = iter_warmup,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    refresh = refresh,
    init = init_fn  # Use informed inits at prior centers
  )

  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "mins"))

  # Get diagnostics
  diag_summary <- fit$diagnostic_summary()
  diagnostics <- list(
    num_divergent = sum(diag_summary$num_divergent),
    max_treedepth_exceeded = sum(diag_summary$num_max_treedepth),
    ebfmi = diag_summary$ebfmi
  )

  message(sprintf("Model fitting complete in %.1f minutes.", elapsed))
  if (diagnostics$num_divergent > 0) {
    warning(sprintf("%d divergent transitions detected!",
                    diagnostics$num_divergent))
  }
  if (diagnostics$max_treedepth_exceeded > 0) {
    warning(sprintf("%d max treedepth exceeded!",
                    diagnostics$max_treedepth_exceeded))
  }

  list(
    fit = fit,
    stan_data = stan_data,
    diagnostics = diagnostics,
    timing = list(
      elapsed_mins = elapsed,
      chains = chains,
      K_spline = K_spline
    )
  )
}
