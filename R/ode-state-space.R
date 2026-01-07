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
#'   (default 0.95, higher for difficult posteriors)
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
                                 adapt_delta = 0.95,
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

  list(
    separation_rates = extract_param("separation_rate"),
    finding_rates = extract_param("finding_rate"),
    shock_2008_effects = extract_param("shock_2008_effect"),
    shock_2020_effects = extract_param("shock_2020_effect"),
    equilibrium_rates = extract_param("u_equilibrium"),
    shock_halflives = fit$summary(c("halflife_2008", "halflife_2020")),
    overdispersion = fit$summary("phi"),
    state_noise = fit$summary("sigma_state")
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
