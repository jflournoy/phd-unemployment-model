# PhD Unemployment Model - Targets Pipeline
#
# This file defines the reproducible data analysis pipeline using the targets
# package. It tracks dependencies between data processing, model fitting, and
# report generation steps.
#
# Usage:
#   targets::tar_make()          # Run the entire pipeline
#   targets::tar_make(model_phd) # Run specific target
#   targets::tar_visnetwork()    # Visualize pipeline
#   targets::tar_outdated()      # Check what needs updating

library(targets)
library(tarchetypes)  # For tar_quarto() and other helpers

# Set target options
tar_option_set(
  packages = c("phdunemployment", "data.table", "mgcv", "ggplot2",
               "cmdstanr", "qs"),  # Added for Stan model caching
  format = "rds",  # Default format for R objects
  error = "continue",  # Continue if one target fails
  memory = "transient",  # Free memory after each target
  garbage_collection = TRUE  # Run GC between targets
)

# Source helper functions if needed
# source("R/targets-helpers.R")

# ==============================================================================
# Pipeline Definition
# ==============================================================================

list(
  # ==========================================================================
  # Data Processing Targets
  # ==========================================================================

  ## Raw IPUMS CPS data (static, rarely changes)
  tar_target(
    raw_data_file,
    here::here("data-raw", "ipums_data.rds"),
    format = "file"
  ),

  tar_target(
    raw_data,
    readRDS(raw_data_file)
  ),

  ## Education spectrum count data (for binomial/quasi-binomial GAMs)
  tar_target(
    education_counts,
    {
      aggregate_monthly_by_education(
        raw_data,
        weight_var = NULL,  # Unweighted counts for binomial
        weighted = FALSE
      )
    }
  ),

  tar_target(
    education_counts_file,
    {
      path <- here::here("data", "education-spectrum-counts.rds")
      saveRDS(education_counts, path)
      path
    },
    format = "file"
  ),

  ## PhD monthly unemployment (for time series analysis)
  tar_target(
    phd_monthly,
    process_cps_data(raw_data)
  ),

  tar_target(
    phd_monthly_file,
    {
      path <- here::here("data", "phd-monthly-unemployment.rds")
      saveRDS(phd_monthly, path)
      path
    },
    format = "file"
  ),

  ## Multi-education unemployment (weighted, for comparisons)
  tar_target(
    multi_education,
    aggregate_monthly_by_education(
      raw_data,
      weight_var = "auto",  # ASECWT for March, WTFINL otherwise
      weighted = TRUE
    )
  ),

  tar_target(
    multi_education_file,
    {
      path <- here::here("data", "multi-education-unemployment.rds")
      saveRDS(multi_education, path)
      path
    },
    format = "file"
  ),

  # ==========================================================================
  # Model Fitting Targets (Frequentist GAMs for now, Bayesian later)
  # ==========================================================================

  ## Primary Education-Binomial GAM Model with Fuzzy Impulse Shock Dynamics
  ## Uses continuous shock intensity variables with exponential decay
  ## Shock effects persist naturally as intensity decays over time
  ## Unpenalized education-specific seasonal effects (fx=TRUE)
  ## Verbose logging for model diagnostics and convergence checking
  tar_target(
    model_education_binomial,
    {
      cat("\n", strrep("=", 80), "\n")
      cat("STARTING MODEL FITTING: Adaptive Education-Binomial GAM\n")
      cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
      cat("Data rows:", nrow(education_counts), "\n")
      cat("Education levels:", paste(unique(education_counts$education), collapse = ", "), "\n")
      cat(strrep("=", 80), "\n\n")

      cat("Model specifications:\n")
      cat("  - Family: Quasi-binomial\n")
      cat("  - Year-based adaptive smooth: s(year_frac, bs='ad', k=30) - no season leakage\n")
      cat("  - Education deviations: s(year_frac, education, bs='fs', k=20) - partial pooling\n")
      cat("  - Global seasonality: s(month, bs='cc', k=12)\n")
      cat("  - Education seasonality: s(month, education, bs='fs', k=12) cyclic\n")
      cat("  - Optimizer: bam() with method='fREML'\n\n")

      start_time <- Sys.time()

      result <- fit_education_binomial_gam(
        education_counts,
        use_quasi = TRUE
      )

      end_time <- Sys.time()
      elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

      cat("\n", strrep("=", 80), "\n")
      cat("MODEL FITTING COMPLETED\n")
      cat("Total runtime:", round(elapsed, 1), "seconds (", round(elapsed/60, 1), "minutes)\n")
      cat("Convergence:", result$convergence_info$converged, "\n")
      cat("Deviance explained:", round(result$summary_stats$deviance_explained * 100, 1), "%\n")
      cat("Dispersion parameter:", round(result$summary_stats$dispersion, 2), "\n")
      cat("Number of smooth terms:", length(result$model$smooth), "\n")
      cat("Time span:", result$summary_stats$time_span, "\n")
      cat("Education levels:", result$summary_stats$n_education_levels, "\n")
      cat(strrep("=", 80), "\n\n")

      result
    }
  ),

  # ==========================================================================
  # Advanced Analysis Targets (using refactored functions)
  # ==========================================================================

  ## Factor smooth model for multiple education levels
  ## Uses refactored fit_factor_smooth_to_cps_data() with data parameter
  tar_target(
    model_factor_smooth_multi,
    {
      fit_factor_smooth_to_cps_data(
        data = multi_education,
        education_levels = c("phd", "masters", "bachelors"),
        formula_type = "full",
        start_year = 2010,
        k_month = 10,
        k_trend = 20
      )
    }
  ),

  ## Nested model comparison across education levels
  ## Uses refactored fit_nested_models_to_cps_data() with data parameter
  tar_target(
    nested_models_comparison,
    {
      fit_nested_models_to_cps_data(
        data = multi_education,
        education_levels = c("phd", "masters", "bachelors"),
        start_year = 2010
      )
    }
  ),

  ## Comprehensive unemployment analysis by education
  ## Uses refactored analyze_cps_unemployment_by_education() with data parameter
  ## This demonstrates full integration with targets pipeline
  tar_target(
    comprehensive_analysis,
    {
      analyze_cps_unemployment_by_education(
        data = multi_education,
        education_levels = c("phd", "masters", "bachelors"),
        start_year = 2010,
        save_models = FALSE,
        save_results = FALSE
      )
    }
  ),

  # ==========================================================================
  # Report Targets
  # ==========================================================================

  ## Binomial vs Quasi-binomial comparison report
  tar_quarto(
    report_binomial_quasi,
    quiet = FALSE,
    path = "reports/quasi-binomial-validation/quasi-binomial-validation.qmd"
  ),

  # ==========================================================================
  # Bayesian State Space Model (Stan via cmdstanr) - Efficient Hierarchical
  # ==========================================================================
  #
  # The efficient ODE state space model with hierarchical pooling takes ~28 minutes.
  # Caching via targets is critical to avoid re-fitting on every report render.

  ## Stan model compilation (efficient version with hierarchical decay)
  tar_target(
    stan_model_compiled,
    {
      stan_file <- here::here("stan", "unemployment-ode-state-space-efficient.stan")
      cmdstanr::cmdstan_model(stan_file, compile = TRUE)
    },
    # Return the model object for use in fitting
    format = "qs"  # Fast serialization
  ),

  ## Fit Efficient ODE State Space Model with Hierarchical Pooling
  ## Hierarchical: u_eq, adj_speed, shock effects, decay rates, seasonal effects
  ## Data-informed priors, prior-centered initialization
  tar_target(
    model_ode_state_space_efficient,
    {
      cat("\n", strrep("=", 80), "\n")
      cat("STARTING STAN MODEL FITTING: Efficient ODE State Space (Hierarchical)\n")
      cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
      cat("Data rows:", nrow(education_counts), "\n")
      cat("Chains: 4, Iterations: 3000 (1500 warmup + 1500 sampling)\n")
      cat("Hierarchical: u_eq, adj_speed, shock_2008/2020, decay_2008/2020, seasonal, spline_smooth\n")
      cat("adapt_delta: 0.95, K_spline: 25\n")
      cat(strrep("=", 80), "\n\n")

      start_time <- Sys.time()

      result <- fit_ode_state_space_efficient(
        education_counts,
        K_spline = 25,
        chains = 4,
        iter_sampling = 1500,
        iter_warmup = 1500,
        adapt_delta = 0.95,
        max_treedepth = 12,
        parallel_chains = 4,
        refresh = 500
      )

      end_time <- Sys.time()
      elapsed <- as.numeric(difftime(end_time, start_time, units = "mins"))

      cat("\n", strrep("=", 80), "\n")
      cat("STAN MODEL FITTING COMPLETED\n")
      cat("Total runtime:", round(elapsed, 1), "minutes\n")
      cat("Divergent transitions:", result$diagnostics$num_divergent, "\n")
      cat("Max treedepth exceeded:", result$diagnostics$max_treedepth_exceeded, "\n")
      cat("E-BFMI:", paste(round(result$diagnostics$ebfmi, 3), collapse = ", "), "\n")
      cat(strrep("=", 80), "\n\n")

      result
    },
    # This target is SLOW - only rerun if data or Stan code changes
    format = "qs"
  ),

  ## Save Stan model results to file for report access
  ## The report can load this instead of re-fitting
  tar_target(
    model_ode_state_space_efficient_file,
    {
      path <- here::here("models", "ode-state-space-efficient-fit.qs")
      dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
      qs::qsave(model_ode_state_space_efficient, path)
      path
    },
    format = "file"
  )
)
