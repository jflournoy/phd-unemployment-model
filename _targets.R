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
               "cmdstanr", "qs", "qs2"),  # Added for Stan model caching
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

  ## Education order for trend modeling (least to most educated)
  tar_target(
    education_order,
    c("less_than_hs", "high_school", "some_college",
      "bachelors", "masters", "professional", "phd")
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
      # Verify critical packages are installed before starting computation
      required_packages <- c("phdunemployment", "data.table", "mgcv", "ggplot2")
      missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
      if (length(missing_packages) > 0) {
        stop("Missing required packages for education-binomial GAM: ",
             paste(missing_packages, collapse = ", "), "\n",
             "Install with: install.packages(c(",
             paste0('"', missing_packages, '"', collapse = ", "), "))")
      }

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
      # Verify critical packages are installed before starting computation
      required_packages <- c("phdunemployment", "data.table", "mgcv", "ggplot2")
      missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
      if (length(missing_packages) > 0) {
        stop("Missing required packages for factor smooth model: ",
             paste(missing_packages, collapse = ", "), "\n",
             "Install with: install.packages(c(",
             paste0('"', missing_packages, '"', collapse = ", "), "))")
      }

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
      # Verify critical packages are installed before starting computation
      required_packages <- c("phdunemployment", "data.table", "mgcv", "ggplot2")
      missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
      if (length(missing_packages) > 0) {
        stop("Missing required packages for nested model comparison: ",
             paste(missing_packages, collapse = ", "), "\n",
             "Install with: install.packages(c(",
             paste0('"', missing_packages, '"', collapse = ", "), "))")
      }

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
      # Verify critical packages are installed before starting computation
      required_packages <- c("phdunemployment", "data.table", "mgcv", "ggplot2")
      missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
      if (length(missing_packages) > 0) {
        stop("Missing required packages for comprehensive analysis: ",
             paste(missing_packages, collapse = ", "), "\n",
             "Install with: install.packages(c(",
             paste0('"', missing_packages, '"', collapse = ", "), "))")
      }

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
      # Verify required packages for Stan model compilation
      required_packages <- c("cmdstanr", "qs", "qs2")
      missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
      if (length(missing_packages) > 0) {
        stop("Missing required packages for Stan compilation: ",
             paste(missing_packages, collapse = ", "), "\n",
             "Install with: install.packages(c(",
             paste0('"', missing_packages, '"', collapse = ", "), "))")
      }

      # Check CmdStan installation
      if (is.na(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
        stop("CmdStan not installed. Required for Stan model compilation.\n",
             "Install with: cmdstanr::install_cmdstan()")
      }

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
      # Verify critical packages are installed before starting long computation
      required_packages <- c("phdunemployment", "data.table", "mgcv", "ggplot2",
                             "cmdstanr", "qs", "qs2", "targets", "tarchetypes")
      missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
      if (length(missing_packages) > 0) {
        stop("Missing required packages for efficient model: ",
             paste(missing_packages, collapse = ", "), "\n",
             "Install with: install.packages(c(",
             paste0('"', missing_packages, '"', collapse = ", "), "))")
      }

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
  ),

  # ==========================================================================
  # Education-Level Parallel Model (OPTIONAL - uses reduce_sum)
  # ==========================================================================
  #
  # PARALLELIZATION STRATEGY:
  # - Parallelizes across EDUCATION LEVELS (N_edu = 7), not time points
  # - Each thread computes the complete trajectory + likelihood for one education level
  # - Time evolution is sequential within each education level (required by ODE)
  # - Education levels are independent (no cross-education dependencies in ODE)
  #
  # EXPECTED SPEEDUP:
  # - With 7 education levels and 4 threads: ~1.5-1.7x speedup
  # - ODE+Likelihood (~90% of runtime) now parallelized across education levels
  #
  # THREADING CONFIGURATION:
  # - Use threads_per_chain = 2-4 depending on N_edu
  # - Keep parallel_chains * threads_per_chain <= available_cores
  # - grainsize = 1 for fine-grained parallelization (N_edu is small)
  #
  # WHEN TO USE:
  # - Use edu-parallel model when runtime matters (development, iteration)
  # - Use serial model (efficient) for final production runs (simpler, same results)

  ## Compile education-parallel Stan model with threading support
  tar_target(
    stan_model_compiled_edu_parallel,
    {
      # Verify required packages for threaded Stan model compilation
      required_packages <- c("cmdstanr", "qs", "qs2")
      missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
      if (length(missing_packages) > 0) {
        stop("Missing required packages for threaded Stan compilation: ",
             paste(missing_packages, collapse = ", "), "\n",
             "Install with: install.packages(c(",
             paste0('"', missing_packages, '"', collapse = ", "), "))")
      }

      # Check CmdStan installation
      if (is.na(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
        stop("CmdStan not installed. Required for threaded Stan model compilation.\n",
             "Install with: cmdstanr::install_cmdstan()")
      }

      stan_file <- here::here("stan", "unemployment-ode-state-space-edu-parallel.stan")
      cmdstanr::cmdstan_model(
        stan_file,
        cpp_options = list(stan_threads = TRUE),
        compile = TRUE
      )
    },
    format = "qs"
  ),

  ## Fit Education-Parallel ODE State Space Model
  ## Uses reduce_sum() to parallelize across education levels
  ## Expected speedup: 1.5-1.7x with 4 threads
  tar_target(
    model_ode_state_space_edu_parallel,
    {
      # Verify critical packages are installed before starting long computation
      required_packages <- c("phdunemployment", "data.table", "mgcv", "ggplot2",
                             "cmdstanr", "qs", "qs2", "targets", "tarchetypes")
      missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
      if (length(missing_packages) > 0) {
        stop("Missing required packages for edu-parallel model: ",
             paste(missing_packages, collapse = ", "), "\n",
             "Install with: install.packages(c(",
             paste0('"', missing_packages, '"', collapse = ", "), "))")
      }

      cat("\n", strrep("=", 80), "\n")
      cat("STARTING EDU-PARALLEL STAN MODEL FITTING\n")
      cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
      cat("Data rows:", nrow(education_counts), "\n")
      cat("Chains: 4, Parallel chains: 2, Threads per chain: 7, Iterations: 3000 (1500 warmup + 1500 sampling)\n")
      cat("Threading: reduce_sum() across education levels, 7 threads per chain\n")
      cat(strrep("=", 80), "\n\n")

      start_time <- Sys.time()

      result <- fit_ode_state_space_edu_parallel(
        education_counts,
        chains = 4,
        iter_sampling = 1500,
        iter_warmup = 1500,
        adapt_delta = 0.99,
        max_treedepth = 15,
        parallel_chains = 2,
        threads_per_chain = 7,
        grainsize = 1L,
        refresh = 500
      )

      end_time <- Sys.time()
      elapsed <- as.numeric(difftime(end_time, start_time, units = "mins"))

      cat("\n", strrep("=", 80), "\n")
      cat("EDU-PARALLEL STAN MODEL FITTING COMPLETED\n")
      cat("Total runtime:", round(elapsed, 1), "minutes\n")
      cat("Divergent transitions:", result$diagnostics$num_divergent, "\n")
      cat("Max treedepth exceeded:", result$diagnostics$max_treedepth_exceeded, "\n")
      cat("E-BFMI:", paste(round(result$diagnostics$ebfmi, 3), collapse = ", "), "\n")
      cat(strrep("=", 80), "\n\n")

      result
    },
    format = "qs"
  ),

  ## Save edu-parallel model results to file
  tar_target(
    model_ode_state_space_edu_parallel_file,
    {
      path <- here::here("models", "ode-state-space-edu-parallel-fit.qs")
      dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
      qs::qsave(model_ode_state_space_edu_parallel, path)
      path
    },
    format = "file"
  ),

  # ==========================================================================
  # Education-Trend Model (OPTIONAL - extends edu-parallel with trends)
  # ==========================================================================
  #
  # SCIENTIFIC QUESTIONS:
  # 1. Does equilibrium unemployment decrease with education?
  # 2. Do more educated workers recover faster from shocks (decay rates)?
  # 3. Are more educated workers less sensitive to economic shocks?
  # 4. Do adjustment speeds vary systematically with education?
  #
  # MODEL STRUCTURE:
  # Current: parameter[i] = mu + sigma * raw[i] (independent across education)
  # New:     parameter[i] = mu + beta * edu_rank_scaled[i] + sigma * raw[i]
  # Where edu_rank_scaled[i] = (i - 1) / (N_edu - 1) [0 to 1 scaling]
  #
  # PARAMETERS WITH EDUCATION TRENDS:
  # 1. logit_u_eq (equilibrium unemployment)
  # 2. log_adj_speed (adjustment speed)
  # 3. log_shock_2008_effect (2008 shock magnitude)
  # 4. log_shock_2020_effect (2020 shock magnitude)
  # 5. decay_2008 (2008 recovery rate)
  # 6. decay_2020 (2020 recovery rate)
  # 7. log_sigma_spline (spline smoothness)
  #
  # THREADING CONFIGURATION:
  # Same as edu-parallel model: reduce_sum() across education levels
  # Uses same threading settings for fair comparison

  ## Compile education-trend Stan model with threading support
  tar_target(
    stan_model_compiled_education_trend,
    {
      # Verify required packages for threaded Stan model compilation
      required_packages <- c("cmdstanr", "qs", "qs2")
      missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
      if (length(missing_packages) > 0) {
        stop("Missing required packages for threaded Stan compilation: ",
             paste(missing_packages, collapse = ", "), "\n",
             "Install with: install.packages(c(",
             paste0('"', missing_packages, '"', collapse = ", "), "))")
      }

      # Check CmdStan installation
      if (is.na(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
        stop("CmdStan not installed. Required for threaded Stan model compilation.\n",
             "Install with: cmdstanr::install_cmdstan()")
      }

      stan_file <- here::here("stan", "unemployment-ode-state-space-education-trend.stan")
      cmdstanr::cmdstan_model(
        stan_file,
        cpp_options = list(stan_threads = TRUE),
        compile = TRUE
      )
    },
    format = "qs"
  ),

  ## Fit Education-Trend ODE State Space Model
  ## Extends edu-parallel model with linear trends in education rank
  ## Uses same threading configuration for fair runtime comparison
  tar_target(
    model_ode_state_space_education_trend,
    {
      # Verify critical packages are installed before starting long computation
      required_packages <- c("phdunemployment", "data.table", "mgcv", "ggplot2",
                             "cmdstanr", "qs", "qs2", "targets", "tarchetypes")
      missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
      if (length(missing_packages) > 0) {
        stop("Missing required packages for education-trend model: ",
             paste(missing_packages, collapse = ", "), "\n",
             "Install with: install.packages(c(",
             paste0('"', missing_packages, '"', collapse = ", "), "))")
      }

      cat("\n", strrep("=", 80), "\n")
      cat("STARTING EDUCATION-TREND STAN MODEL FITTING\n")
      cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
      cat("Data rows:", nrow(education_counts), "\n")
      cat("Chains: 4, Parallel chains: 2, Threads per chain: 7, Iterations: 3000 (1500 warmup + 1500 sampling)\n")
      cat("Threading: reduce_sum() across education levels, 7 threads per chain\n")
      cat("Education trends: u_eq, adj_speed, shock_2008/2020, decay_2008/2020, spline_smooth\n")
      cat(strrep("=", 80), "\n\n")

      start_time <- Sys.time()

      result <- fit_ode_state_space_education_trend(
        education_counts,
        education_order = education_order,  # Use proper ordering from target
        chains = 4,
        iter_sampling = 1500,
        iter_warmup = 1500,
        adapt_delta = 0.99,
        max_treedepth = 15,
        parallel_chains = 2,
        threads_per_chain = 7,
        grainsize = 1L,
        K_spline = 25L,
        refresh = 500
      )

      end_time <- Sys.time()
      elapsed <- as.numeric(difftime(end_time, start_time, units = "mins"))

      cat("\n", strrep("=", 80), "\n")
      cat("EDUCATION-TREND STAN MODEL FITTING COMPLETED\n")
      cat("Total runtime:", round(elapsed, 1), "minutes\n")
      cat("Divergent transitions:", result$diagnostics$num_divergent, "\n")
      cat("Max treedepth exceeded:", result$diagnostics$max_treedepth_exceeded, "\n")
      cat("E-BFMI:", paste(round(result$diagnostics$ebfmi, 3), collapse = ", "), "\n")
      cat(strrep("=", 80), "\n\n")

      result
    },
    format = "qs"
  ),

  ## Save education-trend model results to file
  tar_target(
    model_ode_state_space_education_trend_file,
    {
      path <- here::here("models", "ode-state-space-education-trend-fit.qs")
      dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
      qs::qsave(model_ode_state_space_education_trend, path)
      path
    },
    format = "file"
  ),

  # ==========================================================================
  # Model Comparison: LOO-CV Analysis
  # ==========================================================================
  #
  # Compare education-trend model vs edu-parallel model using LOO-CV
  # Both models compute log_lik in generated quantities
  # Use loo package for model comparison

  ## Compute LOO-CV for edu-parallel model
  tar_target(
    loo_edu_parallel,
    {
      if (!requireNamespace("loo", quietly = TRUE)) {
        stop("loo package required for LOO-CV analysis. Install with: install.packages('loo')")
      }

      message("Computing LOO-CV for edu-parallel model...")
      compute_loo(model_ode_state_space_edu_parallel)
    },
    format = "qs"
  ),

  ## Compute LOO-CV for education-trend model
  tar_target(
    loo_education_trend,
    {
      if (!requireNamespace("loo", quietly = TRUE)) {
        stop("loo package required for LOO-CV analysis. Install with: install.packages('loo')")
      }

      message("Computing LOO-CV for education-trend model...")
      compute_loo(model_ode_state_space_education_trend)
    },
    format = "qs"
  ),

  ## Compare LOO-CV metrics between models
  tar_target(
    loo_comparison,
    {
      if (!requireNamespace("loo", quietly = TRUE)) {
        stop("loo package required for LOO-CV analysis. Install with: install.packages('loo')")
      }

      message("Comparing LOO-CV metrics between edu-parallel and education-trend models...")

      # Compare using loo_compare
      comparison <- loo::loo_compare(loo_edu_parallel, loo_education_trend)

      # Create summary data frame
      summary_df <- data.frame(
        model = c("edu_parallel", "education_trend"),
        elpd_loo = c(loo_edu_parallel$estimates["elpd_loo", "Estimate"],
                     loo_education_trend$estimates["elpd_loo", "Estimate"]),
        se_elpd_loo = c(loo_edu_parallel$estimates["elpd_loo", "SE"],
                        loo_education_trend$estimates["elpd_loo", "SE"]),
        p_loo = c(loo_edu_parallel$estimates["p_loo", "Estimate"],
                  loo_education_trend$estimates["p_loo", "Estimate"]),
        se_p_loo = c(loo_edu_parallel$estimates["p_loo", "SE"],
                     loo_education_trend$estimates["p_loo", "SE"]),
        looic = c(loo_edu_parallel$estimates["looic", "Estimate"],
                  loo_education_trend$estimates["looic", "Estimate"]),
        se_looic = c(loo_edu_parallel$estimates["looic", "SE"],
                     loo_education_trend$estimates["looic", "SE"])
      )

      list(
        loo_compare = comparison,
        summary = summary_df,
        elpd_diff = comparison[2, "elpd_diff"],  # education_trend - edu_parallel
        se_diff = comparison[2, "se_diff"]
      )
    },
    format = "qs"
  )
)
