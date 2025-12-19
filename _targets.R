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
  packages = c("phdunemployment", "data.table", "mgcv", "ggplot2"),
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

  ## Primary Education-Binomial GAM Model with Shock Dynamics
  ## Uses thin plate splines (bs="tp") for maximum flexibility
  ## Extended shock periods (2007-2010 financial crisis, 2019-2021 pandemic)
  ## Verbose logging for model diagnostics and convergence checking
  tar_target(
    model_education_binomial,
    {
      cat("\n", strrep("=", 80), "\n")
      cat("STARTING MODEL FITTING: Education-Binomial GAM with Shock Dynamics\n")
      cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
      cat("Data rows:", nrow(education_counts), "\n")
      cat("Education levels:", paste(unique(education_counts$education), collapse = ", "), "\n")
      cat(strrep("=", 80), "\n\n")

      cat("Model specifications:\n")
      cat("  - Family: Quasi-binomial\n")
      cat("  - Main time smooth: k=150, bs='tp' (thin plate splines)\n")
      cat("  - Shock × time smooths: k=20, bs='tp' (2007-2010, 2019-2021 periods)\n")
      cat("  - Seasonal smooths: k=12 shared + k=12 by-education (cyclic cubic)\n")
      cat("  - Optimizer: bam() with method='fREML'\n\n")

      start_time <- Sys.time()

      result <- fit_education_binomial_gam(
        education_counts,
        use_quasi = TRUE,
        time_k = 150
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
  )

  # Future: Add Bayesian model targets here
  # tar_target(
  #   model_beta_binomial_stan,
  #   {
  #     fit_beta_binomial_brms(education_counts)
  #   },
  #   # These will be SLOW - caching is critical
  #   deployment = "main"  # Run sequentially, not in parallel
  # )
)
