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

  ## Binomial and Quasi-binomial model comparison
  ## This single target fits both models and compares them
  tar_target(
    model_comparison,
    {
      compare_binomial_quasibinomial(
        data = education_counts,
        formula_type = "full",  # Full factor smooth model
        education_var = "education",
        success_col = "n_unemployed",
        total_col = "n_total"
      )
    }
  ),

  ## Extract binomial model from comparison
  tar_target(
    model_binomial,
    model_comparison$binomial_model
  ),

  ## Extract quasi-binomial model from comparison
  tar_target(
    model_quasibinomial,
    model_comparison$quasibinomial_model
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
