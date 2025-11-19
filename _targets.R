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
  garbage_collection = TRUE  #

 Run GC between targets
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

  ## Binomial GAM (baseline model)
  tar_target(
    model_binomial,
    {
      fit_factor_smooth_binomial(
        data = education_counts,
        formula = cbind(n_unemployed, n_total - n_unemployed) ~
          s(time_index, k = 30) +
          s(month, bs = "cc", k = 12) +
          s(time_index, education, bs = "fs", k = 10, m = 1),
        family = binomial()
      )
    }
  ),

  ## Quasi-binomial GAM (handles overdispersion)
  tar_target(
    model_quasibinomial,
    {
      fit_factor_smooth_binomial(
        data = education_counts,
        formula = cbind(n_unemployed, n_total - n_unemployed) ~
          s(time_index, k = 30) +
          s(month, bs = "cc", k = 12) +
          s(time_index, education, bs = "fs", k = 10, m = 1),
        family = quasibinomial()
      )
    }
  ),

  ## Model comparison
  tar_target(
    model_comparison,
    {
      compare_binomial_quasibinomial(
        binomial_model = model_binomial,
        quasibinomial_model = model_quasibinomial,
        data = education_counts
      )
    }
  ),

  # ==========================================================================
  # Report Targets
  # ==========================================================================

  ## Binomial vs Quasi-binomial comparison report
  tar_quarto(
    report_binomial_quasi,
    path = "reports/quasi-binomial-validation"
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
