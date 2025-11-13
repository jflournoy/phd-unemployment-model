#' Parameter Recovery Validation Functions
#'
#' Functions for validating parameter recovery in factor smooth GAMs through
#' simulation-based coverage testing, bias quantification, and false positive
#' rate validation.
#'
#' @name parameter-recovery-validation
NULL

#' Extract Baseline (Intercept) Differences Between Education Levels
#'
#' Extracts the education factor coefficients from a fitted GAM and computes
#' pairwise differences with proper standard errors accounting for covariance.
#'
#' @param model A fitted GAM model with education as a factor variable
#' @param education_pairs List of character vectors, each with two education levels to compare
#' @param alpha Significance level for confidence intervals (default 0.05)
#'
#' @return Data frame with columns:
#'   \item{comparison}{String describing the comparison (e.g., "phd - masters")}
#'   \item{difference}{Point estimate of the baseline difference}
#'   \item{se}{Standard error of the difference}
#'   \item{lower}{Lower bound of CI}
#'   \item{upper}{Upper bound of CI}
#'   \item{significant}{Logical indicating if CI excludes zero}
#'
#' @details
#' The function extracts education factor coefficients from the model. One level
#' (typically the first alphabetically) is the reference, so its coefficient is
#' implicitly 0. For pairwise differences:
#' - If both levels are non-reference: difference = coef[level1] - coef[level2]
#' - If one is reference: difference = coef[other_level]
#'
#' Standard errors properly account for covariance using vcov(model).
#'
#' @examples
#' \dontrun{
#' # Fit model with education factor
#' model <- gam(unemployment_rate ~ education + s(time_index),
#'              data = sim_data)
#'
#' # Extract baseline differences
#' baseline_diffs <- extract_baseline_differences(
#'   model,
#'   education_pairs = list(c("phd", "masters"), c("phd", "bachelors"))
#' )
#' }
#'
#' @export
extract_baseline_differences <- function(model,
                                         education_pairs,
                                         alpha = 0.05) {

  # Get education variable name
  education_var <- attr(model, "education_var")
  if (is.null(education_var)) {
    education_var <- "education"
  }

  # Extract coefficients and vcov
  coefs <- coef(model)
  V <- vcov(model)

  # Find education factor coefficients
  # These will be named like "educationmasters", "educationphd", etc.
  educ_coef_names <- grep(paste0("^", education_var), names(coefs), value = TRUE)

  # Reference level has implicit coefficient of 0
  # Extract the reference level from the model
  reference_level <- levels(model$model[[education_var]])[1]

  # Function to get coefficient for an education level
  get_educ_coef <- function(level) {
    if (level == reference_level) {
      return(0)
    }
    coef_name <- paste0(education_var, level)
    if (coef_name %in% names(coefs)) {
      return(coefs[coef_name])
    } else {
      stop(paste("Could not find coefficient for education level:", level))
    }
  }

  # Function to get variance for an education level difference
  get_diff_variance <- function(level1, level2) {
    # Get coefficient names
    coef1_name <- paste0(education_var, level1)
    coef2_name <- paste0(education_var, level2)

    # Handle reference level (coefficient = 0, no variance term needed)
    if (level1 == reference_level && level2 == reference_level) {
      return(0)
    } else if (level1 == reference_level) {
      # Difference = 0 - coef2 = -coef2
      return(V[coef2_name, coef2_name])
    } else if (level2 == reference_level) {
      # Difference = coef1 - 0 = coef1
      return(V[coef1_name, coef1_name])
    } else {
      # Difference = coef1 - coef2
      # Var(coef1 - coef2) = Var(coef1) + Var(coef2) - 2*Cov(coef1, coef2)
      var1 <- V[coef1_name, coef1_name]
      var2 <- V[coef2_name, coef2_name]
      cov12 <- V[coef1_name, coef2_name]
      return(var1 + var2 - 2 * cov12)
    }
  }

  # Process each education pair
  results_list <- lapply(education_pairs, function(pair) {
    # Compute difference: first - second
    coef1 <- get_educ_coef(pair[1])
    coef2 <- get_educ_coef(pair[2])
    diff <- coef1 - coef2

    # Compute standard error
    var_diff <- get_diff_variance(pair[1], pair[2])
    se <- sqrt(var_diff)

    # Confidence intervals
    crit <- qnorm(1 - alpha / 2)
    lower <- diff - crit * se
    upper <- diff + crit * se
    significant <- abs(diff) > crit * se

    data.frame(
      comparison = paste(pair[1], "-", pair[2]),
      difference = diff,
      se = se,
      lower = lower,
      upper = upper,
      significant = significant,
      stringsAsFactors = FALSE
    )
  })

  # Combine all pairs
  do.call(rbind, results_list)
}


#' Validate Coverage of Difference Confidence Intervals
#'
#' Runs multiple simulations to test whether 95% confidence intervals for
#' differences between education levels contain the true differences in ~95%
#' of simulations (proper coverage).
#'
#' @param n_sims Number of simulations to run (default 300)
#' @param n_years Years of monthly data per simulation (default 15)
#' @param education_levels Character vector of education levels
#' @param baseline_rates Named vector of baseline unemployment rates
#' @param seasonal_amplitudes Named vector of seasonal amplitudes
#' @param trend_slopes Named vector of trend slopes
#' @param noise_sd Standard deviation of observation noise
#' @param difference_type Type of difference to test: "trend", "seasonal", or "baseline"
#' @param seed Random seed for reproducibility
#' @param verbose Print progress messages (default TRUE)
#' @param parallel Use parallel processing (requires future and furrr packages)
#' @param n_cores Number of cores to use if parallel=TRUE (default: detectCores() - 1)
#'
#' @return List with:
#'   \item{coverage_rate}{Proportion of CIs containing true difference}
#'   \item{bias}{Mean difference between estimated and true}
#'   \item{precision}{SD of estimation error}
#'   \item{detailed_results}{Data frame with all simulation results}
#'
#' @export
validate_difference_coverage <- function(n_sims = 300,
                                        n_years = 15,
                                        education_levels = c("phd", "masters", "bachelors"),
                                        baseline_rates = c(phd = 0.040, masters = 0.050, bachelors = 0.060),
                                        seasonal_amplitudes = c(phd = 0.010, masters = 0.015, bachelors = 0.020),
                                        trend_slopes = c(phd = -0.0001, masters = -0.0003, bachelors = -0.0005),
                                        noise_sd = 0.002,
                                        difference_type = c("trend", "seasonal", "baseline"),
                                        seed = NULL,
                                        verbose = TRUE,
                                        parallel = FALSE,
                                        n_cores = NULL) {

  difference_type <- match.arg(difference_type)

  if (!is.null(seed)) set.seed(seed)

  # Set up parallelization if requested
  if (parallel) {
    if (!requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("furrr", quietly = TRUE)) {
      warning("Packages 'future' and 'furrr' required for parallelization. Running sequentially.")
      parallel <- FALSE
    } else {
      if (is.null(n_cores)) {
        n_cores <- max(1, parallel::detectCores() - 1)
      }
      future::plan(future::multisession, workers = n_cores)
      if (verbose) cat(sprintf("Using %d cores for parallel processing\n", n_cores))
    }
  }

  if (verbose) {
    cat(sprintf("\nValidating %s difference coverage (%d simulations)...\n",
                difference_type, n_sims))
  }

  # Compute true differences for all pairs
  education_pairs <- list(
    c(education_levels[1], education_levels[2]),
    c(education_levels[1], education_levels[3]),
    c(education_levels[2], education_levels[3])
  )

  if (difference_type == "trend") {
    true_diffs <- c(
      trend_slopes[1] - trend_slopes[2],
      trend_slopes[1] - trend_slopes[3],
      trend_slopes[2] - trend_slopes[3]
    )
  } else if (difference_type == "seasonal") {
    true_diffs <- c(
      seasonal_amplitudes[1] - seasonal_amplitudes[2],
      seasonal_amplitudes[1] - seasonal_amplitudes[3],
      seasonal_amplitudes[2] - seasonal_amplitudes[3]
    )
  } else {  # baseline
    true_diffs <- c(
      baseline_rates[1] - baseline_rates[2],
      baseline_rates[1] - baseline_rates[3],
      baseline_rates[2] - baseline_rates[3]
    )
  }

  # Function to run one simulation
  run_one_sim <- function(i) {
    # Load package in worker (for parallel execution)
    if (!exists("simulate_multi_education_unemployment")) {
      requireNamespace("phdunemployment", quietly = TRUE)
    }

    # Simulate data
    sim_data <- simulate_multi_education_unemployment(
      n_years = n_years,
      education_levels = education_levels,
      baseline_rates = baseline_rates,
      seasonal_amplitudes = seasonal_amplitudes,
      trend_slopes = trend_slopes,
      noise_sd = noise_sd,
      seed = if (!is.null(seed)) seed + i else NULL
    )

    # Fit full model (m6)
    model <- fit_factor_smooth_gam(
      sim_data,
      formula_type = "full"
    )

    # Extract differences based on type
    if (difference_type == "trend") {
      # For trend differences, we want SLOPES (derivatives), not levels
      # Use compute_trend_slope_differences to estimate rate of change
      # Use middle of time series to avoid boundary effects
      mid_time <- floor(n_years * 12 / 2)
      diffs <- compute_trend_slope_differences(
        model,
        education_pairs = education_pairs,
        time_points = mid_time,  # Evaluate slope at middle of series
        eps = 0.1                # Finite difference step size
      )
    } else if (difference_type == "baseline") {
      diffs <- extract_baseline_differences(
        model,
        education_pairs = education_pairs
      )
    } else {  # seasonal - need to implement this
      # For now, skip seasonal (would require extracting seasonal amplitudes)
      stop("Seasonal amplitude difference extraction not yet implemented")
    }

    # Check coverage for each pair
    results_list <- lapply(1:length(education_pairs), function(j) {
      diff_row <- diffs[j, ]
      true_diff <- true_diffs[j]

      covered <- (true_diff >= diff_row$lower) & (true_diff <= diff_row$upper)
      error <- diff_row$difference - true_diff

      data.frame(
        sim_id = i,
        comparison = diff_row$comparison,
        true_difference = true_diff,
        estimated_difference = diff_row$difference,
        se = diff_row$se,
        lower = diff_row$lower,
        upper = diff_row$upper,
        covered = covered,
        error = error,
        stringsAsFactors = FALSE
      )
    })

    return(do.call(rbind, results_list))
  }

  # Run simulations (parallel or sequential)
  if (parallel) {
    # Export necessary objects to workers
    all_results <- furrr::future_map_dfr(
      1:n_sims,
      run_one_sim,
      .options = furrr::furrr_options(
        seed = TRUE,
        globals = list(
          run_one_sim = run_one_sim,
          n_years = n_years,
          education_levels = education_levels,
          baseline_rates = baseline_rates,
          seasonal_amplitudes = seasonal_amplitudes,
          trend_slopes = trend_slopes,
          noise_sd = noise_sd,
          difference_type = difference_type,
          seed = seed,
          education_pairs = education_pairs,
          true_diffs = true_diffs
        ),
        packages = "phdunemployment"
      )
    )
  } else {
    all_results_list <- lapply(1:n_sims, function(i) {
      if (verbose && i %% 50 == 0) {
        cat(sprintf("  Completed %d/%d simulations\n", i, n_sims))
      }
      run_one_sim(i)
    })
    all_results <- do.call(rbind, all_results_list)
  }

  # Clean up parallel workers
  if (parallel) {
    future::plan(future::sequential)
  }

  # Detailed results
  detailed_results <- all_results

  # Compute summary statistics by comparison
  summary_by_pair <- lapply(split(detailed_results, detailed_results$comparison), function(df) {
    data.frame(
      comparison = unique(df$comparison),
      true_difference = unique(df$true_difference),
      n_sims = nrow(df),
      coverage_rate = mean(df$covered),
      mean_estimate = mean(df$estimated_difference),
      bias = mean(df$error),
      precision = sd(df$estimated_difference),
      rmse = sqrt(mean(df$error^2)),
      median_absolute_error = median(abs(df$error)),
      q90_absolute_error = quantile(abs(df$error), 0.90),
      mean_se = mean(df$se),
      stringsAsFactors = FALSE
    )
  })

  summary_table <- do.call(rbind, summary_by_pair)
  rownames(summary_table) <- NULL

  # Overall coverage rate
  overall_coverage <- mean(detailed_results$covered)

  if (verbose) {
    cat(sprintf("\n=== Coverage Validation Results ===\n"))
    cat(sprintf("Overall coverage rate: %.1f%% (target: 95%%)\n", 100 * overall_coverage))
    cat(sprintf("Mean bias: %.6f\n", mean(detailed_results$error)))
    cat(sprintf("Precision (SD): %.6f\n", sd(detailed_results$estimated_difference)))
  }

  return(list(
    overall_coverage_rate = overall_coverage,
    summary_by_comparison = summary_table,
    detailed_results = detailed_results,
    difference_type = difference_type
  ))
}


#' Test False Positive Rate for Difference Detection
#'
#' Simulates data where NO differences exist between education levels and tests
#' how often we incorrectly conclude there are differences (Type I error rate).
#'
#' @param n_sims Number of simulations (default 200)
#' @param n_years Years of monthly data per simulation
#' @param education_levels Character vector of education levels
#' @param common_baseline Common baseline rate for all education levels
#' @param common_amplitude Common seasonal amplitude for all
#' @param common_trend Common trend slope for all
#' @param noise_sd Standard deviation of observation noise
#' @param seed Random seed for reproducibility
#' @param verbose Print progress (default TRUE)
#'
#' @return List with:
#'   \item{false_positive_rate}{Proportion of tests incorrectly finding significance}
#'   \item{detailed_results}{Data frame with all results}
#'
#' @export
test_false_positive_rate <- function(n_sims = 200,
                                     n_years = 15,
                                     education_levels = c("phd", "masters", "bachelors"),
                                     common_baseline = 0.050,
                                     common_amplitude = 0.015,
                                     common_trend = -0.0002,
                                     noise_sd = 0.002,
                                     seed = NULL,
                                     verbose = TRUE) {

  if (!is.null(seed)) set.seed(seed)

  if (verbose) {
    cat(sprintf("\nTesting false positive rate (%d simulations)...\n", n_sims))
    cat("All education levels have IDENTICAL parameters (no true differences)\n")
  }

  # Set up identical parameters for all levels
  baseline_rates <- setNames(rep(common_baseline, length(education_levels)),
                            education_levels)
  seasonal_amplitudes <- setNames(rep(common_amplitude, length(education_levels)),
                                 education_levels)
  trend_slopes <- setNames(rep(common_trend, length(education_levels)),
                          education_levels)

  education_pairs <- list(
    c(education_levels[1], education_levels[2]),
    c(education_levels[1], education_levels[3]),
    c(education_levels[2], education_levels[3])
  )

  all_results <- vector("list", n_sims)

  for (i in 1:n_sims) {
    if (verbose && i %% 50 == 0) {
      cat(sprintf("  Completed %d/%d simulations\n", i, n_sims))
    }

    # Simulate data with NO differences
    sim_data <- simulate_multi_education_unemployment(
      n_years = n_years,
      education_levels = education_levels,
      baseline_rates = baseline_rates,
      seasonal_amplitudes = seasonal_amplitudes,
      trend_slopes = trend_slopes,
      noise_sd = noise_sd,
      seed = if (!is.null(seed)) seed + i else NULL
    )

    # Fit full model
    model <- fit_factor_smooth_gam(
      sim_data,
      formula_type = "full"
    )

    # Test baseline differences
    baseline_diffs <- extract_baseline_differences(
      model,
      education_pairs = education_pairs
    )

    # Test trend differences (at t=1)
    trend_diffs <- compute_trend_differences(
      model,
      education_pairs = education_pairs,
      time_points = 1
    )

    # Record results
    for (j in 1:length(education_pairs)) {
      all_results[[i]] <- rbind(
        if (is.data.frame(all_results[[i]])) all_results[[i]] else NULL,
        data.frame(
          sim_id = i,
          comparison = paste(education_pairs[[j]][1], "-", education_pairs[[j]][2]),
          baseline_significant = baseline_diffs$significant[j],
          baseline_ci_excludes_zero = baseline_diffs$significant[j],
          trend_significant = trend_diffs$significant[j],
          trend_ci_excludes_zero = trend_diffs$significant[j],
          stringsAsFactors = FALSE
        )
      )
    }
  }

  detailed_results <- do.call(rbind, all_results)

  # Compute false positive rates
  fp_rate_baseline <- mean(detailed_results$baseline_significant)
  fp_rate_trend <- mean(detailed_results$trend_significant)
  fp_rate_any <- mean(detailed_results$baseline_significant | detailed_results$trend_significant)

  if (verbose) {
    cat(sprintf("\n=== False Positive Rate Results ===\n"))
    cat(sprintf("Baseline differences: %.1f%% false positives (target: ≤5%%)\n",
                100 * fp_rate_baseline))
    cat(sprintf("Trend differences: %.1f%% false positives (target: ≤5%%)\n",
                100 * fp_rate_trend))
    cat(sprintf("Any difference: %.1f%% false positives\n", 100 * fp_rate_any))
  }

  return(list(
    false_positive_rate_baseline = fp_rate_baseline,
    false_positive_rate_trend = fp_rate_trend,
    false_positive_rate_any = fp_rate_any,
    detailed_results = detailed_results
  ))
}


#' Analyze Model Selection with Enhanced Diagnostics
#'
#' Extends compare_nested_models with detailed Delta AIC diagnostics to
#' understand when complex models win vs simpler correct models.
#'
#' @param models Named list of fitted models from fit_nested_model_sequence
#' @param true_model Character string indicating the true generating model (e.g., "m4", "m6")
#' @param include_parameters Logical, whether to extract and compare education-specific parameters
#'
#' @return List with:
#'   \item{comparison_table}{Standard AIC comparison table}
#'   \item{top_model}{Name of model with lowest AIC}
#'   \item{true_model}{Name of true generating model (if provided)}
#'   \item{selected_correct}{Logical, was correct model selected?}
#'   \item{delta_aic_to_true}{Delta AIC of selected model vs true model}
#'   \item{models_within_2aic}{Models within 2 AIC units of best (essentially equivalent)}
#'   \item{parameter_cis}{If include_parameters=TRUE, CIs for education-specific effects from m6}
#'
#' @examples
#' \dontrun{
#' models <- fit_nested_model_sequence(sim_data)
#' analysis <- analyze_model_selection_detailed(models, true_model = "m4")
#' }
#'
#' @export
analyze_model_selection_detailed <- function(models,
                                             true_model = NULL,
                                             include_parameters = TRUE) {

  # Get standard comparison table
  comparison <- compare_nested_models(models)

  # Identify top model
  top_model <- comparison$model[1]

  # Check if selection was correct
  selected_correct <- if (!is.null(true_model)) {
    top_model == true_model
  } else {
    NA
  }

  # Delta AIC from selected to true model
  delta_aic_to_true <- if (!is.null(true_model)) {
    selected_aic <- comparison$AIC[comparison$model == top_model]
    true_aic <- comparison$AIC[comparison$model == true_model]
    selected_aic - true_aic
  } else {
    NA
  }

  # Models within 2 AIC units (essentially equivalent)
  min_aic <- min(comparison$AIC)
  models_within_2aic <- comparison$model[comparison$delta_AIC <= 2]

  # Extract parameter CIs from m6 if requested
  parameter_cis <- if (include_parameters && "m6" %in% names(models)) {
    model_m6 <- models[["m6"]]

    # Education pairs (assuming 3 levels)
    education_levels <- levels(model_m6$model$education)
    if (length(education_levels) >= 3) {
      education_pairs <- list(
        c(education_levels[1], education_levels[2]),
        c(education_levels[1], education_levels[3]),
        c(education_levels[2], education_levels[3])
      )

      # Baseline differences
      baseline_diffs <- extract_baseline_differences(
        model_m6,
        education_pairs = education_pairs
      )

      # Trend differences (at t=1 for simplicity)
      trend_diffs <- compute_trend_differences(
        model_m6,
        education_pairs = education_pairs,
        time_points = 1
      )

      list(
        baseline_differences = baseline_diffs,
        trend_differences = trend_diffs
      )
    } else {
      NULL
    }
  } else {
    NULL
  }

  result <- list(
    comparison_table = comparison,
    top_model = top_model,
    true_model = true_model,
    selected_correct = selected_correct,
    delta_aic_to_true = delta_aic_to_true,
    models_within_2aic = models_within_2aic,
    parameter_cis = parameter_cis
  )

  return(result)
}


#' Run Enhanced Model Selection Coverage Validation
#'
#' Runs model selection simulations with enhanced Delta AIC diagnostics to
#' understand when m6 wins and whether parameter CIs reveal negligible differences.
#'
#' @param n_sims Number of simulations per scenario
#' @param scenarios List of scenario definitions, each with simulation parameters and true model
#' @param seed Random seed
#' @param verbose Print progress
#' @param parallel Use parallel processing (requires future and furrr packages)
#' @param n_cores Number of cores to use if parallel=TRUE (default: detectCores() - 1)
#'
#' @return Data frame with detailed results for each simulation including:
#'   - Model selection outcomes
#'   - Delta AIC values
#'   - Parameter CI results when m6 is selected
#'
#' @export
validate_model_selection_enhanced <- function(n_sims = 100,
                                             scenarios = list(
                                               seasonal_only = list(
                                                 baseline_rates = c(phd = 0.040, masters = 0.050, bachelors = 0.065),
                                                 seasonal_amplitudes = c(phd = 0.005, masters = 0.015, bachelors = 0.025),
                                                 trend_slopes = c(phd = 0, masters = 0, bachelors = 0),
                                                 true_models = c("m5", "m6")
                                               ),
                                               trend_only = list(
                                                 baseline_rates = c(phd = 0.042, masters = 0.070, bachelors = 0.115),
                                                 seasonal_amplitudes = c(phd = 0.010, masters = 0.010, bachelors = 0.010),
                                                 trend_slopes = c(phd = -0.0001, masters = -0.0003, bachelors = -0.0005),
                                                 true_models = c("m4", "m6")
                                               ),
                                               both = list(
                                                 baseline_rates = c(phd = 0.042, masters = 0.075, bachelors = 0.125),
                                                 seasonal_amplitudes = c(phd = 0.005, masters = 0.015, bachelors = 0.025),
                                                 trend_slopes = c(phd = -0.0001, masters = -0.0003, bachelors = -0.0005),
                                                 true_models = c("m6")
                                               )
                                             ),
                                             seed = NULL,
                                             verbose = TRUE,
                                             parallel = FALSE,
                                             n_cores = NULL) {

  if (!is.null(seed)) set.seed(seed)

  # Set up parallelization if requested
  if (parallel) {
    if (!requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("furrr", quietly = TRUE)) {
      warning("Packages 'future' and 'furrr' required for parallelization. Running sequentially.")
      parallel <- FALSE
    } else {
      if (is.null(n_cores)) {
        n_cores <- max(1, parallel::detectCores() - 1)
      }
      future::plan(future::multisession, workers = n_cores)
      if (verbose) cat(sprintf("Using %d cores for parallel processing\n", n_cores))
    }
  }

  all_results <- list()
  counter <- 1

  for (scenario_name in names(scenarios)) {
    scenario <- scenarios[[scenario_name]]

    if (verbose) {
      cat(sprintf("\n=== Scenario: %s ===\n", scenario_name))
      cat(sprintf("Running %d simulations...\n", n_sims))
    }

    # Function to run one simulation
    run_one_sim <- function(sim_id) {
      # Simulate data
      sim_data <- simulate_multi_education_unemployment(
        n_years = 15,
        education_levels = c("phd", "masters", "bachelors"),
        baseline_rates = scenario$baseline_rates,
        seasonal_amplitudes = scenario$seasonal_amplitudes,
        trend_slopes = scenario$trend_slopes,
        noise_sd = 0.002,
        seed = if (!is.null(seed)) seed + counter * 1000 + sim_id else NULL
      )

      # Fit models
      models <- fit_nested_model_sequence(sim_data)

      # Analyze with enhanced diagnostics
      analysis <- analyze_model_selection_detailed(
        models,
        true_model = scenario$true_models[1],  # Primary true model
        include_parameters = TRUE
      )

      # Build result row
      result_row <- data.frame(
        scenario = scenario_name,
        sim_id = sim_id,
        top_model = analysis$top_model,
        true_model = scenario$true_models[1],
        selected_correct = analysis$top_model %in% scenario$true_models,
        delta_aic_to_true = analysis$delta_aic_to_true,
        models_within_2aic = paste(analysis$models_within_2aic, collapse = ","),
        stringsAsFactors = FALSE
      )

      # If m6 was selected, add parameter CI info
      if (analysis$top_model == "m6" && !is.null(analysis$parameter_cis)) {
        # Count how many baseline/trend CIs include zero
        baseline_cis_include_zero <- sum(
          analysis$parameter_cis$baseline_differences$lower <= 0 &
          analysis$parameter_cis$baseline_differences$upper >= 0
        )
        trend_cis_include_zero <- sum(
          analysis$parameter_cis$trend_differences$lower <= 0 &
          analysis$parameter_cis$trend_differences$upper >= 0
        )

        result_row$baseline_cis_include_zero <- baseline_cis_include_zero
        result_row$trend_cis_include_zero <- trend_cis_include_zero
        result_row$any_ci_includes_zero <- (baseline_cis_include_zero + trend_cis_include_zero) > 0
      } else {
        result_row$baseline_cis_include_zero <- NA
        result_row$trend_cis_include_zero <- NA
        result_row$any_ci_includes_zero <- NA
      }

      return(result_row)
    }

    # Run simulations (parallel or sequential)
    if (parallel) {
      scenario_results <- furrr::future_map_dfr(1:n_sims, run_one_sim,
                                               .options = furrr::furrr_options(seed = TRUE))
    } else {
      scenario_results <- do.call(rbind, lapply(1:n_sims, function(i) {
        if (verbose && i %% 25 == 0) {
          cat(sprintf("  Completed %d/%d\n", i, n_sims))
        }
        run_one_sim(i)
      }))
    }

    all_results[[scenario_name]] <- scenario_results
    counter <- counter + 1
  }

  # Combine all scenarios
  final_results <- do.call(rbind, all_results)
  rownames(final_results) <- NULL

  # Clean up parallel workers
  if (parallel) {
    future::plan(future::sequential)
  }

  return(final_results)
}
