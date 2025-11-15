#' Validate GAM Model Convergence
#'
#' Checks if a fitted GAM model converged successfully during estimation.
#'
#' @param model A fitted GAM model object from mgcv::gam()
#'
#' @return List with convergence diagnostics:
#'   \item{converged}{Logical. Did the model converge?}
#'   \item{iterations}{Number of iterations used}
#'   \item{message}{Convergence status message}
#'
#' @export
validate_convergence <- function(model) {
  if (!inherits(model, "gam")) {
    stop("model must be a GAM object from mgcv::gam()")
  }

  converged <- model$converged
  iterations <- if (!is.null(model$iter)) as.numeric(model$iter) else NA_real_

  message <- if (converged) {
    paste("Model converged successfully in", iterations, "iterations")
  } else {
    "Model did not converge - results may be unreliable"
  }

  list(
    converged = converged,
    iterations = iterations,
    message = message
  )
}


#' Validate GAM Model Residuals
#'
#' Performs comprehensive residual diagnostics including tests for normality,
#' autocorrelation, and heteroscedasticity.
#'
#' @param model A fitted GAM model object
#' @param data Data frame used to fit the model
#'
#' @return List with residual diagnostic results:
#'   \item{normality}{Shapiro-Wilk test results}
#'   \item{autocorrelation}{Ljung-Box test results}
#'   \item{heteroscedasticity}{Breusch-Pagan test results}
#'   \item{summary}{Overall summary of issues detected}
#'
#' @export
validate_residuals <- function(model, data) {
  residuals <- residuals(model, type = "deviance")

  # Normality test (Shapiro-Wilk)
  # For large samples, use sample of residuals
  n_resid <- length(residuals)
  if (n_resid > 5000) {
    resid_sample <- sample(residuals, 5000)
  } else {
    resid_sample <- residuals
  }

  normality_test <- shapiro.test(resid_sample)

  # Autocorrelation test (Ljung-Box)
  # Test for autocorrelation up to lag 12 (one year for monthly data)
  autocorr_test <- Box.test(residuals, lag = min(12, floor(n_resid / 5)), type = "Ljung-Box")

  # Heteroscedasticity test (Breusch-Pagan)
  # Test if residual variance depends on fitted values
  fitted_vals <- fitted(model)
  bp_lm <- lm(residuals^2 ~ fitted_vals)
  bp_test <- anova(bp_lm)
  bp_statistic <- bp_test$`F value`[1]
  bp_pval <- bp_test$`Pr(>F)`[1]

  # Summary of issues
  issues <- character(0)
  if (normality_test$p.value < 0.05) {
    issues <- c(issues, "Non-normal residuals detected")
  }
  if (autocorr_test$p.value < 0.05) {
    issues <- c(issues, "Significant autocorrelation detected")
  }
  if (bp_pval < 0.05) {
    issues <- c(issues, "Heteroscedasticity detected")
  }

  all_passed <- length(issues) == 0

  list(
    normality = list(
      test_statistic = normality_test$statistic,
      p_value = normality_test$p.value,
      passed = normality_test$p.value >= 0.05
    ),
    autocorrelation = list(
      test_statistic = autocorr_test$statistic,
      p_value = autocorr_test$p.value,
      passed = autocorr_test$p.value >= 0.05
    ),
    heteroscedasticity = list(
      test_statistic = bp_statistic,
      p_value = bp_pval,
      passed = bp_pval >= 0.05
    ),
    summary = list(
      all_checks_passed = all_passed,
      issues = if (length(issues) > 0) paste(issues, collapse = "; ") else "None"
    )
  )
}


#' Validate Concurvity in GAM Model
#'
#' Checks for concurvity (collinearity among smooth terms) which can
#' lead to unstable parameter estimates.
#'
#' @param model A fitted GAM model object
#'
#' @return List with concurvity diagnostics:
#'   \item{worst}{Worst-case concurvity for each smooth}
#'   \item{observed}{Observed concurvity}
#'   \item{estimate}{Estimated concurvity}
#'   \item{summary}{Summary with max concurvity and problematic terms}
#'
#' @export
validate_concurvity <- function(model) {
  if (!inherits(model, "gam")) {
    stop("model must be a GAM object from mgcv::gam()")
  }

  # Get concurvity measures from mgcv
  # concurvity() with full=TRUE returns a matrix with rows: worst, observed, estimate
  concurv <- mgcv::concurvity(model, full = TRUE)

  # Extract rows
  worst <- concurv["worst", ]
  observed <- concurv["observed", ]
  estimate <- concurv["estimate", ]

  # Find maximum concurvity (excluding parametric terms which may have NA)
  max_concurv <- max(worst, na.rm = TRUE)

  # Identify problematic terms (concurvity > 0.8)
  problematic_idx <- which(worst > 0.8)
  problematic_terms <- if (length(problematic_idx) > 0) {
    names(worst)[problematic_idx]
  } else {
    character(0)
  }

  list(
    worst = worst,
    observed = observed,
    estimate = estimate,
    summary = list(
      max_concurvity = max_concurv,
      problematic_terms = problematic_terms,
      high_concurvity = max_concurv > 0.8
    )
  )
}


#' Validate Effective Degrees of Freedom
#'
#' Checks if the model is using a reasonable number of degrees of freedom
#' relative to sample size, which can indicate overfitting.
#'
#' @param model A fitted GAM model object
#' @param data Data frame used to fit the model
#'
#' @return List with EDF diagnostics:
#'   \item{total_edf}{Total effective degrees of freedom}
#'   \item{edf_per_smooth}{EDF for each smooth term}
#'   \item{n_obs}{Number of observations}
#'   \item{edf_ratio}{Ratio of EDF to sample size}
#'   \item{summary}{Summary with overfitting risk assessment}
#'
#' @export
validate_edf <- function(model, data) {
  if (!inherits(model, "gam")) {
    stop("model must be a GAM object from mgcv::gam()")
  }

  total_edf <- sum(model$edf)
  edf_per_smooth <- model$edf
  n_obs <- nrow(data)
  edf_ratio <- total_edf / n_obs

  # Assess overfitting risk
  # Rules of thumb:
  # - edf_ratio < 0.1: low risk
  # - edf_ratio 0.1-0.3: moderate risk
  # - edf_ratio > 0.3: high risk
  overfitting_risk <- if (edf_ratio < 0.1) {
    "low"
  } else if (edf_ratio < 0.3) {
    "moderate"
  } else {
    "high"
  }

  list(
    total_edf = total_edf,
    edf_per_smooth = edf_per_smooth,
    n_obs = n_obs,
    edf_ratio = edf_ratio,
    summary = list(
      overfitting_risk = overfitting_risk,
      message = sprintf(
        "Using %.1f EDF for %d observations (%.1f%% ratio)",
        total_edf, n_obs, 100 * edf_ratio
      )
    )
  )
}


#' Validate Overall Model Fit
#'
#' Assesses model adequacy using multiple fit statistics including
#' R-squared, deviance explained, AIC, and prediction errors.
#'
#' @param model A fitted GAM model object
#' @param data Data frame used to fit the model
#'
#' @return List with fit statistics:
#'   \item{r_squared}{R-squared value}
#'   \item{adj_r_squared}{Adjusted R-squared}
#'   \item{deviance_explained}{Percentage of deviance explained}
#'   \item{aic}{Akaike Information Criterion}
#'   \item{bic}{Bayesian Information Criterion}
#'   \item{rmse}{Root mean squared error}
#'   \item{mae}{Mean absolute error}
#'   \item{summary}{Overall fit quality assessment}
#'
#' @export
validate_model_fit <- function(model, data) {
  if (!inherits(model, "gam")) {
    stop("model must be a GAM object from mgcv::gam()")
  }

  model_summary <- summary(model)

  r_squared <- model_summary$r.sq
  adj_r_squared <- if (!is.null(model_summary$r.sq)) {
    # Calculate adjusted R-squared
    n <- nrow(data)
    p <- sum(model$edf)
    1 - (1 - r_squared) * (n - 1) / (n - p - 1)
  } else {
    NA
  }

  deviance_explained <- model_summary$dev.expl * 100

  aic_val <- AIC(model)
  bic_val <- BIC(model)

  # Calculate prediction errors
  fitted_vals <- fitted(model)
  actual_vals <- model$y
  errors <- actual_vals - fitted_vals
  rmse <- sqrt(mean(errors^2))
  mae <- mean(abs(errors))

  # Categorize fit quality
  fit_quality <- if (r_squared >= 0.9) {
    "excellent"
  } else if (r_squared >= 0.7) {
    "good"
  } else if (r_squared >= 0.5) {
    "adequate"
  } else {
    "poor"
  }

  list(
    r_squared = r_squared,
    adj_r_squared = adj_r_squared,
    deviance_explained = deviance_explained,
    aic = aic_val,
    bic = bic_val,
    rmse = rmse,
    mae = mae,
    summary = list(
      fit_quality = fit_quality,
      message = sprintf(
        "Model explains %.1f%% of deviance (R² = %.3f)",
        deviance_explained, r_squared
      )
    )
  )
}


#' Comprehensive GAM Model Validation
#'
#' Performs complete model validation combining convergence checks,
#' residual diagnostics, concurvity assessment, EDF checks, and fit statistics.
#' Validation thresholds adapt based on whether analyzing real data or simulations.
#'
#' @param model A fitted GAM model object
#' @param data Data frame used to fit the model
#' @param validation_type Character. Type of validation: "exploratory" for real data analysis
#'   or "simulation" for parameter recovery validation. Affects interpretation of diagnostic tests.
#'   Default: "exploratory"
#' @param verbose Logical. Print validation summary? (default: TRUE)
#'
#' @return List with all validation components and overall summary
#'
#' @details
#' Validation type affects diagnostic interpretation:
#'
#' **Exploratory (real data)**:
#' - Normality test is informative but not a hard failure (use Q-Q plots instead)
#' - Large samples (n > 5000) will reject normality even when residuals are nearly perfect
#' - Autocorrelation test is critical for time series data
#' - Heteroscedasticity is common in real data, not necessarily a failure
#' - Convergence is always critical
#'
#' **Simulation (parameter recovery)**:
#' - All diagnostic tests should ideally pass
#' - Normality deviations may indicate model misspecification
#' - Stricter interpretation of all diagnostics
#'
#' @export
validate_gam_model <- function(model, data,
                               validation_type = c("exploratory", "simulation"),
                               verbose = TRUE) {
  if (!inherits(model, "gam")) {
    stop("model must be a GAM object from mgcv::gam()")
  }

  validation_type <- match.arg(validation_type)

  # Run all validation checks
  convergence <- validate_convergence(model)
  residuals_check <- validate_residuals(model, data)
  concurvity <- validate_concurvity(model)
  edf <- validate_edf(model, data)
  fit <- validate_model_fit(model, data)

  # Overall summary - issues and recommendations depend on validation type
  issues <- character(0)
  recommendations <- character(0)
  warnings <- character(0)  # Non-critical issues

  # Check convergence (ALWAYS critical)
  if (!convergence$converged) {
    issues <- c(issues, "Model did not converge")
    recommendations <- c(recommendations, "Try increasing iteration limit or adjusting model complexity")
  }

  # Check residuals - interpretation depends on validation type
  if (!residuals_check$summary$all_checks_passed) {
    # Normality test
    if (!residuals_check$normality$passed) {
      if (validation_type == "exploratory") {
        # For real data, normality test is informative but not critical
        warnings <- c(warnings, "Non-normal residuals detected (use Q-Q plots to assess severity)")
        recommendations <- c(recommendations, "Review Q-Q plot - normality test may be overly sensitive with large samples")
      } else {
        # For simulations, normality deviations may indicate issues
        issues <- c(issues, "Non-normal residuals detected")
        recommendations <- c(recommendations, "Check model specification and data generating process")
      }
    }

    # Autocorrelation test (important for both types)
    if (!residuals_check$autocorrelation$passed) {
      issues <- c(issues, "Significant autocorrelation detected")
      recommendations <- c(recommendations, "Consider adding AR terms or using gamm() for autocorrelation")
    }

    # Heteroscedasticity
    if (!residuals_check$heteroscedasticity$passed) {
      if (validation_type == "exploratory") {
        # For real data, heteroscedasticity is common and not always problematic
        warnings <- c(warnings, "Heteroscedasticity detected (GAMs are robust to moderate heteroscedasticity)")
        recommendations <- c(recommendations, "Consider variance modeling if heteroscedasticity is severe")
      } else {
        # For simulations, this may indicate model issues
        issues <- c(issues, "Heteroscedasticity detected")
        recommendations <- c(recommendations, "Check if variance structure matches data generating process")
      }
    }
  }

  # Check concurvity (important for both types)
  if (concurvity$summary$high_concurvity) {
    issues <- c(issues, "High concurvity detected among smooth terms")
    recommendations <- c(recommendations, "Consider reducing model complexity or removing redundant smooths")
  }

  # Check EDF
  if (edf$summary$overfitting_risk %in% c("moderate", "high")) {
    if (validation_type == "exploratory") {
      warnings <- c(warnings, paste("Overfitting risk:", edf$summary$overfitting_risk))
      recommendations <- c(recommendations, "Consider reducing basis dimensions (k) or using more penalization")
    } else {
      issues <- c(issues, paste("Overfitting risk:", edf$summary$overfitting_risk))
      recommendations <- c(recommendations, "Reduce model complexity for simulation studies")
    }
  }

  # Check fit quality (interpretation differs by type)
  if (fit$summary$fit_quality == "poor") {
    if (validation_type == "exploratory") {
      # For real data, poor fit may be acceptable if model is appropriately simple
      warnings <- c(warnings, "Poor model fit (R² < 0.5) - may be acceptable for complex real data")
      recommendations <- c(recommendations, "Consider if model complexity is appropriate for research question")
    } else {
      # For simulations, poor fit suggests model-DGP mismatch
      issues <- c(issues, "Poor model fit (R² < 0.5)")
      recommendations <- c(recommendations, "Model may not match data generating process")
    }
  }

  # Overall validation status
  # For exploratory: only critical issues cause failure
  # For simulation: both issues and severe warnings cause failure
  validation_passed <- if (validation_type == "exploratory") {
    length(issues) == 0
  } else {
    length(issues) == 0 && length(warnings) == 0
  }

  overall_summary <- list(
    validation_passed = validation_passed,
    validation_type = validation_type,
    critical_issues = if (length(issues) > 0) paste(issues, collapse = "; ") else "None",
    warnings = if (length(warnings) > 0) paste(warnings, collapse = "; ") else "None",
    recommendations = if (length(recommendations) > 0) paste(recommendations, collapse = "; ") else "None",
    n_checks = 5,
    n_critical_issues = length(issues),
    n_warnings = length(warnings)
  )

  result <- list(
    convergence = convergence,
    residuals = residuals_check,
    concurvity = concurvity,
    edf = edf,
    fit = fit,
    overall_summary = overall_summary
  )

  # Add class for S3 method dispatch
  class(result) <- c("gam_validation", "list")

  # Print summary if verbose
  if (verbose) {
    print(result)
  }

  return(result)
}


#' Print Method for GAM Validation Results
#'
#' @param x A gam_validation object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.gam_validation <- function(x, ...) {
  cat("\n=== GAM Model Validation Report ===\n\n")

  # Show validation type if available
  if (!is.null(x$overall_summary$validation_type)) {
    cat("Validation Type:", toupper(x$overall_summary$validation_type), "\n")
  }

  cat("Overall Status:", if (x$overall_summary$validation_passed) "✓ PASSED" else "✗ FAILED", "\n\n")

  cat("Convergence:\n")
  cat("  ", x$convergence$message, "\n\n")

  cat("Model Fit:\n")
  cat("  R-squared:", sprintf("%.3f", x$fit$r_squared), "\n")
  cat("  Deviance explained:", sprintf("%.1f%%", x$fit$deviance_explained), "\n")
  cat("  Quality:", x$fit$summary$fit_quality, "\n\n")

  cat("Residual Diagnostics:\n")
  cat("  Normality:", if (x$residuals$normality$passed) "✓" else "✗",
      sprintf("(p = %.3f)", x$residuals$normality$p_value), "\n")
  cat("  Autocorrelation:", if (x$residuals$autocorrelation$passed) "✓" else "✗",
      sprintf("(p = %.3f)", x$residuals$autocorrelation$p_value), "\n")
  cat("  Heteroscedasticity:", if (x$residuals$heteroscedasticity$passed) "✓" else "✗",
      sprintf("(p = %.3f)", x$residuals$heteroscedasticity$p_value), "\n\n")

  cat("Concurvity:\n")
  cat("  Max concurvity:", sprintf("%.3f", x$concurvity$summary$max_concurvity), "\n")
  if (x$concurvity$summary$high_concurvity) {
    cat("  ⚠ High concurvity detected\n")
  }
  cat("\n")

  cat("Effective Degrees of Freedom:\n")
  cat("  Total EDF:", sprintf("%.1f", x$edf$total_edf), "\n")
  cat("  EDF ratio:", sprintf("%.1f%%", 100 * x$edf$edf_ratio), "\n")
  cat("  Overfitting risk:", x$edf$summary$overfitting_risk, "\n\n")

  # Show critical issues
  if (!is.null(x$overall_summary$critical_issues) && x$overall_summary$critical_issues != "None") {
    cat("Critical Issues:\n")
    cat("  ", x$overall_summary$critical_issues, "\n\n")
  }

  # Show warnings (for exploratory validation)
  if (!is.null(x$overall_summary$warnings) && x$overall_summary$warnings != "None") {
    cat("Warnings (informative):\n")
    cat("  ", x$overall_summary$warnings, "\n\n")
  }

  # Show recommendations
  if (!is.null(x$overall_summary$recommendations) && x$overall_summary$recommendations != "None") {
    cat("Recommendations:\n")
    cat("  ", x$overall_summary$recommendations, "\n\n")
  }

  invisible(x)
}


#' Validate Education-Specific Model Components
#'
#' Performs validation checks separately for each education level
#' in a factor smooth GAM model.
#'
#' @param model A fitted factor smooth GAM model
#' @param data Data frame used to fit the model
#' @param education_levels Character vector of education levels
#'
#' @return List with validation results for each education level
#'
#' @export
validate_education_specific_components <- function(model, data, education_levels) {
  results <- list()

  for (educ in education_levels) {
    # Get residuals for this education level
    educ_data <- data[data$education == educ, ]
    educ_idx <- which(data$education == educ)
    educ_resid <- residuals(model)[educ_idx]

    # Basic diagnostics
    resid_diagnostics <- list(
      mean_residual = mean(educ_resid),
      sd_residual = sd(educ_resid),
      normality_pval = if (length(educ_resid) > 3) {
        shapiro.test(if (length(educ_resid) > 5000) sample(educ_resid, 5000) else educ_resid)$p.value
      } else {
        NA
      }
    )

    # Smooth diagnostics (check EDF for this education level's smooths)
    smooth_names <- sapply(model$smooth, function(s) s$label)
    educ_smooth_idx <- grep(educ, smooth_names)
    educ_edf <- if (length(educ_smooth_idx) > 0) sum(model$edf[educ_smooth_idx]) else 0

    smooth_diagnostics <- list(
      n_smooths = length(educ_smooth_idx),
      total_edf = educ_edf
    )

    results[[educ]] <- list(
      residual_diagnostics = resid_diagnostics,
      smooth_diagnostics = smooth_diagnostics,
      sample_size = nrow(educ_data)
    )
  }

  return(results)
}


#' Create Diagnostic Plots for GAM Model
#'
#' Generates standard diagnostic plots for model checking.
#'
#' @param model A fitted GAM model object
#'
#' @return List of ggplot objects
#'
#' @export
create_diagnostic_plots <- function(model) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required for diagnostic plots")
  }

  residuals_vec <- residuals(model, type = "deviance")
  fitted_vals <- fitted(model)
  linear_pred <- predict(model, type = "link")

  plot_data <- data.frame(
    residuals = residuals_vec,
    fitted = fitted_vals,
    linear_predictor = linear_pred
  )

  # Residuals vs Fitted
  p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = fitted, y = residuals)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::geom_smooth(se = FALSE, color = "blue") +
    ggplot2::labs(
      title = "Residuals vs Fitted Values",
      x = "Fitted Values",
      y = "Deviance Residuals"
    ) +
    ggplot2::theme_minimal()

  # Q-Q Plot
  p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(sample = residuals)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(
      title = "Normal Q-Q Plot",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    ggplot2::theme_minimal()

  # Residuals vs Linear Predictor
  p3 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = linear_predictor, y = residuals)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::geom_smooth(se = FALSE, color = "blue") +
    ggplot2::labs(
      title = "Residuals vs Linear Predictor",
      x = "Linear Predictor",
      y = "Deviance Residuals"
    ) +
    ggplot2::theme_minimal()

  # Histogram of Residuals
  p4 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(count)), color = "red", linewidth = 1) +
    ggplot2::labs(
      title = "Distribution of Residuals",
      x = "Deviance Residuals",
      y = "Count"
    ) +
    ggplot2::theme_minimal()

  list(
    residuals_vs_fitted = p1,
    qq_plot = p2,
    residuals_vs_linear_predictor = p3,
    residuals_histogram = p4
  )
}


#' Export Validation Report to Markdown
#'
#' Creates a markdown report summarizing validation results.
#'
#' @param validation A gam_validation object from validate_gam_model()
#' @param output_file Path to output markdown file
#' @param include_plots Logical. Include diagnostic plots? (default: TRUE)
#'
#' @export
export_validation_report <- function(validation, output_file, include_plots = TRUE) {
  lines <- character(0)

  lines <- c(lines, "# GAM Model Validation Report\n")
  lines <- c(lines, paste("Generated:", Sys.time()), "\n")

  lines <- c(lines, "\n## Overall Status\n")
  status <- if (validation$overall_summary$validation_passed) "**PASSED** ✓" else "**FAILED** ✗"
  lines <- c(lines, paste("Validation Status:", status), "\n")

  lines <- c(lines, "\n## Convergence\n")
  lines <- c(lines, paste("-", validation$convergence$message), "\n")

  lines <- c(lines, "\n## Model Fit\n")
  lines <- c(lines, paste("- R-squared:", sprintf("%.3f", validation$fit$r_squared)))
  lines <- c(lines, paste("- Deviance explained:", sprintf("%.1f%%", validation$fit$deviance_explained)))
  lines <- c(lines, paste("- AIC:", sprintf("%.1f", validation$fit$aic)))
  lines <- c(lines, paste("- Quality:", validation$fit$summary$fit_quality), "\n")

  lines <- c(lines, "\n## Residual Diagnostics\n")
  lines <- c(lines, paste("- Normality test p-value:", sprintf("%.4f", validation$residuals$normality$p_value)))
  lines <- c(lines, paste("- Autocorrelation test p-value:", sprintf("%.4f", validation$residuals$autocorrelation$p_value)))
  lines <- c(lines, paste("- Heteroscedasticity test p-value:", sprintf("%.4f", validation$residuals$heteroscedasticity$p_value)), "\n")

  lines <- c(lines, "\n## Concurvity\n")
  lines <- c(lines, paste("- Maximum concurvity:", sprintf("%.3f", validation$concurvity$summary$max_concurvity)))
  if (validation$concurvity$summary$high_concurvity) {
    lines <- c(lines, "- **Warning**: High concurvity detected")
  }
  lines <- c(lines, "\n")

  lines <- c(lines, "\n## Effective Degrees of Freedom\n")
  lines <- c(lines, paste("- Total EDF:", sprintf("%.1f", validation$edf$total_edf)))
  lines <- c(lines, paste("- EDF ratio:", sprintf("%.1f%%", 100 * validation$edf$edf_ratio)))
  lines <- c(lines, paste("- Overfitting risk:", validation$edf$summary$overfitting_risk), "\n")

  if (!validation$overall_summary$validation_passed) {
    lines <- c(lines, "\n## Issues and Recommendations\n")
    lines <- c(lines, "### Issues Detected\n")
    lines <- c(lines, paste("-", validation$overall_summary$issues_detected), "\n")
    lines <- c(lines, "### Recommendations\n")
    lines <- c(lines, paste("-", validation$overall_summary$recommendations), "\n")
  }

  writeLines(lines, output_file)
  message("Validation report written to: ", output_file)
}
