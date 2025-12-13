#' Check for Overdispersion in Binomial Count Data
#'
#' Detects overdispersion by comparing observed variance to expected binomial variance
#' using Pearson chi-squared statistic.
#'
#' @param data Data frame containing count data
#' @param success_col Character. Name of column with number of successes (default: "n_unemployed")
#' @param total_col Character. Name of column with total trials (default: "n_total")
#' @param threshold Numeric. Dispersion parameter threshold for detecting overdispersion (default: 2)
#'
#' @return List with elements:
#'   \item{overdispersed}{Logical. TRUE if dispersion parameter > threshold}
#'   \item{dispersion_parameter}{Numeric. Estimated dispersion parameter (phi)}
#'   \item{pearson_chisq}{Numeric. Pearson chi-squared statistic}
#'   \item{df_residual}{Integer. Residual degrees of freedom}
#'
#' @details
#' For binomial data, the variance should equal n*p*(1-p). If the observed variance
#' is larger, we have overdispersion. This function fits a simple binomial GLM and
#' computes the dispersion parameter as:
#'
#' phi = Pearson_chi_squared / df_residual
#'
#' Values of phi > 1 indicate overdispersion. Common thresholds:
#' - phi < 2: Mild overdispersion (binomial may be adequate)
#' - phi >= 2: Moderate overdispersion (consider quasi-binomial)
#' - phi > 10: Severe overdispersion (consider beta-binomial or other models)
#'
#' @examples
#' \dontrun{
#' # Check CPS data for overdispersion
#' result <- check_overdispersion(cps_data,
#'                                 success_col = "n_unemployed",
#'                                 total_col = "n_total")
#' if (result$overdispersed) {
#'   message("Use quasi-binomial family (phi = ", round(result$dispersion_parameter, 2), ")")
#' }
#' }
#'
#' @export
check_overdispersion <- function(data,
                                  success_col = "n_unemployed",
                                  total_col = "n_total",
                                  threshold = 2) {

  # Validate inputs
  if (!success_col %in% names(data)) {
    stop(sprintf("Column '%s' not found in data", success_col))
  }
  if (!total_col %in% names(data)) {
    stop(sprintf("Column '%s' not found in data", total_col))
  }

  # Extract counts
  n_success <- data[[success_col]]
  n_total <- data[[total_col]]

  # Fit simple binomial GLM (intercept only)
  # This gives us the Pearson chi-squared statistic
  p_hat <- sum(n_success) / sum(n_total)

  # Expected variance under binomial: n*p*(1-p)
  expected_var <- n_total * p_hat * (1 - p_hat)

  # Pearson residuals: (observed - expected) / sqrt(expected_var)
  expected <- n_total * p_hat
  pearson_resid <- (n_success - expected) / sqrt(expected_var)

  # Pearson chi-squared statistic
  pearson_chisq <- sum(pearson_resid^2)

  # Degrees of freedom (n observations - 1 parameter)
  df_resid <- length(n_success) - 1

  # Dispersion parameter estimate
  phi <- pearson_chisq / df_resid

  # Test for overdispersion
  overdispersed <- phi > threshold

  list(
    overdispersed = overdispersed,
    dispersion_parameter = phi,
    pearson_chisq = pearson_chisq,
    df_residual = df_resid
  )
}


#' Fit Factor Smooth Binomial/Quasi-Binomial GAM
#'
#' Fits a GAM with factor-by-smooth interactions for multiple education levels
#' using binomial or quasi-binomial family. For use with count data.
#'
#' @param data Data frame with unemployment count data
#' @param formula_type Character. Type of model formula (default: "full"):
#'   \itemize{
#'     \item "shared": Shared trend and seasonality across education levels
#'     \item "seasonal_by_education": Education-specific seasonality, shared trend
#'     \item "trend_by_education": Education-specific trend, shared seasonality
#'     \item "full": Education-specific trend and seasonality
#'   }
#' @param family_type Character. Family to use: "binomial" or "quasibinomial" (default: "binomial")
#' @param education_var Character. Name of education variable (default: "education")
#' @param success_col Character. Name of column with number of successes (default: "n_unemployed")
#' @param total_col Character. Name of column with total trials (default: "n_total")
#' @param shared_wiggliness Logical. If TRUE, use shared smoothing parameters across education levels (default: TRUE)
#'
#' @return GAM model object with attributes:
#'   \item{formula_type}{Character. Formula type used}
#'   \item{family_type}{Character. Family type used}
#'   \item{shared_wiggliness}{Logical. Whether wiggliness was shared}
#'
#' @details
#' Key differences between binomial and quasi-binomial:
#'
#' **Binomial**:
#' - Assumes variance = n*p*(1-p)
#' - Narrower prediction intervals
#' - More wiggly fits (lower REML smoothing parameters)
#' - Use when no evidence of overdispersion (phi ≈ 1)
#'
#' **Quasi-binomial**:
#' - Allows variance = phi * n*p*(1-p) where phi > 1
#' - Wider prediction intervals (more realistic with overdispersion)
#' - Smoother fits (REML λ ~1000× larger due to scale parameter)
#' - Use when overdispersion detected (phi > 2)
#'
#' Neither is "true" parameter recovery - this is model comparison on real data.
#'
#' @examples
#' \dontrun{
#' # Fit quasi-binomial model for CPS data with overdispersion
#' model <- fit_factor_smooth_binomial(
#'   data = cps_counts,
#'   formula_type = "full",
#'   family_type = "quasibinomial"
#' )
#' }
#'
#' @export
fit_factor_smooth_binomial <- function(data,
                                        formula_type = "full",
                                        family_type = "binomial",
                                        education_var = "education",
                                        success_col = "n_unemployed",
                                        total_col = "n_total",
                                        shared_wiggliness = TRUE) {

  # Validate inputs
  if (!success_col %in% names(data)) {
    stop(sprintf("Column '%s' not found in data. Need count data for binomial/quasi-binomial.", success_col))
  }
  if (!total_col %in% names(data)) {
    stop(sprintf("Column '%s' not found in data. Need total trials for binomial/quasi-binomial.", total_col))
  }
  if (!education_var %in% names(data)) {
    stop(sprintf("Column '%s' not found in data", education_var))
  }

  # Ensure education is factor
  if (!is.factor(data[[education_var]])) {
    data[[education_var]] <- as.factor(data[[education_var]])
  }

  # Validate family
  if (!family_type %in% c("binomial", "quasibinomial")) {
    stop("family_type must be 'binomial' or 'quasibinomial'")
  }

  # Create response variable for binomial family
  # cbind(successes, failures)
  data$n_failures <- data[[total_col]] - data[[success_col]]

  # Check number of education levels
  n_levels <- length(unique(data[[education_var]]))
  include_education_term <- n_levels > 1

  # Build formula based on formula_type
  if (formula_type == "shared") {
    # Shared smooth for all education levels
    if (shared_wiggliness) {
      if (include_education_term) {
        formula_str <- sprintf(
          "cbind(%s, n_failures) ~ %s + s(time_index, k = 10, id = 1) + s(month, bs = 'cc', k = 6, id = 2)",
          success_col, education_var
        )
      } else {
        # Only one level - omit education term
        formula_str <- sprintf(
          "cbind(%s, n_failures) ~ s(time_index, k = 10, id = 1) + s(month, bs = 'cc', k = 6, id = 2)",
          success_col
        )
      }
    } else {
      if (include_education_term) {
        formula_str <- sprintf(
          "cbind(%s, n_failures) ~ %s + s(time_index, k = 10) + s(month, bs = 'cc', k = 6)",
          success_col, education_var
        )
      } else {
        formula_str <- sprintf(
          "cbind(%s, n_failures) ~ s(time_index, k = 10) + s(month, bs = 'cc', k = 6)",
          success_col
        )
      }
    }
  } else if (formula_type == "seasonal_by_education") {
    # Education-specific seasonality, shared trend
    if (shared_wiggliness) {
      if (include_education_term) {
        formula_str <- sprintf(
          "cbind(%s, n_failures) ~ %s + s(time_index, k = 10, id = 1) + s(month, bs = 'cc', k = 6, by = %s, id = 2)",
          success_col, education_var, education_var
        )
      } else {
        # Single level - no need for 'by' term
        formula_str <- sprintf(
          "cbind(%s, n_failures) ~ s(time_index, k = 10, id = 1) + s(month, bs = 'cc', k = 6, id = 2)",
          success_col
        )
      }
    } else {
      if (include_education_term) {
        formula_str <- sprintf(
          "cbind(%s, n_failures) ~ %s + s(time_index, k = 10) + s(month, bs = 'cc', k = 6, by = %s)",
          success_col, education_var, education_var
        )
      } else {
        formula_str <- sprintf(
          "cbind(%s, n_failures) ~ s(time_index, k = 10) + s(month, bs = 'cc', k = 6)",
          success_col
        )
      }
    }
  } else if (formula_type == "trend_by_education") {
    # Education-specific trend, shared seasonality
    if (shared_wiggliness) {
      if (include_education_term) {
        formula_str <- sprintf(
          "cbind(%s, n_failures) ~ %s + s(time_index, k = 10, by = %s, id = 1) + s(month, bs = 'cc', k = 6, id = 2)",
          success_col, education_var, education_var
        )
      } else {
        formula_str <- sprintf(
          "cbind(%s, n_failures) ~ s(time_index, k = 10, id = 1) + s(month, bs = 'cc', k = 6, id = 2)",
          success_col
        )
      }
    } else {
      if (include_education_term) {
        formula_str <- sprintf(
          "cbind(%s, n_failures) ~ %s + s(time_index, k = 10, by = %s) + s(month, bs = 'cc', k = 6)",
          success_col, education_var, education_var
        )
      } else {
        formula_str <- sprintf(
          "cbind(%s, n_failures) ~ s(time_index, k = 10) + s(month, bs = 'cc', k = 6)",
          success_col
        )
      }
    }
  } else if (formula_type == "full") {
    # Education-specific trend AND seasonality
    if (shared_wiggliness) {
      if (include_education_term) {
        formula_str <- sprintf(
          "cbind(%s, n_failures) ~ %s + s(time_index, k = 10, by = %s, id = 1) + s(month, bs = 'cc', k = 6, by = %s, id = 2)",
          success_col, education_var, education_var, education_var
        )
      } else {
        formula_str <- sprintf(
          "cbind(%s, n_failures) ~ s(time_index, k = 10, id = 1) + s(month, bs = 'cc', k = 6, id = 2)",
          success_col
        )
      }
    } else {
      if (include_education_term) {
        formula_str <- sprintf(
          "cbind(%s, n_failures) ~ %s + s(time_index, k = 10, by = %s) + s(month, bs = 'cc', k = 6, by = %s)",
          success_col, education_var, education_var, education_var
        )
      } else {
        formula_str <- sprintf(
          "cbind(%s, n_failures) ~ s(time_index, k = 10) + s(month, bs = 'cc', k = 6)",
          success_col
        )
      }
    }
  } else {
    stop("formula_type must be one of: 'shared', 'seasonal_by_education', 'trend_by_education', 'full'")
  }

  # Convert to formula
  model_formula <- as.formula(formula_str)

  # Set family
  if (family_type == "binomial") {
    family_obj <- binomial()
  } else {
    family_obj <- quasibinomial()
  }

  # Fit model
  model <- mgcv::gam(
    formula = model_formula,
    data = data,
    family = family_obj,
    method = "REML"
  )

  # Store metadata
  attr(model, "formula_type") <- formula_type
  attr(model, "family_type") <- family_type
  attr(model, "shared_wiggliness") <- shared_wiggliness
  attr(model, "education_var") <- education_var

  return(model)
}


#' Compare Binomial and Quasi-Binomial GAMs
#'
#' Fits both binomial and quasi-binomial GAMs with same formula and compares
#' their properties (smoothing parameters, dispersion, deviance).
#'
#' @param data Data frame with unemployment count data
#' @param formula_type Character. Type of model formula (default: "full")
#' @param education_var Character. Name of education variable (default: "education")
#' @param success_col Character. Name of column with number of successes (default: "n_unemployed")
#' @param total_col Character. Name of column with total trials (default: "n_total")
#' @param shared_wiggliness Logical. If TRUE, use shared smoothing parameters (default: TRUE)
#'
#' @return List with elements:
#'   \item{binomial_model}{GAM fitted with binomial family}
#'   \item{quasibinomial_model}{GAM fitted with quasibinomial family}
#'   \item{comparison_table}{Data frame comparing key metrics}
#'
#' @details
#' This function helps evaluate the trade-off between binomial and quasi-binomial models:
#'
#' - **Binomial**: Narrower PIs, more wiggly (lower λ)
#' - **Quasi-binomial**: Wider PIs, smoother (higher λ due to scale parameter in REML)
#'
#' Key insight from validation: Quasi-binomial λ can be ~1000× larger than binomial λ,
#' leading to substantially smoother fits (not just SE inflation).
#'
#' @examples
#' \dontrun{
#' # Compare models on CPS data
#' comparison <- compare_binomial_quasibinomial(
#'   data = cps_counts,
#'   formula_type = "full"
#' )
#'
#' print(comparison$comparison_table)
#' }
#'
#' @export
compare_binomial_quasibinomial <- function(data,
                                            formula_type = "full",
                                            education_var = "education",
                                            success_col = "n_unemployed",
                                            total_col = "n_total",
                                            shared_wiggliness = TRUE) {

  # Fit binomial model
  model_binom <- fit_factor_smooth_binomial(
    data = data,
    formula_type = formula_type,
    family_type = "binomial",
    education_var = education_var,
    success_col = success_col,
    total_col = total_col,
    shared_wiggliness = shared_wiggliness
  )

  # Fit quasi-binomial model
  model_quasi <- fit_factor_smooth_binomial(
    data = data,
    formula_type = formula_type,
    family_type = "quasibinomial",
    education_var = education_var,
    success_col = success_col,
    total_col = total_col,
    shared_wiggliness = shared_wiggliness
  )

  # Extract comparison metrics
  comparison_table <- data.frame(
    model = c("binomial", "quasibinomial"),
    deviance = c(deviance(model_binom), deviance(model_quasi)),
    df_residual = c(model_binom$df.residual, model_quasi$df.residual),
    dispersion = c(1, summary(model_quasi)$dispersion),  # Binomial always 1
    mean_lambda = c(mean(model_binom$sp), mean(model_quasi$sp)),
    edf = c(sum(model_binom$edf), sum(model_quasi$edf)),
    stringsAsFactors = FALSE
  )

  # Create summary list with key metrics
  dispersion_param <- summary(model_quasi)$dispersion
  lambda_binom <- mean(model_binom$sp)
  lambda_quasi <- mean(model_quasi$sp)

  summary_list <- list(
    dispersion_parameter = dispersion_param,
    binomial_deviance = deviance(model_binom),
    quasibinomial_deviance = deviance(model_quasi),
    deviance_per_df = deviance(model_binom) / model_binom$df.residual,
    mean_lambda_binomial = lambda_binom,
    mean_lambda_quasibinomial = lambda_quasi,
    lambda_ratio = lambda_quasi / lambda_binom,
    binomial_edf = sum(model_binom$edf),
    quasibinomial_edf = sum(model_quasi$edf)
  )

  # Get fitted values for comparison
  # Extract time_index from model data (handles NA removal by GAM)
  model_data <- model_binom$model
  time_var <- if ("time_index" %in% names(data)) {
    data$time_index[rownames(model_data)]
  } else if ("date" %in% names(data)) {
    data$date[rownames(model_data)]
  } else {
    seq_len(nrow(model_data))  # Fallback to row numbers
  }

  fitted_values <- data.frame(
    time_index = time_var,
    observed = fitted(model_binom, type = "response"),  # Use fitted response for observed
    binomial_fit = fitted(model_binom),
    quasibinomial_fit = fitted(model_quasi)
  )

  # Return comparison
  list(
    binomial_model = model_binom,
    quasibinomial_model = model_quasi,
    comparison_table = comparison_table,
    summary = summary_list,
    fitted_values = fitted_values
  )
}


#' Extract Prediction Intervals from Binomial/Quasi-Binomial GAM
#'
#' Computes prediction intervals for new observations, accounting for both
#' uncertainty in the mean function and binomial sampling variance.
#'
#' @param model GAM model object (binomial or quasi-binomial family)
#' @param newdata Data frame with predictor values for new observations
#' @param alpha Numeric. Significance level for intervals (default: 0.05 for 95% intervals)
#'
#' @return Data frame with columns:
#'   \item{fit}{Predicted probability on response scale}
#'   \item{se_fit}{Standard error of fitted values (on link scale)}
#'   \item{pi_lower}{Lower bound of prediction interval}
#'   \item{pi_upper}{Upper bound of prediction interval}
#'
#' @details
#' Prediction intervals account for TWO sources of uncertainty:
#'
#' 1. **Uncertainty in mean function**: SE from GAM fit (se.fit)
#' 2. **Binomial sampling variance**: For given n and p, variance = p*(1-p)/n
#'
#' On link scale (logit):
#' - var_pred = se.fit^2 + 1/(n*p*(1-p))  [for binomial]
#' - var_pred = phi * [se.fit^2 + 1/(n*p*(1-p))]  [for quasi-binomial]
#'
#' IMPORTANT: These are prediction intervals for NEW observations, not confidence
#' intervals for the mean function.
#'
#' @examples
#' \dontrun{
#' model <- fit_factor_smooth_binomial(data, family_type = "quasibinomial")
#' pi <- extract_prediction_intervals(model, newdata = test_data)
#' }
#'
#' @export
extract_prediction_intervals <- function(model, newdata, alpha = 0.05) {

  # Get predictions on link scale with SE
  pred <- predict(model, newdata = newdata, type = "link", se.fit = TRUE)

  # Get fitted probabilities
  p_hat <- plogis(pred$fit)

  # Get sample sizes
  if ("n_total" %in% names(newdata)) {
    n_total <- newdata$n_total
  } else {
    stop("newdata must include 'n_total' column for prediction intervals")
  }

  # Variance on link scale
  var_link <- pred$se.fit^2

  # Sampling variance (binomial)
  # On link scale: var(logit(p)) ≈ 1/(n*p*(1-p))
  var_sampling <- 1 / (n_total * p_hat * (1 - p_hat))

  # Get dispersion parameter
  if (model$family$family == "quasibinomial") {
    phi <- summary(model)$dispersion
  } else {
    phi <- 1
  }

  # Total variance for prediction intervals
  var_pred <- phi * (var_link + var_sampling)
  se_pred <- sqrt(var_pred)

  # Compute prediction intervals on link scale
  z <- qnorm(1 - alpha/2)
  pi_lower_link <- pred$fit - z * se_pred
  pi_upper_link <- pred$fit + z * se_pred

  # Transform to response scale
  pi_lower <- plogis(pi_lower_link)
  pi_upper <- plogis(pi_upper_link)

  # Return results
  data.frame(
    fit = p_hat,
    se_fit = pred$se.fit,
    pi_lower = pi_lower,
    pi_upper = pi_upper
  )
}


#' Evaluate Factor Smooth Effects by Education Level
#'
#' Extracts education-specific predictions from a factor smooth GAM to visualize
#' and compare patterns across education levels.
#'
#' @param model GAM model object with factor smooth interactions
#' @param education_levels Character vector. Education levels to extract (default: all levels in model)
#' @param n_points Integer. Number of time points for predictions (default: 120)
#'
#' @return List of data frames, one per education level, each containing:
#'   \item{time_index}{Time index values}
#'   \item{month}{Month values}
#'   \item{fit}{Predicted unemployment probability}
#'   \item{se}{Standard error of predictions}
#'   \item{lower}{Lower CI bound}
#'   \item{upper}{Upper CI bound}
#'
#' @details
#' This function generates predictions across the time range for each education level,
#' allowing comparison of:
#' - Seasonal patterns (amplitude, timing)
#' - Long-term trends
#' - Baseline differences
#'
#' @examples
#' \dontrun{
#' model <- fit_factor_smooth_binomial(data, formula_type = "full")
#' effects <- evaluate_factor_smooth(model, education_levels = c("phd", "masters", "bachelors"))
#'
#' # Plot seasonal patterns
#' for (educ in names(effects)) {
#'   plot(effects[[educ]]$month, effects[[educ]]$fit, type = "l", main = educ)
#' }
#' }
#'
#' @export
evaluate_factor_smooth <- function(model, education_levels = NULL, n_points = 120) {

  # Get education variable name
  education_var <- attr(model, "education_var")
  if (is.null(education_var)) {
    education_var <- "education"  # Default
  }

  # Get education levels from model if not provided
  if (is.null(education_levels)) {
    education_levels <- levels(model$model[[education_var]])
  }

  # Create newdata grid for predictions
  time_index <- seq(1, n_points, length.out = n_points)
  month <- ((time_index - 1) %% 12) + 1

  # Extract predictions for each education level
  effects <- lapply(education_levels, function(educ) {
    # Create prediction grid for this education level
    newdata <- data.frame(
      time_index = time_index,
      month = month,
      n_total = rep(1000, n_points)  # Arbitrary for extracting smooth effects
    )
    newdata[[education_var]] <- factor(educ, levels = education_levels)

    # Get predictions
    pred <- predict(model, newdata = newdata, type = "link", se.fit = TRUE)

    # Transform to response scale
    fit <- plogis(pred$fit)
    se <- pred$se.fit

    # Confidence intervals (for mean function, not prediction)
    z <- qnorm(0.975)
    lower_link <- pred$fit - z * se
    upper_link <- pred$fit + z * se

    lower <- plogis(lower_link)
    upper <- plogis(upper_link)

    data.frame(
      time_index = time_index,
      month = month,
      fit = fit,
      se = se,
      lower = lower,
      upper = upper
    )
  })

  names(effects) <- education_levels
  return(effects)
}


#' Plot Binomial vs Quasi-Binomial Model Comparison
#'
#' Creates publication-quality ggplot2 visualizations comparing binomial and
#' quasi-binomial GAM models on real CPS unemployment data.
#'
#' @param binomial_model GAM fitted with binomial family
#' @param quasibinomial_model GAM fitted with quasibinomial family
#' @param data Data frame used to fit models
#' @param education_levels Character vector. Education levels to plot (default: all)
#'
#' @return List of ggplot objects:
#'   \item{fitted_comparison}{Side-by-side fitted values}
#'   \item{prediction_intervals}{Comparison of prediction interval widths}
#'   \item{smoothing_parameters}{Comparison of λ values}
#'   \item{dispersion_diagnostic}{Residual diagnostic for overdispersion}
#'   \item{combined}{Combined multi-panel figure}
#'
#' @details
#' This function generates comprehensive visualizations to understand the practical
#' differences between binomial and quasi-binomial models:
#'
#' **Key Visualizations**:
#' 1. **Fitted Values**: How smooth are the fits? (quasi more smooth due to λ)
#' 2. **Prediction Intervals**: How wide are uncertainties? (quasi wider)
#' 3. **Smoothing Parameters**: Magnitude of λ difference (often 1000×)
#' 4. **Dispersion**: Evidence of overdispersion in residuals
#'
#' **Design Choices**:
#' - Uses ggplot2 for publication-quality figures
#' - Color scheme distinguishes models clearly
#' - Includes confidence bands and observed data
#' - Facets by education level when applicable
#'
#' @examples
#' \dontrun{
#' # Compare models
#' comparison <- compare_binomial_quasibinomial(cps_counts)
#' plots <- plot_binomial_quasibinomial_comparison(
#'   binomial_model = comparison$binomial_model,
#'   quasibinomial_model = comparison$quasibinomial_model,
#'   data = cps_counts
#' )
#'
#' # Display combined figure
#' print(plots$combined)
#' }
#'
#' @export
plot_binomial_quasibinomial_comparison <- function(binomial_model,
                                                     quasibinomial_model,
                                                     data,
                                                     education_levels = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required for visualization")
  }

  # Get education variable
  education_var <- attr(binomial_model, "education_var")
  if (is.null(education_var)) education_var <- "education"

  # Get education levels
  if (is.null(education_levels)) {
    if (education_var %in% names(data)) {
      education_levels <- levels(as.factor(data[[education_var]]))
    }
  }

  # Create date column if not present
  if (!"date" %in% names(data)) {
    data$date <- as.Date(paste(data$year, data$month, "01", sep = "-"))
  }

  # Get predictions from both models
  pred_binom <- predict(binomial_model, type = "response", se.fit = TRUE)
  pred_quasi <- predict(quasibinomial_model, type = "response", se.fit = TRUE)

  # Prepare plotting data
  plot_data <- data.frame(
    date = data$date,
    time_index = data$time_index,
    month = data$month,
    observed = data$n_unemployed / data$n_total,
    fit_binomial = pred_binom$fit,
    se_binomial = pred_binom$se.fit,
    fit_quasibinomial = pred_quasi$fit,
    se_quasibinomial = pred_quasi$se.fit
  )

  # Add education if present
  if (education_var %in% names(data)) {
    plot_data$education <- data[[education_var]]
  }

  # Calculate dispersion parameter
  phi <- summary(quasibinomial_model)$dispersion

  # ===== PLOT 1: Fitted Values Comparison =====
  fitted_long <- reshape2::melt(
    plot_data,
    id.vars = c("date", "time_index", if(education_var %in% names(plot_data)) "education"),
    measure.vars = c("observed", "fit_binomial", "fit_quasibinomial"),
    variable.name = "model",
    value.name = "unemployment_rate"
  )

  fitted_long$model <- factor(
    fitted_long$model,
    levels = c("observed", "fit_binomial", "fit_quasibinomial"),
    labels = c("Observed", "Binomial", "Quasi-binomial")
  )

  p1 <- ggplot2::ggplot(fitted_long, ggplot2::aes(x = date, y = unemployment_rate,
                                                   color = model, linetype = model)) +
    ggplot2::geom_line(linewidth = 0.7, alpha = 0.8) +
    ggplot2::scale_color_manual(values = c("Observed" = "#2E86AB",
                                             "Binomial" = "#E63946",
                                             "Quasi-binomial" = "#06A77D")) +
    ggplot2::scale_linetype_manual(values = c("Observed" = "solid",
                                               "Binomial" = "dashed",
                                               "Quasi-binomial" = "solid")) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(
      title = "Binomial vs Quasi-Binomial: Fitted Values",
      subtitle = sprintf("Quasi-binomial smoother due to dispersion (φ = %.2f)", phi),
      x = "Date",
      y = "Unemployment Rate",
      color = "Model",
      linetype = "Model"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )

  # Facet by education if present
  if ("education" %in% names(fitted_long) && !is.null(education_levels) && length(education_levels) > 1) {
    p1 <- p1 + ggplot2::facet_wrap(~ education, ncol = 1, scales = "free_y")
  }

  # ===== PLOT 2: Prediction Interval Width Comparison =====
  # Calculate PI widths (approximation on response scale)
  plot_data$pi_width_binomial <- 2 * 1.96 * plot_data$se_binomial
  plot_data$pi_width_quasibinomial <- 2 * 1.96 * plot_data$se_quasibinomial * sqrt(phi)

  pi_long <- reshape2::melt(
    plot_data,
    id.vars = c("date", if(education_var %in% names(plot_data)) "education"),
    measure.vars = c("pi_width_binomial", "pi_width_quasibinomial"),
    variable.name = "model",
    value.name = "pi_width"
  )

  pi_long$model <- factor(
    pi_long$model,
    levels = c("pi_width_binomial", "pi_width_quasibinomial"),
    labels = c("Binomial", "Quasi-binomial")
  )

  p2 <- ggplot2::ggplot(pi_long, ggplot2::aes(x = date, y = pi_width, color = model)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_color_manual(values = c("Binomial" = "#E63946",
                                             "Quasi-binomial" = "#06A77D")) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
    ggplot2::labs(
      title = "Prediction Interval Width Comparison",
      subtitle = sprintf("Quasi-binomial PIs √φ = %.2f× wider", sqrt(phi)),
      x = "Date",
      y = "95% PI Width",
      color = "Model"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )

  if ("education" %in% names(pi_long) && !is.null(education_levels) && length(education_levels) > 1) {
    p2 <- p2 + ggplot2::facet_wrap(~ education, ncol = 1, scales = "free_y")
  }

  # ===== PLOT 3: Smoothing Parameters =====
  smooth_data <- data.frame(
    model = c("Binomial", "Quasi-binomial"),
    mean_lambda = c(mean(binomial_model$sp), mean(quasibinomial_model$sp)),
    max_lambda = c(max(binomial_model$sp), max(quasibinomial_model$sp))
  )

  smooth_data$model <- factor(smooth_data$model, levels = c("Binomial", "Quasi-binomial"))

  p3 <- ggplot2::ggplot(smooth_data, ggplot2::aes(x = model, y = mean_lambda, fill = model)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("λ = %.1f", mean_lambda)),
                       vjust = -0.5, size = 4, fontface = "bold") +
    ggplot2::scale_fill_manual(values = c("Binomial" = "#E63946",
                                            "Quasi-binomial" = "#06A77D")) +
    ggplot2::scale_y_log10(labels = scales::comma) +
    ggplot2::labs(
      title = "Smoothing Parameter Comparison",
      subtitle = sprintf("Quasi λ is %.0f× larger (log scale)",
                         smooth_data$mean_lambda[2] / smooth_data$mean_lambda[1]),
      x = "Model",
      y = "Mean Smoothing Parameter (λ, log scale)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )

  # ===== PLOT 4: Dispersion Diagnostic =====
  # Pearson residuals for binomial model
  resid_pearson <- residuals(binomial_model, type = "pearson")

  resid_data <- data.frame(
    fitted = fitted(binomial_model),
    residual = resid_pearson
  )

  p4 <- ggplot2::ggplot(resid_data, ggplot2::aes(x = fitted, y = residual)) +
    ggplot2::geom_point(alpha = 0.4, color = "#2E86AB") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_hline(yintercept = c(-2, 2), linetype = "dotted", color = "#E63946") +
    ggplot2::geom_smooth(method = "loess", se = TRUE, color = "#06A77D", linewidth = 1) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(
      title = "Overdispersion Diagnostic (Binomial Model)",
      subtitle = sprintf("Dispersion parameter φ = %.2f (>2 suggests quasi-binomial)", phi),
      x = "Fitted Unemployment Rate",
      y = "Pearson Residuals"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )

  # ===== COMBINED PLOT =====
  if (requireNamespace("patchwork", quietly = TRUE)) {
    p_combined <- (p1 / p2) | (p3 / p4)
    p_combined <- p_combined +
      patchwork::plot_annotation(
        title = "Binomial vs Quasi-Binomial GAM Comparison",
        subtitle = sprintf("Real CPS Data | φ = %.2f | Quasi λ %.0f× larger",
                          phi, smooth_data$mean_lambda[2] / smooth_data$mean_lambda[1]),
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(size = 16, face = "bold"),
          plot.subtitle = ggplot2::element_text(size = 12)
        )
      )
  } else {
    p_combined <- NULL
    message("Install patchwork for combined plot: install.packages('patchwork')")
  }

  # Return all plots
  list(
    fitted_comparison = p1,
    prediction_intervals = p2,
    smoothing_parameters = p3,
    dispersion_diagnostic = p4,
    combined = p_combined
  )
}
