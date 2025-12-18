#' Fit Quasi-Binomial GAM Model Across Education Levels
#'
#' Fits a quasi-binomial GAM to unemployment count data across all education levels,
#' with shared seasonal component and education-specific main effects.
#'
#' @param data Data frame with columns: n_unemployed, n_employed, time_index, month, education
#' @param education_levels Character vector of education levels to include.
#'   If NULL (default), uses all levels in data.
#' @param shared_seasonal Logical. If TRUE (default), shares seasonal pattern across
#'   education levels. If FALSE, allows education-specific seasonal effects.
#'
#' @return List containing:
#'   - model: Fitted GAM object
#'   - summary: Model summary statistics
#'   - predictions: Data frame with fitted values
#'   - dispersion: Estimated dispersion parameter
#'   - convergence_info: Convergence diagnostics
#'
#' @details
#' The model structure is:
#' cbind(n_unemployed, n_employed) ~ education + s(time_index) + s(month, bs="cc")
#'
#' This allows:
#' - Education main effects (intercept differences)
#' - Shared trend over time (time_index smooth)
#' - Shared seasonal pattern (month smooth with cyclic cubic spline)
#'
#' The quasi-binomial family properly accounts for overdispersion common in
#' unemployment count data. Use dispersion parameter to assess goodness of fit:
#' - dispersion ~  1: binomial model sufficient
#' - dispersion >> 1: overdispersion, quasi-binomial appropriate
#'
#' @examples
#' \dontrun{
#' data <- readRDS("data/education-spectrum-counts.rds")
#' result <- fit_education_binomial_gam(data)
#' summary(result$model)
#' }
#'
#' @export
fit_education_binomial_gam <- function(data,
                                        education_levels = NULL,
                                        shared_seasonal = TRUE) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  required_cols <- c("n_unemployed", "n_employed", "time_index", "month", "education")
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
  }

  # Filter to specified education levels if provided
  if (!is.null(education_levels)) {
    data <- data[data$education %in% education_levels, ]
  }

  # Ensure factors are properly set
  data$education <- factor(data$education)
  data$month <- as.integer(data$month)
  data$time_index <- as.numeric(data$time_index)

  # Add year if not present (for reference)
  if (!"year" %in% names(data)) {
    # Estimate year from time_index if possible (assumes monthly data starting 2000)
    data$year <- 2000 + floor((data$time_index - 1) / 12)
  }

  # Fit quasi-binomial GAM with shared seasonal component
  if (shared_seasonal) {
    formula <- cbind(n_unemployed, n_employed) ~ education +
      s(time_index) +
      s(month, bs = "cc")
  } else {
    # Alternative: education-specific seasonal components
    formula <- cbind(n_unemployed, n_employed) ~ education +
      s(time_index) +
      s(month, bs = "cc", by = education)
  }

  # Fit model
  model <- mgcv::gam(
    formula,
    data = data,
    family = quasibinomial(),
    method = "REML",
    control = list(maxit = 500)
  )

  # Extract diagnostics
  dispersion <- summary(model)$dispersion
  convergence_info <- list(
    converged = model$converged,
    gcv_score = model$gcv.ubre
  )

  # Generate predictions on original data
  predictions <- data.frame(
    education = data$education,
    year = data$year,
    time_index = data$time_index,
    month = data$month,
    n_unemployed = data$n_unemployed,
    n_employed = data$n_employed,
    fitted_prob = as.numeric(model$fitted.values),
    fitted_unemployed = as.numeric(model$fitted.values * (data$n_unemployed + data$n_employed)),
    residuals = residuals(model, type = "deviance")
  )

  # Return comprehensive result
  list(
    model = model,
    formula = formula,
    data = data,
    summary_stats = list(
      n_observations = nrow(data),
      n_education_levels = length(unique(data$education)),
      n_months = length(unique(data$month)),
      time_span = paste(range(data$year), collapse = "-"),
      dispersion = dispersion,
      deviance_explained = summary(model)$dev.expl
    ),
    predictions = predictions,
    convergence_info = convergence_info,
    call = match.call()
  )
}


#' Extract Education-Specific Unemployment Estimates
#'
#' Extract fitted unemployment rates for each education level at a specific time point
#'
#' @param model_result Result from fit_education_binomial_gam()
#' @param time_point Numeric. Time index for prediction (1-308 for monthly data 2000-2025)
#' @param month Numeric. Month (1-12) for prediction
#'
#' @return Data frame with unemployment estimates and standard errors for each education level
#'
#' @export
predict_education_unemployment <- function(model_result, time_point = 308, month = 6) {
  model <- model_result$model
  education_levels <- levels(model_result$data$education)

  # Create prediction data
  pred_data <- data.frame(
    education = factor(education_levels, levels = education_levels),
    time_index = time_point,
    month = month
  )

  # Generate predictions with standard errors
  preds <- predict(model, newdata = pred_data, type = "response", se.fit = TRUE)

  result <- data.frame(
    education = education_levels,
    unemployment_rate = preds$fit,
    se = preds$se.fit,
    ci_lower = pmax(0, preds$fit - 1.96 * preds$se.fit),
    ci_upper = pmin(1, preds$fit + 1.96 * preds$se.fit)
  )

  result
}
