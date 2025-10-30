#' PhD Unemployment Model Package
#'
#' Statistical modeling of PhD unemployment rates using R and Stan
#'
#' @docType package
#' @name phdunemployment-package
NULL

#' Calculate unemployment rate
#'
#' Calculate the unemployment rate as a proportion from employment counts
#'
#' @param data A data frame with columns `employed` and `unemployed`
#' @return Numeric vector of unemployment rates (proportions)
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(employed = 90, unemployed = 10)
#' calculate_unemployment_rate(data)
#' }
calculate_unemployment_rate <- function(data) {
  # Validate inputs
  if (!all(c("employed", "unemployed") %in% names(data))) {
    stop("Data must contain 'employed' and 'unemployed' columns")
  }

  if (any(data$employed < 0) || any(data$unemployed < 0)) {
    stop("Employment counts cannot be negative")
  }

  # Calculate labor force and rate
  labor_force <- data$employed + data$unemployed

  # Handle zero labor force
  if (any(labor_force == 0)) {
    warning("Some observations have zero labor force")
    rate <- ifelse(labor_force == 0, NA_real_, data$unemployed / labor_force)
  } else {
    rate <- data$unemployed / labor_force
  }

  return(rate)
}

#' Validate model convergence diagnostics
#'
#' Check that a brms model has converged based on standard diagnostics
#'
#' @param model A fitted brms model object
#' @param rhat_threshold Maximum acceptable Rhat value (default: 1.01)
#' @param ess_threshold Minimum acceptable ESS ratio (default: 0.1)
#' @return Logical indicating whether model has converged
#' @export
check_model_convergence <- function(model, rhat_threshold = 1.01, ess_threshold = 0.1) {
  # This is a placeholder - will be implemented when brms is available
  stop("Not yet implemented - requires brms package")
}
