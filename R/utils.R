#' PhD Unemployment Model Package
#'
#' Statistical modeling of PhD unemployment rates using R and Stan
#'
#' @docType package
#' @name phdunemployment-package
NULL

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
