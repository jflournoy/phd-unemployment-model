#' Validate Parallel Model Results for Reports
#'
#' Check that required model results exist before rendering reports that
#' compare serial and parallel models. If results are missing, provides
#' clear instructions for generating them via the targets pipeline.
#'
#' @param parallel_file Path to parallel model results file (QS format).
#'   Defaults to `here::here("models", "ode-state-space-edu-parallel-fit.qs")`.
#' @param serial_file Optional path to serial model results file (QS format).
#'   If provided, also checks for serial model results.
#'
#' @return Invisibly returns TRUE if all required files exist. Otherwise,
#'   stops with informative error message.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Check parallel model exists
#' validate_parallel_model_exists()
#'
#' # Check both serial and parallel models
#' validate_parallel_model_exists(
#'   serial_file = here::here("models", "ode-state-space-efficient-fit.qs")
#' )
#' }
validate_parallel_model_exists <- function(
  parallel_file = here::here("models", "ode-state-space-edu-parallel-fit.qs"),
  serial_file = NULL
) {
  # Check parallel model file
  if (!file.exists(parallel_file)) {
    stop(
      "Parallel model results not found: ", parallel_file, "\n",
      "Please run the parallel model using the targets pipeline:\n",
      "  targets::tar_make(model_ode_state_space_edu_parallel)\n",
      "\n",
      "If you need to fit both serial and parallel models, run:\n",
      "  targets::tar_make()  # runs entire pipeline\n",
      call. = FALSE
    )
  }

  # Check if file appears to be valid (not a dummy/corrupted file)
  file_size <- file.size(parallel_file)
  if (file_size < 1e5) {  # Less than 100KB suggests dummy/corrupted file
    stop(
      "Parallel model results file appears to be corrupted or dummy: ", parallel_file, "\n",
      "File size: ", file_size, " bytes (expected > 100KB)\n",
      "This may be a leftover test file. Please run the parallel model:\n",
      "  targets::tar_make(model_ode_state_space_edu_parallel)\n",
      call. = FALSE
    )
  }

  # Optionally check serial model file
  if (!is.null(serial_file) && !file.exists(serial_file)) {
    stop(
      "Serial model results not found: ", serial_file, "\n",
      "Please run the serial model using the targets pipeline:\n",
      "  targets::tar_make(model_ode_state_space_efficient)\n",
      "\n",
      "If you need to fit both serial and parallel models, run:\n",
      "  targets::tar_make()  # runs entire pipeline\n",
      call. = FALSE
    )
  }

  # Check serial file size if provided
  if (!is.null(serial_file) && file.exists(serial_file)) {
    file_size <- file.size(serial_file)
    if (file_size < 1e6) {  # Less than 1MB suggests dummy/corrupted file
      stop(
        "Serial model results file appears to be corrupted or dummy: ", serial_file, "\n",
        "File size: ", file_size, " bytes (expected > 1MB)\n",
        "This may be a leftover test file. Please run the serial model:\n",
        "  targets::tar_make(model_ode_state_space_efficient)\n",
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}