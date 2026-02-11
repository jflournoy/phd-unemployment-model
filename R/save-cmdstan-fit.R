#' Save CmdStan Fit with CSV Files Preserved
#'
#' CRITICAL: CmdStanMCMC objects need their CSV files to work. This function
#' ensures CSV files are saved alongside the fit object, preventing the
#' "File does not exist" error when loading saved fits later.
#'
#' @param fit_result A list containing at minimum a 'fit' component (CmdStanMCMC object)
#' @param output_file Path where to save the result (should end in .qs or .rds)
#' @param format Either "qs" (default, faster) or "rds"
#' @param csv_dir Directory to save CSV files (default: same directory as output_file with _csv suffix)
#'
#' @return Invisibly returns the input fit_result
#' @export
#'
#' @examples
#' \dontrun{
#' result <- fit_ode_state_space_monotonic_spline(data)
#' save_cmdstan_fit(result, "models/my-fit.qs")
#' }
save_cmdstan_fit <- function(fit_result,
                              output_file,
                              format = c("qs", "rds"),
                              csv_dir = NULL) {

  format <- match.arg(format)

  # Validate input
  if (!is.list(fit_result)) {
    stop("fit_result must be a list")
  }

  if (!"fit" %in% names(fit_result)) {
    stop("fit_result must contain a 'fit' component")
  }

  if (!inherits(fit_result$fit, "CmdStanMCMC")) {
    stop("fit_result$fit must be a CmdStanMCMC object")
  }

  # Determine CSV directory
  if (is.null(csv_dir)) {
    csv_dir <- paste0(tools::file_path_sans_ext(output_file), "_csv")
  }

  # Create CSV directory
  if (!dir.exists(csv_dir)) {
    dir.create(csv_dir, recursive = TRUE)
  }

  # Save CSV files
  message("Saving Stan CSV files to: ", csv_dir)
  fit_result$fit$save_output_files(dir = csv_dir, basename = "chain")

  # Store CSV directory path in result
  fit_result$csv_dir <- csv_dir
  fit_result$csv_saved <- TRUE
  fit_result$save_time <- Sys.time()

  # Save the result
  message("Saving fit result to: ", output_file)
  if (format == "qs") {
    qs::qsave(fit_result, output_file, preset = "high")
  } else {
    saveRDS(fit_result, output_file, compress = "xz")
  }

  # Verify CSV files exist
  csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_files) == 0) {
    warning("No CSV files found after save! This shouldn't happen.")
  } else {
    message("Successfully saved ", length(csv_files), " CSV files")
  }

  message("\n✓ Fit saved with CSV files preserved!")
  message("  Fit file: ", output_file, " (",
          round(file.size(output_file)/1024/1024, 1), " MB)")
  message("  CSV dir:  ", csv_dir)

  invisible(fit_result)
}


#' Load CmdStan Fit with CSV Files
#'
#' Loads a saved fit and verifies CSV files are accessible. If CSV files are
#' in a non-standard location, attempts to locate them.
#'
#' @param input_file Path to saved fit (.qs or .rds)
#' @param csv_dir Optional: explicit path to CSV directory
#'
#' @return The loaded fit_result list
#' @export
#'
#' @examples
#' \dontrun{
#' result <- load_cmdstan_fit("models/my-fit.qs")
#' }
load_cmdstan_fit <- function(input_file, csv_dir = NULL) {

  # Determine format
  format <- if (grepl("\\.qs$", input_file)) "qs" else "rds"

  # Load the result
  message("Loading fit from: ", input_file)
  if (format == "qs") {
    result <- qs::qread(input_file)
  } else {
    result <- readRDS(input_file)
  }

  # Check if CSV directory is stored
  if (is.null(csv_dir) && "csv_dir" %in% names(result)) {
    csv_dir <- result$csv_dir
  }

  # Try to locate CSV files
  if (is.null(csv_dir)) {
    # Try default location
    csv_dir <- paste0(tools::file_path_sans_ext(input_file), "_csv")
  }

  # Verify CSV files exist
  if (!dir.exists(csv_dir)) {
    warning("CSV directory not found: ", csv_dir)
    warning("The fit object may not work properly for extracting summaries.")
    warning("You may need to re-fit the model with save_cmdstan_fit().")
  } else {
    csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
    if (length(csv_files) == 0) {
      warning("CSV directory exists but contains no CSV files!")
    } else {
      message("✓ Found ", length(csv_files), " CSV files in ", csv_dir)
    }
  }

  return(result)
}


#' Validate CmdStan Fit Before Saving (Helper)
#'
#' @keywords internal
validate_fit_before_save <- function(fit) {
  if (!inherits(fit, "CmdStanMCMC")) {
    stop("fit must be a CmdStanMCMC object")
  }

  # Try to get a simple summary to verify it works
  tryCatch({
    fit$summary(variables = "lp__")
  }, error = function(e) {
    stop("Fit object appears corrupted - cannot extract summaries. Error: ", e$message)
  })

  return(TRUE)
}
