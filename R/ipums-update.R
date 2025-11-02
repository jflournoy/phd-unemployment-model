#' Smart IPUMS CPS Data Update
#'
#' Checks if IPUMS CPS data exists and is current, downloads new data only when needed.
#' This function intelligently manages data freshness by:
#' - Downloading data if none exists
#' - Downloading data if existing data is older than max_age_days
#' - Skipping download if data is recent
#' - Forcing download when requested
#'
#' @param output_dir Character. Directory where data file is/will be saved.
#'   Default is "data-raw".
#' @param max_age_days Numeric. Maximum age of data in days before re-downloading.
#'   Default is 30 days.
#' @param force Logical. If TRUE, always download new data regardless of age.
#'   Default is FALSE.
#' @param use_api Logical. If TRUE, uses IPUMS API. If FALSE, uses placeholder.
#' @param samples Character vector. IPUMS CPS sample IDs (passed to download_ipums_data).
#'   Use generate_cps_samples() to create lists for time series (e.g., 2000-2025).
#' @param variables Character vector. IPUMS variables (passed to download_ipums_data).
#' @param extract_description Character. Description for extract request.
#' @param collection Character. IPUMS collection to use. Default is "cps".
#' @param ... Additional arguments passed to download_ipums_data()
#'
#' @return A list with:
#'   \item{downloaded}{Logical. TRUE if new data was downloaded}
#'   \item{reason}{Character. Why data was/wasn't downloaded}
#'   \item{file_path}{Character. Path to data file}
#'   \item{data_age_days}{Numeric. Age of data file in days (NA if just downloaded)}
#'   \item{extract_info}{List. IPUMS extract metadata (only if downloaded with use_api=TRUE)}
#'
#' @details
#' ## Update Logic
#'
#' The function follows this decision tree:
#'
#' 1. **force = TRUE**: Download new data (reason: "forced")
#' 2. **No data exists**: Download new data (reason: "no_data")
#' 3. **Data exists and age < max_age_days**: Skip download (reason: "data_recent")
#' 4. **Data exists and age >= max_age_days**: Download new data (reason: "data_stale")
#'
#' ## Use Cases
#'
#' **Automated Updates**: Run daily/weekly with default max_age_days to keep data current
#'
#' **Manual Refresh**: Use force=TRUE to get latest data regardless of age
#'
#' **Check-then-download**: Returns metadata about data age for reporting
#'
#' @examples
#' \dontrun{
#' # Check and update if data is older than 30 days (default)
#' result <- update_ipums_data()
#' if (result$downloaded) {
#'   message("Downloaded new data: ", result$reason)
#' } else {
#'   message("Using existing data (", round(result$data_age_days, 1), " days old)")
#' }
#'
#' # Force fresh download with full time series
#' samples <- generate_cps_samples(2000, 2025)
#' result <- update_ipums_data(
#'   force = TRUE,
#'   use_api = TRUE,
#'   samples = samples
#' )
#'
#' # Check weekly (7 days)
#' result <- update_ipums_data(max_age_days = 7, use_api = TRUE)
#' }
#'
#' @export
update_ipums_data <- function(output_dir = "data-raw",
                                max_age_days = 30,
                                force = FALSE,
                                use_api = FALSE,
                                samples = NULL,
                                variables = c("YEAR", "MONTH", "EMPSTAT", "EDUC", "AGE", "SEX", "WTFINL"),
                                extract_description = "PhD unemployment data",
                                collection = "cps",
                                ...) {
  # Construct expected file path
  file_path <- file.path(output_dir, "ipums_data.rds")

  # Check if data exists
  data_exists <- file.exists(file_path)

  # Initialize result
  result <- list(
    downloaded = FALSE,
    reason = NA_character_,
    file_path = file_path,
    data_age_days = NA_real_
  )

  # Calculate data age if file exists
  if (data_exists) {
    file_info <- file.info(file_path)
    file_age <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))
    result$data_age_days <- file_age
  }

  # Determine if download is needed
  should_download <- FALSE
  download_reason <- NA_character_

  if (force) {
    should_download <- TRUE
    download_reason <- "forced"
  } else if (!data_exists) {
    should_download <- TRUE
    download_reason <- "no_data"
  } else if (result$data_age_days >= max_age_days) {
    should_download <- TRUE
    download_reason <- "data_stale"
  } else {
    should_download <- FALSE
    download_reason <- "data_recent"
  }

  # Download if needed
  if (should_download) {
    message("Downloading IPUMS data (", download_reason, ")...")

    download_result <- download_ipums_data(
      output_dir = output_dir,
      use_api = use_api,
      samples = samples,
      variables = variables,
      extract_description = extract_description,
      collection = collection,
      ...
    )

    result$downloaded <- TRUE
    result$reason <- download_reason
    result$file_path <- download_result$file_path
    result$data_age_days <- 0  # Just downloaded

    # Include extract info if available
    if ("extract_info" %in% names(download_result)) {
      result$extract_info <- download_result$extract_info
    }
  } else {
    message("Using existing data (", round(result$data_age_days, 1), " days old)")
    result$downloaded <- FALSE
    result$reason <- download_reason
  }

  return(result)
}
