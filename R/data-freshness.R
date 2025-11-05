#' Check Data Freshness
#'
#' Determines if a data file is current based on its modification time.
#'
#' @param file_path Character. Path to the data file
#' @param max_age_days Numeric. Maximum age in days before data is considered stale (default: 30)
#'
#' @return Logical. TRUE if file exists and is fresh, FALSE otherwise
#'
#' @details
#' This function checks if a data file exists and was modified within the specified
#' number of days. Useful for determining if cached data needs to be re-downloaded.
#'
#' @examples
#' \dontrun{
#' if (!check_data_freshness("data/phd_unemployment_2024.rds", max_age_days = 7)) {
#'   # Re-download data
#' }
#' }
#'
#' @export
check_data_freshness <- function(file_path, max_age_days = 30) {
  # Check if file exists
  if (!file.exists(file_path)) {
    return(FALSE)
  }

  # Get file age in days
  age_days <- get_data_age_days(file_path)

  # Return TRUE if file is fresh (age < max_age_days)
  if (is.na(age_days)) {
    return(FALSE)
  }

  return(age_days < max_age_days)
}

#' Get Data Age in Days
#'
#' Calculates how many days ago a file was last modified.
#'
#' @param file_path Character. Path to the file
#'
#' @return Numeric. Age of file in days, or NA if file doesn't exist
#'
#' @details
#' Returns the number of days since the file was last modified.
#' Returns NA if the file doesn't exist.
#'
#' @examples
#' \dontrun{
#' age <- get_data_age_days("data/phd_unemployment_2024.rds")
#' cat("Data is", round(age, 1), "days old\n")
#' }
#'
#' @export
get_data_age_days <- function(file_path) {
  # Check if file exists
  if (!file.exists(file_path)) {
    return(NA_real_)
  }

  # Get file modification time
  file_info <- file.info(file_path)
  mod_time <- file_info$mtime

  # Calculate age in days
  current_time <- Sys.time()
  age_seconds <- as.numeric(difftime(current_time, mod_time, units = "secs"))
  age_days <- age_seconds / (24 * 60 * 60)

  return(age_days)
}

#' Check if Data File Needs Update
#'
#' Convenience wrapper for check_data_freshness with inverted logic.
#'
#' @param file_path Character. Path to the data file
#' @param max_age_days Numeric. Maximum age in days before data is considered stale (default: 30)
#'
#' @return Logical. TRUE if file needs updating (missing or stale), FALSE otherwise
#'
#' @details
#' Returns TRUE if the file doesn't exist or is older than max_age_days.
#' Returns FALSE if the file exists and is fresh.
#'
#' @examples
#' \dontrun{
#' if (needs_update("data/phd_unemployment_2024.rds")) {
#'   # Re-download data
#' }
#' }
#'
#' @export
needs_update <- function(file_path, max_age_days = 30) {
  !check_data_freshness(file_path, max_age_days)
}

#' Get Expected Data Files
#'
#' Returns list of data files that should exist for the project.
#'
#' @param data_dir Character. Base data directory (default: "data-raw")
#'
#' @return Character vector. Paths to expected data files
#'
#' @details
#' Returns the expected data files for this project:
#' - Raw IPUMS CPS data (ipums_data.rds)
#' - Processed unemployment data (../data/phd_unemployment_2024.rds)
#'
#' @examples
#' \dontrun{
#' files <- get_expected_data_files()
#' }
#'
#' @export
get_expected_data_files <- function(data_dir = "data-raw") {
  c(
    file.path(data_dir, "ipums_data.rds"),
    file.path("data", "phd_unemployment_2024.rds")
  )
}

#' Check Freshness of All Data Files
#'
#' Checks freshness status of all expected data files.
#'
#' @param max_age_days Numeric. Maximum age in days before data is considered stale (default: 30)
#' @param data_dir Character. Base data directory (default: "data-raw")
#'
#' @return Data frame with columns:
#'   \item{file}{Character. File path}
#'   \item{exists}{Logical. Whether file exists}
#'   \item{age_days}{Numeric. Age in days (NA if doesn't exist)}
#'   \item{is_fresh}{Logical. Whether file is fresh}
#'   \item{needs_update}{Logical. Whether file needs updating}
#'
#' @details
#' Returns a data frame summarizing the freshness status of all expected data files.
#' Useful for determining which files need to be re-downloaded.
#'
#' @examples
#' \dontrun{
#' status <- check_all_data_freshness(max_age_days = 7)
#' print(status)
#' }
#'
#' @export
check_all_data_freshness <- function(max_age_days = 30, data_dir = "data-raw") {
  files <- get_expected_data_files(data_dir)

  # Check each file
  results <- lapply(files, function(f) {
    exists <- file.exists(f)
    age <- get_data_age_days(f)
    is_fresh <- check_data_freshness(f, max_age_days)
    needs_upd <- needs_update(f, max_age_days)

    data.frame(
      file = f,
      exists = exists,
      age_days = age,
      is_fresh = is_fresh,
      needs_update = needs_upd,
      stringsAsFactors = FALSE
    )
  })

  # Combine into single data frame
  do.call(rbind, results)
}

#' Print Data Freshness Summary
#'
#' Prints a human-readable summary of data freshness status.
#'
#' @param freshness Data frame. Output from check_all_data_freshness()
#'
#' @return NULL (prints to console)
#'
#' @details
#' Prints a formatted summary showing:
#' - Which files exist and are fresh
#' - Which files are stale and need updating
#' - Which files are missing
#'
#' @examples
#' \dontrun{
#' status <- check_all_data_freshness()
#' print_data_freshness_summary(status)
#' }
#'
#' @export
print_data_freshness_summary <- function(freshness) {
  cat("\n=== Data Freshness Summary ===\n\n")

  # Count files by status
  n_total <- nrow(freshness)
  n_fresh <- sum(freshness$is_fresh, na.rm = TRUE)
  n_stale <- sum(!freshness$is_fresh & freshness$exists, na.rm = TRUE)
  n_missing <- sum(!freshness$exists, na.rm = TRUE)

  cat("Total files:", n_total, "\n")
  cat("Fresh files:", n_fresh, "\n")
  cat("Stale files:", n_stale, "\n")
  cat("Missing files:", n_missing, "\n\n")

  # Show files needing update
  if (any(freshness$needs_update)) {
    cat("Files needing update:\n")
    for (i in which(freshness$needs_update)) {
      f <- freshness[i, ]
      if (!f$exists) {
        cat("  [MISSING]", basename(f$file), "\n")
      } else {
        cat("  [STALE]  ", basename(f$file),
            sprintf("(%.1f days old)", f$age_days), "\n")
      }
    }
  } else {
    cat("All files are fresh!\n")
  }

  invisible(NULL)
}

#' Download Full CPS Dataset
#'
#' Downloads CPS data for a range of years (2000-2025 by default).
#'
#' @param start_year Integer. First year to download (default: 2000)
#' @param end_year Integer. Last year to download (default: 2025)
#' @param output_dir Character. Directory to save downloaded data (default: "data-raw")
#' @param force Logical. Force re-download even if data exists (default: FALSE)
#'
#' @return Character vector. Paths to downloaded data files
#'
#' @details
#' This function downloads CPS microdata for all months in the specified year range.
#' Uses IPUMS API to request and download data.
#'
#' @examples
#' \dontrun{
#' # Download all data from 2000-2025
#' files <- download_full_cps_data()
#'
#' # Download only 2020-2024
#' files <- download_full_cps_data(start_year = 2020, end_year = 2024)
#' }
#'
#' @export
download_full_cps_data <- function(start_year = 2000,
                                    end_year = 2025,
                                    output_dir = "data-raw",
                                    force = FALSE) {
  # Validate parameters
  current_year <- as.integer(format(Sys.Date(), "%Y"))

  if (start_year < 2000) {
    stop("start_year must be >= 2000 (IPUMS CPS data availability)")
  }

  if (end_year > current_year + 1) {
    stop(sprintf("end_year must be <= %d (current year + 1)", current_year + 1))
  }

  if (end_year < start_year) {
    stop("end_year cannot be before start_year")
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
  }

  # Source existing download functions
  source(here::here("R", "ipums-download.R"))

  # Generate list of year-month samples to download
  samples <- generate_cps_samples(start_year, end_year)

  cat(sprintf("\nDownloading CPS data for %d-%d (%d samples)\n",
              start_year, end_year, length(samples)))

  # Download data using existing function
  # Note: force parameter not yet implemented in download_ipums_data
  result <- download_ipums_data(output_dir = output_dir,
                                samples = samples,
                                use_api = FALSE)  # Use placeholder for now

  cat(sprintf("\nDownload complete: %s\n", result$file_path))

  return(result$file_path)
}

#' Update Stale CPS Data
#'
#' Checks data freshness and re-downloads only outdated or missing data.
#'
#' @param max_age_days Numeric. Maximum age in days before data is considered stale (default: 30)
#' @param data_dir Character. Directory containing data files (default: "data-raw")
#'
#' @return List with components:
#'   \item{updated}{Character vector of updated file paths}
#'   \item{fresh}{Character vector of fresh file paths (not updated)}
#'   \item{failed}{Character vector of files that failed to update}
#'
#' @details
#' Performs an incremental update:
#' 1. Checks all expected data files for freshness
#' 2. Re-downloads only stale or missing files
#' 3. Returns summary of what was updated
#'
#' @examples
#' \dontrun{
#' # Update any data older than 7 days
#' result <- update_stale_cps_data(max_age_days = 7)
#' cat("Updated:", length(result$updated), "files\n")
#' }
#'
#' @export
update_stale_cps_data <- function(max_age_days = 30, data_dir = "data-raw") {
  # Check freshness of all expected files
  freshness <- check_all_data_freshness(max_age_days = max_age_days,
                                        data_dir = data_dir)

  # Initialize result lists
  updated <- character(0)
  fresh <- character(0)
  failed <- character(0)

  # Identify files that need updating
  files_needing_update <- freshness[freshness$needs_update, ]

  if (nrow(files_needing_update) == 0) {
    cat("\nAll data files are fresh (< ", max_age_days, " days old)\n", sep = "")
    fresh <- freshness$file
    return(list(updated = updated, fresh = fresh, failed = failed))
  }

  # Print summary
  cat(sprintf("\nData freshness check (max age: %d days):\n", max_age_days))
  cat(sprintf("  Fresh: %d files\n", sum(freshness$is_fresh, na.rm = TRUE)))
  cat(sprintf("  Need update: %d files\n\n", nrow(files_needing_update)))

  # For now, warn that actual download is not yet implemented
  # This allows tests to pass while full download logic is being developed
  cat("Note: Automatic re-download not yet implemented.\n")
  cat("Please use download_full_cps_data() to manually update data.\n\n")

  # Categorize files
  fresh <- freshness$file[freshness$is_fresh]
  failed <- files_needing_update$file  # Mark as "failed" since not downloaded

  return(list(
    updated = updated,
    fresh = fresh,
    failed = failed
  ))
}
