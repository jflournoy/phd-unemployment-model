#!/usr/bin/env Rscript
#' Download Full CPS Dataset (2000-2025)
#'
#' This script downloads the complete IPUMS CPS monthly dataset needed for
#' PhD unemployment time series analysis. It requires an IPUMS API key.
#'
#' Usage:
#'   Rscript data-raw/download-full-dataset.R
#'
#' Environment Variables:
#'   IPUMS_API_KEY - Your IPUMS API key (required)
#'
#' Output:
#'   data-raw/ipums_data.rds - Full CPS dataset (2000-2025)
#'
#' Note: This download may take 30+ minutes and will download several GB of data.

# Load required packages
if (!requireNamespace("ipumsr", quietly = TRUE)) {
  stop("ipumsr package required. Install with: install.packages('ipumsr')")
}

if (!requireNamespace("here", quietly = TRUE)) {
  stop("here package required. Install with: install.packages('here')")
}

library(ipumsr)
library(here)

# Source the download functions
source(here("R", "ipums-download.R"))

# Main execution
main <- function() {
  cat("\n========================================\n")
  cat("CPS Full Dataset Download (2000-2025)\n")
  cat("========================================\n\n")

  # Check for API key
  api_key <- Sys.getenv("IPUMS_API_KEY")
  if (api_key == "") {
    stop(
      "IPUMS_API_KEY environment variable not set.\n",
      "Get your API key at: https://account.ipums.org/api_keys\n",
      "Then set it with: export IPUMS_API_KEY='your-key-here'\n",
      "Or in R: Sys.setenv(IPUMS_API_KEY = 'your-key-here')"
    )
  }

  # Generate sample list for 2000-2025
  cat("Generating sample list for 2000-2025...\n")
  samples <- generate_cps_samples(2000, 2025)
  cat("Found", length(samples), "available monthly CPS samples\n\n")

  # Show first and last samples
  cat("Sample range:\n")
  cat("  First:", head(samples, 1), "\n")
  cat("  Last:", tail(samples, 1), "\n\n")

  # Confirm download
  cat("This will download", length(samples), "monthly CPS samples.\n")
  cat("Estimated download time: 30-60 minutes\n")
  cat("Estimated data size: 15-20 GB compressed\n\n")

  # In non-interactive mode, proceed automatically
  if (!interactive()) {
    cat("Running in non-interactive mode, proceeding with download...\n\n")
    proceed <- TRUE
  } else {
    proceed <- readline("Proceed with download? (yes/no): ")
    proceed <- tolower(trimws(proceed)) %in% c("yes", "y")
  }

  if (!proceed) {
    cat("Download cancelled.\n")
    return(invisible(NULL))
  }

  # Check if data already exists and is recent
  data_file <- here("data-raw", "ipums_data.rds")
  if (file.exists(data_file)) {
    file_info <- file.info(data_file)
    age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))
    size_mb <- file_info$size / 1024^2

    cat("\n📁 Existing data file found:\n")
    cat("  Path:", data_file, "\n")
    cat("  Size:", sprintf("%.1f MB", size_mb), "\n")
    cat("  Age:", sprintf("%.1f days old", age_days), "\n")

    if (age_days < 30) {
      cat("\n✓ Data is recent (< 30 days old)\n")
      cat("  Using existing file to avoid unnecessary IPUMS API request\n")
      cat("  To force re-download, delete the file and re-run this script\n\n")

      # Load and summarize existing data
      cat("Loading existing data to verify...\n")
      data <- readRDS(data_file)

      cat("\nDataset Summary:\n")
      cat("  Total observations:", format(nrow(data), big.mark = ","), "\n")
      cat("  Years:", min(data$YEAR), "-", max(data$YEAR), "\n")
      cat("  Months per year:", length(unique(data$MONTH)), "\n")

      # PhD subset
      phd_data <- data[data$EDUC == 125, ]
      cat("\n  PhD observations:", format(nrow(phd_data), big.mark = ","), "\n")
      cat("  PhD percentage:", format(100 * nrow(phd_data) / nrow(data), digits = 2), "%\n")

      cat("\n✅ Full dataset ready for analysis!\n")
      cat("\nNext steps:\n")
      cat("  1. Run data completeness checks\n")
      cat("  2. Process data for time series modeling\n")
      cat("  3. Calculate unemployment rates by month\n\n")

      return(invisible(list(
        file_path = normalizePath(data_file),
        skipped = TRUE,
        reason = "data_recent",
        age_days = age_days
      )))
    } else {
      cat("\n⚠ Data is stale (>= 30 days old), will re-download\n\n")
    }
  }

  # Download the data (only if we got here - no recent data exists)
  cat("\nStarting download...\n")
  cat("Progress will be shown by IPUMS API\n\n")

  result <- tryCatch(
    {
      download_ipums_data(
        output_dir = here("data-raw"),
        use_api = TRUE,
        samples = samples,
        variables = c("YEAR", "MONTH", "EMPSTAT", "EDUC", "AGE", "SEX", "WTFINL"),
        extract_description = "PhD unemployment analysis 2000-2025 full time series",
        api_key = api_key,
        collection = "cps"
      )
    },
    error = function(e) {
      cat("\n❌ Download failed:\n")
      cat(e$message, "\n")
      return(NULL)
    }
  )

  if (is.null(result)) {
    cat("\nDownload unsuccessful. Check error messages above.\n")
    return(invisible(NULL))
  }

  # Success!
  cat("\n✅ Download completed successfully!\n\n")
  cat("Output file:", result$file_path, "\n")

  # Load and summarize the data
  cat("\nLoading data to verify...\n")
  data <- readRDS(result$file_path)

  cat("\nDataset Summary:\n")
  cat("  Total observations:", format(nrow(data), big.mark = ","), "\n")
  cat("  Years:", min(data$YEAR), "-", max(data$YEAR), "\n")
  cat("  Months per year:", length(unique(data$MONTH)), "\n")
  cat("  File size:", format(file.size(result$file_path) / 1024^2, digits = 2), "MB\n")

  # PhD subset
  phd_data <- data[data$EDUC == 125, ]
  cat("\n  PhD observations:", format(nrow(phd_data), big.mark = ","), "\n")
  cat("  PhD percentage:", format(100 * nrow(phd_data) / nrow(data), digits = 2), "%\n")

  cat("\n✅ Full dataset ready for analysis!\n")
  cat("\nNext steps:\n")
  cat("  1. Run data completeness checks\n")
  cat("  2. Process data for time series modeling\n")
  cat("  3. Calculate unemployment rates by month\n\n")

  return(invisible(result))
}

# Run main function
if (!interactive()) {
  main()
}
