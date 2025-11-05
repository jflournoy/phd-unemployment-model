#' Data Completeness Validation and Reporting
#'
#' Functions for validating completeness of downloaded CPS data and generating
#' diagnostic reports.

#' Generate Completeness Report
#'
#' Creates a detailed report on data completeness by month.
#'
#' @param data Data frame. CPS data to analyze
#' @param expected_months Integer vector. Expected months (default: 1:12)
#' @param min_obs Integer. Minimum observations per month (default: 100)
#' @param required_vars Character vector. Required variables to check
#'
#' @return Data frame with columns:
#'   \item{year}{Integer. Year}
#'   \item{month}{Integer. Month}
#'   \item{n_obs}{Integer. Number of observations}
#'   \item{has_data}{Logical. Whether month has any data}
#'   \item{n_missing_vars}{Integer. Count of required vars with missing values}
#'   \item{pct_complete}{Numeric. Percentage of non-missing values}
#'   \item{issue}{Character. Description of any issues found}
#'
#' @details
#' Checks each expected month for:
#' - Presence of data
#' - Sufficient sample size
#' - Variable completeness (no missing values)
#'
#' @examples
#' \dontrun{
#' data <- readRDS("data-raw/ipums_data.rds")
#' report <- generate_completeness_report(data)
#' print_completeness_report(report)
#' }
#'
#' @export
generate_completeness_report <- function(data,
                                          expected_months = 1:12,
                                          min_obs = 100,
                                          required_vars = c("YEAR", "MONTH",
                                                           "EMPSTAT", "EDUC",
                                                           "WTFINL")) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!"MONTH" %in% names(data)) {
    stop("data must contain MONTH variable")
  }

  # Get year(s) in data
  years <- if ("YEAR" %in% names(data)) {
    unique(data$YEAR)
  } else {
    NA
  }

  # Initialize results
  results <- list()

  for (month in expected_months) {
    # Filter to this month
    month_data <- data[data$MONTH == month, ]
    n_obs <- nrow(month_data)
    has_data <- n_obs > 0

    if (!has_data) {
      # No data for this month
      results[[length(results) + 1]] <- data.frame(
        year = ifelse(length(years) == 1, years, NA),
        month = month,
        n_obs = 0,
        has_data = FALSE,
        n_missing_vars = NA_integer_,
        pct_complete = NA_real_,
        issue = "Missing data for month",
        stringsAsFactors = FALSE
      )
      next
    }

    # Check for required variables and calculate completeness
    vars_in_data <- required_vars[required_vars %in% names(month_data)]
    n_missing_vars <- 0
    total_values <- 0
    missing_values <- 0

    for (var in vars_in_data) {
      total_values <- total_values + n_obs
      missing_count <- sum(is.na(month_data[[var]]))
      missing_values <- missing_values + missing_count

      if (missing_count > 0) {
        n_missing_vars <- n_missing_vars + 1
      }
    }

    pct_complete <- if (total_values > 0) {
      ((total_values - missing_values) / total_values) * 100
    } else {
      0
    }

    # Determine issues
    issues <- character(0)
    if (n_obs < min_obs) {
      issues <- c(issues, sprintf("Small sample size (n=%d, expected >=%d)",
                                 n_obs, min_obs))
    }
    if (n_missing_vars > 0) {
      issues <- c(issues, sprintf("%d variables with missing values",
                                 n_missing_vars))
    }
    if (length(issues) == 0) {
      issues <- "OK"
    }

    results[[length(results) + 1]] <- data.frame(
      year = ifelse(length(years) == 1, years, NA),
      month = month,
      n_obs = n_obs,
      has_data = TRUE,
      n_missing_vars = n_missing_vars,
      pct_complete = pct_complete,
      issue = paste(issues, collapse = "; "),
      stringsAsFactors = FALSE
    )
  }

  # Combine into single data frame
  do.call(rbind, results)
}

#' Print Completeness Report
#'
#' Prints a human-readable summary of data completeness.
#'
#' @param report Data frame. Output from generate_completeness_report()
#'
#' @return NULL (prints to console)
#'
#' @details
#' Displays:
#' - Overall summary statistics
#' - Month-by-month breakdown
#' - Identified issues
#'
#' @examples
#' \dontrun{
#' report <- generate_completeness_report(data)
#' print_completeness_report(report)
#' }
#'
#' @export
print_completeness_report <- function(report) {
  cat("\n=== Data Completeness Report ===\n\n")

  # Overall statistics
  n_months_expected <- nrow(report)
  n_months_with_data <- sum(report$has_data)
  n_months_complete <- sum(report$issue == "OK", na.rm = TRUE)

  cat("Overall Summary:\n")
  cat("  Expected months:", n_months_expected, "\n")
  cat("  Months with data:", n_months_with_data,
      sprintf("(%.1f%%)\n", 100 * n_months_with_data / n_months_expected))
  cat("  Months complete:", n_months_complete,
      sprintf("(%.1f%%)\n", 100 * n_months_complete / n_months_expected))

  if (any(report$has_data)) {
    cat("  Total observations:", sum(report$n_obs, na.rm = TRUE), "\n")
    cat("  Avg obs/month:",
        sprintf("%.1f\n", mean(report$n_obs[report$has_data], na.rm = TRUE)))
  }

  # Month-by-month breakdown
  cat("\nMonth-by-Month Breakdown:\n\n")
  cat(sprintf("%-6s %-8s %-8s %-10s %s\n",
              "Month", "N Obs", "Complete", "Status", "Issues"))
  cat(strrep("-", 70), "\n")

  for (i in seq_len(nrow(report))) {
    r <- report[i, ]
    month_name <- month.abb[r$month]

    if (!r$has_data) {
      cat(sprintf("%-6s %-8s %-8s %-10s %s\n",
                  month_name, "0", "N/A", "MISSING", r$issue))
    } else {
      complete_pct <- sprintf("%.1f%%", r$pct_complete)
      status <- if (r$issue == "OK") "OK" else "ISSUES"

      cat(sprintf("%-6s %-8d %-8s %-10s %s\n",
                  month_name, r$n_obs, complete_pct, status, r$issue))
    }
  }

  cat("\n")

  # Summary of issues
  issues <- report[report$issue != "OK", ]
  if (nrow(issues) > 0) {
    cat("Issues Found:\n")
    for (i in seq_len(nrow(issues))) {
      cat("  -", month.abb[issues$month[i]], ":", issues$issue[i], "\n")
    }
  } else {
    cat("No issues found - data is complete!\n")
  }

  invisible(NULL)
}

#' Validate Data Completeness
#'
#' Quick validation check for data completeness.
#'
#' @param data Data frame. CPS data to validate
#' @param expected_months Integer vector. Expected months (default: 1:12)
#' @param min_obs Integer. Minimum observations per month (default: 100)
#' @param required_vars Character vector. Required variables to check
#'
#' @return Logical. TRUE if data passes all checks, FALSE otherwise
#'
#' @details
#' Returns FALSE if any of these conditions are met:
#' - Missing expected months
#' - Any month has < min_obs observations
#' - Any required variable has missing values
#'
#' @examples
#' \dontrun{
#' if (validate_data_completeness(data)) {
#'   cat("Data is complete!\n")
#' } else {
#'   cat("Data has issues - see report for details\n")
#' }
#' }
#'
#' @export
validate_data_completeness <- function(data,
                                       expected_months = 1:12,
                                       min_obs = 100,
                                       required_vars = c("YEAR", "MONTH",
                                                        "EMPSTAT", "EDUC",
                                                        "WTFINL")) {
  # Generate full report
  report <- generate_completeness_report(data,
                                         expected_months = expected_months,
                                         min_obs = min_obs,
                                         required_vars = required_vars)

  # Check if all months pass
  all(report$issue == "OK")
}
