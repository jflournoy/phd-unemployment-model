#' Data Completeness Validation and Reporting
#'
#' Functions for validating completeness of downloaded CPS data and generating
#' diagnostic reports.
#'
#' Uses data.table for efficient operations on large datasets.

#' Generate Completeness Report
#'
#' Creates a detailed report on data completeness by month and year.
#'
#' @param data Data frame. CPS data to analyze
#' @param expected_months Integer vector. Expected months (default: 1:12)
#' @param start_year Integer. First year to check (default: auto-detect from data)
#' @param end_year Integer. Last year to check (default: auto-detect from data)
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
#' Checks each expected year-month combination for:
#' - Presence of data
#' - Sufficient sample size
#' - Variable completeness (no missing values)
#'
#' For multi-year data (2000-2025), use start_year and end_year parameters.
#'
#' @examples
#' \dontrun{
#' # Single year
#' data_2024 <- readRDS("data-raw/ipums_data.rds")
#' data_2024 <- data_2024[data_2024$YEAR == 2024, ]
#' report <- generate_completeness_report(data_2024)
#'
#' # Multi-year
#' data_full <- readRDS("data-raw/ipums_data.rds")
#' report <- generate_completeness_report(data_full,
#'                                         start_year = 2000,
#'                                         end_year = 2025)
#' }
#'
#' @export
generate_completeness_report <- function(data,
                                          expected_months = 1:12,
                                          start_year = NULL,
                                          end_year = NULL,
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

  if (!"YEAR" %in% names(data)) {
    stop("data must contain YEAR variable")
  }

  # Determine year range
  if (is.null(start_year)) {
    start_year <- min(data$YEAR, na.rm = TRUE)
  }
  if (is.null(end_year)) {
    end_year <- max(data$YEAR, na.rm = TRUE)
  }

  years <- start_year:end_year

  # Convert to data.table for efficient operations
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table package required for efficient processing. Install with: install.packages('data.table')")
  }

  dt <- data.table::as.data.table(data)

  # Create complete grid of all expected year-month combinations
  expected_combinations <- data.table::CJ(
    year = years,
    month = expected_months
  )

  # Single-pass aggregation: calculate stats for all year-months at once
  vars_in_data <- required_vars[required_vars %in% names(dt)]

  # Count observations per year-month
  obs_counts <- dt[, .(n_obs = .N), by = .(year = YEAR, month = MONTH)]

  # Calculate missing value statistics
  if (length(vars_in_data) > 0) {
    missing_stats <- dt[, {
      total_vals <- .N * length(vars_in_data)
      missing_vals <- sum(sapply(.SD, function(x) sum(is.na(x))))
      n_vars_with_missing <- sum(sapply(.SD, function(x) any(is.na(x))))

      list(
        n_missing_vars = n_vars_with_missing,
        pct_complete = if (total_vals > 0) ((total_vals - missing_vals) / total_vals) * 100 else 0
      )
    }, by = .(year = YEAR, month = MONTH), .SDcols = vars_in_data]

    # Merge counts with missing stats
    result_dt <- data.table::merge.data.table(
      obs_counts,
      missing_stats,
      by = c("year", "month"),
      all = TRUE
    )
  } else {
    # No variables to check for missing values
    result_dt <- obs_counts
    result_dt[, `:=`(n_missing_vars = 0L, pct_complete = 100.0)]
  }

  # Merge with expected combinations to find missing year-months
  result_dt <- data.table::merge.data.table(
    expected_combinations,
    result_dt,
    by = c("year", "month"),
    all.x = TRUE
  )

  # Fill in missing values for year-months with no data
  result_dt[is.na(n_obs), `:=`(
    n_obs = 0L,
    n_missing_vars = NA_integer_,
    pct_complete = NA_real_
  )]

  # Add has_data flag
  result_dt[, has_data := n_obs > 0]

  # Generate issue descriptions
  result_dt[, issue := {
    issues <- character(.N)

    # Missing data
    issues[!has_data] <- sprintf("Missing data for %d-%02d", year[!has_data], month[!has_data])

    # Small sample size
    small_sample <- has_data & n_obs < min_obs
    if (any(small_sample)) {
      issues[small_sample] <- sprintf("Small sample size (n=%d, expected >=%d)",
                                      n_obs[small_sample], min_obs)
    }

    # Missing variables (append to existing issues)
    missing_vars <- has_data & n_missing_vars > 0
    if (any(missing_vars)) {
      var_msg <- sprintf("%d variables with missing values", n_missing_vars[missing_vars])
      issues[missing_vars] <- ifelse(
        issues[missing_vars] == "",
        var_msg,
        paste(issues[missing_vars], var_msg, sep = "; ")
      )
    }

    # Default to OK if no issues
    issues[has_data & issues == ""] <- "OK"

    issues
  }]

  # Sort by year and month
  data.table::setorder(result_dt, year, month)

  # Convert back to data.frame for compatibility
  as.data.frame(result_dt)
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

#' Validate Completeness for Time Range
#'
#' Validates that all expected year-month combinations have complete data.
#'
#' @param data Data frame. CPS data to validate
#' @param start_year Integer. First year to validate
#' @param end_year Integer. Last year to validate
#' @param expected_months Integer vector. Expected months (default: 1:12)
#' @param min_obs Integer. Minimum observations per month (default: 100)
#' @param required_vars Character vector. Required variables to check
#'
#' @return Logical. TRUE if all year-month combinations pass checks, FALSE otherwise
#'
#' @details
#' Validates full time series (e.g., 2000-2025) by checking all year-month
#' combinations for completeness.
#'
#' @examples
#' \dontrun{
#' data <- readRDS("data-raw/ipums_data.rds")
#' is_complete <- validate_completeness_time_range(data,
#'                                                  start_year = 2000,
#'                                                  end_year = 2025)
#' if (is_complete) {
#'   cat("Full 2000-2025 dataset is complete!\n")
#' }
#' }
#'
#' @export
validate_completeness_time_range <- function(data,
                                              start_year,
                                              end_year,
                                              expected_months = 1:12,
                                              min_obs = 100,
                                              required_vars = c("YEAR", "MONTH",
                                                               "EMPSTAT", "EDUC",
                                                               "WTFINL")) {
  # Generate full report
  report <- generate_completeness_report(data,
                                         expected_months = expected_months,
                                         start_year = start_year,
                                         end_year = end_year,
                                         min_obs = min_obs,
                                         required_vars = required_vars)

  # Check if all year-month combinations pass
  all(report$issue == "OK")
}

#' Plot Seasonal Pattern by Year
#'
#' Creates a line plot showing seasonal patterns across multiple years.
#' X-axis is months (1-12), with separate lines for each year.
#'
#' @param data Data frame with columns: YEAR, MONTH, and the variable to plot
#' @param y_var Character. Name of variable to plot on y-axis
#' @param y_label Character. Label for y-axis
#' @param title Character. Plot title
#' @param year_range Integer vector. Years to include (default: all)
#' @param highlight_years Integer vector. Years to highlight (default: none)
#'
#' @return NULL (creates plot)
#'
#' @details
#' Useful for visualizing:
#' - Sample size patterns across years
#' - Seasonal unemployment patterns
#' - Data quality trends over time
#'
#' @examples
#' \dontrun{
#' report <- generate_completeness_report(data, start_year = 2000, end_year = 2025)
#' plot_seasonal_pattern_by_year(report,
#'                                y_var = "n_obs",
#'                                y_label = "Sample Size",
#'                                title = "CPS Monthly Sample Sizes (2000-2025)")
#' }
#'
#' @export
plot_seasonal_pattern_by_year <- function(data,
                                           y_var,
                                           y_label = y_var,
                                           title = "Seasonal Pattern by Year",
                                           year_range = NULL,
                                           highlight_years = NULL) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  required_cols <- c("YEAR", "MONTH", y_var)
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    stop(sprintf("data must contain columns: %s",
                paste(missing_cols, collapse = ", ")))
  }

  # Filter to year range if specified
  if (!is.null(year_range)) {
    data <- data[data$YEAR %in% year_range, ]
  }

  # Get unique years
  years <- sort(unique(data$YEAR))
  n_years <- length(years)

  if (n_years == 0) {
    stop("No data to plot")
  }

  # Create color palette
  if (n_years <= 10) {
    # Use distinct colors for <= 10 years
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
    year_colors <- colors[1:n_years]
  } else {
    # Use color gradient for > 10 years
    year_colors <- colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))(n_years)
  }

  # Highlight specific years if requested
  if (!is.null(highlight_years)) {
    lwd_vals <- ifelse(years %in% highlight_years, 3, 1)
  } else {
    lwd_vals <- rep(1, n_years)
  }

  # Set up plot
  y_range <- range(data[[y_var]], na.rm = TRUE)
  plot(NULL, xlim = c(1, 12), ylim = y_range,
       xlab = "Month", ylab = y_label,
       main = title,
       xaxt = "n")

  # Add x-axis with month labels
  axis(1, at = 1:12, labels = month.abb)

  # Add grid
  grid(nx = 12, ny = NULL, col = "lightgray", lty = "dotted")

  # Plot lines for each year
  for (i in seq_along(years)) {
    year <- years[i]
    year_data <- data[data$YEAR == year, ]
    year_data <- year_data[order(year_data$MONTH), ]

    lines(year_data$MONTH, year_data[[y_var]],
          col = year_colors[i], lwd = lwd_vals[i])
  }

  # Add legend (simplified for many years)
  if (n_years <= 10) {
    legend("topright",
           legend = years,
           col = year_colors,
           lwd = lwd_vals,
           cex = 0.8,
           bg = "white")
  } else {
    # For many years, show gradient legend
    legend("topright",
           legend = c(years[1], "...", years[n_years]),
           col = c(year_colors[1], "gray", year_colors[n_years]),
           lwd = c(lwd_vals[1], 1, lwd_vals[n_years]),
           cex = 0.8,
           bg = "white")
  }

  invisible(NULL)
}
