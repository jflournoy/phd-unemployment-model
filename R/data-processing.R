#' Remove haven_labelled Classes from IPUMS Data
#'
#' Converts haven_labelled columns to base R types for compatibility with data.table.
#'
#' @param data Data frame with potential haven_labelled columns from IPUMS
#'
#' @return Data frame with haven_labelled classes removed
#'
#' @details
#' IPUMS data loaded with ipumsr contains haven_labelled columns that can cause
#' issues with data.table operations. This function strips the haven_labelled class
#' while preserving the underlying integer/numeric values.
#'
#' @examples
#' \dontrun{
#' cps_data <- readRDS("data-raw/ipums_data.rds")
#' cps_clean <- remove_haven_labels(cps_data)
#' }
#'
#' @export
remove_haven_labels <- function(data) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Convert each column, removing haven_labelled class
  for (col in names(data)) {
    if (inherits(data[[col]], "haven_labelled")) {
      # Strip haven_labelled class, keeping underlying integer/numeric
      data[[col]] <- as.vector(data[[col]])
    }
  }

  return(data)
}

#' Filter CPS Data to PhD Holders Only
#'
#' Extracts only respondents with doctoral degrees from CPS microdata.
#'
#' @param cps_data Data frame. CPS microdata with EDUC variable
#'
#' @return Data frame containing only PhD holders (EDUC == 125)
#'
#' @details
#' In CPS, EDUC codes education levels. Code 125 = Doctorate degree.
#' This function is typically the first step in analyzing PhD-specific unemployment.
#'
#' Uses data.table for efficient filtering of large datasets.
#'
#' @examples
#' \dontrun{
#' cps_data <- readRDS("data-raw/ipums_data.rds")
#' phd_data <- filter_phd_holders(cps_data)
#' }
#'
#' @export
filter_phd_holders <- function(cps_data) {
  # Validate input
  if (!is.data.frame(cps_data)) {
    stop("cps_data must be a data frame")
  }

  if (!"EDUC" %in% names(cps_data)) {
    stop("cps_data must contain EDUC variable")
  }

  # Remove haven_labelled classes for data.table compatibility
  cps_data <- remove_haven_labels(cps_data)

  # Use data.table for efficient filtering
  if (!requireNamespace("data.table", quietly = TRUE)) {
    # Fallback to base R if data.table not available
    phd_data <- cps_data[cps_data$EDUC == 125, ]
  } else {
    dt <- data.table::as.data.table(cps_data)
    phd_data <- as.data.frame(dt[EDUC == 125])
  }

  return(phd_data)
}

#' Calculate Weighted Unemployment Rate
#'
#' Computes unemployment rate using CPS employment status codes and person weights.
#'
#' @param data Data frame with EMPSTAT and WTFINL variables
#'
#' @return Numeric scalar. Unemployment rate (0-1 scale)
#'
#' @details
#' CPS EMPSTAT codes (detailed):
#' - 0: NIU (not in universe)
#' - 1: Armed Forces
#' - 10: At work
#' - 12: Has job, not at work last week
#' - 20: Unemployed
#' - 21: Unemployed, experienced worker
#' - 22: Unemployed, new worker
#' - 30-36: Not in labor force (retired, school, housework, disabled, other)
#'
#' Unemployment rate = (unemployed) / (employed + unemployed)
#' - Employed: EMPSTAT in [10, 12]
#' - Unemployed: EMPSTAT in [20, 21, 22]
#' - Not in labor force: EMPSTAT in [30-36] (excluded from calculation)
#'
#' Uses WTFINL (final person weight) for regular monthly samples or ASECWT for
#' March ASEC supplement to compute population-representative estimates.
#'
#' @examples
#' \dontrun{
#' rate <- calculate_unemployment_rate(phd_data)
#' }
#'
#' @export
calculate_unemployment_rate <- function(data) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!"EMPSTAT" %in% names(data)) {
    stop("data must contain EMPSTAT variable")
  }

  # Determine which weight variable to use
  # ASECWT for March ASEC supplement, WTFINL for regular monthly samples
  if ("ASECWT" %in% names(data) && any(!is.na(data$ASECWT))) {
    weight_var <- "ASECWT"
  } else if ("WTFINL" %in% names(data)) {
    weight_var <- "WTFINL"
  } else {
    stop("data must contain either WTFINL or ASECWT weight variable")
  }

  # Define employed and unemployed based on CPS EMPSTAT codes
  # Employed: 10 (at work) or 12 (has job, not at work)
  employed <- data$EMPSTAT %in% c(10, 12)

  # Unemployed: 20-22 only (unemployed persons in labor force)
  # 20: Unemployed, 21: Unemployed experienced worker, 22: Unemployed new worker
  # NOTE: Codes 30-36 are NOT in labor force (retired, school, housework, etc.)
  unemployed <- data$EMPSTAT %in% c(20, 21, 22)

  # Labor force = employed + unemployed
  in_labor_force <- employed | unemployed

  # Filter to labor force participants only
  lf_data <- data[in_labor_force, ]

  if (nrow(lf_data) == 0) {
    warning("No labor force participants found in data")
    return(NA_real_)
  }

  # Calculate weighted unemployment rate using the appropriate weight
  unemployed_lf <- lf_data$EMPSTAT %in% c(20, 21, 22)
  weights <- lf_data[[weight_var]]
  total_unemployed_weight <- sum(weights[unemployed_lf], na.rm = TRUE)
  total_lf_weight <- sum(weights, na.rm = TRUE)

  if (total_lf_weight == 0) {
    warning("Total weight is zero - cannot calculate unemployment rate")
    return(NA_real_)
  }

  unemployment_rate <- total_unemployed_weight / total_lf_weight

  return(unemployment_rate)
}

#' Calculate Monthly Unemployment Rates
#'
#' Aggregates CPS data by year-month and computes unemployment rates for each period.
#'
#' @param data Data frame with YEAR, MONTH, EMPSTAT, and WTFINL variables
#'
#' @return Data frame with columns:
#'   \item{YEAR}{Year}
#'   \item{MONTH}{Month (1-12)}
#'   \item{unemployment_rate}{Weighted unemployment rate (0-1 scale)}
#'   \item{n_obs}{Number of observations in that month}
#'
#' @details
#' This function:
#' 1. Groups data by YEAR and MONTH
#' 2. Calculates weighted unemployment rate for each month
#' 3. Returns sorted time series of monthly rates
#'
#' Result is sorted chronologically for time series analysis.
#'
#' @examples
#' \dontrun{
#' monthly_rates <- calculate_monthly_unemployment(phd_data)
#' }
#'
#' @export
calculate_monthly_unemployment <- function(data) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  required_vars <- c("YEAR", "MONTH", "EMPSTAT", "WTFINL")
  missing_vars <- setdiff(required_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("data missing required variables: ", paste(missing_vars, collapse = ", "))
  }

  # Use data.table for efficient single-pass aggregation
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table package required for efficient processing. Install with: install.packages('data.table')")
  }

  dt <- data.table::as.data.table(data)

  # Single-pass aggregation: calculate unemployment rate for all year-months at once
  # Define employed and unemployed status
  dt[, `:=`(
    is_employed = EMPSTAT %in% c(10, 12),
    is_unemployed = EMPSTAT %in% c(20, 21, 22)
  )]

  # Filter to labor force only
  dt_lf <- dt[is_employed | is_unemployed]

  # Check if ASECWT exists for use in March
  has_asecwt <- "ASECWT" %in% names(dt_lf)

  # Calculate unemployment rate by year-month
  # Use ASECWT for March (MONTH == 3) if available, WTFINL for all other months
  result_dt <- dt_lf[, {
    # Select appropriate weight variable
    # Use ASECWT for March (MONTH == 3) if available and has non-zero values
    # Use WTFINL for all other months
    if (has_asecwt && .BY$MONTH == 3 && any(!is.na(ASECWT) & ASECWT > 0)) {
      weight_col <- ASECWT
    } else {
      weight_col <- WTFINL
    }

    total_unemployed_weight <- sum(weight_col[is_unemployed], na.rm = TRUE)
    total_lf_weight <- sum(weight_col, na.rm = TRUE)

    if (total_lf_weight == 0 || is.na(total_lf_weight)) {
      list(
        unemployment_rate = NA_real_,
        n_obs = .N
      )
    } else {
      list(
        unemployment_rate = total_unemployed_weight / total_lf_weight,
        n_obs = .N
      )
    }
  }, by = .(YEAR, MONTH)]

  # Sort chronologically
  data.table::setorder(result_dt, YEAR, MONTH)

  # Add time_index (sequential: 1, 2, 3, ...)
  result_dt[, time_index := seq_len(.N)]

  # Add date column (first day of each month)
  result_dt[, date := as.Date(paste(YEAR, MONTH, "01", sep = "-"))]

  # Convert back to data.frame for compatibility
  as.data.frame(result_dt)
}

#' Process CPS Data Pipeline
#'
#' Complete data processing pipeline: filter PhDs and calculate monthly unemployment.
#'
#' @param cps_data Data frame. Raw CPS microdata
#'
#' @return Data frame with monthly PhD unemployment rates
#'
#' @details
#' This is the main pipeline function that:
#' 1. Filters to PhD holders only (EDUC == 125)
#' 2. Calculates monthly unemployment rates
#' 3. Returns time series ready for analysis
#'
#' @examples
#' \dontrun{
#' cps_data <- readRDS("data-raw/ipums_data.rds")
#' phd_unemployment <- process_cps_data(cps_data)
#' }
#'
#' @export
process_cps_data <- function(cps_data) {
  # Filter to PhDs
  phd_data <- filter_phd_holders(cps_data)

  # Calculate monthly unemployment
  monthly_rates <- calculate_monthly_unemployment(phd_data)

  return(monthly_rates)
}
