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

  # Filter to PhD holders (EDUC == 125)
  phd_data <- cps_data[cps_data$EDUC == 125, ]

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
#' - 20: Unemployed, experienced worker
#' - 21: Unemployed, experienced worker, job loser - on layoff
#' - 22: Unemployed, experienced worker, other job loser
#' - 30-36: Unemployed, new/re-entrant
#'
#' Unemployment rate = (unemployed) / (employed + unemployed)
#' - Employed: EMPSTAT in [10, 12]
#' - Unemployed: EMPSTAT in [20, 21, 22, 30-36]
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

  # Unemployed: 20-22 (experienced), 30-36 (new/re-entrant)
  unemployed <- data$EMPSTAT >= 20 & data$EMPSTAT <= 36

  # Labor force = employed + unemployed
  in_labor_force <- employed | unemployed

  # Filter to labor force participants only
  lf_data <- data[in_labor_force, ]

  if (nrow(lf_data) == 0) {
    warning("No labor force participants found in data")
    return(NA_real_)
  }

  # Calculate weighted unemployment rate using the appropriate weight
  unemployed_lf <- lf_data$EMPSTAT >= 20 & lf_data$EMPSTAT <= 36
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

  # Get unique year-month combinations
  year_months <- unique(data[, c("YEAR", "MONTH")])

  # Calculate unemployment rate for each month
  results <- lapply(seq_len(nrow(year_months)), function(i) {
    year <- year_months$YEAR[i]
    month <- year_months$MONTH[i]

    # Filter to this month
    month_data <- data[data$YEAR == year & data$MONTH == month, ]

    # Calculate unemployment rate
    unemp_rate <- calculate_unemployment_rate(month_data)

    # Return result
    data.frame(
      YEAR = year,
      MONTH = month,
      unemployment_rate = unemp_rate,
      n_obs = nrow(month_data),
      stringsAsFactors = FALSE
    )
  })

  # Combine results
  result_df <- do.call(rbind, results)

  # Sort chronologically
  result_df <- result_df[order(result_df$YEAR, result_df$MONTH), ]

  # Reset row names
  rownames(result_df) <- NULL

  return(result_df)
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
