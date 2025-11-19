# CPS Data Aggregation Functions
#
# Weighted aggregation functions built on CPS code constants
# Ensures weights are always used correctly and no magic numbers in code

# ==============================================================================
# Weighted Counting Primitives
# ==============================================================================

#' Count Employed Persons (Weighted)
#'
#' Returns weighted count of employed persons using EMPSTAT codes from constants.
#'
#' @param data Data frame with EMPSTAT column
#' @param weight_var Character. Name of weight variable (default: "WTFINL").
#'   Use NULL for unweighted count.
#'
#' @return Numeric. Weighted count of employed persons
#'
#' @details
#' Uses get_employed_codes() to identify employed persons, ensuring consistency
#' with CPS code definitions.
#'
#' @examples
#' \dontrun{
#' n_employed <- count_employed(cps_data, weight_var = "WTFINL")
#' }
#'
#' @export
count_employed <- function(data, weight_var = "WTFINL") {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  if (!"EMPSTAT" %in% names(data)) {
    stop("data must contain EMPSTAT column")
  }

  # Use constant to identify employed
  employed_codes <- get_employed_codes()
  is_emp <- data$EMPSTAT %in% employed_codes

  # Calculate weighted or unweighted count
  if (is.null(weight_var)) {
    # Unweighted count
    sum(is_emp)
  } else {
    if (!weight_var %in% names(data)) {
      stop(sprintf("Weight variable '%s' not found in data", weight_var))
    }
    # Weighted count
    sum(data[[weight_var]][is_emp], na.rm = TRUE)
  }
}

#' Count Unemployed Persons (Weighted)
#'
#' Returns weighted count of unemployed persons using EMPSTAT codes from constants.
#'
#' @param data Data frame with EMPSTAT column
#' @param weight_var Character. Name of weight variable (default: "WTFINL").
#'   Use NULL for unweighted count.
#'
#' @return Numeric. Weighted count of unemployed persons
#'
#' @export
count_unemployed <- function(data, weight_var = "WTFINL") {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  if (!"EMPSTAT" %in% names(data)) {
    stop("data must contain EMPSTAT column")
  }

  # Use constant to identify unemployed
  unemployed_codes <- get_unemployed_codes()
  is_unemp <- data$EMPSTAT %in% unemployed_codes

  # Calculate weighted or unweighted count
  if (is.null(weight_var)) {
    sum(is_unemp)
  } else {
    if (!weight_var %in% names(data)) {
      stop(sprintf("Weight variable '%s' not found in data", weight_var))
    }
    sum(data[[weight_var]][is_unemp], na.rm = TRUE)
  }
}

#' Count Labor Force Participants (Weighted)
#'
#' Returns weighted count of labor force participants (employed + unemployed).
#'
#' @param data Data frame with EMPSTAT column
#' @param weight_var Character. Name of weight variable (default: "WTFINL").
#'   Use NULL for unweighted count.
#'
#' @return Numeric. Weighted count of labor force participants
#'
#' @export
count_labor_force <- function(data, weight_var = "WTFINL") {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  if (!"EMPSTAT" %in% names(data)) {
    stop("data must contain EMPSTAT column")
  }

  # Use constant to identify labor force
  lf_codes <- get_labor_force_codes()
  is_lf <- data$EMPSTAT %in% lf_codes

  # Calculate weighted or unweighted count
  if (is.null(weight_var)) {
    sum(is_lf)
  } else {
    if (!weight_var %in% names(data)) {
      stop(sprintf("Weight variable '%s' not found in data", weight_var))
    }
    sum(data[[weight_var]][is_lf], na.rm = TRUE)
  }
}


# ==============================================================================
# Calculate Unemployment Rate (Weighted)
# ==============================================================================

#' Calculate Weighted Unemployment Rate
#'
#' Computes unemployment rate using proper weights and CPS code constants.
#'
#' @param data Data frame with EMPSTAT column
#' @param weight_var Character. Weight variable to use. Options:
#'   - "WTFINL": Use WTFINL (default)
#'   - "ASECWT": Use ASECWT
#'   - "auto": Auto-detect (ASECWT for March, WTFINL otherwise)
#'   - NULL: Unweighted rate
#'
#' @return Numeric. Unemployment rate (0-1 scale), or NA if no labor force
#'
#' @details
#' Unemployment rate = unemployed / (employed + unemployed)
#'
#' Uses CPS code constants to identify employment status, ensuring no magic numbers.
#'
#' @export
calculate_unemployment_rate_weighted <- function(data, weight_var = "WTFINL") {
  # Handle auto weight selection
  if (!is.null(weight_var) && weight_var == "auto") {
    weight_var <- select_weight_variable(data)
  }

  # Count employed and unemployed
  n_employed <- count_employed(data, weight_var = weight_var)
  n_unemployed <- count_unemployed(data, weight_var = weight_var)

  n_lf <- n_employed + n_unemployed

  if (n_lf == 0 || is.na(n_lf)) {
    return(NA_real_)
  }

  n_unemployed / n_lf
}


# ==============================================================================
# Weight Variable Selection
# ==============================================================================

#' Select Appropriate Weight Variable
#'
#' Auto-detects whether to use ASECWT (March ASEC) or WTFINL (regular monthly).
#'
#' @param data Data frame with MONTH column
#'
#' @return Character. Name of weight variable to use ("ASECWT" or "WTFINL")
#'
#' @details
#' ASEC (Annual Social and Economic Supplement) is conducted in March and uses
#' ASECWT. All other months use WTFINL.
#'
#' @export
select_weight_variable <- function(data) {
  # Check if MONTH column exists
  if (!"MONTH" %in% names(data)) {
    return("WTFINL")  # Default to WTFINL if no month info
  }

  # Check if data is from March (ASEC month)
  has_march <- any(data$MONTH == 3, na.rm = TRUE)

  # Check if ASECWT column exists and has non-NA values
  has_asecwt <- "ASECWT" %in% names(data) &&
    any(!is.na(data$ASECWT) & data$ASECWT > 0, na.rm = TRUE)

  # Use ASECWT if it's March and ASECWT is available
  if (has_march && has_asecwt) {
    return("ASECWT")
  }

  # Default to WTFINL
  return("WTFINL")
}


# ==============================================================================
# Aggregate by Month
# ==============================================================================

#' Aggregate Monthly Unemployment (Weighted)
#'
#' Aggregates CPS data to monthly weighted unemployment counts and rates.
#'
#' @param data Data frame with YEAR, MONTH, EMPSTAT columns
#' @param weight_var Character. Weight variable (default: "WTFINL")
#'
#' @return Data frame with columns:
#'   - year, month, time_index
#'   - n_employed, n_unemployed, n_total (weighted counts)
#'   - unemployment_rate
#'
#' @details
#' Uses CPS code constants internally - no magic numbers.
#'
#' @export
aggregate_monthly_unemployment <- function(data, weight_var = "WTFINL") {
  # Validate
  required <- c("YEAR", "MONTH", "EMPSTAT")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
  }

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table required. Install with: install.packages('data.table')")
  }

  dt <- data.table::as.data.table(data)

  # Use constants to define employment status
  employed_codes <- get_employed_codes()
  unemployed_codes <- get_unemployed_codes()

  dt[, `:=`(
    is_employed = EMPSTAT %in% employed_codes,
    is_unemployed = EMPSTAT %in% unemployed_codes
  )]

  # Check if we need per-month weight selection
  use_auto_weights <- !is.null(weight_var) && weight_var == "auto"
  has_asecwt <- "ASECWT" %in% names(dt)

  # Aggregate by year-month (including months with 0 labor force)
  if (is.null(weight_var)) {
    # Unweighted
    result_dt <- dt[, .(
      n_employed = sum(is_employed),
      n_unemployed = sum(is_unemployed),
      n_total = sum(is_employed | is_unemployed)
    ), by = .(year = YEAR, month = MONTH)]
  } else if (use_auto_weights && has_asecwt) {
    # Auto weight selection: Use ASECWT for March, WTFINL for other months
    result_dt <- dt[, {
      # Select weight based on month
      if (.BY$month == 3 && any(!is.na(ASECWT) & ASECWT > 0)) {
        w <- ASECWT
      } else {
        w <- WTFINL
      }

      in_lf <- is_employed | is_unemployed
      list(
        n_employed = sum(w[is_employed], na.rm = TRUE),
        n_unemployed = sum(w[is_unemployed], na.rm = TRUE),
        n_total = sum(w[in_lf], na.rm = TRUE)
      )
    }, by = .(year = YEAR, month = MONTH)]
  } else if (use_auto_weights && !has_asecwt) {
    # Auto mode but no ASECWT - fall back to WTFINL
    result_dt <- dt[, {
      w <- WTFINL
      in_lf <- is_employed | is_unemployed
      list(
        n_employed = sum(w[is_employed], na.rm = TRUE),
        n_unemployed = sum(w[is_unemployed], na.rm = TRUE),
        n_total = sum(w[in_lf], na.rm = TRUE)
      )
    }, by = .(year = YEAR, month = MONTH)]
  } else {
    # Use specified weight variable for all months
    result_dt <- dt[, {
      w <- get(weight_var)
      in_lf <- is_employed | is_unemployed
      list(
        n_employed = sum(w[is_employed], na.rm = TRUE),
        n_unemployed = sum(w[is_unemployed], na.rm = TRUE),
        n_total = sum(w[in_lf], na.rm = TRUE)
      )
    }, by = .(year = YEAR, month = MONTH)]
  }

  # Calculate rate
  result_dt[, unemployment_rate := ifelse(n_total > 0, n_unemployed / n_total, NA_real_)]

  # Sort and add time index
  data.table::setorder(result_dt, year, month)
  result_dt[, time_index := seq_len(.N)]

  # Add date column (first day of each month)
  result_dt[, date := as.Date(paste(year, month, "01", sep = "-"))]

  as.data.frame(result_dt)
}


# ==============================================================================
# Aggregate by Education
# ==============================================================================

#' Aggregate by Education Level (Weighted)
#'
#' Aggregates CPS data by education level with weighted counts and rates.
#'
#' @param data Data frame with EDUC, EMPSTAT columns
#' @param weight_var Character. Weight variable (default: "WTFINL")
#'
#' @return Data frame with columns:
#'   - education (character labels from constants)
#'   - n_employed, n_unemployed, n_total (weighted counts)
#'   - unemployment_rate
#'
#' @export
aggregate_by_education <- function(data, weight_var = "WTFINL") {
  # Validate
  required <- c("EDUC", "EMPSTAT")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
  }

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table required")
  }

  dt <- data.table::as.data.table(data)

  # Map education codes to labels using constants
  educ_map <- get_education_code_map()
  dt[, education := educ_map[as.character(EDUC)]]

  # Use constants for employment status
  employed_codes <- get_employed_codes()
  unemployed_codes <- get_unemployed_codes()

  dt[, `:=`(
    is_employed = EMPSTAT %in% employed_codes,
    is_unemployed = EMPSTAT %in% unemployed_codes
  )]

  # Filter to labor force
  dt_lf <- dt[is_employed | is_unemployed]

  # Remove unmapped education levels
  dt_lf <- dt_lf[!is.na(education)]

  # Aggregate by education
  if (is.null(weight_var)) {
    result_dt <- dt_lf[, .(
      n_employed = sum(is_employed),
      n_unemployed = sum(is_unemployed),
      n_total = .N
    ), by = .(education)]
  } else {
    result_dt <- dt_lf[, {
      w <- get(weight_var)
      list(
        n_employed = sum(w[is_employed], na.rm = TRUE),
        n_unemployed = sum(w[is_unemployed], na.rm = TRUE),
        n_total = sum(w, na.rm = TRUE)
      )
    }, by = .(education)]
  }

  # Calculate rate
  result_dt[, unemployment_rate := ifelse(n_total > 0, n_unemployed / n_total, NA_real_)]

  as.data.frame(result_dt)
}


# ==============================================================================
# Aggregate by Month AND Education
# ==============================================================================

#' Aggregate Monthly Unemployment by Education Level
#'
#' Full cross-tabulation of month × education with weighted counts and rates.
#'
#' @param data Data frame with YEAR, MONTH, EDUC, EMPSTAT columns
#' @param weight_var Character. Weight variable (default: "WTFINL")
#' @param weighted Logical. If TRUE, use weights; if FALSE, count persons (default: TRUE)
#'
#' @return Data frame with columns:
#'   - year, month, education, time_index
#'   - n_employed, n_unemployed, n_total
#'   - unemployment_rate
#'
#' @details
#' This is the main aggregation function for education-level analysis.
#' Uses CPS code constants throughout - no magic numbers.
#'
#' @export
aggregate_monthly_by_education <- function(data,
                                            weight_var = "WTFINL",
                                            weighted = TRUE) {
  # Validate
  required <- c("YEAR", "MONTH", "EDUC", "EMPSTAT")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
  }

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table required")
  }

  dt <- data.table::as.data.table(data)

  # Map education codes using constants
  educ_map <- get_education_code_map()
  dt[, education := educ_map[as.character(EDUC)]]

  # Use constants for employment status
  employed_codes <- get_employed_codes()
  unemployed_codes <- get_unemployed_codes()

  dt[, `:=`(
    is_employed = EMPSTAT %in% employed_codes,
    is_unemployed = EMPSTAT %in% unemployed_codes
  )]

  # Filter to labor force
  dt_lf <- dt[is_employed | is_unemployed]

  # Remove unmapped education
  dt_lf <- dt_lf[!is.na(education)]

  # Aggregate by year-month-education
  if (!weighted || is.null(weight_var)) {
    # Unweighted (count persons)
    result_dt <- dt_lf[, .(
      n_employed = sum(is_employed),
      n_unemployed = sum(is_unemployed),
      n_total = .N
    ), by = .(year = YEAR, month = MONTH, education)]
  } else {
    # Weighted (sum weights)
    result_dt <- dt_lf[, {
      w <- get(weight_var)
      list(
        n_employed = sum(w[is_employed], na.rm = TRUE),
        n_unemployed = sum(w[is_unemployed], na.rm = TRUE),
        n_total = sum(w, na.rm = TRUE)
      )
    }, by = .(year = YEAR, month = MONTH, education)]
  }

  # Calculate rate
  result_dt[, unemployment_rate := ifelse(n_total > 0, n_unemployed / n_total, NA_real_)]

  # Sort chronologically
  data.table::setorder(result_dt, year, month, education)

  # Add time_index (by unique year-month, not by row)
  result_dt[, time_index := .GRP, by = .(year, month)]

  as.data.frame(result_dt)
}
