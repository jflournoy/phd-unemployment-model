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

#' Filter CPS Data by Education Level
#'
#' Generic function to filter CPS data by any education code.
#'
#' @param cps_data Data frame. CPS microdata with EDUC variable
#' @param educ_code Numeric. IPUMS education code to filter by
#'
#' @return Data frame containing only respondents with specified education level
#'
#' @details
#' Common IPUMS CPS education codes:
#' - 125: Doctorate degree
#' - 123: Master's degree
#' - 111: Bachelor's degree
#' - 91-110: Some college or Associate's degree
#' - 73: High school diploma
#'
#' Uses data.table for efficient filtering of large datasets.
#'
#' @examples
#' \dontrun{
#' cps_data <- readRDS("data-raw/ipums_data.rds")
#' masters_data <- filter_education_level(cps_data, educ_code = 123)
#' }
#'
#' @export
filter_education_level <- function(cps_data, educ_code) {
  # Validate input
  if (!is.data.frame(cps_data)) {
    stop("cps_data must be a data frame")
  }

  if (!"EDUC" %in% names(cps_data)) {
    stop("cps_data must contain EDUC variable")
  }

  if (!is.numeric(educ_code) || length(educ_code) != 1) {
    stop("educ_code must be a single numeric value")
  }

  # Remove haven_labelled classes for data.table compatibility
  cps_data <- remove_haven_labels(cps_data)

  # Use data.table for efficient filtering
  if (!requireNamespace("data.table", quietly = TRUE)) {
    # Fallback to base R if data.table not available
    filtered_data <- cps_data[cps_data$EDUC == educ_code, ]
  } else {
    dt <- data.table::as.data.table(cps_data)
    filtered_data <- as.data.frame(dt[EDUC == educ_code])
  }

  return(filtered_data)
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
#' This is a convenience wrapper around filter_education_level() for PhD analysis.
#' Uses get_education_code() from cps-codes.R to ensure correct code.
#'
#' @examples
#' \dontrun{
#' cps_data <- readRDS("data-raw/ipums_data.rds")
#' phd_data <- filter_phd_holders(cps_data)
#' }
#'
#' @export
filter_phd_holders <- function(cps_data) {
  phd_code <- get_education_code("phd")
  filter_education_level(cps_data, educ_code = phd_code)
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

  # Define employed and unemployed based on CPS code constants
  employed <- data$EMPSTAT %in% get_employed_codes()
  unemployed <- data$EMPSTAT %in% get_unemployed_codes()

  # Labor force = employed + unemployed
  in_labor_force <- employed | unemployed

  # Filter to labor force participants only
  lf_data <- data[in_labor_force, ]

  if (nrow(lf_data) == 0) {
    warning("No labor force participants found in data")
    return(NA_real_)
  }

  # Calculate weighted unemployment rate using the appropriate weight
  unemployed_lf <- lf_data$EMPSTAT %in% get_unemployed_codes()
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
#' 2. Calculates monthly unemployment rates using new aggregation functions
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

  # Calculate monthly unemployment using new aggregation function
  monthly_rates <- aggregate_monthly_unemployment(phd_data, weight_var = "auto")

  # Add date column for compatibility with existing code
  monthly_rates$date <- as.Date(paste(monthly_rates$year, monthly_rates$month, "01", sep = "-"))

  # Rename columns for compatibility
  monthly_rates$YEAR <- monthly_rates$year
  monthly_rates$MONTH <- monthly_rates$month
  monthly_rates$n_obs <- monthly_rates$n_total  # Use n_total as proxy for n_obs

  return(monthly_rates)
}

#' Generate Education Spectrum Count Data for Binomial/Quasi-Binomial GAMs
#'
#' Loads raw IPUMS CPS microdata and generates monthly unemployment counts by
#' education level for use with binomial/quasi-binomial factor smooth GAMs.
#'
#' @param input_file Character. Path to raw IPUMS CPS data (.rds file)
#' @param output_file Character. Path where count data should be saved (.rds file)
#' @param weighted Logical. If TRUE, use weights; if FALSE, count persons (default: FALSE)
#'
#' @return Character. Path to output file (invisibly)
#'
#' @details
#' This function:
#' 1. Loads raw CPS microdata from input_file
#' 2. Calls aggregate_monthly_by_education() to aggregate to counts
#' 3. Saves result to output_file
#' 4. Creates output directory if it doesn't exist
#'
#' **Output data structure** suitable for binomial/quasi-binomial GAMs:
#' - year, month, education (factor)
#' - n_unemployed (integer): count of unemployed
#' - n_total (integer): count of labor force participants
#' - time_index (integer): sequential time index
#'
#' **Education levels**: less_than_hs, high_school, some_college, bachelors,
#' professional, masters, phd
#'
#' @examples
#' \dontrun{
#' # Generate count data from raw IPUMS data
#' generate_education_spectrum_counts(
#'   input_file = "data-raw/ipums_data.rds",
#'   output_file = "data/education-spectrum-counts.rds"
#' )
#' }
#'
#' @export
generate_education_spectrum_counts <- function(input_file, output_file, weighted = FALSE) {
  # Validate input file exists
  if (!file.exists(input_file)) {
    stop(sprintf("Input file does not exist: %s", input_file))
  }

  # Create output directory if needed
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
  }

  # Load raw CPS data
  cat("Loading raw CPS data from:", input_file, "\n")
  raw_data <- readRDS(input_file)

  # Validate required columns
  required_vars <- c("YEAR", "MONTH", "EMPSTAT", "EDUC")
  missing_vars <- setdiff(required_vars, names(raw_data))
  if (length(missing_vars) > 0) {
    stop(sprintf("Input data missing required variables: %s",
                 paste(missing_vars, collapse = ", ")))
  }

  # Generate count data using new aggregation function
  cat("Generating monthly unemployment counts by education level...\n")
  count_data <- aggregate_monthly_by_education(
    raw_data,
    weight_var = NULL,  # Use unweighted counts for binomial GAMs
    weighted = weighted
  )

  # Print summary
  cat("\nCount data summary:\n")
  cat(sprintf("  Rows: %d\n", nrow(count_data)))
  cat(sprintf("  Years: %d - %d\n",
              min(count_data$year, na.rm = TRUE),
              max(count_data$year, na.rm = TRUE)))
  cat(sprintf("  Education levels (%d): %s\n",
              length(unique(count_data$education)),
              paste(sort(unique(count_data$education)), collapse = ", ")))
  cat(sprintf("  Total unemployed: %d\n", sum(count_data$n_unemployed)))
  cat(sprintf("  Total labor force: %d\n", sum(count_data$n_total)))

  # Save to output file
  cat("\nSaving to:", output_file, "\n")
  saveRDS(count_data, output_file)

  cat("Done!\n")

  invisible(output_file)
}
