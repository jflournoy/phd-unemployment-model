#' IPUMS CPS Data Configuration for PhD Unemployment Analysis
#'
#' This script defines the IPUMS CPS extract configuration for monthly
#' unemployment data from 2000-2025, suitable for seasonal adjustment.
#'
#' Data Source: IPUMS CPS (Current Population Survey)
#' URL: https://cps.ipums.org/cps/

# Required packages
library(ipumsr)

#' Generate CPS sample codes for monthly data
#'
#' @param start_year Integer. First year to include (default: 2000)
#' @param end_year Integer. Last year to include (default: 2025)
#' @return Character vector of IPUMS CPS sample codes
#'
#' @details
#' CPS samples are named with pattern: cpsYYYYMM
#' For example:
#' - cps200001 = January 2000
#' - cps202512 = December 2025
#'
#' Note: Not all months may be available in IPUMS. Check availability at:
#' https://cps.ipums.org/cps-action/samples/sample_ids
generate_cps_samples <- function(start_year = 2000, end_year = 2025) {
  years <- start_year:end_year
  months <- sprintf("%02d", 1:12)

  # Generate all year-month combinations
  samples <- expand.grid(
    year = years,
    month = months,
    stringsAsFactors = FALSE
  )

  # Format as IPUMS CPS sample codes
  sample_codes <- paste0("cps", samples$year, samples$month)

  # Note: Some months may not be available in IPUMS
  # The IPUMS API will return an error for unavailable samples
  # You may need to filter this list based on actual availability

  return(sample_codes)
}

#' IPUMS CPS Variables for PhD Unemployment Analysis
#'
#' Core variables needed for unemployment rate calculation with
#' demographic disaggregation and proper weighting.
#'
#' @return Named list with variable specifications
get_cps_variables <- function() {
  list(
    # Time identifiers (REQUIRED)
    time = c("YEAR", "MONTH"),

    # Employment status (REQUIRED)
    employment = "EMPSTAT",  # 1=employed, 2=unemployed, 3=not in labor force

    # Education (REQUIRED for PhD filtering)
    education = "EDUC",  # 111=Master's degree, 114=Professional degree, 116=Doctorate

    # Weights (REQUIRED for population estimates)
    weights = "WTFINL",  # Final person weight for monthly CPS

    # Demographics (OPTIONAL but recommended for disaggregation)
    demographics = c(
      "AGE",        # Age in years
      "SEX",        # 1=Male, 2=Female
      "RACE",       # Race
      "HISPAN"      # Hispanic origin
    ),

    # Additional context (OPTIONAL)
    context = c(
      "STATEFIP",   # State FIPS code for geographic analysis
      "METRO"       # Metropolitan status
    )
  )
}

#' Create IPUMS CPS Extract Definition
#'
#' @param samples Character vector. CPS sample codes (from generate_cps_samples)
#' @param variables Character vector. Variable names to include
#' @param description Character. Description of extract purpose
#'
#' @return ipumsr extract definition object
create_cps_extract <- function(
    samples = generate_cps_samples(2000, 2025),
    variables = unlist(get_cps_variables()[c("time", "employment", "education", "weights", "demographics")]),
    description = "PhD unemployment monthly data 2000-2025 for seasonal adjustment"
) {

  # Create extract definition
  extract <- define_extract_micro(
    collection = "cps",
    description = description,
    samples = samples,
    variables = variables
  )

  return(extract)
}

#' Download IPUMS CPS Data for PhD Unemployment Analysis
#'
#' Wrapper function that handles the full extract workflow:
#' 1. Define extract with appropriate samples and variables
#' 2. Submit to IPUMS
#' 3. Wait for completion
#' 4. Download and read data
#' 5. Save as RDS
#'
#' @param output_dir Character. Directory to save data (default: "data-raw")
#' @param start_year Integer. First year of data (default: 2000)
#' @param end_year Integer. Last year of data (default: 2025)
#' @param variable_groups Character vector. Which variable groups to include
#'   from get_cps_variables(). Default: c("time", "employment", "education", "weights", "demographics")
#' @param api_key Character. IPUMS API key (defaults to IPUMS_API_KEY env var)
#'
#' @return List with file_path and extract metadata
#'
#' @examples
#' \dontrun{
#' # Download full dataset (2000-2025)
#' result <- download_cps_unemployment_data()
#'
#' # Download subset (2020-2025 for testing)
#' result <- download_cps_unemployment_data(
#'   start_year = 2020,
#'   end_year = 2025
#' )
#'
#' # Minimal variables for faster download
#' result <- download_cps_unemployment_data(
#'   variable_groups = c("time", "employment", "education", "weights")
#' )
#' }
download_cps_unemployment_data <- function(
    output_dir = "data-raw",
    start_year = 2000,
    end_year = 2025,
    variable_groups = c("time", "employment", "education", "weights", "demographics"),
    api_key = NULL
) {

  # Check API key
  if (is.null(api_key)) {
    api_key <- Sys.getenv("IPUMS_API_KEY")
    if (api_key == "") {
      stop(
        "IPUMS API key required. Set IPUMS_API_KEY environment variable or provide api_key parameter.\n",
        "Get your key at: https://account.ipums.org/api_keys"
      )
    }
  }

  # Generate samples for date range
  message("Generating CPS sample list for ", start_year, "-", end_year)
  samples <- generate_cps_samples(start_year, end_year)
  message("Total samples requested: ", length(samples))
  message("Note: Some samples may not be available in IPUMS CPS")

  # Get variables
  all_vars <- get_cps_variables()
  selected_vars <- unique(unlist(all_vars[variable_groups]))
  message("Variables requested: ", paste(selected_vars, collapse = ", "))

  # Create extract definition
  message("\nCreating IPUMS CPS extract definition...")
  extract_def <- create_cps_extract(
    samples = samples,
    variables = selected_vars,
    description = sprintf(
      "PhD unemployment monthly data %d-%d for seasonal adjustment",
      start_year, end_year
    )
  )

  # Submit extract
  message("Submitting extract to IPUMS CPS...")
  message("This may take several minutes to hours depending on data size...")
  submitted <- submit_extract(extract_def)
  message("Extract submitted: #", submitted$number)

  # Wait for completion
  message("\nWaiting for extract to complete...")
  message("Large extracts (300+ samples) can take 30+ minutes")
  completed <- wait_for_extract(submitted)
  message("Extract completed!")

  # Download
  message("\nDownloading extract files...")
  download_dir <- file.path(output_dir, "ipums_cps_raw")
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

  download_result <- download_extract(completed, download_dir = download_dir)

  # Read data
  message("Reading IPUMS microdata...")
  cps_data <- read_ipums_micro(download_result)

  # Save as RDS
  output_file <- file.path(output_dir, sprintf(
    "cps_phd_unemployment_%d_%d.rds",
    start_year, end_year
  ))

  message("Saving to: ", output_file)
  saveRDS(cps_data, file = output_file)

  # Return metadata
  result <- list(
    file_path = normalizePath(output_file),
    extract_number = completed$number,
    start_year = start_year,
    end_year = end_year,
    n_samples = length(samples),
    variables = selected_vars,
    download_date = Sys.Date(),
    n_rows = nrow(cps_data),
    n_cols = ncol(cps_data)
  )

  # Print summary
  message("\n=== Download Complete ===")
  message("File: ", result$file_path)
  message("Rows: ", format(result$n_rows, big.mark = ","))
  message("Columns: ", result$n_cols)
  message("Date range: ", start_year, "-", end_year)
  message("Extract #: ", result$extract_number)

  return(result)
}

# Example usage (not run automatically)
if (FALSE) {
  # Full dataset (WARNING: This will be very large!)
  result_full <- download_cps_unemployment_data(
    start_year = 2000,
    end_year = 2025
  )

  # Test with recent data only
  result_test <- download_cps_unemployment_data(
    start_year = 2020,
    end_year = 2023,
    variable_groups = c("time", "employment", "education", "weights")
  )
}
