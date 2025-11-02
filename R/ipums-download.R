#' Download IPUMS CPS Data
#'
#' Downloads IPUMS CPS monthly data for PhD unemployment analysis using the IPUMS API.
#' Supports both automated API downloads and placeholder data for development.
#'
#' @param output_dir Character. Directory where data file will be saved.
#'   Default is "data-raw" in the project root.
#' @param use_api Logical. If TRUE, uses IPUMS API to download real data.
#'   If FALSE, creates placeholder data. Default is FALSE for backward compatibility.
#' @param samples Character vector. IPUMS CPS sample IDs to include in extract.
#'   Only used when use_api = TRUE. Default is 2000-2025 monthly samples.
#'   Use generate_cps_samples() to create sample lists.
#' @param variables Character vector. IPUMS variables to include.
#'   Default includes YEAR, EMPSTAT, EDUC, AGE, SEX, PERWT for unemployment analysis.
#' @param extract_description Character. Description for the IPUMS extract request.
#' @param api_key Character. IPUMS API key. If NULL, uses IPUMS_API_KEY environment variable.
#' @param collection Character. IPUMS collection to use. Default is "cps" for monthly data.
#'
#' @return A list with the following elements:
#'   \item{file_path}{Character. Full path to the downloaded data file}
#'   \item{extract_info}{List. IPUMS extract metadata (only when use_api = TRUE)}
#'
#' @details
#' ## IPUMS API Authentication
#'
#' To use the IPUMS API (use_api = TRUE), you need an API key from
#' https://account.ipums.org/api_keys
#'
#' Set your API key using:
#' \code{ipumsr::set_ipums_api_key("your-key-here", save = TRUE)}
#'
#' Or set the IPUMS_API_KEY environment variable in your .Renviron file.
#'
#' ## CPS Monthly Data
#'
#' This function downloads CPS (Current Population Survey) monthly data, which is
#' different from the annual ACS data in the USA collection. CPS provides monthly
#' employment statistics ideal for time series analysis.
#'
#' Sample naming: cps2024_01s, cps2024_02b, etc.
#' - Use generate_cps_samples(2000, 2025) to create a full time series
#' - March has both regular (03b) and ASEC (03s) versions
#'
#' ## Data Variables
#'
#' Default variables for CPS unemployment analysis:
#' - YEAR: Survey year
#' - MONTH: Survey month (1-12)
#' - EMPSTAT: Employment status (1=employed, 2=unemployed, 3=not in labor force)
#' - EDUC: Education level (125=Doctorate degree in CPS)
#' - AGE: Age in years
#' - SEX: Sex (1=male, 2=female)
#' - WTFINL: Final person weight for calculating population estimates (CPS weight variable)
#'
#' ## Placeholder Mode
#'
#' When use_api = FALSE (default), creates minimal placeholder data for testing
#' and development. Replace with real API data for production analyses.
#'
#' @examples
#' \dontrun{
#' # Download using placeholder data (for development)
#' result <- download_ipums_data()
#'
#' # Download CPS monthly data for 2020-2025
#' samples <- generate_cps_samples(2020, 2025)
#' result <- download_ipums_data(
#'   use_api = TRUE,
#'   samples = samples,
#'   extract_description = "PhD unemployment analysis 2020-2025"
#' )
#'
#' # Download just recent year
#' samples <- generate_cps_samples(2024, 2024)
#' result <- download_ipums_data(use_api = TRUE, samples = samples)
#'
#' # Read the downloaded data
#' data <- readRDS(result$file_path)
#' }
#'
#' @export
download_ipums_data <- function(output_dir = "data-raw",
                                  use_api = FALSE,
                                  samples = NULL,
                                  variables = c("YEAR", "MONTH", "EMPSTAT", "EDUC", "AGE", "SEX", "WTFINL"),
                                  extract_description = "PhD unemployment data",
                                  api_key = NULL,
                                  collection = "cps") {
  # Validate input
  if (!is.character(output_dir) || length(output_dir) != 1) {
    stop("output_dir must be a single character string")
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Construct file path
  file_path <- file.path(output_dir, "ipums_data.rds")

  if (use_api) {
    # Use IPUMS API to download real data
    message("Using IPUMS API to download data...")

    # Check for API key
    if (is.null(api_key)) {
      api_key <- Sys.getenv("IPUMS_API_KEY")
      if (api_key == "") {
        stop(
          "IPUMS API key not found. ",
          "Set IPUMS_API_KEY environment variable or provide api_key parameter. ",
          "Get your key at: https://account.ipums.org/api_keys"
        )
      }
    }

    # If samples not specified, use recent CPS monthly samples
    if (is.null(samples)) {
      # Default to last 2 years of monthly data
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      samples <- generate_cps_samples(current_year - 1, current_year)
      message("No samples specified, using default: ", length(samples), " monthly samples from ", current_year - 1, "-", current_year)
    }

    # Define extract using ipumsr
    extract_definition <- ipumsr::define_extract_micro(
      collection = collection,
      description = extract_description,
      samples = samples,
      variables = variables
    )

    message("Submitting extract request to IPUMS...")
    submitted_extract <- ipumsr::submit_extract(extract_definition)

    message("Waiting for extract to complete (this may take several minutes)...")
    completed_extract <- ipumsr::wait_for_extract(submitted_extract)

    message("Downloading extract files...")
    download_result <- ipumsr::download_extract(completed_extract, download_dir = output_dir)

    message("Reading IPUMS microdata...")
    ipums_data <- ipumsr::read_ipums_micro(download_result)

    # Save as RDS for easier R usage
    message("Saving data as RDS file...")
    saveRDS(ipums_data, file = file_path)

    # Return result with extract info
    return(list(
      file_path = normalizePath(file_path, mustWork = TRUE),
      extract_info = list(
        extract_number = completed_extract$number,
        samples = samples,
        variables = variables,
        download_date = Sys.Date()
      )
    ))
  } else {
    # Create placeholder data (backward compatibility and testing)
    message("Creating placeholder data (use_api = FALSE)...")

    placeholder_data <- data.frame(
      YEAR = c(2024, 2024, 2024),
      MONTH = c(1, 2, 3),
      EMPSTAT = c(1, 1, 2),
      EDUC = c(125, 125, 125),  # CPS doctorate code
      AGE = c(35, 40, 32),
      SEX = c(1, 2, 1),
      WTFINL = c(1500, 1600, 1400),
      stringsAsFactors = FALSE
    )

    # Save the data
    tryCatch(
      {
        saveRDS(placeholder_data, file = file_path)
      },
      error = function(e) {
        stop("Failed to save IPUMS data: ", e$message)
      }
    )

    # Return result structure
    return(list(
      file_path = normalizePath(file_path, mustWork = TRUE)
    ))
  }
}
