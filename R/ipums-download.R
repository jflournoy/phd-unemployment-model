#' Download IPUMS Data
#'
#' Downloads IPUMS USA data for PhD unemployment analysis using the IPUMS API.
#' Supports both automated API downloads and placeholder data for development.
#'
#' @param output_dir Character. Directory where data file will be saved.
#'   Default is "data-raw" in the project root.
#' @param use_api Logical. If TRUE, uses IPUMS API to download real data.
#'   If FALSE, creates placeholder data. Default is FALSE for backward compatibility.
#' @param samples Character vector. IPUMS sample IDs to include in extract.
#'   Only used when use_api = TRUE. Default is latest available sample.
#' @param variables Character vector. IPUMS variables to include.
#'   Default includes YEAR, EMPSTAT, EDUC, MONTH, WTFINL for unemployment analysis.
#' @param extract_description Character. Description for the IPUMS extract request.
#' @param api_key Character. IPUMS API key. If NULL, uses IPUMS_API_KEY environment variable.
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
#' ## Data Variables
#'
#' Default variables for unemployment analysis:
#' - YEAR: Survey year
#' - EMPSTAT: Employment status (1=employed, 2=unemployed, 3=not in labor force)
#' - EDUC: Education level (116=Doctorate degree)
#' - AGE: Age in years
#' - SEX: Sex (1=male, 2=female)
#' - PERWT: Person weight for calculating population estimates
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
#' # Download real data using IPUMS API
#' result <- download_ipums_data(
#'   use_api = TRUE,
#'   samples = c("us2022a", "us2021a"),
#'   extract_description = "PhD unemployment analysis"
#' )
#'
#' # Read the downloaded data
#' data <- readRDS(result$file_path)
#' }
#'
#' @export
download_ipums_data <- function(output_dir = "data-raw",
                                  use_api = FALSE,
                                  samples = NULL,
                                  variables = c("YEAR", "EMPSTAT", "EDUC", "AGE", "SEX", "PERWT"),
                                  extract_description = "PhD unemployment data",
                                  api_key = NULL) {
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

    # If samples not specified, use recent sample
    if (is.null(samples)) {
      samples <- c("us2022a")  # Default to 2022 ACS
      message("No samples specified, using default: ", paste(samples, collapse = ", "))
    }

    # Define extract using ipumsr
    extract_definition <- ipumsr::define_extract_micro(
      collection = "usa",
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
      YEAR = c(2020, 2021, 2022),
      EMPSTAT = c(1, 1, 2),
      EDUC = c(116, 116, 116),
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
