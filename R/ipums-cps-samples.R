#' Generate IPUMS CPS Monthly Sample List
#'
#' Creates a vector of CPS monthly sample IDs for a given year range.
#' Useful for downloading historical time series data.
#'
#' @param start_year Numeric. First year to include (e.g., 2000).
#' @param end_year Numeric. Last year to include (e.g., 2025).
#' @param months Numeric vector. Months to include (1-12). Default is all months.
#' @param exclude_asec Logical. If TRUE, excludes March ASEC supplement.
#'   Default is TRUE (only basic monthly samples).
#'
#' @return Character vector of CPS sample IDs (e.g., "cps2024_01s", "cps2024_02b").
#'
#' @details
#' CPS monthly samples have different suffixes:
#' - 's' = standard monthly sample
#' - 'b' = basic monthly sample
#' - March typically has both regular ('03b') and ASEC ('03s') versions
#'
#' For unemployment analysis, we typically want:
#' - All months (1-12)
#' - Exclude ASEC (use basic monthly instead)
#' - Consistent time series 2000-2025
#'
#' @examples
#' # Get all months for 2020-2025
#' samples <- generate_cps_samples(2020, 2025)
#'
#' # Get just recent year
#' samples <- generate_cps_samples(2024, 2024)
#'
#' # Custom months (quarterly)
#' samples <- generate_cps_samples(2020, 2025, months = c(3, 6, 9, 12))
#'
#' @export
generate_cps_samples <- function(start_year,
                                  end_year = NULL,
                                  months = 1:12,
                                  exclude_asec = TRUE) {
  # Default end_year to start_year if not specified
  if (is.null(end_year)) {
    end_year <- start_year
  }

  # Validate inputs
  if (!is.numeric(start_year) || !is.numeric(end_year)) {
    stop("start_year and end_year must be numeric")
  }

  if (start_year > end_year) {
    stop("start_year must be <= end_year")
  }

  if (!all(months %in% 1:12)) {
    stop("months must be between 1 and 12")
  }

  # Generate sample IDs
  samples <- character()

  for (year in start_year:end_year) {
    for (month in months) {
      month_str <- sprintf("%02d", month)

      # March has special handling for ASEC
      if (month == 3 && exclude_asec) {
        # Use basic monthly (03b) instead of ASEC (03s)
        sample_id <- paste0("cps", year, "_", month_str, "b")
      } else {
        # Most months use 's' suffix, but check what's actually available
        # For simplicity, we'll use 's' as default
        sample_id <- paste0("cps", year, "_", month_str, "s")
      }

      samples <- c(samples, sample_id)
    }
  }

  return(samples)
}

#' Get Latest Available CPS Month
#'
#' Queries IPUMS API to find the most recent CPS monthly sample available.
#'
#' @param api_key Character. IPUMS API key. If NULL, uses IPUMS_API_KEY environment variable.
#'
#' @return List with:
#'   \item{sample}{Character. Sample ID (e.g., "cps2025_08s")}
#'   \item{year}{Numeric. Year}
#'   \item{month}{Numeric. Month}
#'   \item{description}{Character. Sample description}
#'
#' @examples
#' \dontrun{
#' latest <- get_latest_cps_month()
#' cat("Latest CPS data:", latest$sample, latest$description, "\n")
#' }
#'
#' @export
get_latest_cps_month <- function(api_key = NULL) {
  # Check for API key
  if (is.null(api_key)) {
    api_key <- Sys.getenv("IPUMS_API_KEY")
    if (api_key == "") {
      stop(
        "IPUMS API key not found. ",
        "Set IPUMS_API_KEY environment variable or provide api_key parameter."
      )
    }
  }

  # Get all CPS samples
  samples <- ipumsr::get_sample_info("cps")

  # Filter for monthly samples (exclude ASEC and special supplements)
  monthly <- samples[
    grepl("cps[0-9]{4}_[0-9]{2}[sb]$", samples$name) &
    !grepl("_03s$", samples$name),  # Exclude ASEC
  ]

  # Sort and get latest
  monthly <- monthly[order(monthly$name, decreasing = TRUE), ]
  latest <- monthly[1, ]

  # Extract year and month
  year <- as.numeric(sub("cps([0-9]{4})_.*", "\\1", latest$name))
  month <- as.numeric(sub("cps[0-9]{4}_([0-9]{2})[sb]", "\\1", latest$name))

  return(list(
    sample = latest$name,
    year = year,
    month = month,
    description = latest$description
  ))
}
