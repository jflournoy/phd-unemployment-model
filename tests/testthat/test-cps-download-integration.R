#' Integration Tests for CPS Data Download
#'
#' Tests that verify actual IPUMS CPS API downloads work correctly.
#' These tests require IPUMS_API_KEY to be set and will make real API calls.

library(testthat)

# Source the functions
source(here::here("R", "ipums-download.R"))

test_that("download_ipums_data integrates with generate_cps_samples", {
  # This should work even without API - uses placeholder
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Generate samples
  samples <- generate_cps_samples(2024, 2024)

  # Should be able to pass generated samples to download function
  expect_no_error({
    result <- download_ipums_data(
      output_dir = temp_dir,
      samples = samples,
      use_api = FALSE  # Use placeholder for this test
    )
  })

  expect_true(file.exists(result$file_path))
})

test_that("downloaded CPS data has required variables for unemployment analysis", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if(Sys.getenv("RUN_INTEGRATION_TESTS") != "true",
          "Integration tests disabled (set RUN_INTEGRATION_TESTS=true to enable)")

  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Download just 2 months of recent data for faster test
  samples <- generate_cps_samples(2024, 2024)[1:2]  # Just Jan-Feb 2024

  result <- download_ipums_data(
    output_dir = temp_dir,
    samples = samples,
    variables = c("YEAR", "MONTH", "EMPSTAT", "EDUC", "WTFINL"),
    use_api = TRUE,
    collection = "cps"
  )

  # Load the data
  data <- readRDS(result$file_path)

  # Should have required variables
  required_vars <- c("YEAR", "MONTH", "EMPSTAT", "EDUC", "WTFINL")
  expect_true(all(required_vars %in% names(data)))

  # YEAR should include 2024
  expect_true(2024 %in% data$YEAR)

  # MONTH should be in range 1-12
  expect_true(all(data$MONTH >= 1 & data$MONTH <= 12))

  # EMPSTAT should exist and be numeric
  # Note: CPS uses detailed coding (0, 1, 10, 12, 21, 22, 32, 34, 36)
  # Not the simplified 1-3 coding
  expect_true(is.numeric(data$EMPSTAT))
  expect_gt(length(unique(data$EMPSTAT)), 1)  # Should have variation
})

test_that("CPS data has MONTH variable for seasonal adjustment", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if(Sys.getenv("RUN_INTEGRATION_TESTS") != "true",
          "Integration tests disabled (set RUN_INTEGRATION_TESTS=true to enable)")

  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Download 1 month to verify MONTH variable exists
  samples <- generate_cps_samples(2024, 2024)[1]  # Just January

  result <- download_ipums_data(
    output_dir = temp_dir,
    samples = samples,
    variables = c("YEAR", "MONTH", "EMPSTAT", "EDUC", "WTFINL"),
    use_api = TRUE,
    collection = "cps"
  )

  data <- readRDS(result$file_path)

  # MONTH variable must exist
  expect_true("MONTH" %in% names(data))

  # MONTH should be January (1) for this sample
  expect_true(1 %in% data$MONTH)
})

test_that("CPS data includes sufficient observations for PhD analysis", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if(Sys.getenv("RUN_INTEGRATION_TESTS") != "true",
          "Integration tests disabled (set RUN_INTEGRATION_TESTS=true to enable)")

  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Download 1 month of data
  samples <- generate_cps_samples(2024, 2024)[1]

  result <- download_ipums_data(
    output_dir = temp_dir,
    samples = samples,
    variables = c("YEAR", "MONTH", "EMPSTAT", "EDUC", "WTFINL"),
    use_api = TRUE,
    collection = "cps"
  )

  data <- readRDS(result$file_path)

  # CPS samples are large (50-60k observations per month)
  expect_gt(nrow(data), 10000)

  # Should have multiple education levels including potential PhDs
  expect_gt(length(unique(data$EDUC)), 5)
})

test_that("placeholder data mimics CPS structure", {
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Use placeholder mode
  result <- download_ipums_data(
    output_dir = temp_dir,
    use_api = FALSE
  )

  data <- readRDS(result$file_path)

  # Placeholder should have MONTH variable
  expect_true("MONTH" %in% names(data))

  # Should have minimal required variables
  required_vars <- c("YEAR", "MONTH", "EMPSTAT", "EDUC", "WTFINL")
  expect_true(all(required_vars %in% names(data)))

  # Values should be realistic
  expect_true(all(data$MONTH >= 1 & data$MONTH <= 12))
  expect_true(is.numeric(data$EMPSTAT))
  expect_gt(min(data$WTFINL), 0)
})

# TDD RED: Test for full 2000-2025 dataset download
test_that("download_full_cps_dataset generates samples for 2000-2025", {
  # Test that we can generate the correct sample list for full time series
  samples <- generate_cps_samples(2000, 2025)

  # Should have samples from multiple years
  expect_gt(length(samples), 200)  # At least 200 months (~17 years * 12)

  # Samples should follow cpsYYYY_MMx format
  expect_true(all(grepl("^cps\\d{4}_\\d{2}[bs]$", samples)))

  # Should start with 2000 samples
  expect_true(any(grepl("^cps2000_", samples)))

  # Should include recent samples (2024 or 2025)
  expect_true(any(grepl("^cps202[45]_", samples)))
})

test_that("download_full_cps_dataset creates complete time series", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if(Sys.getenv("RUN_FULL_DOWNLOAD_TEST") != "true",
          "Full download test disabled (set RUN_FULL_DOWNLOAD_TEST=true to enable)")

  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Generate full sample list for 2000-2025
  samples <- generate_cps_samples(2000, 2025)

  message("Downloading ", length(samples), " monthly CPS samples from 2000-2025")
  message("This may take 30+ minutes...")

  # Download the full dataset
  result <- download_ipums_data(
    output_dir = temp_dir,
    samples = samples,
    variables = c("YEAR", "MONTH", "EMPSTAT", "EDUC", "AGE", "SEX", "WTFINL"),
    use_api = TRUE,
    collection = "cps",
    extract_description = "PhD unemployment analysis 2000-2025 full time series"
  )

  # Verify the download succeeded
  expect_true(file.exists(result$file_path))
  expect_true("extract_info" %in% names(result))

  # Load and verify data
  data <- readRDS(result$file_path)

  # Should have data from multiple years
  expect_true(2000 %in% data$YEAR)
  expect_true(max(data$YEAR) >= 2024)

  # Should have all 12 months represented
  expect_equal(sort(unique(data$MONTH)), 1:12)

  # Should have substantial sample size (millions of observations)
  expect_gt(nrow(data), 1000000)

  # Should have required variables
  required_vars <- c("YEAR", "MONTH", "EMPSTAT", "EDUC", "WTFINL")
  expect_true(all(required_vars %in% names(data)))
})

test_that("full dataset includes sufficient PhD observations", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if(Sys.getenv("RUN_FULL_DOWNLOAD_TEST") != "true",
          "Full download test disabled")
  skip_if(!file.exists("data-raw/ipums_data.rds"),
          "Full dataset not yet downloaded")

  # Load the full dataset
  data <- readRDS("data-raw/ipums_data.rds")

  # Filter to doctorate holders (EDUC == 125 in CPS)
  phd_data <- data[data$EDUC == 125, ]

  # Should have thousands of PhD observations per year
  expect_gt(nrow(phd_data), 10000)

  # Should span the full time range
  expect_true(2000 %in% phd_data$YEAR)
  expect_true(max(phd_data$YEAR) >= 2024)

  # Should have sufficient observations per month for stable estimates
  monthly_counts <- aggregate(YEAR ~ YEAR + MONTH, data = phd_data, FUN = length)
  expect_true(all(monthly_counts$YEAR >= 50))  # At least 50 PhDs per month
})
