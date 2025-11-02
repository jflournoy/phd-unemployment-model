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
