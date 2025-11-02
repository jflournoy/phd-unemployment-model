#' Tests for IPUMS data download functionality
#'
#' TDD RED Phase: These tests define the interface for download_ipums_data()
#' before implementation exists.

library(testthat)

# Source the function for testing
source(here::here("R", "ipums-download.R"))

test_that("download_ipums_data function exists", {
  # This test will fail initially because the function doesn't exist
  expect_true(exists("download_ipums_data", mode = "function"))
})

test_that("download_ipums_data creates a data file", {
  # Use temporary directory for testing
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Call the function
  result <- download_ipums_data(output_dir = temp_dir)

  # Verify a file was created
  expect_true(file.exists(result$file_path))

  # Verify file is not empty
  expect_gt(file.size(result$file_path), 0)
})

test_that("download_ipums_data returns expected structure", {
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  result <- download_ipums_data(output_dir = temp_dir)

  # Should return a list
  expect_type(result, "list")

  # Should have file_path element
  expect_true("file_path" %in% names(result))

  # file_path should be a character string
  expect_type(result$file_path, "character")
})

test_that("download_ipums_data handles missing output_dir parameter", {
  # Should use default directory if not specified
  # Clean up after test
  on.exit({
    if (file.exists("data-raw/ipums_data.rds")) {
      unlink("data-raw/ipums_data.rds")
    }
  }, add = TRUE)

  result <- download_ipums_data()

  expect_true(file.exists(result$file_path))
})
