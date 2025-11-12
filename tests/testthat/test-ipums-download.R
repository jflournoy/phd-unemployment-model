#' Tests for IPUMS data download functionality
#'
#' TDD: Tests for download_ipums_data() and smart download features

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

# New tests for IPUMS CPS API integration
test_that("download_ipums_data can accept CPS sample specifications", {
  # Skip if no API key is set (for CI/development without credentials)
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if(Sys.getenv("RUN_INTEGRATION_TESTS") != "true",
          "Integration tests disabled (set RUN_INTEGRATION_TESTS=true to enable)")

  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Should accept CPS monthly samples
  result <- download_ipums_data(
    output_dir = temp_dir,
    samples = c("cps2024_01s"),  # CPS monthly sample
    use_api = TRUE,
    collection = "cps"
  )

  expect_true(file.exists(result$file_path))
  expect_true("extract_info" %in% names(result))
  expect_equal(result$extract_info$samples, c("cps2024_01s"))
})

test_that("download_ipums_data can accept CPS variable specifications", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if(Sys.getenv("RUN_INTEGRATION_TESTS") != "true",
          "Integration tests disabled (set RUN_INTEGRATION_TESTS=true to enable)")

  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Should accept variables parameter with CPS samples
  result <- download_ipums_data(
    output_dir = temp_dir,
    samples = c("cps2024_01s"),  # CPS monthly sample
    variables = c("YEAR", "EMPSTAT", "EDUC"),
    use_api = TRUE,
    collection = "cps"
  )

  expect_true(file.exists(result$file_path))
  expect_true("extract_info" %in% names(result))
  expect_equal(result$extract_info$variables, c("YEAR", "EMPSTAT", "EDUC"))
})

test_that("download_ipums_data falls back to placeholder when use_api=FALSE", {
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Should still work with placeholder data when use_api = FALSE
  result <- download_ipums_data(
    output_dir = temp_dir,
    use_api = FALSE
  )

  expect_true(file.exists(result$file_path))

  # Verify it's placeholder data
  data <- readRDS(result$file_path)
  expect_equal(nrow(data), 3)  # Placeholder has 3 rows
})

# TDD REFACTOR: Tests for skip_if_exists parameter

test_that("download_ipums_data with skip_if_exists=FALSE always downloads", {
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # First download
  result1 <- download_ipums_data(
    output_dir = temp_dir,
    skip_if_exists = FALSE,
    use_api = FALSE
  )

  expect_true(file.exists(result1$file_path))
  expect_false(isTRUE(result1$skipped))

  # Second download with skip_if_exists=FALSE should still download
  result2 <- download_ipums_data(
    output_dir = temp_dir,
    skip_if_exists = FALSE,
    use_api = FALSE
  )

  expect_true(file.exists(result2$file_path))
  expect_false(isTRUE(result2$skipped))
})

test_that("download_ipums_data with skip_if_exists=TRUE skips when file exists", {
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # First download
  result1 <- download_ipums_data(
    output_dir = temp_dir,
    skip_if_exists = FALSE,
    use_api = FALSE
  )

  expect_true(file.exists(result1$file_path))
  first_mtime <- file.info(result1$file_path)$mtime

  # Wait a moment to ensure mtime would be different if file was recreated
  Sys.sleep(0.1)

  # Second download with skip_if_exists=TRUE should skip
  result2 <- download_ipums_data(
    output_dir = temp_dir,
    skip_if_exists = TRUE,
    use_api = FALSE
  )

  expect_true(file.exists(result2$file_path))
  expect_true(result2$skipped)
  expect_equal(result2$file_path, result1$file_path)

  # Verify file wasn't modified
  second_mtime <- file.info(result2$file_path)$mtime
  expect_equal(first_mtime, second_mtime)
})

test_that("download_ipums_data with skip_if_exists=TRUE downloads when file missing", {
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # First call with skip_if_exists=TRUE but no existing file
  result <- download_ipums_data(
    output_dir = temp_dir,
    skip_if_exists = TRUE,
    use_api = FALSE
  )

  expect_true(file.exists(result$file_path))
  expect_false(isTRUE(result$skipped))
})

test_that("skip_if_exists returns correct structure when skipping", {
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create existing file
  download_ipums_data(output_dir = temp_dir, use_api = FALSE)

  # Try to download with skip_if_exists=TRUE
  result <- download_ipums_data(
    output_dir = temp_dir,
    skip_if_exists = TRUE,
    use_api = FALSE
  )

  expect_type(result, "list")
  expect_true("file_path" %in% names(result))
  expect_true("skipped" %in% names(result))
  expect_true(result$skipped)
  expect_type(result$file_path, "character")
})
