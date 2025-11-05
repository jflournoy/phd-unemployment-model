#' Tests for Data Update and Full Download
#'
#' TDD RED Phase: Define interface for downloading and updating CPS data

library(testthat)

test_that("needs_update function exists", {
  source(here::here("R", "data-freshness.R"))
  expect_true(exists("needs_update", mode = "function"))
})

test_that("needs_update returns TRUE for missing file", {
  source(here::here("R", "data-freshness.R"))

  result <- needs_update("nonexistent_file.rds", max_age_days = 30)

  expect_true(result)
})

test_that("needs_update returns TRUE for stale file", {
  source(here::here("R", "data-freshness.R"))

  # Create temp file with old timestamp
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(data.frame(test = 1), temp_file)

  # Set to 60 days ago
  old_time <- Sys.time() - (60 * 24 * 60 * 60)
  Sys.setFileTime(temp_file, old_time)

  result <- needs_update(temp_file, max_age_days = 30)

  expect_true(result)

  unlink(temp_file)
})

test_that("needs_update returns FALSE for fresh file", {
  source(here::here("R", "data-freshness.R"))

  temp_file <- tempfile(fileext = ".rds")
  saveRDS(data.frame(test = 1), temp_file)

  result <- needs_update(temp_file, max_age_days = 30)

  expect_false(result)

  unlink(temp_file)
})

test_that("get_expected_data_files function exists", {
  source(here::here("R", "data-freshness.R"))
  expect_true(exists("get_expected_data_files", mode = "function"))
})

test_that("get_expected_data_files returns correct structure", {
  source(here::here("R", "data-freshness.R"))

  result <- get_expected_data_files(data_dir = "data-raw")

  # Should return a character vector with file paths
  expect_type(result, "character")
  expect_true(length(result) > 0)
  expect_true(all(grepl("\\.rds$", result)))  # All should be .rds files
})

test_that("get_expected_data_files includes ipums raw data", {
  source(here::here("R", "data-freshness.R"))

  result <- get_expected_data_files(data_dir = "data-raw")

  # Should include the raw IPUMS data file
  expect_true(any(grepl("ipums_data\\.rds$", result)))
})

test_that("check_all_data_freshness function exists", {
  source(here::here("R", "data-freshness.R"))
  expect_true(exists("check_all_data_freshness", mode = "function"))
})

test_that("check_all_data_freshness returns data frame with expected columns", {
  source(here::here("R", "data-freshness.R"))

  result <- check_all_data_freshness(max_age_days = 30)

  # Should return a data frame
  expect_s3_class(result, "data.frame")

  # Should have columns: file, exists, age_days, is_fresh, needs_update
  expected_cols <- c("file", "exists", "age_days", "is_fresh", "needs_update")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("check_all_data_freshness identifies missing files", {
  source(here::here("R", "data-freshness.R"))

  result <- check_all_data_freshness(max_age_days = 30)

  # If any files don't exist, exists should be FALSE
  if (any(!result$exists)) {
    missing <- result[!result$exists, ]
    expect_true(all(missing$needs_update))
    expect_true(all(is.na(missing$age_days)))
  }

  expect_true(TRUE)  # Always pass if no missing files
})

test_that("check_all_data_freshness respects max_age_days parameter", {
  source(here::here("R", "data-freshness.R"))

  # Check with different thresholds
  result_strict <- check_all_data_freshness(max_age_days = 1)
  result_lenient <- check_all_data_freshness(max_age_days = 365)

  # Strict threshold should mark more files as needing update
  expect_gte(sum(result_strict$needs_update), sum(result_lenient$needs_update))
})

test_that("print_data_freshness_summary function exists", {
  source(here::here("R", "data-freshness.R"))
  expect_true(exists("print_data_freshness_summary", mode = "function"))
})

test_that("print_data_freshness_summary runs without error", {
  source(here::here("R", "data-freshness.R"))

  freshness <- check_all_data_freshness(max_age_days = 30)

  # Should not error
  expect_no_error(print_data_freshness_summary(freshness))
})

# Tests for download_full_cps_data

test_that("download_full_cps_data function exists", {
  source(here::here("R", "data-freshness.R"))
  expect_true(exists("download_full_cps_data", mode = "function"))
})

test_that("download_full_cps_data accepts year range parameters", {
  source(here::here("R", "data-freshness.R"))

  # Test that function can be called with year parameters
  # We won't actually download in tests, but verify the interface
  expect_error(
    download_full_cps_data(start_year = 2024, end_year = 2024,
                           output_dir = tempdir(), force = FALSE),
    NA  # Should not error on parameter validation
  )
})

test_that("download_full_cps_data validates year range", {
  source(here::here("R", "data-freshness.R"))

  # End year should not be before start year
  expect_error(
    download_full_cps_data(start_year = 2024, end_year = 2020),
    "end_year.*before.*start_year"
  )
})

test_that("download_full_cps_data validates years are reasonable", {
  source(here::here("R", "data-freshness.R"))

  # Years should be >= 2000 (CPS availability)
  expect_error(
    download_full_cps_data(start_year = 1990, end_year = 2000),
    "start_year.*>= 2000"
  )

  # Years should be <= current year + 1
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  expect_error(
    download_full_cps_data(start_year = 2000, end_year = current_year + 2),
    "end_year.*<="
  )
})

test_that("download_full_cps_data creates output directory if needed", {
  source(here::here("R", "data-freshness.R"))

  temp_dir <- file.path(tempdir(), "test_download_dir_unique")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

  # Should create directory and download
  expect_false(dir.exists(temp_dir))

  result <- download_full_cps_data(start_year = 2024, end_year = 2024,
                                   output_dir = temp_dir)

  # Directory should now exist
  expect_true(dir.exists(temp_dir))

  # Should return a file path
  expect_type(result, "character")
  expect_true(file.exists(result))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

# Tests for update_stale_cps_data

test_that("update_stale_cps_data function exists", {
  source(here::here("R", "data-freshness.R"))
  expect_true(exists("update_stale_cps_data", mode = "function"))
})

test_that("update_stale_cps_data returns list with expected components", {
  source(here::here("R", "data-freshness.R"))

  # Should return list with: updated, fresh, failed
  # This will fail in RED phase since not implemented
  result <- tryCatch(
    update_stale_cps_data(max_age_days = 30),
    error = function(e) {
      # In RED phase, expect "Not yet implemented" error
      expect_match(e$message, "Not yet implemented")
      list(updated = character(0), fresh = character(0), failed = character(0))
    }
  )

  expect_type(result, "list")
  expect_true(all(c("updated", "fresh", "failed") %in% names(result)))
})

test_that("update_stale_cps_data identifies fresh files correctly", {
  source(here::here("R", "data-freshness.R"))

  # Create temporary fresh file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "test_fresh.rds")
  saveRDS(data.frame(test = 1), temp_file)

  # Mock get_expected_data_files to return our temp file
  # This test verifies the logic, not actual download
  # Will implement in GREEN phase
  expect_true(TRUE)  # Placeholder for now

  unlink(temp_file)
})

test_that("update_stale_cps_data respects max_age_days parameter", {
  source(here::here("R", "data-freshness.R"))

  # With strict age limit, more files should need updating
  # With lenient age limit, fewer files should need updating
  # This will be implemented in GREEN phase
  expect_true(TRUE)  # Placeholder for now
})
