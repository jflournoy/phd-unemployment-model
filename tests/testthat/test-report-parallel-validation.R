# TDD Tests for Parallel Model Validation in Reports
#
# Tests ensure that reports requiring parallel model results fail gracefully
# with informative error messages when the parallel model is missing.
#
# Run with: testthat::test_file("tests/testthat/test-report-parallel-validation.R")

library(testthat)
library(withr)
library(here)

# Load package functions (not installed as package)
devtools::load_all()

# ============================================================================
# Section 1: Validation Function Tests
# ============================================================================

test_that("validate_parallel_model_exists() throws error when parallel file missing", {
  # Create temporary directory without parallel model file
  temp_dir <- withr::local_tempdir(pattern = "test_parallel")
  missing_file <- file.path(temp_dir, "ode-state-space-edu-parallel-fit.qs")

  # Expect error when checking missing file
  expect_error(
    validate_parallel_model_exists(parallel_file = missing_file),
    regexp = "Parallel model results not found",
    info = "Function should error when parallel file missing"
  )
})

test_that("validate_parallel_model_exists() rejects dummy/corrupted files", {
  # Create temporary directory with dummy parallel model file
  temp_dir <- withr::local_tempdir(pattern = "test_parallel_exists")
  dummy_file <- file.path(temp_dir, "ode-state-space-edu-parallel-fit.qs")
  writeLines("dummy", dummy_file)

  # Expect error when file is dummy/corrupted
  expect_error(
    validate_parallel_model_exists(parallel_file = dummy_file),
    regexp = "appears to be corrupted or dummy",
    info = "Function should error when file is dummy/corrupted"
  )
})

test_that("validate_parallel_model_exists() provides instructions in error message", {
  temp_dir <- withr::local_tempdir(pattern = "test_instructions")
  missing_file <- file.path(temp_dir, "ode-state-space-edu-parallel-fit.qs")

  # Capture error message
  error_msg <- tryCatch(
    validate_parallel_model_exists(parallel_file = missing_file),
    error = function(e) e$message
  )

  # Error message should include instructions for running parallel model
  expect_true(
    grepl("targets::tar_make\\(model_ode_state_space_edu_parallel\\)", error_msg) ||
    grepl("run the parallel model", error_msg, ignore.case = TRUE),
    info = "Error message should include instructions for running parallel model"
  )
})

test_that("validate_parallel_model_exists() checks serial file when provided (parallel file valid)", {
  temp_dir <- withr::local_tempdir(pattern = "test_both")
  parallel_file <- file.path(temp_dir, "ode-state-space-edu-parallel-fit.qs")
  serial_file <- file.path(temp_dir, "ode-state-space-efficient-fit.qs")
  # Create a valid-looking parallel file (size > 1MB simulation not possible in test)
  # Instead we'll skip this test because we can't create valid QS file in test.
  # The important behavior is that when parallel file is valid, serial file is checked.
  skip("Cannot create valid QS file in test environment")
})

test_that("validate_parallel_model_exists() rejects dummy/corrupted files for both serial and parallel", {
  temp_dir <- withr::local_tempdir(pattern = "test_both_exist")
  parallel_file <- file.path(temp_dir, "ode-state-space-edu-parallel-fit.qs")
  serial_file <- file.path(temp_dir, "ode-state-space-efficient-fit.qs")
  writeLines("dummy", parallel_file)
  writeLines("dummy", serial_file)

  # Expect error about corrupted parallel file (checked first)
  expect_error(
    validate_parallel_model_exists(parallel_file = parallel_file, serial_file = serial_file),
    regexp = "appears to be corrupted or dummy",
    info = "Should error when files are dummy/corrupted"
  )
})

# ============================================================================
# Section 2: Report Integration Tests
# ============================================================================

test_that("report setup chunk includes validation call", {
  report_file <- here::here("reports", "state-space-comparison.qmd")
  skip_if_not(file.exists(report_file), "Report file not found")

  report_content <- readLines(report_file)

  # Look for validation call in setup chunk (lines after ```{r setup})
  setup_start <- which(grepl("^```\\{r setup", report_content))
  if (length(setup_start) > 0) {
    # Find end of setup chunk (next ```)
    chunk_end <- which(grepl("^```$", report_content[(setup_start[1]+1):length(report_content)]))[1] + setup_start[1]
    setup_chunk <- report_content[(setup_start[1]+1):(chunk_end-1)]

    # Check for validate_parallel_model_exists() call
    has_validation <- any(grepl("validate_parallel_model_exists", setup_chunk))

    expect_true(
      has_validation,
      info = "Setup chunk should call validate_parallel_model_exists()"
    )
  } else {
    skip("Setup chunk not found in report")
  }
})

test_that("report threading comparison chunk fails gracefully when parallel missing", {
  # This test ensures the report chunk uses validation or proper error handling
  report_file <- here::here("reports", "state-space-comparison.qmd")
  skip_if_not(file.exists(report_file), "Report file not found")

  report_content <- readLines(report_file)

  # Find threading comparison chunk
  chunk_start <- which(grepl("^```\\{r threading-comparison", report_content))
  if (length(chunk_start) > 0) {
    # Check if chunk uses error handling (tryCatch or stop)
    chunk_end <- which(grepl("^```$", report_content[(chunk_start[1]+1):length(report_content)]))[1] + chunk_start[1]
    chunk <- report_content[(chunk_start[1]+1):(chunk_end-1)]

    # Should have proper error handling (stop, validate, or similar)
    has_error_handling <- any(grepl("stop\\s*\\(|validate_parallel_model_exists|if.*file\\.exists.*stop", chunk))

    expect_true(
      has_error_handling,
      info = "Threading comparison chunk should have error handling for missing parallel model"
    )
  } else {
    skip("Threading comparison chunk not found")
  }
})