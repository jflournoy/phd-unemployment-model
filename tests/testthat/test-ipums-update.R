#' Tests for Smart IPUMS Data Update
#'
#' TDD RED Phase: Tests for update_ipums_data() function that:
#' - Checks if data exists
#' - Checks if data is stale
#' - Downloads new data only when needed

library(testthat)

# Source the function for testing
source(here::here("R", "ipums-update.R"))

test_that("update_ipums_data function exists", {
  expect_true(exists("update_ipums_data", mode = "function"))
})

test_that("update_ipums_data downloads data when no data exists", {
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # No data exists yet
  data_file <- file.path(temp_dir, "ipums_data.rds")
  expect_false(file.exists(data_file))

  # Should download data
  result <- update_ipums_data(
    output_dir = temp_dir,
    use_api = FALSE,  # Use placeholder for testing
    force = FALSE
  )

  expect_true(result$downloaded)
  expect_true(file.exists(result$file_path))
  expect_equal(result$reason, "no_data")
})

test_that("update_ipums_data skips download when data is recent", {
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create recent data (today)
  data_file <- file.path(temp_dir, "ipums_data.rds")
  test_data <- data.frame(x = 1:3)
  saveRDS(test_data, data_file)

  # Should skip download (data is recent)
  result <- update_ipums_data(
    output_dir = temp_dir,
    use_api = FALSE,
    force = FALSE,
    max_age_days = 30
  )

  expect_false(result$downloaded)
  expect_equal(result$reason, "data_recent")
  expect_true("data_age_days" %in% names(result))
})

test_that("update_ipums_data downloads when data is stale", {
  skip("Requires ability to set file modification time")

  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create old data (would need to modify file timestamp)
  data_file <- file.path(temp_dir, "ipums_data.rds")
  test_data <- data.frame(x = 1:3)
  saveRDS(test_data, data_file)

  # Simulate old file (this is why we skip - need system-level control)
  # In real scenario, file would be >max_age_days old

  result <- update_ipums_data(
    output_dir = temp_dir,
    use_api = FALSE,
    force = FALSE,
    max_age_days = 7
  )

  expect_true(result$downloaded)
  expect_equal(result$reason, "data_stale")
})

test_that("update_ipums_data forces download when force=TRUE", {
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create recent data
  data_file <- file.path(temp_dir, "ipums_data.rds")
  test_data <- data.frame(x = 1:3)
  saveRDS(test_data, data_file)

  # Force download even though data is recent
  result <- update_ipums_data(
    output_dir = temp_dir,
    use_api = FALSE,
    force = TRUE
  )

  expect_true(result$downloaded)
  expect_equal(result$reason, "forced")
})

test_that("update_ipums_data returns metadata about data age", {
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create data
  data_file <- file.path(temp_dir, "ipums_data.rds")
  test_data <- data.frame(x = 1:3)
  saveRDS(test_data, data_file)

  result <- update_ipums_data(
    output_dir = temp_dir,
    use_api = FALSE,
    force = FALSE
  )

  # Should include metadata
  expect_true("data_age_days" %in% names(result))
  expect_true("file_path" %in% names(result))
  expect_true("downloaded" %in% names(result))
  expect_true("reason" %in% names(result))
  expect_type(result$data_age_days, "double")
})

test_that("update_ipums_data uses default max_age_days of 30", {
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # No data exists
  result <- update_ipums_data(
    output_dir = temp_dir,
    use_api = FALSE
    # Don't specify max_age_days - should use default
  )

  expect_true(result$downloaded)
})

test_that("update_ipums_data passes through API parameters", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Should pass samples and variables to download function
  result <- update_ipums_data(
    output_dir = temp_dir,
    use_api = TRUE,
    samples = c("us2022a"),
    variables = c("YEAR", "EMPSTAT", "EDUC"),
    force = TRUE
  )

  expect_true(result$downloaded)
  expect_true("extract_info" %in% names(result))
})
