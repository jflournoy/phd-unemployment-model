#' Tests for Data Freshness Checking
#'
#' TDD RED Phase: Define interface for checking if downloaded data is current

library(testthat)

test_that("check_data_freshness function exists", {
  source(here::here("R", "data-freshness.R"))
  expect_true(exists("check_data_freshness", mode = "function"))
})

test_that("check_data_freshness returns TRUE for fresh data", {
  source(here::here("R", "data-freshness.R"))

  # Create a temporary test file modified today
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(data.frame(test = 1), temp_file)

  result <- check_data_freshness(temp_file, max_age_days = 30)

  expect_true(result)

  unlink(temp_file)
})

test_that("check_data_freshness returns FALSE for stale data", {
  source(here::here("R", "data-freshness.R"))

  # Create a temporary test file and manually set old modification time
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(data.frame(test = 1), temp_file)

  # Set file modification time to 60 days ago
  old_time <- Sys.time() - (60 * 24 * 60 * 60)
  Sys.setFileTime(temp_file, old_time)

  result <- check_data_freshness(temp_file, max_age_days = 30)

  expect_false(result)

  unlink(temp_file)
})

test_that("check_data_freshness returns FALSE for non-existent file", {
  source(here::here("R", "data-freshness.R"))

  result <- check_data_freshness("nonexistent_file.rds", max_age_days = 30)

  expect_false(result)
})

test_that("check_data_freshness accepts custom max_age_days", {
  source(here::here("R", "data-freshness.R"))

  temp_file <- tempfile(fileext = ".rds")
  saveRDS(data.frame(test = 1), temp_file)

  # Set file modification time to 10 days ago
  old_time <- Sys.time() - (10 * 24 * 60 * 60)
  Sys.setFileTime(temp_file, old_time)

  # Should be fresh with 30 day threshold
  expect_true(check_data_freshness(temp_file, max_age_days = 30))

  # Should be stale with 5 day threshold
  expect_false(check_data_freshness(temp_file, max_age_days = 5))

  unlink(temp_file)
})

test_that("get_data_age_days function exists", {
  source(here::here("R", "data-freshness.R"))
  expect_true(exists("get_data_age_days", mode = "function"))
})

test_that("get_data_age_days returns numeric age for existing file", {
  source(here::here("R", "data-freshness.R"))

  temp_file <- tempfile(fileext = ".rds")
  saveRDS(data.frame(test = 1), temp_file)

  age <- get_data_age_days(temp_file)

  expect_type(age, "double")
  expect_gte(age, 0)
  expect_lt(age, 1)  # Should be less than 1 day old

  unlink(temp_file)
})

test_that("get_data_age_days returns NA for non-existent file", {
  source(here::here("R", "data-freshness.R"))

  age <- get_data_age_days("nonexistent_file.rds")

  expect_true(is.na(age))
})
