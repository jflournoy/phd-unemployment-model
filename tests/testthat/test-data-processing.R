#' Tests for CPS Data Processing Functions
#'
#' TDD RED Phase: Define interface for data processing pipeline

library(testthat)

# Will source the function once it exists
test_that("filter_phd_holders function exists", {
  source(here::here("R", "data-processing.R"))
  expect_true(exists("filter_phd_holders", mode = "function"))
})

test_that("filter_phd_holders extracts PhD holders correctly", {
  source(here::here("R", "data-processing.R"))

  # Create test data
  test_data <- data.frame(
    YEAR = c(2024, 2024, 2024, 2024),
    MONTH = c(1, 1, 1, 1),
    EDUC = c(125, 111, 125, 116),  # 125 = PhD, others not
    EMPSTAT = c(1, 1, 2, 1),
    WTFINL = c(1500, 1600, 1400, 1550)
  )

  result <- filter_phd_holders(test_data)

  # Should only keep EDUC == 125
  expect_equal(nrow(result), 2)
  expect_true(all(result$EDUC == 125))
})

test_that("filter_phd_holders preserves all columns", {
  source(here::here("R", "data-processing.R"))

  test_data <- data.frame(
    YEAR = c(2024, 2024),
    MONTH = c(1, 2),
    EDUC = c(125, 125),
    EMPSTAT = c(1, 2),
    AGE = c(35, 40),
    SEX = c(1, 2),
    WTFINL = c(1500, 1600)
  )

  result <- filter_phd_holders(test_data)

  # Should preserve all columns
  expect_equal(ncol(result), ncol(test_data))
  expect_true(all(names(test_data) %in% names(result)))
})

test_that("calculate_unemployment_rate function exists", {
  source(here::here("R", "data-processing.R"))
  expect_true(exists("calculate_unemployment_rate", mode = "function"))
})

test_that("calculate_unemployment_rate computes weighted rates correctly", {
  source(here::here("R", "data-processing.R"))

  # Create test data with known unemployment rate
  # EMPSTAT: 10=employed at work, 12=employed but absent, 20-22=unemployed
  # Let's use simplified: 1=employed, 2=unemployed for this test
  test_data <- data.frame(
    YEAR = c(2024, 2024, 2024, 2024),
    MONTH = c(1, 1, 1, 1),
    EMPSTAT = c(10, 10, 20, 20),  # 2 employed, 2 unemployed
    WTFINL = c(1000, 1000, 1000, 1000)  # Equal weights
  )

  result <- calculate_unemployment_rate(test_data)

  # Should return a single number
  expect_type(result, "double")
  expect_length(result, 1)

  # With equal weights, 50% unemployed
  expect_equal(result, 0.5, tolerance = 0.01)
})

test_that("calculate_unemployment_rate handles weighted data correctly", {
  source(here::here("R", "data-processing.R"))

  # Test with unequal weights
  test_data <- data.frame(
    EMPSTAT = c(10, 10, 20),  # 2 employed, 1 unemployed
    WTFINL = c(500, 500, 2000)  # Unemployed person has 2x weight
  )

  result <- calculate_unemployment_rate(test_data)

  # Weighted unemployment: 2000 / (500 + 500 + 2000) = 2000/3000 = 0.667
  expect_equal(result, 2000/3000, tolerance = 0.01)
})

test_that("calculate_unemployment_rate excludes NILF (codes 30-36) from labor force", {
  source(here::here("R", "data-processing.R"))

  # Test that NILF codes (30-36: retired, school, housework, etc.) are excluded
  test_data <- data.frame(
    EMPSTAT = c(10, 10, 20, 21, 32, 34, 36),  # employed, unemployed, NILF
    WTFINL = c(1000, 1000, 1000, 1000, 1000, 1000, 1000)
  )

  result <- calculate_unemployment_rate(test_data)

  # Labor force = 4 (2 employed + 2 unemployed)
  # NILF = 3 (codes 32, 34, 36) should be excluded
  # Unemployment rate = 2 / 4 = 0.5
  expect_equal(result, 0.5, tolerance = 0.01)
})

test_that("calculate_unemployment_rate only counts codes 20-22 as unemployed", {
  source(here::here("R", "data-processing.R"))

  # Verify that only codes 20, 21, 22 are counted as unemployed
  test_data <- data.frame(
    EMPSTAT = c(10, 20, 21, 22),  # All should be in labor force
    WTFINL = c(1000, 1000, 1000, 1000)
  )

  result <- calculate_unemployment_rate(test_data)

  # 1 employed, 3 unemployed -> 3/4 = 0.75
  expect_equal(result, 0.75, tolerance = 0.01)
})

test_that("process_cps_data function exists (main pipeline)", {
  source(here::here("R", "data-processing.R"))
  expect_true(exists("process_cps_data", mode = "function"))
})

test_that("process_cps_data runs complete pipeline", {
  source(here::here("R", "data-processing.R"))

  # Create realistic test data
  test_data <- data.frame(
    YEAR = rep(2024, 8),
    MONTH = rep(1:2, each = 4),
    EDUC = c(125, 125, 111, 116, 125, 125, 111, 125),  # Mix of PhD and non-PhD
    EMPSTAT = c(10, 20, 10, 10, 10, 10, 20, 20),
    AGE = rep(35, 8),
    SEX = rep(1, 8),
    WTFINL = rep(1000, 8)
  )

  result <- process_cps_data(test_data)

  # Should filter to PhDs only (5 PhDs across 2 months)
  # Should return monthly aggregates (2 rows)
  expect_equal(nrow(result), 2)

  # Should have required columns
  expect_true(all(c("YEAR", "MONTH", "unemployment_rate", "n_obs") %in% names(result)))

  # January: 2 PhDs, 1 unemployed -> 50%
  jan <- result[result$MONTH == 1, ]
  expect_equal(jan$unemployment_rate, 0.5, tolerance = 0.01)
  expect_equal(jan$n_obs, 2)

  # February: 3 PhDs, 1 unemployed -> 33%
  feb <- result[result$MONTH == 2, ]
  expect_equal(feb$unemployment_rate, 1/3, tolerance = 0.01)
  expect_equal(feb$n_obs, 3)
})

test_that("filter_education_level function exists for generic education filtering", {
  source(here::here("R", "data-processing.R"))
  expect_true(exists("filter_education_level", mode = "function"))
})

test_that("filter_education_level filters to specified education code", {
  source(here::here("R", "data-processing.R"))

  # Create test data with multiple education levels
  test_data <- data.frame(
    YEAR = rep(2024, 6),
    MONTH = rep(1, 6),
    EDUC = c(125, 123, 111, 125, 123, 73),  # PhD, Masters, Bachelors, PhD, Masters, HS
    EMPSTAT = c(10, 10, 10, 20, 20, 20),
    WTFINL = rep(1000, 6)
  )

  # Filter to Masters (123)
  result <- filter_education_level(test_data, educ_code = 123)

  expect_equal(nrow(result), 2)
  expect_true(all(result$EDUC == 123))

  # Filter to Bachelors (111)
  result_ba <- filter_education_level(test_data, educ_code = 111)
  expect_equal(nrow(result_ba), 1)
  expect_equal(result_ba$EDUC[1], 111)
})

