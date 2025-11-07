#' Tests for Data Completeness Report
#'
#' TDD RED Phase: Define interface for validating data download completeness

library(testthat)

test_that("generate_completeness_report function exists", {
  source(here::here("R", "data-completeness.R"))
  expect_true(exists("generate_completeness_report", mode = "function"))
})

test_that("generate_completeness_report returns data frame with expected structure", {
  source(here::here("R", "data-completeness.R"))

  # Create minimal test data
  test_data <- data.frame(
    YEAR = c(2024, 2024, 2024),
    MONTH = c(1, 1, 2),
    EMPSTAT = c(10, 20, 10),
    EDUC = c(125, 125, 125),
    WTFINL = c(1000, 1000, 1000)
  )

  result <- generate_completeness_report(test_data)

  # Should return a data frame
  expect_s3_class(result, "data.frame")

  # Should have expected columns
  expected_cols <- c("year", "month", "n_obs", "has_data", "n_missing_vars",
                    "pct_complete", "issue")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("generate_completeness_report identifies missing months", {
  source(here::here("R", "data-completeness.R"))

  # Data with gaps (only Jan, Feb, Apr - missing March)
  test_data <- data.frame(
    YEAR = rep(2024, 6),
    MONTH = c(1, 1, 2, 2, 4, 4),
    EMPSTAT = c(10, 20, 10, 20, 10, 20),
    EDUC = rep(125, 6),
    WTFINL = rep(1000, 6)
  )

  result <- generate_completeness_report(test_data, expected_months = 1:4)

  # Should have 4 rows (one per expected month)
  expect_equal(nrow(result), 4)

  # March should be flagged as missing
  march <- result[result$month == 3, ]
  expect_false(march$has_data)
  expect_true(grepl("missing", march$issue, ignore.case = TRUE))
})

test_that("generate_completeness_report calculates variable completeness", {
  source(here::here("R", "data-completeness.R"))

  # Data with some missing values
  test_data <- data.frame(
    YEAR = rep(2024, 4),
    MONTH = c(1, 1, 1, 1),
    EMPSTAT = c(10, 20, NA, 10),
    EDUC = c(125, 125, 125, NA),
    WTFINL = c(1000, 1000, 1000, 1000)
  )

  result <- generate_completeness_report(test_data)

  # Should calculate completeness percentage
  expect_true("pct_complete" %in% names(result))
  jan <- result[result$month == 1, ]

  # With 2 NAs out of 12 values (4 obs × 3 vars), completeness = 10/12 ≈ 83%
  expect_lt(jan$pct_complete, 100)
  expect_gt(jan$pct_complete, 80)
})

test_that("generate_completeness_report identifies small sample sizes", {
  source(here::here("R", "data-completeness.R"))

  # Very small sample in one month
  test_data <- data.frame(
    YEAR = rep(2024, 3),
    MONTH = c(1, 1, 2),
    EMPSTAT = c(10, 20, 10),
    EDUC = rep(125, 3),
    WTFINL = rep(1000, 3)
  )

  result <- generate_completeness_report(test_data, min_obs = 5)

  # February should be flagged for small sample
  feb <- result[result$month == 2, ]
  expect_true(grepl("small sample|insufficient", feb$issue, ignore.case = TRUE))
})

test_that("print_completeness_report function exists", {
  source(here::here("R", "data-completeness.R"))
  expect_true(exists("print_completeness_report", mode = "function"))
})

test_that("print_completeness_report runs without error", {
  source(here::here("R", "data-completeness.R"))

  test_data <- data.frame(
    YEAR = rep(2024, 12),
    MONTH = 1:12,
    EMPSTAT = rep(10, 12),
    EDUC = rep(125, 12),
    WTFINL = rep(1000, 12)
  )

  report <- generate_completeness_report(test_data)

  # Should not error
  expect_no_error(print_completeness_report(report))
})

test_that("validate_data_completeness function exists", {
  source(here::here("R", "data-completeness.R"))
  expect_true(exists("validate_data_completeness", mode = "function"))
})

test_that("validate_data_completeness returns logical", {
  source(here::here("R", "data-completeness.R"))

  test_data <- data.frame(
    YEAR = rep(2024, 12),
    MONTH = 1:12,
    EMPSTAT = rep(10, 12),
    EDUC = rep(125, 12),
    WTFINL = rep(1000, 12)
  )

  result <- validate_data_completeness(test_data)

  expect_type(result, "logical")
  expect_equal(length(result), 1)
})

test_that("validate_data_completeness returns FALSE for incomplete data", {
  source(here::here("R", "data-completeness.R"))

  # Missing months
  test_data <- data.frame(
    YEAR = rep(2024, 2),
    MONTH = c(1, 1),
    EMPSTAT = c(10, 20),
    EDUC = rep(125, 2),
    WTFINL = rep(1000, 2)
  )

  result <- validate_data_completeness(test_data, expected_months = 1:12)

  expect_false(result)
})

test_that("validate_data_completeness returns TRUE for complete data", {
  source(here::here("R", "data-completeness.R"))

  # All months present with sufficient data
  test_data <- data.frame(
    YEAR = rep(2024, 120),
    MONTH = rep(1:12, each = 10),
    EMPSTAT = rep(c(10, 20), 60),
    EDUC = rep(125, 120),
    WTFINL = rep(1000, 120)
  )

  result <- validate_data_completeness(test_data, expected_months = 1:12,
                                       min_obs = 5)

  expect_true(result)
})

# TDD RED: Tests for multi-year completeness validation
test_that("generate_completeness_report handles multi-year data", {
  source(here::here("R", "data-completeness.R"))

  # Data spanning multiple years
  test_data <- data.frame(
    YEAR = c(rep(2000, 24), rep(2001, 24)),
    MONTH = rep(1:12, 4),
    EMPSTAT = rep(10, 48),
    EDUC = rep(125, 48),
    WTFINL = rep(1000, 48)
  )

  result <- generate_completeness_report(test_data,
                                          start_year = 2000,
                                          end_year = 2001)

  # Should have one row per year-month combination
  expect_equal(nrow(result), 24)  # 2 years × 12 months

  # Should have both years represented
  expect_true(2000 %in% result$year)
  expect_true(2001 %in% result$year)

  # Each year should have all 12 months
  expect_equal(sum(result$year == 2000), 12)
  expect_equal(sum(result$year == 2001), 12)
})

test_that("generate_completeness_report identifies missing year-month combinations", {
  source(here::here("R", "data-completeness.R"))

  # Data with missing year-month: missing Jan 2001
  test_data <- data.frame(
    YEAR = c(rep(2000, 24), rep(2001, 22)),
    MONTH = c(rep(1:12, 2), 2:12, 2:12),
    EMPSTAT = rep(10, 46),
    EDUC = rep(125, 46),
    WTFINL = rep(1000, 46)
  )

  result <- generate_completeness_report(test_data,
                                          start_year = 2000,
                                          end_year = 2001)

  # Should still have 24 rows (all year-month combinations)
  expect_equal(nrow(result), 24)

  # Jan 2001 should be flagged as missing
  jan_2001 <- result[result$year == 2001 & result$month == 1, ]
  expect_equal(nrow(jan_2001), 1)
  expect_false(jan_2001$has_data)
})

test_that("validate_completeness_time_range validates 2000-2025 data", {
  source(here::here("R", "data-completeness.R"))

  # Simulate 2000-2025 data (26 years × 12 months = 312 combinations)
  n_years <- 26
  n_obs_per_month <- 10

  test_data <- data.frame(
    YEAR = rep(2000:2025, each = 12 * n_obs_per_month),
    MONTH = rep(rep(1:12, each = n_obs_per_month), n_years),
    EMPSTAT = rep(10, n_years * 12 * n_obs_per_month),
    EDUC = rep(125, n_years * 12 * n_obs_per_month),
    WTFINL = rep(1000, n_years * 12 * n_obs_per_month)
  )

  # Should validate successfully (use min_obs = 5 since test data has 10 obs/month)
  result <- validate_completeness_time_range(test_data,
                                             start_year = 2000,
                                             end_year = 2025,
                                             min_obs = 5)

  expect_type(result, "logical")
  expect_true(result)
})

test_that("plot_seasonal_pattern_by_year function exists", {
  source(here::here("R", "data-completeness.R"))
  expect_true(exists("plot_seasonal_pattern_by_year", mode = "function"))
})

test_that("plot_seasonal_pattern_by_year creates plot with year lines", {
  source(here::here("R", "data-completeness.R"))

  # Create multi-year test data with varying sample sizes by month
  test_data <- data.frame(
    YEAR = rep(2023:2025, each = 12),
    MONTH = rep(1:12, 3),
    n_obs = rep(c(50000, 51000, 52000, 53000, 54000, 55000,
                  56000, 57000, 58000, 59000, 60000, 61000), 3) +
           rep(c(0, 1000, 2000), each = 12)  # Slight variation by year
  )

  # Should not error when creating plot
  expect_no_error({
    plot_seasonal_pattern_by_year(test_data,
                                   y_var = "n_obs",
                                   y_label = "Sample Size",
                                   title = "Test Plot")
  })
})

test_that("plot_seasonal_pattern_by_year handles single year gracefully", {
  source(here::here("R", "data-completeness.R"))

  # Single year data
  test_data <- data.frame(
    YEAR = rep(2024, 12),
    MONTH = 1:12,
    n_obs = seq(50000, 61000, length.out = 12)
  )

  # Should still work with just one line
  expect_no_error({
    plot_seasonal_pattern_by_year(test_data,
                                   y_var = "n_obs",
                                   y_label = "Sample Size")
  })
})
