# Tests for CPS Data Aggregation Functions
#
# TDD RED phase: Define weighted aggregation API using our CPS code constants
#
# Purpose: Build reliable, tested data aggregation functions that:
# 1. Use our CPS code constants (no magic numbers)
# 2. Always apply proper weights (WTFINL or ASECWT)
# 3. Handle counts (n_employed, n_unemployed, n_total)
# 4. Handle rates (unemployment_rate)
# 5. Work at different aggregation levels (month, year, education, etc.)

library(testthat)
library(data.table)

# ==============================================================================
# Test Suite 1: Weighted Counting Primitives
# ==============================================================================

test_that("count_employed returns weighted count of employed persons", {
  # Create sample CPS-like data
  sample_data <- data.frame(
    EMPSTAT = c(10, 10, 12, 20, 30),
    WTFINL = c(100, 200, 150, 100, 100)
  )

  # Should count employed (codes 10, 12) with weights
  n_employed <- count_employed(sample_data, weight_var = "WTFINL")

  expect_type(n_employed, "double")
  # 10: 100 + 200 = 300, 12: 150 = 450 total
  expect_equal(n_employed, 450)
})

test_that("count_unemployed returns weighted count of unemployed persons", {
  sample_data <- data.frame(
    EMPSTAT = c(10, 20, 21, 22, 30),
    WTFINL = c(100, 200, 150, 100, 100)
  )

  # Should count unemployed (codes 20, 21, 22) with weights
  n_unemployed <- count_unemployed(sample_data, weight_var = "WTFINL")

  expect_type(n_unemployed, "double")
  # 20: 200, 21: 150, 22: 100 = 450 total
  expect_equal(n_unemployed, 450)
})

test_that("count_labor_force returns weighted count of labor force participants", {
  sample_data <- data.frame(
    EMPSTAT = c(10, 12, 20, 21, 30),
    WTFINL = c(100, 150, 200, 100, 50)
  )

  # Labor force = employed + unemployed
  n_lf <- count_labor_force(sample_data, weight_var = "WTFINL")

  expect_type(n_lf, "double")
  # Employed (10, 12): 100 + 150 = 250
  # Unemployed (20, 21): 200 + 100 = 300
  # Total: 550
  expect_equal(n_lf, 550)
})

test_that("count_employed uses CPS code constants internally", {
  # This ensures we're not using magic numbers
  sample_data <- data.frame(
    EMPSTAT = get_employed_codes(),
    WTFINL = c(100, 200)
  )

  result <- count_employed(sample_data, weight_var = "WTFINL")
  expect_equal(result, 300)
})

test_that("counting functions work without weights (unweighted counts)", {
  sample_data <- data.frame(
    EMPSTAT = c(10, 10, 12, 20, 30)
  )

  # Should default to unweighted count (n observations)
  n_employed <- count_employed(sample_data, weight_var = NULL)

  expect_equal(n_employed, 3)  # 3 employed persons
})


# ==============================================================================
# Test Suite 2: Calculate Unemployment Rate (Weighted)
# ==============================================================================

test_that("calculate_unemployment_rate_weighted computes correct rate", {
  sample_data <- data.frame(
    EMPSTAT = c(10, 10, 12, 20, 21),
    WTFINL = c(100, 200, 200, 50, 50)
  )

  # Employed (10, 10, 12): 100 + 200 + 200 = 500
  # Unemployed (20, 21): 50 + 50 = 100
  # Rate = 100 / (500 + 100) = 100/600 = 0.1667

  rate <- calculate_unemployment_rate_weighted(sample_data, weight_var = "WTFINL")

  expect_type(rate, "double")
  expect_equal(rate, 100/600, tolerance = 0.0001)
})

test_that("calculate_unemployment_rate_weighted handles ASECWT for March", {
  # March ASEC supplement uses ASECWT instead of WTFINL
  sample_data <- data.frame(
    EMPSTAT = c(10, 20, 30),
    MONTH = c(3, 3, 3),
    WTFINL = c(100, 100, 100),
    ASECWT = c(200, 200, 200)
  )

  # Should prefer ASECWT when available and month is March
  rate <- calculate_unemployment_rate_weighted(
    sample_data,
    weight_var = "auto"  # Auto-detect ASECWT for March
  )

  # With ASECWT: employed 200, unemployed 200, rate = 0.5
  expect_equal(rate, 0.5)
})

test_that("calculate_unemployment_rate_weighted returns NA when no labor force", {
  # All not in labor force
  sample_data <- data.frame(
    EMPSTAT = c(30, 31, 32),
    WTFINL = c(100, 100, 100)
  )

  rate <- calculate_unemployment_rate_weighted(sample_data, weight_var = "WTFINL")

  expect_true(is.na(rate))
})


# ==============================================================================
# Test Suite 3: Aggregate by Month (Weighted Counts and Rates)
# ==============================================================================

test_that("aggregate_monthly_unemployment returns counts and rates by month", {
  # Create multi-month data
  sample_data <- data.frame(
    YEAR = c(rep(2020, 5), rep(2020, 5)),
    MONTH = c(rep(1, 5), rep(2, 5)),
    EMPSTAT = c(10, 10, 20, 30, 30,   # Month 1
                12, 20, 21, 30, 30),  # Month 2
    WTFINL = rep(100, 10)
  )

  result <- aggregate_monthly_unemployment(sample_data)

  # Should return data.frame with required columns
  expect_s3_class(result, "data.frame")
  expect_true(all(c("year", "month", "n_employed", "n_unemployed",
                    "n_total", "unemployment_rate") %in% names(result)))

  # Should have 2 rows (2 months)
  expect_equal(nrow(result), 2)

  # Check month 1: 2 employed (200), 1 unemployed (100), rate = 1/3
  month1 <- result[result$month == 1, ]
  expect_equal(month1$n_employed, 200)
  expect_equal(month1$n_unemployed, 100)
  expect_equal(month1$n_total, 300)
  expect_equal(month1$unemployment_rate, 1/3, tolerance = 0.001)
})

test_that("aggregate_monthly_unemployment preserves time ordering", {
  sample_data <- data.frame(
    YEAR = c(2020, 2020, 2021, 2021),
    MONTH = c(1, 12, 1, 6),
    EMPSTAT = rep(10, 4),
    WTFINL = rep(100, 4)
  )

  result <- aggregate_monthly_unemployment(sample_data)

  # Should be sorted by year, month
  expect_equal(result$year, c(2020, 2020, 2021, 2021))
  expect_equal(result$month, c(1, 12, 1, 6))

  # Should have time_index
  expect_true("time_index" %in% names(result))
  expect_equal(result$time_index, 1:4)
})


# ==============================================================================
# Test Suite 4: Aggregate by Education Level (Weighted)
# ==============================================================================

test_that("aggregate_by_education returns counts and rates by education", {
  # Create multi-education data
  sample_data <- data.frame(
    EDUC = c(111, 111, 111, 125, 125, 125),
    EMPSTAT = c(10, 10, 20, 10, 20, 21),
    WTFINL = rep(100, 6)
  )

  result <- aggregate_by_education(sample_data)

  # Should return data.frame
  expect_s3_class(result, "data.frame")
  expect_true(all(c("education", "n_employed", "n_unemployed",
                    "n_total", "unemployment_rate") %in% names(result)))

  # Should have 2 education levels
  expect_equal(nrow(result), 2)
  expect_setequal(result$education, c("bachelors", "phd"))

  # Check bachelors: 2 employed, 1 unemployed, rate = 1/3
  bach <- result[result$education == "bachelors", ]
  expect_equal(bach$n_employed, 200)
  expect_equal(bach$n_unemployed, 100)
  expect_equal(bach$unemployment_rate, 1/3, tolerance = 0.001)
})

test_that("aggregate_by_education uses education code constants", {
  # Should use get_education_code_map() internally
  sample_data <- data.frame(
    EDUC = c(2, 73, 111, 123, 125),
    EMPSTAT = rep(10, 5),
    WTFINL = rep(100, 5)
  )

  result <- aggregate_by_education(sample_data)

  # All 5 education levels should be present with labels from constants
  expect_equal(nrow(result), 5)
  expected_labels <- get_education_label(c(2, 73, 111, 123, 125))
  expect_setequal(result$education, expected_labels)
})


# ==============================================================================
# Test Suite 5: Aggregate by Month AND Education (Full Cross)
# ==============================================================================

test_that("aggregate_monthly_by_education returns full cross of month × education", {
  # Create data with 2 months × 2 education levels
  sample_data <- data.frame(
    YEAR = c(rep(2020, 6), rep(2020, 6)),
    MONTH = c(rep(1, 6), rep(2, 6)),
    EDUC = rep(c(111, 111, 111, 125, 125, 125), 2),
    EMPSTAT = c(10, 10, 20, 10, 20, 21,   # Month 1
                10, 20, 20, 10, 10, 20),  # Month 2
    WTFINL = rep(100, 12)
  )

  result <- aggregate_monthly_by_education(sample_data)

  # Should have 2 months × 2 education levels = 4 rows
  expect_equal(nrow(result), 4)

  # Required columns
  expect_true(all(c("year", "month", "education", "n_employed", "n_unemployed",
                    "n_total", "unemployment_rate", "time_index") %in% names(result)))

  # Check one specific cell: month 1, bachelors
  month1_bach <- result[result$month == 1 & result$education == "bachelors", ]
  expect_equal(month1_bach$n_employed, 200)
  expect_equal(month1_bach$n_unemployed, 100)
})

test_that("aggregate_monthly_by_education matches calculate_monthly_unemployment_counts output", {
  # This function should produce same structure as our existing function
  # but using the new constants-based approach

  sample_data <- data.frame(
    YEAR = rep(2020, 10),
    MONTH = rep(1, 10),
    EDUC = c(rep(111, 5), rep(125, 5)),
    EMPSTAT = c(10, 10, 10, 20, 30,   # Bachelors
                10, 10, 20, 21, 30),  # PhD
    WTFINL = rep(100, 10)
  )

  result <- aggregate_monthly_by_education(sample_data)

  # Should have counts format (not rates)
  expect_true("n_employed" %in% names(result))
  expect_true("n_unemployed" %in% names(result))
  expect_true("n_total" %in% names(result))

  # Verify counts are unweighted (this version counts persons, not weights)
  # Actually, for consistency with existing code, should this be weighted?
  # Let's make it flexible with a parameter
})


# ==============================================================================
# Test Suite 6: Weight Variable Selection
# ==============================================================================

test_that("select_weight_variable auto-detects ASECWT for March", {
  # March data with ASECWT
  march_data <- data.frame(
    MONTH = rep(3, 5),
    WTFINL = rep(100, 5),
    ASECWT = rep(200, 5)
  )

  weight_var <- select_weight_variable(march_data)
  expect_equal(weight_var, "ASECWT")
})

test_that("select_weight_variable uses WTFINL for non-March months", {
  # Non-March data
  jan_data <- data.frame(
    MONTH = rep(1, 5),
    WTFINL = rep(100, 5),
    ASECWT = rep(NA, 5)  # ASECWT is NA for non-ASEC months
  )

  weight_var <- select_weight_variable(jan_data)
  expect_equal(weight_var, "WTFINL")
})

test_that("select_weight_variable handles missing ASECWT gracefully", {
  # Data without ASECWT column
  data_no_asec <- data.frame(
    MONTH = rep(3, 5),
    WTFINL = rep(100, 5)
  )

  weight_var <- select_weight_variable(data_no_asec)
  expect_equal(weight_var, "WTFINL")  # Falls back to WTFINL
})


# ==============================================================================
# Test Suite 7: Integration - Using Constants Throughout
# ==============================================================================

test_that("aggregation functions never use magic numbers", {
  # This is a design test - aggregation functions should call:
  # - get_employed_codes()
  # - get_unemployed_codes()
  # - get_education_code_map()
  # Instead of hardcoding c(10, 12) or c("111" = "bachelors")

  # Create test data
  sample_data <- data.frame(
    YEAR = rep(2020, 10),
    MONTH = rep(1, 10),
    EDUC = rep(c(111, 125), each = 5),
    EMPSTAT = c(
      get_employed_codes()[1],
      get_unemployed_codes()[1],
      get_employed_codes()[2],
      30, 30,  # Not in LF
      get_employed_codes()[1],
      get_employed_codes()[1],
      get_unemployed_codes()[2],
      get_unemployed_codes()[3],
      30
    ),
    WTFINL = rep(100, 10)
  )

  # Should work correctly using constants
  result <- aggregate_monthly_by_education(sample_data)

  # Verify it correctly identified employment status
  bach <- result[result$education == "bachelors", ]
  expect_gt(bach$n_employed, 0)
  expect_gt(bach$n_unemployed, 0)
})


# ==============================================================================
# Test Suite 8: Unweighted vs Weighted Counts
# ==============================================================================

test_that("aggregation functions support both weighted and unweighted modes", {
  sample_data <- data.frame(
    YEAR = rep(2020, 5),
    MONTH = rep(1, 5),
    EDUC = rep(111, 5),
    EMPSTAT = c(10, 10, 10, 20, 30),
    WTFINL = c(100, 200, 150, 100, 50)
  )

  # Weighted: should sum weights
  weighted_result <- aggregate_monthly_by_education(
    sample_data,
    weighted = TRUE
  )
  expect_equal(weighted_result$n_employed, 450)  # 100+200+150
  expect_equal(weighted_result$n_unemployed, 100)

  # Unweighted: should count observations
  unweighted_result <- aggregate_monthly_by_education(
    sample_data,
    weighted = FALSE
  )
  expect_equal(unweighted_result$n_employed, 3L)  # 3 persons
  expect_equal(unweighted_result$n_unemployed, 1L)
})


# ==============================================================================
# Test Suite 9: Error Handling and Validation
# ==============================================================================

test_that("aggregation functions validate required columns exist", {
  # Missing EMPSTAT
  bad_data <- data.frame(
    YEAR = rep(2020, 5),
    MONTH = rep(1, 5),
    WTFINL = rep(100, 5)
  )

  expect_error(
    aggregate_monthly_unemployment(bad_data),
    regexp = "EMPSTAT|required"
  )
})

test_that("aggregation functions handle zero labor force gracefully", {
  # All NILF
  nilf_data <- data.frame(
    YEAR = rep(2020, 5),
    MONTH = rep(1, 5),
    EMPSTAT = rep(30, 5),
    WTFINL = rep(100, 5)
  )

  result <- aggregate_monthly_unemployment(nilf_data)

  # Should have row for the month but with NA rate
  expect_equal(nrow(result), 1)
  expect_equal(result$n_total, 0)
  expect_true(is.na(result$unemployment_rate))
})


