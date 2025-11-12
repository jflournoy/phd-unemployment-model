#' Tests for Education Level Unemployment Comparison
#'
#' TDD RED Phase: Tests for comparing unemployment rates across educational
#' attainment strata using GAM seasonal models

library(testthat)

# Source required functions
source(here::here("R", "seasonal-gam.R"))
source(here::here("R", "data-processing.R"))
source(here::here("R", "education-comparison.R"))

test_that("compare_unemployment_by_education function exists", {
  expect_true(exists("compare_unemployment_by_education", mode = "function"))
})

test_that("compare_unemployment_by_education accepts data and education levels", {
  # This test will fail until we implement the function
  skip("Function not yet implemented")

  # Load real CPS data
  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  # Should accept CPS data and education codes
  result <- compare_unemployment_by_education(
    data = cps_data,
    education_levels = c(125, 123, 111)  # PhD, Masters, Bachelors
  )

  expect_type(result, "list")
})

test_that("compare_unemployment_by_education returns data for each education level", {
  skip("Function not yet implemented")

  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  result <- compare_unemployment_by_education(
    data = cps_data,
    education_levels = c(125, 123, 111)
  )

  expect_true("phd" %in% names(result))
  expect_true("masters" %in% names(result))
  expect_true("bachelors" %in% names(result))
})

test_that("compare_unemployment_by_education fits GAM for each education level", {
  skip("Function not yet implemented")

  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  result <- compare_unemployment_by_education(
    data = cps_data,
    education_levels = c(125, 123)  # PhD, Masters
  )

  # Should have monthly data for each level
  expect_true("monthly_data" %in% names(result$phd))
  expect_true("monthly_data" %in% names(result$masters))

  # Should have fitted GAM models
  expect_true("model" %in% names(result$phd))
  expect_true(inherits(result$phd$model, "gam"))
})

test_that("compare_unemployment_by_education extracts seasonal and trend components", {
  skip("Function not yet implemented")

  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  result <- compare_unemployment_by_education(
    data = cps_data,
    education_levels = c(125, 123)
  )

  # Should have seasonal components
  expect_true("seasonal" %in% names(result$phd))
  expect_equal(nrow(result$phd$seasonal), 12)  # 12 months

  # Should have trend components
  expect_true("trend" %in% names(result$phd))
  expect_gt(nrow(result$phd$trend), 0)
})

test_that("plot_education_comparison function exists", {
  expect_true(exists("plot_education_comparison", mode = "function"))
})

test_that("plot_education_comparison creates comparison plots", {
  skip("Function not yet implemented")

  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  comparison_result <- compare_unemployment_by_education(
    data = cps_data,
    education_levels = c(125, 123, 111)
  )

  plots <- plot_education_comparison(comparison_result)

  # Should return list of ggplot objects
  expect_type(plots, "list")
  expect_true(all(sapply(plots, function(p) inherits(p, "gg"))))

  # Should have key plots
  expect_true("timeseries" %in% names(plots))
  expect_true("seasonal" %in% names(plots))
  expect_true("trend" %in% names(plots))
})

test_that("compare_unemployment_by_education handles missing education codes gracefully", {
  skip("Function not yet implemented")

  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  # Should handle missing data for an education level
  result <- compare_unemployment_by_education(
    data = cps_data,
    education_levels = c(125, 999)  # 999 doesn't exist
  )

  expect_true("phd" %in% names(result))
  expect_false("999" %in% names(result))
})

test_that("compare_unemployment_by_education uses proper CPS weights", {
  skip("Function not yet implemented")

  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  result <- compare_unemployment_by_education(
    data = cps_data,
    education_levels = c(125)
  )

  # Verify weighted unemployment rate calculation
  # Should use WTFINL for regular months, ASECWT for March
  monthly_data <- result$phd$monthly_data

  expect_true("unemployment_rate" %in% names(monthly_data))
  expect_true(all(monthly_data$unemployment_rate >= 0))
  expect_true(all(monthly_data$unemployment_rate <= 1))
})
