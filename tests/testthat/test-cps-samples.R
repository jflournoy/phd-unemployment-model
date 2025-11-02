#' Tests for CPS Sample Generation
#'
#' TDD RED Phase: Define interface for generate_cps_samples() before implementation

library(testthat)

# Will source the function once it exists
test_that("generate_cps_samples function exists", {
  # Load the function
  source(here::here("R", "ipums-download.R"))

  # Function should exist
  expect_true(exists("generate_cps_samples", mode = "function"))
})

test_that("generate_cps_samples returns character vector", {
  source(here::here("R", "ipums-download.R"))

  # Should return character vector
  samples <- generate_cps_samples(2024, 2024)
  expect_type(samples, "character")
})

test_that("generate_cps_samples generates correct number of samples for single year", {
  source(here::here("R", "ipums-download.R"))

  # Single year should have 12 months
  samples <- generate_cps_samples(2024, 2024)
  expect_equal(length(samples), 12)
})

test_that("generate_cps_samples generates correct number of samples for multiple years", {
  source(here::here("R", "ipums-download.R"))

  # 2020-2024 is 5 years = 60 months (may vary by IPUMS availability)
  samples <- generate_cps_samples(2020, 2024)
  expect_gte(length(samples), 50)  # Should have most months
  expect_lte(length(samples), 60)  # But not more than possible

  # 2000-2025 is 26 years = 312 months max (may vary by availability)
  samples <- generate_cps_samples(2000, 2025)
  expect_gte(length(samples), 300)  # Should have most months
  expect_lte(length(samples), 312)  # But not more than possible
})

test_that("generate_cps_samples uses correct CPS naming convention", {
  source(here::here("R", "ipums-download.R"))

  samples <- generate_cps_samples(2024, 2024)

  # Should start with "cps"
  expect_true(all(grepl("^cps", samples)))

  # Should have year and month pattern (cps2024_01s, etc.)
  expect_true(all(grepl("^cps\\d{4}_\\d{2}[bs]$", samples)))

  # First sample should be January (01)
  expect_match(samples[1], "cps2024_01")

  # Last sample should be December (12)
  expect_match(samples[12], "cps2024_12")
})

test_that("generate_cps_samples handles March ASEC correctly", {
  source(here::here("R", "ipums-download.R"))

  samples <- generate_cps_samples(2024, 2024)

  # March should use ASEC supplement (03s, not 03b)
  march_sample <- samples[grepl("_03", samples)]
  expect_match(march_sample, "s$")  # Should end with 's' for ASEC
})

test_that("generate_cps_samples handles year range validation", {
  source(here::here("R", "ipums-download.R"))

  # Should fail if end_year < start_year
  expect_error(
    generate_cps_samples(2025, 2020),
    "end_year must be >= start_year"
  )

  # Should fail if years are not numeric
  expect_error(
    generate_cps_samples("2020", 2024)
  )
})

test_that("generate_cps_samples returns samples in chronological order", {
  source(here::here("R", "ipums-download.R"))

  samples <- generate_cps_samples(2020, 2022)

  # First sample should be 2020-01
  expect_match(samples[1], "cps2020_01")

  # Last sample should be 2022-12
  expect_match(samples[length(samples)], "cps2022_12")

  # Sample 13 should be 2021-01 (first month of second year)
  expect_match(samples[13], "cps2021_01")
})

test_that("generate_cps_samples works with edge cases", {
  source(here::here("R", "ipums-download.R"))

  # Single month (start_year == end_year, January)
  samples <- generate_cps_samples(2024, 2024)
  expect_equal(length(samples), 12)

  # Very old data (year 2000)
  samples <- generate_cps_samples(2000, 2000)
  expect_equal(length(samples), 12)
  expect_match(samples[1], "cps2000_01")
})
