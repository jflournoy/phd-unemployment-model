# Tests for CPS Code Constants and Lookup Functions
#
# TDD RED phase: Define the API we want for reliable CPS data manipulation
#
# Purpose: Centralize all IPUMS CPS code definitions in tested, documented functions
# to prevent bugs from hardcoded magic numbers scattered across the codebase

library(testthat)

# ==============================================================================
# Test Suite 1: Employment Status Codes
# ==============================================================================

test_that("get_employed_codes returns correct EMPSTAT codes", {
  # EMPSTAT codes for employed persons:
  # 10 = At work
  # 12 = Has job, not at work last week

  employed_codes <- get_employed_codes()

  expect_type(employed_codes, "integer")
  expect_equal(employed_codes, c(10L, 12L))
})

test_that("get_unemployed_codes returns correct EMPSTAT codes", {
  # EMPSTAT codes for unemployed persons (in labor force, seeking work):
  # 20 = Unemployed
  # 21 = Unemployed, experienced worker
  # 22 = Unemployed, new worker

  unemployed_codes <- get_unemployed_codes()

  expect_type(unemployed_codes, "integer")
  expect_equal(unemployed_codes, c(20L, 21L, 22L))
})

test_that("get_labor_force_codes returns employed + unemployed", {
  # Labor force = employed + unemployed

  lf_codes <- get_labor_force_codes()
  employed <- get_employed_codes()
  unemployed <- get_unemployed_codes()

  expect_type(lf_codes, "integer")
  expect_setequal(lf_codes, c(employed, unemployed))
  expect_equal(length(lf_codes), 5)
})

test_that("get_not_in_labor_force_codes returns NIU codes", {
  # Not in labor force codes (partial list, main ones):
  # 30 = Not in labor force
  # 31 = Retired
  # 32 = Disabled
  # 33 = Unable to work
  # 34 = School
  # 35 = Taking care of house or family
  # 36 = Other

  nilf_codes <- get_not_in_labor_force_codes()

  expect_type(nilf_codes, "integer")
  expect_true(all(nilf_codes >= 30))
  expect_true(30 %in% nilf_codes)
})

test_that("is_employed correctly identifies employment status", {
  # Helper function to check if EMPSTAT code means employed

  expect_true(is_employed(10))   # At work
  expect_true(is_employed(12))   # Has job

  expect_false(is_employed(20))  # Unemployed
  expect_false(is_employed(30))  # Not in LF
  expect_false(is_employed(1))   # Armed forces
  expect_false(is_employed(0))   # NIU

  # Vector input
  status_vec <- c(10, 12, 20, 30)
  expect_equal(is_employed(status_vec), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("is_unemployed correctly identifies unemployment status", {
  # Helper function to check if EMPSTAT code means unemployed

  expect_true(is_unemployed(20))   # Unemployed
  expect_true(is_unemployed(21))   # Unemployed, experienced
  expect_true(is_unemployed(22))   # Unemployed, new

  expect_false(is_unemployed(10))  # Employed
  expect_false(is_unemployed(30))  # Not in LF

  # Vector input
  status_vec <- c(20, 21, 10, 30)
  expect_equal(is_unemployed(status_vec), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("is_in_labor_force correctly identifies labor force participation", {
  # Helper function to check if in labor force (employed or unemployed)

  # In labor force
  expect_true(is_in_labor_force(10))   # Employed
  expect_true(is_in_labor_force(12))   # Employed
  expect_true(is_in_labor_force(20))   # Unemployed
  expect_true(is_in_labor_force(21))   # Unemployed

  # Not in labor force
  expect_false(is_in_labor_force(30))  # NILF
  expect_false(is_in_labor_force(0))   # NIU

  # Vector input
  status_vec <- c(10, 20, 30, 0)
  expect_equal(is_in_labor_force(status_vec), c(TRUE, TRUE, FALSE, FALSE))
})


# ==============================================================================
# Test Suite 2: Education Level Codes
# ==============================================================================

test_that("get_education_codes returns all 7 education level codes", {
  # IPUMS CPS EDUC codes for our 7 education categories

  educ_codes <- get_education_codes()

  expect_type(educ_codes, "integer")
  expect_equal(length(educ_codes), 7)

  # Check specific codes
  expect_true(2 %in% educ_codes)     # less_than_hs
  expect_true(73 %in% educ_codes)    # high_school
  expect_true(81 %in% educ_codes)    # some_college
  expect_true(111 %in% educ_codes)   # bachelors
  expect_true(123 %in% educ_codes)   # masters
  expect_true(124 %in% educ_codes)   # professional
  expect_true(125 %in% educ_codes)   # phd
})

test_that("get_education_code_map returns named vector mapping codes to labels", {
  # Returns c("2" = "less_than_hs", "73" = "high_school", ...)

  educ_map <- get_education_code_map()

  expect_type(educ_map, "character")
  expect_named(educ_map)

  # Check mapping
  expect_equal(educ_map["2"], c("2" = "less_than_hs"))
  expect_equal(educ_map["73"], c("73" = "high_school"))
  expect_equal(educ_map["81"], c("81" = "some_college"))
  expect_equal(educ_map["111"], c("111" = "bachelors"))
  expect_equal(educ_map["123"], c("123" = "masters"))
  expect_equal(educ_map["124"], c("124" = "professional"))
  expect_equal(educ_map["125"], c("125" = "phd"))
})

test_that("get_education_label converts EDUC code to readable label", {
  # Helper to convert code to label

  expect_equal(get_education_label(2), "less_than_hs")
  expect_equal(get_education_label(73), "high_school")
  expect_equal(get_education_label(81), "some_college")
  expect_equal(get_education_label(111), "bachelors")
  expect_equal(get_education_label(123), "masters")
  expect_equal(get_education_label(124), "professional")
  expect_equal(get_education_label(125), "phd")

  # Vector input
  codes <- c(111, 123, 125)
  expect_equal(get_education_label(codes), c("bachelors", "masters", "phd"))

  # Unknown code should return NA
  expect_true(is.na(get_education_label(999)))
})

test_that("get_education_code converts label to EDUC code", {
  # Reverse lookup: label -> code

  expect_equal(get_education_code("less_than_hs"), 2L)
  expect_equal(get_education_code("high_school"), 73L)
  expect_equal(get_education_code("some_college"), 81L)
  expect_equal(get_education_code("bachelors"), 111L)
  expect_equal(get_education_code("masters"), 123L)
  expect_equal(get_education_code("professional"), 124L)
  expect_equal(get_education_code("phd"), 125L)

  # Vector input
  labels <- c("bachelors", "masters", "phd")
  expect_equal(get_education_code(labels), c(111L, 123L, 125L))

  # Unknown label should return NA
  expect_true(is.na(get_education_code("unknown")))
})

test_that("get_education_levels returns ordered factor levels", {
  # Returns education levels in order from least to most educated

  levels <- get_education_levels()

  expect_type(levels, "character")
  expect_equal(length(levels), 7)

  # Check order (least to most educated)
  expect_equal(levels, c(
    "less_than_hs",
    "high_school",
    "some_college",
    "bachelors",
    "masters",
    "professional",
    "phd"
  ))
})

test_that("is_advanced_degree identifies graduate degrees", {
  # Masters, professional, and PhD are advanced degrees

  expect_false(is_advanced_degree(2))    # less_than_hs
  expect_false(is_advanced_degree(73))   # high_school
  expect_false(is_advanced_degree(81))   # some_college
  expect_false(is_advanced_degree(111))  # bachelors

  expect_true(is_advanced_degree(123))   # masters
  expect_true(is_advanced_degree(124))   # professional
  expect_true(is_advanced_degree(125))   # phd

  # Vector input
  codes <- c(111, 123, 124, 125)
  expect_equal(is_advanced_degree(codes), c(FALSE, TRUE, TRUE, TRUE))
})

test_that("is_college_degree identifies college graduates", {
  # Bachelor's and above are college degrees

  expect_false(is_college_degree(2))    # less_than_hs
  expect_false(is_college_degree(73))   # high_school
  expect_false(is_college_degree(81))   # some_college

  expect_true(is_college_degree(111))   # bachelors
  expect_true(is_college_degree(123))   # masters
  expect_true(is_college_degree(124))   # professional
  expect_true(is_college_degree(125))   # phd

  # Vector input
  codes <- c(73, 81, 111, 125)
  expect_equal(is_college_degree(codes), c(FALSE, FALSE, TRUE, TRUE))
})


# ==============================================================================
# Test Suite 3: Integration - No Magic Numbers in Code
# ==============================================================================

test_that("functions use constants instead of magic numbers", {
  # Test that our existing calculate_monthly_unemployment_counts
  # could use these functions instead of hardcoded values

  # This is a design test - we should be able to refactor to:
  # dt[, is_employed := EMPSTAT %in% get_employed_codes()]
  # dt[, is_unemployed := EMPSTAT %in% get_unemployed_codes()]

  # For now, just verify the constants match what's in data-processing.R
  expect_equal(get_employed_codes(), c(10L, 12L))
  expect_equal(get_unemployed_codes(), c(20L, 21L, 22L))

  # Education map should match
  expected_map <- c(
    "2" = "less_than_hs",
    "73" = "high_school",
    "81" = "some_college",
    "111" = "bachelors",
    "123" = "masters",
    "124" = "professional",
    "125" = "phd"
  )
  expect_equal(get_education_code_map(), expected_map)
})


# ==============================================================================
# Test Suite 4: Documentation and Constants Source
# ==============================================================================

test_that("get_empstat_description provides human-readable descriptions", {
  # Documents what each code means

  desc_10 <- get_empstat_description(10)
  expect_match(desc_10, "at work", ignore.case = TRUE)

  desc_20 <- get_empstat_description(20)
  expect_match(desc_20, "unemployed", ignore.case = TRUE)

  desc_30 <- get_empstat_description(30)
  expect_match(desc_30, "not in labor force", ignore.case = TRUE)
})

test_that("get_education_description provides human-readable descriptions", {
  # Documents what each education code means

  desc_2 <- get_education_description(2)
  expect_match(desc_2, "less than|preschool|grade", ignore.case = TRUE)

  desc_111 <- get_education_description(111)
  expect_match(desc_111, "bachelor", ignore.case = TRUE)

  desc_124 <- get_education_description(124)
  expect_match(desc_124, "professional|MD|JD|DVM", ignore.case = TRUE)

  desc_125 <- get_education_description(125)
  expect_match(desc_125, "doctoral|phd", ignore.case = TRUE)
})


# ==============================================================================
# Test Suite 5: Validation Helpers
# ==============================================================================

test_that("validate_empstat_codes validates EMPSTAT vector", {
  # Checks that all codes are valid IPUMS EMPSTAT codes

  valid_codes <- c(10, 12, 20, 30)
  expect_true(validate_empstat_codes(valid_codes))

  # Should warn about invalid codes but not error
  expect_warning(
    validate_empstat_codes(c(10, 999)),
    regexp = "invalid|unknown"
  )
})

test_that("validate_education_codes validates EDUC vector", {
  # Checks that all codes are valid IPUMS EDUC codes we support

  valid_codes <- c(2, 73, 111, 125)
  expect_true(validate_education_codes(valid_codes))

  # Should warn about codes we don't map
  expect_warning(
    validate_education_codes(c(111, 999)),
    regexp = "unmapped|unknown"
  )
})
