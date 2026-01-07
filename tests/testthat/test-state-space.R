# =============================================================================
# TDD Tests for ODE State Space Model
# =============================================================================

# =============================================================================
# Data Preparation Tests
# =============================================================================

test_that("prepare_stan_data creates valid Stan input structure", {
  # Load test data
  data_file <- here::here("data", "education-spectrum-counts.rds")
  skip_if_not(file.exists(data_file), "Count data file not found")
  counts_data <- readRDS(data_file)

  # Prepare data
  stan_data <- prepare_stan_data(counts_data)

  # Check required fields exist
  expect_true("T" %in% names(stan_data))
  expect_true("N_edu" %in% names(stan_data))
  expect_true("n_unemployed" %in% names(stan_data))
  expect_true("n_total" %in% names(stan_data))
  expect_true("month" %in% names(stan_data))
  expect_true("year_frac" %in% names(stan_data))

  # Check dimensions
  expect_equal(dim(stan_data$n_unemployed), c(stan_data$T, stan_data$N_edu))
  expect_equal(dim(stan_data$n_total), c(stan_data$T, stan_data$N_edu))
  expect_equal(length(stan_data$month), stan_data$T)
  expect_equal(length(stan_data$year_frac), stan_data$T)

  # Check types
  expect_type(stan_data$n_unemployed, "integer")
  expect_type(stan_data$n_total, "integer")
  expect_type(stan_data$month, "integer")
  expect_type(stan_data$year_frac, "double")

  # Check value constraints
  expect_true(all(stan_data$month >= 1 & stan_data$month <= 12))
  expect_true(all(stan_data$n_unemployed >= 0))
  expect_true(all(stan_data$n_total >= stan_data$n_unemployed))
})


test_that("prepare_stan_data includes shock timing parameters", {
  data_file <- here::here("data", "education-spectrum-counts.rds")
  skip_if_not(file.exists(data_file), "Count data file not found")
  counts_data <- readRDS(data_file)

  stan_data <- prepare_stan_data(counts_data)

  # Check shock timing parameters exist
  expect_true("shock_2008_onset" %in% names(stan_data))
  expect_true("shock_2008_peak" %in% names(stan_data))
  expect_true("shock_2020_onset" %in% names(stan_data))
  expect_true("shock_2020_peak" %in% names(stan_data))

  # Check reasonable values
  expect_gt(stan_data$shock_2008_onset, 2007)
  expect_lt(stan_data$shock_2008_onset, 2008)
  expect_gt(stan_data$shock_2008_peak, 2009)
  expect_lt(stan_data$shock_2008_peak, 2010)

  expect_gt(stan_data$shock_2020_onset, 2020)
  expect_lt(stan_data$shock_2020_onset, 2020.5)
  expect_gt(stan_data$shock_2020_peak, 2020.2)
  expect_lt(stan_data$shock_2020_peak, 2020.5)
})


test_that("prepare_stan_data preserves education level metadata", {
  data_file <- here::here("data", "education-spectrum-counts.rds")
  skip_if_not(file.exists(data_file), "Count data file not found")
  counts_data <- readRDS(data_file)

  stan_data <- prepare_stan_data(counts_data)

  # Check metadata exists
  expect_true("education_levels" %in% names(stan_data))
  expect_true("time_points" %in% names(stan_data))

  # Check education levels match data
  expect_equal(length(stan_data$education_levels), stan_data$N_edu)
  expect_true("phd" %in% stan_data$education_levels)
  expect_true("bachelors" %in% stan_data$education_levels)

  # Check time points
  expect_equal(length(stan_data$time_points), stan_data$T)
})


# =============================================================================
# Stan Model Compilation Tests
# =============================================================================

test_that("Stan model file exists and has correct structure", {
  stan_file <- here::here("stan", "unemployment-ode-state-space.stan")

  # File should exist
  expect_true(file.exists(stan_file))

  # Read file content
  stan_code <- readLines(stan_file)
  stan_text <- paste(stan_code, collapse = "\n")

  # Check for required blocks
  expect_true(grepl("functions\\s*\\{", stan_text))
  expect_true(grepl("data\\s*\\{", stan_text))
  expect_true(grepl("parameters\\s*\\{", stan_text))
  expect_true(grepl("transformed parameters\\s*\\{", stan_text))
  expect_true(grepl("model\\s*\\{", stan_text))
  expect_true(grepl("generated quantities\\s*\\{", stan_text))

  # Check for key parameters
  expect_true(grepl("separation_rate", stan_text))
  expect_true(grepl("finding_rate", stan_text))
  expect_true(grepl("shock_2008_effect", stan_text))
  expect_true(grepl("shock_2020_effect", stan_text))
  expect_true(grepl("u_equilibrium", stan_text))
  expect_true(grepl("log_lik", stan_text))
})


test_that("Stan model compiles successfully", {
  skip_if_not(requireNamespace("cmdstanr", quietly = TRUE),
              "cmdstanr not available")

  stan_file <- here::here("stan", "unemployment-ode-state-space.stan")
  skip_if_not(file.exists(stan_file), "Stan file not found")

  # Attempt to compile - should not error
  expect_no_error({
    model <- cmdstanr::cmdstan_model(stan_file, compile = TRUE)
  })
})


# =============================================================================
# Model Fitting Tests (require Stan installation)
# =============================================================================

test_that("fit_ode_state_space runs on minimal data", {
  skip("Long-running Stan model - run manually with RUN_STAN_TESTS=true")
  skip_if_not(Sys.getenv("RUN_STAN_TESTS") == "true",
              "Stan tests disabled (set RUN_STAN_TESTS=true)")

  data_file <- here::here("data", "education-spectrum-counts.rds")
  skip_if_not(file.exists(data_file), "Count data file not found")
  counts_data <- readRDS(data_file)

  # Use subset for faster testing
  subset_data <- counts_data[counts_data$year >= 2020, ]

  # Fit with minimal iterations
  result <- fit_ode_state_space(
    subset_data,
    chains = 2,
    iter_sampling = 100,
    iter_warmup = 100,
    parallel_chains = 2
  )

  # Check return structure
  expect_type(result, "list")
  expect_true("fit" %in% names(result))
  expect_true("stan_data" %in% names(result))
  expect_true("diagnostics" %in% names(result))

  # Check fit object
  expect_s3_class(result$fit, "CmdStanMCMC")
})


# =============================================================================
# Parameter Extraction Tests
# =============================================================================

test_that("extract_economic_params returns expected structure", {
  skip("Requires fitted model - run manually")

  # This test documents expected output structure
  # Once model is fitted, verify:
  # - separation_rates has one row per education level
  # - finding_rates has one row per education level
  # - shock effects have education-specific estimates
  # - equilibrium_rates are in valid range [0, 1]
  # - shock_halflives are positive and reasonable
})


# =============================================================================
# Economic Interpretation Tests
# =============================================================================

test_that("equilibrium unemployment rates are economically reasonable",
          {
            skip("Requires fitted model - run manually")

            # Expected ranges based on labor economics literature:
            # - PhD: 1-3% equilibrium unemployment
            # - Less than HS: 5-10% equilibrium unemployment
            # - Separation rates: 1-4% per month
            # - Finding rates: 15-50% per month
          })


test_that("shock effects are positive and significant", {
  skip("Requires fitted model - run manually")

  # Key success criterion:
  # 2008 and 2020 shock effects should be:
  # - Positive (shocks increase unemployment)
  # - 95% CI excludes zero
  # - Larger for less educated groups (historical pattern)
})


# =============================================================================
# Comparison with GAM Tests
# =============================================================================

test_that("state space model captures shock effects that GAM misses", {
  skip("Requires both models fitted - run in comparison report")

  # Key comparison:
  # - GAM shock effects penalized to ~0

  # - State space shock effects positive with uncertainty
  # - State space provides interpretable economic parameters
})
