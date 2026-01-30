test_that("run-profiling script exists and is executable", {
  script_path <- here::here("scripts", "run-model-profiling.R")
  expect_true(file.exists(script_path),
              info = "Script should exist at scripts/run-model-profiling.R")
})

test_that("run-profiling script can be sourced without errors", {
  skip_if_not_installed("cmdstanr")
  skip_if(!cmdstanr::cmdstan_version() >= "2.28.0", "CmdStan not available")

  script_path <- here::here("scripts", "run-model-profiling.R")
  skip_if(!file.exists(script_path), "Profiling script doesn't exist yet")

  # Source script should not produce errors
  expect_no_error(
    source(script_path, local = TRUE)
  )
})

test_that("run_model_profiling function produces valid output", {
  skip_if_not_installed("cmdstanr")
  skip_if(!cmdstanr::cmdstan_version() >= "2.28.0", "CmdStan not available")

  script_path <- here::here("scripts", "run-model-profiling.R")
  skip_if(!file.exists(script_path), "Profiling script doesn't exist yet")

  # Source the script
  source(script_path, local = TRUE)

  # Check that function exists
  expect_true(exists("run_model_profiling"),
              info = "Script should define run_model_profiling function")

  # Create minimal test data
  test_data <- data.table::data.table(
    time_index = rep(1:12, 2),
    month = rep(1:12, 2),
    year = rep(2020, 24),
    education = rep(c("PhD", "High School"), each = 12),
    n_unemployed = sample(10:20, 24, replace = TRUE),
    n_employed = rep(1000, 24)
  )

  # Run profiling with minimal iterations for speed
  result <- run_model_profiling(
    data = test_data,
    chains = 1,
    iter_sampling = 10,
    iter_warmup = 10,
    verbose = FALSE
  )

  # Check output structure
  expect_type(result, "list")
  expect_true("profile" %in% names(result),
              info = "Result should contain profiling data")
  expect_true("summary" %in% names(result),
              info = "Result should contain summary")

  # Check that summary is readable text
  expect_type(result$summary, "character")
  expect_true(nchar(result$summary) > 50)
})

test_that("run_model_profiling handles data loading from file", {
  skip_if_not_installed("cmdstanr")
  skip_if(!cmdstanr::cmdstan_version() >= "2.28.0", "CmdStan not available")

  script_path <- here::here("scripts", "run-model-profiling.R")
  skip_if(!file.exists(script_path), "Profiling script doesn't exist yet")

  # Check if data file exists
  data_file <- here::here("data", "education-spectrum-counts.rds")
  skip_if(!file.exists(data_file), "Data file not available")

  source(script_path, local = TRUE)

  # Should accept file path as input
  result <- run_model_profiling(
    data = data_file,
    chains = 1,
    iter_sampling = 5,
    iter_warmup = 5,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_true("profile" %in% names(result))
})

test_that("script produces timing breakdown table", {
  skip_if_not_installed("cmdstanr")
  skip_if(!cmdstanr::cmdstan_version() >= "2.28.0", "CmdStan not available")

  script_path <- here::here("scripts", "run-model-profiling.R")
  skip_if(!file.exists(script_path), "Profiling script doesn't exist yet")

  source(script_path, local = TRUE)

  # Create minimal test data
  test_data <- data.table::data.table(
    time_index = rep(1:6, 2),
    month = rep(1:6, 2),
    year = rep(2020, 12),
    education = rep(c("PhD", "Bachelor"), each = 6),
    n_unemployed = sample(5:15, 12, replace = TRUE),
    n_employed = rep(500, 12)
  )

  result <- run_model_profiling(
    data = test_data,
    chains = 1,
    iter_sampling = 5,
    iter_warmup = 5,
    verbose = FALSE
  )

  # Check timing breakdown exists
  expect_true("profile" %in% names(result))
  expect_true("timing_breakdown" %in% names(result$profile))

  breakdown <- result$profile$timing_breakdown
  expect_s3_class(breakdown, "data.table")

  # Should have ODE, likelihood, and other components
  expect_true(all(c("ode_computation", "likelihood_evaluation", "other") %in%
                  breakdown$component))

  # Should have time and percentage columns
  expect_true("time_secs" %in% names(breakdown))
  expect_true("pct_total" %in% names(breakdown))
})
