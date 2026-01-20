test_that("profile_model_computation returns proper structure", {
  skip_if_not_installed("cmdstanr")

  # Load test data
  data(unemployment_test, package = "phd.unemployment")

  stan_data <- prepare_stan_data(
    unemployment_test,
    education_order = c("PhD", "Professional", "Masters", "Bachelors",
                       "Some College", "High School", "Less than HS")
  )

  # Create a minimal profile with very short iterations for speed
  profile_result <- profile_model_computation(
    stan_data = stan_data,
    chains = 1,
    iter_sampling = 10,
    iter_warmup = 10,
    verbose = FALSE
  )

  # Check structure
  expect_is(profile_result, "list")
  expect_named(profile_result, c("timing_breakdown", "raw_timings", "summary"))

  # Check timing_breakdown
  expect_is(profile_result$timing_breakdown, "data.table")
  expect_named(profile_result$timing_breakdown,
               c("component", "time_secs", "pct_total"))
  expect_true(all(c("ode_computation", "likelihood_evaluation", "other") %in%
                  profile_result$timing_breakdown$component))

  # Check that percentages sum to ~100%
  total_pct <- sum(profile_result$timing_breakdown$pct_total)
  expect_true(abs(total_pct - 100) < 1)

  # Check raw_timings structure
  expect_is(profile_result$raw_timings, "list")
  expect_true("total_time" %in% names(profile_result$raw_timings))
  expect_true("ode_time" %in% names(profile_result$raw_timings))
  expect_true("likelihood_time" %in% names(profile_result$raw_timings))

  # Check summary has key statistics
  expect_is(profile_result$summary, "character")
  expect_true(any(grepl("ODE", profile_result$summary)))
  expect_true(any(grepl("Likelihood", profile_result$summary)))
})

test_that("profile_model_computation accuracy with known ratio", {
  skip_if_not_installed("cmdstanr")

  data(unemployment_test, package = "phd.unemployment")

  stan_data <- prepare_stan_data(
    unemployment_test,
    education_order = c("PhD", "Professional", "Masters", "Bachelors",
                       "Some College", "High School", "Less than HS")
  )

  profile_result <- profile_model_computation(
    stan_data = stan_data,
    chains = 1,
    iter_sampling = 5,
    iter_warmup = 5,
    verbose = FALSE
  )

  # ODE and likelihood times should be positive
  expect_true(profile_result$raw_timings$ode_time > 0)
  expect_true(profile_result$raw_timings$likelihood_time > 0)

  # Sum should roughly equal total (allowing for overhead)
  component_sum <- profile_result$raw_timings$ode_time +
                   profile_result$raw_timings$likelihood_time
  total_time <- profile_result$raw_timings$total_time

  # Allow 20% overhead for other operations
  expect_true(component_sum > total_time * 0.8)
  expect_true(component_sum < total_time * 1.2)
})

test_that("get_profiling_summary returns readable output", {
  skip_if_not_installed("cmdstanr")

  data(unemployment_test, package = "phd.unemployment")

  stan_data <- prepare_stan_data(
    unemployment_test,
    education_order = c("PhD", "Professional", "Masters", "Bachelors",
                       "Some College", "High School", "Less than HS")
  )

  profile_result <- profile_model_computation(
    stan_data = stan_data,
    chains = 1,
    iter_sampling = 5,
    iter_warmup = 5,
    verbose = FALSE
  )

  summary_text <- get_profiling_summary(profile_result)

  # Check that summary is readable
  expect_is(summary_text, "character")
  expect_true(nchar(summary_text) > 100)
  expect_true(any(grepl("bottleneck", summary_text, ignore.case = TRUE)))
  expect_true(any(grepl("%", summary_text)))
})
