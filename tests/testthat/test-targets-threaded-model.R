# TDD Tests for Threaded Model in Targets Pipeline
#
# Tests verify that the threaded Stan model integrates properly with the
# targets workflow and produces expected outputs.
#
# Run with: testthat::test_file("tests/testthat/test-targets-threaded-model.R")

library(testthat)
library(targets)
library(data.table)

# Helper: Check if targets project exists
skip_if_no_targets <- function() {
  if (!file.exists(here::here("_targets.R"))) {
    skip("Targets pipeline not found")
  }
}

# ============================================================================
# Section 1: Target Definition Tests
# ============================================================================

test_that("_targets.R includes threaded model compilation target", {
  skip_if_no_targets()

  # Read targets file
  targets_file <- here::here("_targets.R")
  targets_code <- readLines(targets_file)

  # Check for threaded model compilation target
  expect_true(any(grepl("stan_model_compiled_threaded", targets_code)),
              info = "stan_model_compiled_threaded target not found in _targets.R")

  # Check it references the threaded Stan file
  expect_true(any(grepl("unemployment-ode-state-space-threaded\\.stan", targets_code)),
              info = "Threaded Stan file not referenced in compilation target")

  # Check it enables threading
  expect_true(any(grepl("stan_threads\\s*=\\s*TRUE", targets_code)),
              info = "Threading not enabled in compilation (stan_threads = TRUE missing)")
})

test_that("_targets.R includes threaded model fitting target", {
  skip_if_no_targets()

  targets_code <- readLines(here::here("_targets.R"))

  # Check for threaded model fitting target
  expect_true(any(grepl("model_ode_state_space_threaded", targets_code)),
              info = "model_ode_state_space_threaded target not found")

  # Check it uses threads_per_chain parameter
  has_threads_param <- any(grepl("threads_per_chain", targets_code))
  expect_true(has_threads_param,
              info = "threads_per_chain parameter not found in threaded model target")

  # Check it saves results
  expect_true(any(grepl("model_ode_state_space_threaded_file", targets_code)),
              info = "File output target for threaded model not found")
})

test_that("_targets.R includes performance comparison target", {
  skip_if_no_targets()

  targets_code <- readLines(here::here("_targets.R"))

  # Check for performance comparison target
  expect_true(any(grepl("performance_comparison", targets_code)),
              info = "performance_comparison target not found")

  # Check it references both serial and threaded models
  # Look for a target that uses both model outputs
  has_both <- any(grepl("model_ode_state_space_efficient", targets_code)) &&
              any(grepl("model_ode_state_space_threaded", targets_code))

  expect_true(has_both,
              info = "Performance comparison should reference both serial and threaded models")
})

# ============================================================================
# Section 2: Target Execution Tests
# ============================================================================

test_that("threaded model target validates successfully", {
  skip_if_no_targets()
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  # Load targets project
  tar_config_set(store = here::here("_targets"))

  # Validate the target network
  expect_no_error(tar_manifest(names = "stan_model_compiled_threaded"),
                 info = "stan_model_compiled_threaded target fails validation")

  expect_no_error(tar_manifest(names = "model_ode_state_space_threaded"),
                 info = "model_ode_state_space_threaded target fails validation")
})

test_that("threaded model can be built via targets", {
  skip_if_no_targets()
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  # This is a smoke test - just verify it doesn't error
  # Don't actually run (would take too long)
  tar_config_set(store = here::here("_targets"))

  # Check if target is defined
  manifest <- tar_manifest()
  expect_true("stan_model_compiled_threaded" %in% manifest$name,
              info = "Threaded compilation target not in manifest")

  expect_true("model_ode_state_space_threaded" %in% manifest$name,
              info = "Threaded fitting target not in manifest")
})

# ============================================================================
# Section 3: Output Structure Tests
# ============================================================================

test_that("threaded model output has expected structure", {
  skip_if_no_targets()
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  # Check if cached output exists (from previous run)
  threaded_file <- here::here("models", "ode-state-space-threaded-fit.qs")

  if (!file.exists(threaded_file)) {
    skip("Threaded model output not cached - run tar_make() first")
  }

  # Load cached result
  result <- qs::qread(threaded_file)

  # Check structure
  expect_true(is.list(result), info = "Result should be a list")

  expect_true("fit" %in% names(result),
              info = "Result should have 'fit' component")

  expect_true("stan_data" %in% names(result),
              info = "Result should have 'stan_data' component")

  expect_true("diagnostics" %in% names(result),
              info = "Result should have 'diagnostics' component")

  expect_true("timing" %in% names(result),
              info = "Result should have 'timing' component")

  # Check timing includes threading info
  expect_true("threads_per_chain" %in% names(result$timing),
              info = "Timing should include threads_per_chain")
})

test_that("performance comparison output has expected structure", {
  skip_if_no_targets()
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  # Check if cached output exists
  comparison_file <- here::here("results", "performance-comparison.rds")

  if (!file.exists(comparison_file)) {
    skip("Performance comparison not cached - run tar_make() first")
  }

  # Load comparison
  comparison <- readRDS(comparison_file)

  # Check structure
  expect_true(is.list(comparison) || is.data.frame(comparison),
              info = "Comparison should be list or data frame")

  # Should compare serial vs threaded
  if (is.data.frame(comparison)) {
    expect_true("model_type" %in% names(comparison),
                info = "Comparison should have model_type column")

    expect_true(any(grepl("serial|efficient", comparison$model_type, ignore.case = TRUE)),
                info = "Should include serial/efficient model")

    expect_true(any(grepl("threaded|parallel", comparison$model_type, ignore.case = TRUE)),
                info = "Should include threaded/parallel model")

    # Should have timing metrics
    expect_true(any(grepl("time|duration|elapsed", names(comparison), ignore.case = TRUE)),
                info = "Should include timing metrics")

    # Should have speedup calculation
    if ("speedup" %in% names(comparison)) {
      speedup_vals <- comparison$speedup[!is.na(comparison$speedup)]
      if (length(speedup_vals) > 0) {
        expect_true(all(speedup_vals > 0),
                   info = "Speedup values should be positive")
      }
    }
  }
})

# ============================================================================
# Section 4: Performance Tests
# ============================================================================

test_that("threaded model shows expected speedup characteristics", {
  skip_if_no_targets()
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "TRUE", "Skipping slow test")

  comparison_file <- here::here("results", "performance-comparison.rds")

  if (!file.exists(comparison_file)) {
    skip("Performance comparison not available")
  }

  comparison <- readRDS(comparison_file)

  # If we have speedup data, verify it's reasonable
  if (is.data.frame(comparison) && "speedup" %in% names(comparison)) {
    speedup <- comparison$speedup[!is.na(comparison$speedup)]

    if (length(speedup) > 0) {
      # Speedup should be positive and less than number of threads
      # (due to Amdahl's law and overhead)
      expect_true(all(speedup > 0.8),
                 info = "Speedup should be close to 1x at minimum")

      expect_true(all(speedup < 8),
                 info = "Speedup unlikely to exceed 8x with typical threading")

      # Log performance for inspection
      cat("\nPerformance metrics:\n")
      print(comparison)
    }
  }
})

# ============================================================================
# Section 5: Integration Tests
# ============================================================================

test_that("report can load threaded model results", {
  skip_if_no_targets()
  skip_on_cran()

  # Check that report file exists
  report_file <- here::here("reports", "state-space-comparison.qmd")
  expect_true(file.exists(report_file), info = "Report file not found")

  # Check report references threaded model
  report_content <- readLines(report_file)

  # Should mention threading somewhere
  has_threading_ref <- any(grepl("thread|parallel|reduce_sum",
                                 report_content, ignore.case = TRUE))

  expect_true(has_threading_ref,
              info = "Report should reference threading or parallelization")
})

test_that("R function exists to fit threaded model", {
  # Load the package or source the file
  devtools::load_all(here::here(), quiet = TRUE)

  # Check if wrapper function exists
  expect_true(exists("fit_ode_state_space_threaded"),
              info = "fit_ode_state_space_threaded() function should exist")

  # Check function signature
  if (exists("fit_ode_state_space_threaded")) {
    fn <- get("fit_ode_state_space_threaded")
    args <- names(formals(fn))

    # Should have threads_per_chain parameter
    expect_true("threads_per_chain" %in% args,
                info = "Function should have threads_per_chain parameter")

    # Should have grainsize parameter
    expect_true("grainsize" %in% args,
                info = "Function should have grainsize parameter")
  }
})
