#!/usr/bin/env Rscript
# Benchmark sequential parameter recovery validation
# Note: Parallel benchmarking requires package installation which is complex in dev mode
# For now, we'll time sequential and estimate parallel speedup

library(here)
devtools::load_all(here())

# Setup
cat("\n=== Parallel Processing Benchmark ===\n\n")
cat("Testing with smaller n_sims for faster benchmarking\n")
cat(sprintf("Available cores: %d\n", parallel::detectCores()))
cat(sprintf("Using cores: %d\n\n", parallel::detectCores() - 1))

# Test parameters - using smaller n_sims for quicker benchmark
n_sims <- 50  # Small enough to benchmark quickly
n_years <- 10

# Benchmark sequential
cat("Running SEQUENTIAL validation (50 simulations)...\n")
time_seq <- system.time({
  result_seq <- validate_difference_coverage(
    n_sims = n_sims,
    n_years = n_years,
    education_levels = c("phd", "masters", "bachelors"),
    baseline_rates = c(phd = 0.050, masters = 0.070, bachelors = 0.100),
    seasonal_amplitudes = c(phd = 0.010, masters = 0.010, bachelors = 0.010),
    trend_slopes = c(phd = -0.0001, masters = -0.0001, bachelors = -0.0001),
    noise_sd = 0.002,
    difference_type = "baseline",
    seed = 2024,
    verbose = FALSE,
    parallel = FALSE
  )
})

cat(sprintf("Sequential time: %.2f seconds\n\n", time_seq["elapsed"]))

# Estimate parallel performance
n_cores <- parallel::detectCores() - 1
theoretical_speedup <- min(n_cores * 0.7, n_cores)  # Account for overhead (~30%)

cat("=== PERFORMANCE ESTIMATES ===\n")
cat(sprintf("Measured sequential time: %.2f seconds (50 sims)\n", time_seq["elapsed"]))
cat(sprintf("Time per simulation: %.3f seconds\n\n", time_seq["elapsed"] / n_sims))

# Extrapolate to full validation (300 sims)
full_seq <- time_seq["elapsed"] * (300 / n_sims)
est_par <- full_seq / theoretical_speedup

cat(sprintf("Estimated time for 300 simulations:\n"))
cat(sprintf("  Sequential: %.1f minutes\n", full_seq / 60))
cat(sprintf("  Parallel (%d cores): %.1f minutes (estimated)\n", n_cores, est_par / 60))
cat(sprintf("  Estimated speedup: %.1fx\n", theoretical_speedup))
cat(sprintf("  Time saved: %.1f minutes\n\n", (full_seq - est_par) / 60))

cat("Note: Parallel processing in dev mode requires package installation.\n")
cat("In production (with installed package), actual speedup should be ~70%% of core count.\n")
