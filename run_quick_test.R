#!/usr/bin/env Rscript
# Quick test of education-trend model with minimal iterations
# Verifies the complete pipeline works before full refitting

library(phdunemployment)
library(data.table)
library(cmdstanr)
library(loo)
library(targets)

cat("Education-Trend Model Quick Test with LOO-CV Verification\n")
cat("=========================================================\n\n")

# Load the data
cat("1. Loading data from targets pipeline...\n")
data_target <- tar_read('education_counts')
cat(sprintf("   Rows: %d\n", nrow(data_target)))
cat(sprintf("   Education levels: %s\n",
            paste(sort(unique(data_target$education)), collapse = ", ")))
cat("\n")

# Run quick fit with minimal iterations
cat("2. Fitting education-trend model with minimal iterations...\n")
cat("   Settings: 2 chains, 50 warmup + 50 sampling, 1 thread/chain\n")
cat("   Expected runtime: ~5-10 minutes\n\n")

start_time <- Sys.time()

quick_fit <- fit_ode_state_space_education_trend(
  data_target,
  education_order = c("less_than_hs", "high_school", "some_college",
                      "bachelors", "masters", "professional", "phd"),  # Proper ordering from least to most educated
  chains = 2,
  iter_sampling = 50,      # Reduced for quicker test
  iter_warmup = 50,        # Reduced for quicker test
  adapt_delta = 0.99,      # More conservative sampling
  max_treedepth = 12,
  parallel_chains = 1,
  threads_per_chain = 1,
  grainsize = 1L,
  K_spline = 25L,
  refresh = 10
)

end_time <- Sys.time()
elapsed <- as.numeric(difftime(end_time, start_time, units = "mins"))

cat(sprintf("   Model fitting complete (%.1f minutes)\n", elapsed))
cat("\n")

# Verify log_lik extraction
cat("3. Verifying log_lik extraction...\n")
if (!is.null(quick_fit$log_lik)) {
  cat("   ✓ log_lik matrix exists\n")
  cat(sprintf("   Dimensions: %d rows × %d columns\n",
              nrow(quick_fit$log_lik),
              ncol(quick_fit$log_lik)))
  cat(sprintf("   Rows = iterations (%d chains × %d samples = %d)\n",
              2, 50, nrow(quick_fit$log_lik)))
  cat(sprintf("   Columns = observations (%d)\n", ncol(quick_fit$log_lik)))
} else {
  cat("   ✗ ERROR: log_lik matrix NOT found\n")
  stop("log_lik extraction failed")
}
cat("\n")

# Test LOO-CV computation
cat("4. Testing LOO-CV computation...\n")
tryCatch({
  quick_loo <- compute_loo(quick_fit)
  cat("   ✓ LOO-CV computation successful\n")
  cat("   Estimates:\n")
  print(quick_loo$estimates)

  # Check for problematic observations
  if (!is.null(quick_loo$diagnostics$pareto_k)) {
    n_problematic <- sum(quick_loo$diagnostics$pareto_k > 0.7, na.rm = TRUE)
    cat(sprintf("   Problematic observations (Pareto k > 0.7): %d\n", n_problematic))
  }

}, error = function(e) {
  cat(sprintf("   ✗ ERROR in LOO-CV computation: %s\n", e$message))
})
cat("\n")

# Check diagnostics
cat("5. Checking model diagnostics...\n")
if (!is.null(quick_fit$diagnostics)) {
  cat(sprintf("   Divergent transitions: %d\n", quick_fit$diagnostics$num_divergent))
  cat(sprintf("   Max treedepth exceeded: %d\n", quick_fit$diagnostics$max_treedepth_exceeded))
  cat(sprintf("   E-BFMI: %s\n", paste(round(quick_fit$diagnostics$ebfmi, 3), collapse = ", ")))

  if (quick_fit$diagnostics$num_divergent > 0) {
    cat("   ⚠️  WARNING: Divergent transitions detected\n")
  }
} else {
  cat("   ✗ Diagnostics not available\n")
}
cat("\n")

# Save quick test results
cat("6. Saving quick test results...\n")
output_dir <- here::here("models", "quick-test-results")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

output_file <- file.path(output_dir, "education-trend-quick-test.qs")
qs::qsave(quick_fit, output_file)
cat(sprintf("   Saved to: %s\n", output_file))
cat(sprintf("   File size: %.1f KB\n", file.info(output_file)$size / 1024))
cat("\n")

# Summary and recommendations
cat("7. Test Summary\n")
cat("   -------------\n")
if (!is.null(quick_fit$log_lik) && exists("quick_loo")) {
  cat("   ✅ SUCCESS: Pipeline verification complete\n")
  cat("   - log_lik extraction works\n")
  cat("   - LOO-CV computation works\n")
  cat("   - Model fitting completes without errors\n")
  cat("\n")
  cat("   Next steps:\n")
  cat("   1. Run full edu-parallel model refitting\n")
  cat("   2. Run full education-trend model refitting\n")
  cat("   3. Perform LOO-CV comparison\n")
  cat("   4. Extract and visualize education trends\n")
} else {
  cat("   ❌ FAILURE: Pipeline issues detected\n")
  cat("   Check errors above and fix before full refitting\n")
}
cat("\n")

cat("Quick test completed!\n")