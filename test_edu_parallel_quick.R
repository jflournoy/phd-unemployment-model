#!/usr/bin/env Rscript
# Quick test of edu-parallel model with minimal iterations
# Tests the simpler baseline model before education-trend model

library(phdunemployment)
library(data.table)
library(cmdstanr)
library(targets)

cat("Edu-Parallel Model Quick Test\n")
cat("=============================\n\n")

# Load the data
cat("1. Loading data from targets pipeline...\n")
data_target <- tar_read('education_counts')
cat(sprintf("   Rows: %d\n", nrow(data_target)))
cat(sprintf("   Education levels: %s\n",
            paste(sort(unique(data_target$education)), collapse = ", ")))
cat("\n")

# Run quick fit with minimal iterations
cat("2. Fitting edu-parallel model with minimal iterations...\n")
cat("   Settings: 2 chains, 50 warmup + 50 sampling, 1 thread/chain\n")
cat("   Expected runtime: ~5-10 minutes\n\n")

start_time <- Sys.time()

tryCatch({
  quick_fit <- fit_ode_state_space_edu_parallel(
    data_target,
    chains = 2,
    iter_sampling = 50,
    iter_warmup = 50,
    adapt_delta = 0.99,  # More conservative sampling
    max_treedepth = 12,
    parallel_chains = 1,
    threads_per_chain = 1,
    grainsize = 1L,
    refresh = 10
  )

  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "mins"))

  cat(sprintf("   Model fitting complete (%.1f minutes)\n", elapsed))
  cat("\n")

  # Check if fit succeeded
  cat("3. Checking model fit...\n")
  if (!is.null(quick_fit$fit)) {
    cat("   ✓ Stan fit object exists\n")

    # Check for warnings/errors
    cat("   Checking for warnings...\n")
    if (any(grepl("NaN", quick_fit$fit$output()))) {
      cat("   ⚠️  WARNING: NaN values detected in output\n")
    } else {
      cat("   ✓ No NaN warnings detected\n")
    }

    # Check diagnostics
    cat("   Checking diagnostics...\n")
    if (!is.null(quick_fit$diagnostics)) {
      cat(sprintf("   Divergent transitions: %d\n", quick_fit$diagnostics$num_divergent))
      cat(sprintf("   Max treedepth exceeded: %d\n", quick_fit$diagnostics$max_treedepth_exceeded))

      if (quick_fit$diagnostics$num_divergent > 0) {
        cat("   ⚠️  WARNING: Divergent transitions detected\n")
      }
    }

    # Check log_lik
    cat("   Checking log_lik extraction...\n")
    if (!is.null(quick_fit$log_lik)) {
      cat("   ✓ log_lik matrix exists\n")
      cat(sprintf("   Dimensions: %d rows × %d columns\n",
                  nrow(quick_fit$log_lik),
                  ncol(quick_fit$log_lik)))
    } else {
      cat("   ✗ log_lik matrix NOT found\n")
    }

  } else {
    cat("   ✗ ERROR: Stan fit object is NULL\n")
  }

}, error = function(e) {
  cat(sprintf("   ✗ ERROR in model fitting: %s\n", e$message))
  cat("\n   Full error trace:\n")
  print(e)
})

cat("\n")
cat("Test completed!\n")