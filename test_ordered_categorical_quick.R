#!/usr/bin/env Rscript
# Quick test of ordered categorical model with minimal iterations
# Tests monotonic education trends (ordered categorical) model

library(phdunemployment)
library(data.table)
library(cmdstanr)
library(targets)

cat("Ordered Categorical Model Quick Test\n")
cat("====================================\n\n")

# Load the data
cat("1. Loading data from targets pipeline...\n")
data_target <- tar_read('education_counts')
cat(sprintf("   Rows: %d\n", nrow(data_target)))
cat(sprintf("   Education levels: %s\n",
            paste(sort(unique(data_target$education)), collapse = ", ")))
cat("\n")

# Run quick fit with minimal iterations
cat("2. Fitting ordered categorical model with minimal iterations...\n")
cat("   Settings: 2 chains, 50 warmup + 50 sampling, 1 thread/chain\n")
cat("   Expected runtime: ~5-10 minutes\n\n")

start_time <- Sys.time()

tryCatch({
  quick_fit <- fit_ode_state_space_ordered_categorical(
    data_target,
    education_order = c("less_than_hs", "high_school", "some_college",
                       "bachelors", "masters", "professional", "phd"),
    chains = 2,
    iter_sampling = 50,
    iter_warmup = 50,
    adapt_delta = 0.99,  # More conservative sampling for ordered parameters
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

    # Check ordered parameters
    cat("   Checking ordered parameter extraction...\n")
    if (!is.null(quick_fit$fit)) {
      # Try to extract some ordered parameters
      ordered_vars <- c("theta_logit_u_eq_raw", "theta_log_adj_speed_raw",
                       "theta_log_sigma_spline_raw")
      available_vars <- quick_fit$fit$metadata()$stan_variables

      cat(sprintf("   Available variables: %d total\n", length(available_vars)))

      # Check if ordered parameters are in the output
      found_ordered <- sum(ordered_vars %in% available_vars)
      cat(sprintf("   Found %d/%d ordered parameter vectors\n", found_ordered, length(ordered_vars)))

      if (found_ordered > 0) {
        cat("   ✓ Ordered categorical parameters found in output\n")
      }
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
cat("4. Ordered categorical model structure:\n")
cat("   Parameters with ordered categorical trends:\n")
cat("   1. logit_u_eq (equilibrium unemployment)\n")
cat("   2. log_adj_speed (adjustment speed)\n")
cat("   3. log_shock_2008_effect (2008 shock magnitude)\n")
cat("   4. log_shock_2020_effect (2020 shock magnitude)\n")
cat("   5. decay_2008 (2008 recovery rate)\n")
cat("   6. decay_2020 (2020 recovery rate)\n")
cat("   7. log_sigma_spline (spline smoothness)\n")
cat("\n")
cat("   Model formula:\n")
cat("   parameter[i] = mu + sigma * theta[i] where theta is ordered\n")
cat("   theta[i] ~ std_normal() with ordered constraint\n")
cat("\n")

cat("5. Scientific questions addressed:\n")
cat("   - Does equilibrium unemployment decrease with education? (ordered constraint)\n")
cat("   - Do more educated workers recover faster from shocks? (ordered constraint)\n")
cat("   - Are more educated workers less sensitive to economic shocks? (ordered constraint)\n")
cat("   - Do adjustment speeds vary systematically with education? (ordered constraint)\n")
cat("\n")

cat("6. Comparison with other models:\n")
cat("   Edu-parallel: parameter[i] = mu + sigma * raw[i] (independent)\n")
cat("   Education-trend: Adds linear trend: mu + beta * edu_rank_scaled[i] + sigma * raw[i]\n")
cat("   Ordered categorical: Adds monotonic trend: mu + sigma * theta[i] (theta ordered)\n")
cat("\n")

cat("Test completed!\n")
cat("Next steps:\n")
cat("1. Run full model fitting (1500 warmup + 1500 sampling)\n")
cat("2. Compare with edu-parallel and education-trend models using LOO-CV\n")
cat("3. Extract and visualize ordered trends\n")
cat("4. Check if monotonic constraints improve predictive performance\n")