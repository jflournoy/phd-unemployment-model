#!/usr/bin/env Rscript
# Test LOO extraction from saved model

library(targets)
library(cmdstanr)
library(loo)

# Load the education-trend model from targets store
if (requireNamespace("targets", quietly = TRUE)) {
  cat("Loading education-trend model from targets store...\n")
  model <- tar_read(model_ode_state_space_education_trend)

  cat("Model structure:\n")
  print(names(model))

  if (!is.null(model$fit)) {
    cat("\nFit object class:", class(model$fit), "\n")

    # Try to get output files
    cat("\nOutput files:\n")
    tryCatch({
      files <- model$fit$output_files()
      print(files)

      # Check if files exist
      for (f in files) {
        cat("File exists:", file.exists(f), " - ", f, "\n")
      }
    }, error = function(e) {
      cat("Error getting output files:", e$message, "\n")
    })

    # Try to extract log-likelihood
    cat("\nAttempting to extract log_lik...\n")
    tryCatch({
      log_lik <- model$fit$draws(variables = "log_lik", format = "matrix")
      cat("Success! log_lik dimensions:", dim(log_lik), "\n")

      # Compute LOO
      loo_result <- loo::loo(log_lik, cores = 4)
      cat("\nLOO computed successfully:\n")
      print(loo_result)
    }, error = function(e) {
      cat("Error extracting log_lik:", e$message, "\n")
    })
  }
}