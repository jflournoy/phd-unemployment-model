#!/usr/bin/env Rscript
# Test if we can extract any parameter from saved fit

library(targets)
library(cmdstanr)

cat("Loading education-trend model from targets store...\n")
model <- tar_read(model_ode_state_space_education_trend)

if (!is.null(model$fit)) {
  cat("Fit object class:", class(model$fit), "\n")

  # Try to get output files
  cat("\nOutput files:\n")
  tryCatch({
    files <- model$fit$output_files()
    cat("Number of output files:", length(files), "\n")
    if (length(files) > 0) {
      cat("First file:", files[1], "\n")
      cat("Exists:", file.exists(files[1]), "\n")
    }
  }, error = function(e) {
    cat("Error getting output files:", e$message, "\n")
  })

  # Try to extract a small parameter
  cat("\nAttempting to extract mu_logit_u_eq...\n")
  tryCatch({
    draws <- model$fit$draws(variables = "mu_logit_u_eq", format = "matrix")
    cat("Success! Dimensions:", dim(draws), "\n")
    cat("Mean:", mean(draws), "\n")
  }, error = function(e) {
    cat("Error extracting parameter:", e$message, "\n")
  })

  # Check if result has log_lik field
  cat("\nResult has log_lik field:", !is.null(model$log_lik), "\n")
}