#!/usr/bin/env Rscript
# Analyze existing model results

library(phdunemployment)
library(data.table)
library(targets)

cat("Analyzing existing model results\n")
cat("================================\n\n")

# Try to load models from targets cache
cat("1. Loading models from targets cache...\n")

models_to_check <- c(
  'model_ode_state_space_edu_parallel',
  'model_ode_state_space_education_trend',
  'model_ode_state_space_efficient'
)

for (model_name in models_to_check) {
  cat(sprintf("\n  %s:\n", model_name))

  tryCatch({
    model <- tar_read(model_name)

    # Basic info
    cat(sprintf("    - Loaded successfully\n"))
    cat(sprintf("    - Class: %s\n", paste(class(model), collapse = ", ")))

    # Check for log_lik
    has_log_lik <- !is.null(model$log_lik)
    cat(sprintf("    - Has log_lik: %s\n", has_log_lik))

    if (has_log_lik) {
      cat(sprintf("    - log_lik dimensions: %d x %d\n",
                  nrow(model$log_lik), ncol(model$log_lik)))
    }

    # Check for fit object
    has_fit <- !is.null(model$fit)
    cat(sprintf("    - Has fit object: %s\n", has_fit))

    if (has_fit) {
      cat(sprintf("    - Fit class: %s\n", class(model$fit)))

      # Try to extract log_lik from fit if not already stored
      if (!has_log_lik) {
        cat("    - Attempting to extract log_lik from fit object...\n")
        tryCatch({
          log_lik_from_fit <- model$fit$draws(variables = "log_lik", format = "matrix")
          cat(sprintf("      Success! Dimensions: %d x %d\n",
                      nrow(log_lik_from_fit), ncol(log_lik_from_fit)))
          # Store it back
          model$log_lik <- log_lik_from_fit
          has_log_lik <- TRUE
        }, error = function(e) {
          cat(sprintf("      Failed: %s\n", e$message))
        })
      }
    }

    # Check diagnostics
    has_diagnostics <- !is.null(model$diagnostics)
    cat(sprintf("    - Has diagnostics: %s\n", has_diagnostics))

    if (has_diagnostics) {
      cat(sprintf("    - Divergent transitions: %d\n",
                  model$diagnostics$num_divergent))
    }

    # Check timing
    has_timing <- !is.null(model$timing)
    cat(sprintf("    - Has timing: %s\n", has_timing))

    if (has_timing) {
      cat(sprintf("    - Elapsed minutes: %.1f\n",
                  model$timing$elapsed_mins))
    }

  }, error = function(e) {
    cat(sprintf("    - Error loading: %s\n", e$message))
  })
}

# Check for saved model files
cat("\n\n2. Checking for saved model files...\n")
model_files <- c(
  'models/ode-state-space-edu-parallel-fit.qs',
  'models/ode-state-space-education-trend-fit.qs',
  'models/ode-state-space-efficient-fit.qs'
)

for (file in model_files) {
  cat(sprintf("\n  %s:\n", file))

  if (file.exists(file)) {
    cat("    - File exists\n")

    # Try to load it
    tryCatch({
      if (grepl("\\.qs$", file)) {
        model <- qs::qread(file)
        cat("    - Loaded successfully via qs\n")

        # Check for log_lik
        has_log_lik <- !is.null(model$log_lik)
        cat(sprintf("    - Has log_lik: %s\n", has_log_lik))

        if (has_log_lik) {
          cat(sprintf("    - log_lik dimensions: %d x %d\n",
                      nrow(model$log_lik), ncol(model$log_lik)))
        }
      } else if (grepl("\\.rds$", file)) {
        model <- readRDS(file)
        cat("    - Loaded successfully via readRDS\n")

        # Check for log_lik
        has_log_lik <- !is.null(model$log_lik)
        cat(sprintf("    - Has log_lik: %s\n", has_log_lik))
      }
    }, error = function(e) {
      cat(sprintf("    - Error loading: %s\n", e$message))
    })
  } else {
    cat("    - File does not exist\n")
  }
}

# Check Stan output directories
cat("\n\n3. Checking Stan output directories...\n")
output_dirs <- list.dirs("models/stan-output", recursive = FALSE, full.names = FALSE)

if (length(output_dirs) > 0) {
  cat(sprintf("  Found %d output directories:\n", length(output_dirs)))
  for (dir in output_dirs) {
    cat(sprintf("    - %s\n", dir))

    # Count CSV files
    csv_files <- list.files(file.path("models/stan-output", dir),
                           pattern = "\\.csv$", full.names = TRUE)
    cat(sprintf("      CSV files: %d\n", length(csv_files)))

    if (length(csv_files) > 0) {
      # Check file sizes
      file_sizes <- file.size(csv_files)
      cat(sprintf("      Total size: %.1f MB\n", sum(file_sizes) / 1e6))
    }
  }
} else {
  cat("  No output directories found\n")
}

cat("\n\n4. Recommendations:\n")
cat("   - If models don't have log_lik, they need to be refitted\n")
cat("   - Refitting can take several hours per model\n")
cat("   - Consider running models with fewer iterations for testing\n")
cat("   - Check if log_lik can be computed from existing output files\n")