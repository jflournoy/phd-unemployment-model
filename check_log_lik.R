#!/usr/bin/env Rscript
# Check if existing model fits have log_lik matrices

library(qs)

cat("Checking existing model fits for log_lik matrices...\n")
cat("===================================================\n\n")

# Check education-trend model
edu_trend_file <- "models/ode-state-space-education-trend-fit.qs"
if (file.exists(edu_trend_file)) {
  cat("1. Education-trend model:", edu_trend_file, "\n")
  file_size <- file.info(edu_trend_file)$size
  cat(sprintf("   File size: %.1f KB\n", file_size / 1024))

  tryCatch({
    edu_trend_fit <- qs::qread(edu_trend_file)
    cat("   Model loaded successfully\n")

    # Check for log_lik
    if (!is.null(edu_trend_fit$log_lik)) {
      cat("   ✓ log_lik matrix exists\n")
      cat(sprintf("   Dimensions: %d x %d\n",
                  nrow(edu_trend_fit$log_lik),
                  ncol(edu_trend_fit$log_lik)))
    } else {
      cat("   ✗ log_lik matrix NOT found\n")
    }

    # Check other components
    if (!is.null(edu_trend_fit$fit)) {
      cat("   ✓ Stan fit object exists\n")
    }
    if (!is.null(edu_trend_fit$diagnostics)) {
      cat("   ✓ Diagnostics exist\n")
    }

  }, error = function(e) {
    cat(sprintf("   Error loading file: %s\n", e$message))
  })
} else {
  cat("1. Education-trend model file not found:", edu_trend_file, "\n")
}
cat("\n")

# Check edu-parallel model
edu_parallel_file <- "models/ode-state-space-edu-parallel-fit.qs"
if (file.exists(edu_parallel_file)) {
  cat("2. Edu-parallel model:", edu_parallel_file, "\n")
  file_size <- file.info(edu_parallel_file)$size
  cat(sprintf("   File size: %.1f KB\n", file_size / 1024))

  tryCatch({
    edu_parallel_fit <- qs::qread(edu_parallel_file)
    cat("   Model loaded successfully\n")

    # Check for log_lik
    if (!is.null(edu_parallel_fit$log_lik)) {
      cat("   ✓ log_lik matrix exists\n")
      cat(sprintf("   Dimensions: %d x %d\n",
                  nrow(edu_parallel_fit$log_lik),
                  ncol(edu_parallel_fit$log_lik)))
    } else {
      cat("   ✗ log_lik matrix NOT found\n")
    }

    # Check other components
    if (!is.null(edu_parallel_fit$fit)) {
      cat("   ✓ Stan fit object exists\n")
    }
    if (!is.null(edu_parallel_fit$diagnostics)) {
      cat("   ✓ Diagnostics exist\n")
    }

  }, error = function(e) {
    cat(sprintf("   Error loading file: %s\n", e$message))
  })
} else {
  cat("2. Edu-parallel model file not found:", edu_parallel_file, "\n")
}
cat("\n")

# Check efficient model (for comparison)
efficient_file <- "models/ode-state-space-efficient-fit.qs"
if (file.exists(efficient_file)) {
  cat("3. Efficient model (for comparison):", efficient_file, "\n")
  file_size <- file.info(efficient_file)$size
  cat(sprintf("   File size: %.1f MB\n", file_size / (1024 * 1024)))

  tryCatch({
    efficient_fit <- qs::qread(efficient_file)
    cat("   Model loaded successfully\n")

    # Check for log_lik
    if (!is.null(efficient_fit$log_lik)) {
      cat("   ✓ log_lik matrix exists\n")
      cat(sprintf("   Dimensions: %d x %d\n",
                  nrow(efficient_fit$log_lik),
                  ncol(efficient_fit$log_lik)))
    } else {
      cat("   ✗ log_lik matrix NOT found\n")
    }

  }, error = function(e) {
    cat(sprintf("   Error loading file: %s\n", e$message))
  })
} else {
  cat("3. Efficient model file not found:", efficient_file, "\n")
}
cat("\n")

cat("Summary:\n")
cat("--------\n")
cat("For LOO-CV analysis, we need log_lik matrices in both edu-parallel and education-trend models.\n")
cat("If log_lik is missing, models need to be refitted with updated functions.\n")