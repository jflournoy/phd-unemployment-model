#!/usr/bin/env Rscript

cat("Running full test suite for phdunemployment...\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Set user library path first
user_lib <- "/home/rstudio/R/x86_64-pc-linux-gnu-library/4.3"
.libPaths(c(user_lib, .libPaths()))

cat("Library paths:\n")
print(.libPaths())

# Load devtools
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools", lib = user_lib, repos = "https://cloud.r-project.org")
}

# Load testthat
if (!requireNamespace("testthat", quietly = TRUE)) {
  install.packages("testthat", lib = user_lib, repos = "https://cloud.r-project.org")
}

cat("\nLoading package...\n")
tryCatch({
  devtools::load_all()
  cat("✅ Package loaded successfully\n")
}, error = function(e) {
  cat("❌ Failed to load package:", conditionMessage(e), "\n")
  quit(status = 1)
})

# Run tests
cat("\n", strrep("=", 60), "\n")
cat("Starting test suite...\n")
cat(strrep("=", 60), "\n\n")

start_time <- Sys.time()
test_results <- devtools::test(reporter = "summary")
end_time <- Sys.time()

cat("\n", strrep("=", 60), "\n")
cat("Test suite completed in", round(difftime(end_time, start_time, units = "secs"), 1), "seconds\n")

# Summary
cat("\nTest summary:\n")
print(test_results)

# Detailed failures
if (sum(test_results$failed) > 0) {
  cat("\n❌ FAILURES DETECTED:\n")
  for (i in seq_len(nrow(test_results))) {
    if (test_results$failed[i] > 0) {
      cat(sprintf("\nFile: %s\n", test_results$file[i]))
      cat(sprintf("Context: %s\n", test_results$context[i]))
      cat(sprintf("Failed: %d, Passed: %d, Skipped: %d\n",
                  test_results$failed[i], test_results$passed[i], test_results$skipped[i]))
    }
  }
  quit(status = 1)
} else {
  cat("\n✅ ALL TESTS PASSED!\n")
  quit(status = 0)
}