#!/usr/bin/env Rscript
# Fix saved fit structure: rename 'data' to 'stan_data' for compatibility

library(here)

cat("Loading saved fit...\n")
fit_path <- here("models/ode-state-space-monotonic-spline-fit.qs")
result <- qs::qread(fit_path)

cat("Current keys:", paste(names(result), collapse = ", "), "\n")

# Fix: rename 'data' to 'stan_data'
if ("data" %in% names(result) && !"stan_data" %in% names(result)) {
  cat("Renaming 'data' to 'stan_data'...\n")
  result$stan_data <- result$data
  result$data <- NULL
} else if ("stan_data" %in% names(result)) {
  cat("Already has 'stan_data', no fix needed.\n")
} else {
  stop("Unexpected structure - neither 'data' nor 'stan_data' found!")
}

cat("Updated keys:", paste(names(result), collapse = ", "), "\n")

# Re-save
cat("Re-saving fit...\n")
qs::qsave(result, fit_path, preset = "high")

cat("\n✓ Fix complete! The fit object now has the correct structure.\n")
