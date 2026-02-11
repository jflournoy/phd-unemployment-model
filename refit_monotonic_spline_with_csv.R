#!/usr/bin/env Rscript
# Refit monotonic spline model with proper CSV preservation
# This enables the full state-space-comparison report to render

library(here)

cat("=================================================================\n")
cat("Refitting Monotonic I-Spline Model with CSV Preservation\n")
cat("=================================================================\n\n")

# Source functions
source(here("R/ode-state-space.R"))
source(here("R/data-processing.R"))
source(here("R/save-cmdstan-fit.R"))

# Load data
cat("Loading data...\n")
education_counts <- readRDS(here("data/education-spectrum-counts.rds"))

cat("\nData summary:\n")
cat(sprintf("  Observations: %d\n", nrow(education_counts)))
cat(sprintf("  Time range: %d-%d to %d-%d\n",
            min(education_counts$year), min(education_counts$month),
            max(education_counts$year), max(education_counts$month)))
cat(sprintf("  Education levels: %d\n", length(unique(education_counts$education))))

# Education order
education_order <- c("less_than_hs", "high_school", "some_college",
                     "bachelors", "masters", "professional", "phd")

cat("\n=================================================================\n")
cat("Fitting model...\n")
cat("=================================================================\n")
cat("\nModel settings:\n")
cat("  - Model: Monotonic I-Spline (4 basis functions)\n")
cat("  - Chains: 4, parallel\n")
cat("  - Iterations: 1000 warmup + 2000 sampling\n")
cat("  - Threads per chain: 7\n")
cat("  - adapt_delta = 0.95\n")
cat("  - max_treedepth = 12\n")
cat("  - Initialization: Prior means + conservative beta (sum=1 not K)\n\n")

start_time <- Sys.time()

# Fit the model
fit_result <- fit_ode_state_space_monotonic_spline(
  data = education_counts,
  education_order = education_order,
  chains = 4,
  iter_sampling = 2000,
  iter_warmup = 1000,
  adapt_delta = 0.95,
  max_treedepth = 12,
  parallel_chains = 4,
  threads_per_chain = 7,
  grainsize = 1L,
  n_ispline_basis = 4L,
  refresh = 100
)

end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "mins")

cat("\n=================================================================\n")
cat("Fitting completed!\n")
cat("=================================================================\n")
cat(sprintf("\nElapsed time: %.1f minutes\n", as.numeric(elapsed)))

# CRITICAL: Save CSV files IMMEDIATELY before doing anything else
cat("\n=================================================================\n")
cat("Saving with CSV preservation (IMMEDIATE)...\n")
cat("=================================================================\n\n")

output_file <- here("models/ode-state-space-monotonic-spline-fit.qs")
save_cmdstan_fit(fit_result, output_file)

# Now safe to check diagnostics
cat("\n=================================================================\n")
cat("Checking convergence diagnostics...\n")
cat("=================================================================\n\n")
diag <- fit_result$fit$diagnostic_summary()
print(diag)

cat("\n=================================================================\n")
cat("COMPLETE!\n")
cat("=================================================================\n\n")

cat("Model saved to:", output_file, "\n")
cat("CSV files preserved in:", paste0(tools::file_path_sans_ext(output_file), "_csv"), "\n\n")

cat("You can now render the full report:\n")
cat("  cd reports && quarto render state-space-comparison.qmd\n\n")

cat("✓ Done!\n")
