#!/usr/bin/env Rscript
# Re-fit edu-parallel model with time-varying equilibrium
# 4 chains, 2 parallel × 7 threads = 14 cores
# Usage: Rscript R/refit-edu-parallel.R

library(here)
source(here("R", "ode-state-space.R"))
source(here("R", "save-cmdstan-fit.R"))

counts_data <- readRDS(here("data", "education-spectrum-counts.rds"))

cat("Data loaded:", nrow(counts_data), "rows\n")
cat("Education levels:", paste(unique(counts_data$education), collapse = ", "), "\n")

result <- fit_ode_state_space_edu_parallel(
  data = counts_data,
  chains = 4,
  parallel_chains = 2,
  threads_per_chain = 7,
  iter_sampling = 1500,
  iter_warmup = 1500,
  adapt_delta = 0.99,
  max_treedepth = 15,
  grainsize = 1L,
  K_spline = 10L,
  refresh = 50
)

cat("\n=== Diagnostics ===\n")
cat("Divergent transitions:", result$diagnostics$num_divergent, "\n")
cat("Max treedepth exceeded:", result$diagnostics$max_treedepth_exceeded, "\n")
cat("EBFMI:", paste(round(result$diagnostics$ebfmi, 3), collapse = ", "), "\n")
cat("Elapsed:", round(result$timing$elapsed_mins, 1), "minutes\n")

out_file <- here("models", "ode-state-space-edu-parallel-fit-v4.rds")
save_cmdstan_fit(result, out_file, format = "rds")
cat("Saved to:", out_file, "\n")
