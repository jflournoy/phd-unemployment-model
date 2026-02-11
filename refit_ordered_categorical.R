#!/usr/bin/env Rscript
# Refit ordered categorical model with fixed initialization
# Uses higher adapt_delta and max_treedepth for better sampling

library(here)

cat("=================================================================\n")
cat("Refitting Ordered Categorical Model with Fixed Initialization\n")
cat("=================================================================\n\n")

# Source functions
source(here("R/ode-state-space.R"))
source(here("R/data-processing.R"))

# Load data
cat("Loading data...\n")
cps_phd_all <- readRDS(here("data/education-spectrum-counts.rds"))

cat("\nData summary:\n")
cat(sprintf("  Observations: %d\n", nrow(cps_phd_all)))
cat(sprintf("  Time range: %d-%d to %d-%d\n",
            min(cps_phd_all$year), min(cps_phd_all$month),
            max(cps_phd_all$year), max(cps_phd_all$month)))
cat(sprintf("  Education levels: %d\n", length(unique(cps_phd_all$education))))

# Fit model with improved settings
cat("\n=================================================================\n")
cat("Fitting model...\n")
cat("=================================================================\n")
cat("\nModel settings:\n")
cat("  - Fixed initialization: centered at prior means\n")
cat("  - adapt_delta = 0.98 (increased from 0.95)\n")
cat("  - max_treedepth = 14 (increased from 12)\n")
cat("  - chains = 4, iterations = 1000 warmup + 1000 sampling\n")
cat("  - threads_per_chain = 2, grainsize = 1\n\n")

start_time <- Sys.time()

fit_ordered <- fit_ode_state_space_ordered_categorical(
  data = cps_phd_all,
  chains = 4,
  iter_sampling = 2000,  # Increased to 2000
  iter_warmup = 1000,
  adapt_delta = 0.98,    # Increased from 0.95
  max_treedepth = 14,    # Increased from 12
  parallel_chains = 4,
  threads_per_chain = 2,
  grainsize = 1L,
  K_spline = 25L,
  refresh = 100
)

end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "mins")

cat("\n=================================================================\n")
cat("Fitting completed!\n")
cat("=================================================================\n")
cat(sprintf("\nElapsed time: %.1f minutes\n", as.numeric(elapsed)))

# Check diagnostics
cat("\nChecking convergence diagnostics...\n")
diag <- fit_ordered$diagnostic_summary()
print(diag)

# Check summary statistics
cat("\nKey parameter summaries:\n")
params_to_check <- c("mu_logit_u_eq", "mu_log_adj_speed", "mu_log_shock_2008",
                     "mu_log_shock_2020", "mu_decay_2008", "mu_decay_2020",
                     "sigma_logit_u_eq", "sigma_log_adj_speed")
summary_df <- fit_ordered$summary(variables = params_to_check)
print(summary_df)

# Save fit
output_file <- here("models/unemployment-ode-state-space-ordered-categorical-fixed.rds")
cat("\nSaving fit to:", output_file, "\n")
saveRDS(fit_ordered, output_file)

cat("\n✓ Done! Model fit saved.\n")
cat("\nNext steps:\n")
cat("  1. Review diagnostics above\n")
cat("  2. Compare to monotonic spline model\n")
cat("  3. Generate posterior predictions\n")
