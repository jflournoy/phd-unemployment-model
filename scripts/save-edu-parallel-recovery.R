#!/usr/bin/env Rscript
# Recovery script: reconstruct edu-parallel fit from CSV files and save
# Run this if the main script's save_cmdstan_fit call fails

library(phdunemployment)
library(cmdstanr)
library(data.table)

# Find the most recent edu-parallel stan-output directory
output_dirs <- Sys.glob("models/stan-output/edu-parallel-*")
if (length(output_dirs) == 0) {
  stop("No edu-parallel output directories found in models/stan-output/")
}

# Sort by timestamp and take the latest
output_dirs <- sort(output_dirs)
latest_dir <- output_dirs[length(output_dirs)]
cat("Latest output directory:", latest_dir, "\n")

# Find CSV files
csv_files <- Sys.glob(file.path(latest_dir, "*.csv"))
csv_files <- sort(csv_files)
cat("Found", length(csv_files), "CSV files:\n")
for (f in csv_files) cat("  -", f, "\n")

if (length(csv_files) == 0) {
  stop("No CSV files found in ", latest_dir)
}

# Also check /tmp for files that haven't been moved yet
tmp_csv <- Sys.glob("/tmp/Rtmp*/unemployment-ode-state-space-edu-parallel-*.csv")
if (length(tmp_csv) > 0) {
  tmp_csv <- sort(tmp_csv)
  cat("\nAlso found", length(tmp_csv), "CSV files in /tmp:\n")
  for (f in tmp_csv) cat("  -", f, "\n")
}

# Reconstruct CmdStanMCMC from CSV files
cat("\nReconstructing CmdStanMCMC from CSV files...\n")
fit <- as_cmdstan_fit(csv_files)

# Create result list matching fit_ode_state_space_edu_parallel output
cat("Loading stan data for metadata...\n")
counts <- readRDS("data/education-spectrum-counts.rds")
stan_data <- suppressMessages(prepare_stan_data(counts))

cat("Computing diagnostics...\n")

result <- list(
  fit = fit,
  stan_data = stan_data,
  time = list(
    chains = NA_real_,
    total = NA_real_
  ),
  diagnostics = list(
    num_divergent = sum(fit$sampler_diagnostics()[,,"divergent__"]),
    max_treedepth_exceeded = sum(fit$sampler_diagnostics()[,,"treedepth__"] >= 15),
    ebfmi = as.numeric(fit$diagnostic_summary()$ebfmi)
  )
)

cat("Diagnostics:\n")
cat("  Divergent transitions:", result$diagnostics$num_divergent, "\n")
cat("  Max treedepth exceeded:", result$diagnostics$max_treedepth_exceeded, "\n")
cat("  E-BFMI:", paste(round(result$diagnostics$ebfmi, 3), collapse = ", "), "\n\n")

# Save model (manual save since as_cmdstan_fit objects don't support save_output_files)
cat("Saving model...\n")

output_file <- "models/ode-state-space-edu-parallel-fit.rds"
csv_dir <- paste0(tools::file_path_sans_ext(output_file), "_csv")

# Create CSV directory and copy files
if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)

cat("Copying CSV files to:", csv_dir, "\n")
for (f in csv_files) {
  file.copy(f, csv_dir, overwrite = TRUE)
  cat("  Copied:", basename(f), "\n")
}

# Store CSV directory path in result
result$csv_dir <- csv_dir
result$csv_saved <- TRUE
result$save_time <- Sys.time()

# Save RDS
cat("Saving fit result to:", output_file, "\n")
saveRDS(result, file = output_file, compress = "xz")

cat("\nDone! Model saved successfully.\n")
cat("  Fit file:", output_file, "\n")
cat("  CSV dir:", csv_dir, "\n")
