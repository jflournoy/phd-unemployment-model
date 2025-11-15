#!/usr/bin/env Rscript

#' Performance Benchmark: FST vs RDS
#'
#' Compares file I/O performance between RDS and FST formats using the full
#' 2000-2025 CPS dataset (674MB, 40M+ observations).

library(here)

cat("=== Performance Benchmark: FST vs RDS ===\n\n")

# Source file-io functions
source(here("R", "file-io.R"))

# Paths
rds_file <- here("data-raw", "ipums_data.rds")
fst_file <- here("data-raw", "ipums_data_benchmark.fst")

# Check if RDS file exists
if (!file.exists(rds_file)) {
  stop("RDS file not found: ", rds_file, "\n",
       "Please ensure the full 2000-2025 dataset is downloaded.")
}

cat("Dataset: 2000-2025 CPS microdata\n")
cat("RDS file size:", round(file.size(rds_file) / 1024^2, 1), "MB\n\n")

# Benchmark 1: Loading performance
cat("1. LOAD Performance\n")
cat("-------------------\n")

cat("Loading from RDS... ")
rds_load_time <- system.time({
  data_rds <- readRDS(rds_file)
})
cat(sprintf("%.2f seconds\n", rds_load_time["elapsed"]))
cat("  Rows:", nrow(data_rds), "\n")
cat("  Cols:", ncol(data_rds), "\n\n")

# Create FST version if it doesn't exist
if (!file.exists(fst_file)) {
  cat("Creating FST version for benchmark... ")
  fst_convert_time <- system.time({
    save_cps_data(data_rds, fst_file)
  })
  cat(sprintf("done (%.2f seconds)\n", fst_convert_time["elapsed"]))
} else {
  cat("Using existing FST file for benchmark\n")
}

cat("FST file size:", round(file.size(fst_file) / 1024^2, 1), "MB\n\n")

cat("Loading from FST... ")
fst_load_time <- system.time({
  data_fst <- load_cps_data(fst_file)
})
cat(sprintf("%.2f seconds\n", fst_load_time["elapsed"]))

cat("\n  Speedup:", round(rds_load_time["elapsed"] / fst_load_time["elapsed"], 1), "x faster\n\n")

# Benchmark 2: Saving performance
cat("2. SAVE Performance\n")
cat("-------------------\n")

# Use smaller subset for save benchmark to avoid doubling disk usage
subset_data <- data_rds[1:1000000, ]  # 1M rows

temp_rds <- tempfile(fileext = ".rds")
temp_fst <- tempfile(fileext = ".fst")

cat("Saving 1M rows to RDS... ")
rds_save_time <- system.time({
  saveRDS(subset_data, temp_rds)
})
cat(sprintf("%.2f seconds\n", rds_save_time["elapsed"]))

cat("Saving 1M rows to FST... ")
fst_save_time <- system.time({
  save_cps_data(subset_data, temp_fst)
})
cat(sprintf("%.2f seconds\n", fst_save_time["elapsed"]))

cat("\n  Speedup:", round(rds_save_time["elapsed"] / fst_save_time["elapsed"], 1), "x faster\n\n")

# Benchmark 3: Partial column loading (FST-only feature)
cat("3. PARTIAL LOAD Performance (FST advantage)\n")
cat("-------------------------------------------\n")

key_vars <- c("YEAR", "MONTH", "EMPSTAT", "EDUC", "WTFINL")

cat("Loading 5 columns from FST... ")
fst_partial_time <- system.time({
  data_partial <- load_cps_data(fst_file, columns = key_vars)
})
cat(sprintf("%.2f seconds\n", fst_partial_time["elapsed"]))

cat("Loading all columns from FST... ")
fst_full_time <- system.time({
  data_full <- load_cps_data(fst_file)
})
cat(sprintf("%.2f seconds\n", fst_full_time["elapsed"]))

cat("\n  Speedup for partial load:", round(fst_full_time["elapsed"] / fst_partial_time["elapsed"], 1), "x faster\n")
cat("  (RDS cannot do partial column loading)\n\n")

# Summary
cat("=== SUMMARY ===\n\n")
cat("For full 2000-2025 dataset:\n")
cat("  Load time:  RDS:", sprintf("%.2f", rds_load_time["elapsed"]), "sec  |  FST:", sprintf("%.2f", fst_load_time["elapsed"]), "sec  |  Speedup:", sprintf("%.1fx", rds_load_time["elapsed"] / fst_load_time["elapsed"]), "\n")
cat("  File size:  RDS:", round(file.size(rds_file) / 1024^2, 1), "MB  |  FST:", round(file.size(fst_file) / 1024^2, 1), "MB  |  Ratio:", sprintf("%.2f", file.size(fst_file) / file.size(rds_file)), "\n\n")

cat("Recommendation: Use FST for faster data loading\n")
cat("  - ", round(rds_load_time["elapsed"] / fst_load_time["elapsed"], 1), "x faster load times\n")
cat("  - Supports partial column loading for additional speedups\n")
cat("  - Similar file sizes\n\n")

# Clean up temp files
unlink(c(temp_rds, temp_fst))

# Optionally clean up benchmark FST file
cat("Keep FST benchmark file? (", fst_file, ") [y/N]: ")
response <- readLines("stdin", n = 1)
if (tolower(response) != "y") {
  unlink(fst_file)
  cat("Benchmark FST file removed.\n")
} else {
  cat("Benchmark FST file kept for future use.\n")
}

cat("\nBenchmark complete!\n")
