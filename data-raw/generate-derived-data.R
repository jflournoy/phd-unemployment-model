#!/usr/bin/env Rscript
# Generate Derived Data from Raw IPUMS CPS Data
#
# This script generates all derived datasets from the raw IPUMS CPS microdata.
# It is designed to be run standalone or as part of the targets pipeline.
#
# Usage:
#   Rscript data-raw/generate-derived-data.R
#
# Or from R:
#   source("data-raw/generate-derived-data.R")

# Load package (use devtools::load_all() if in development)
if (requireNamespace("devtools", quietly = TRUE) &&
    file.exists(here::here("DESCRIPTION"))) {
  devtools::load_all(quiet = TRUE)
} else {
  library(phdunemployment)
}

library(here)

# ==============================================================================
# Configuration
# ==============================================================================

RAW_DATA_FILE <- here("data-raw", "ipums_data.rds")
OUTPUT_DIR <- here("data")

# Ensure output directory exists
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

cat("========================================\n")
cat("Generating Derived Datasets\n")
cat("========================================\n\n")

# ==============================================================================
# 1. Education Spectrum Count Data (for binomial/quasi-binomial GAMs)
# ==============================================================================

cat("1. Generating education spectrum count data...\n")

education_counts_file <- file.path(OUTPUT_DIR, "education-spectrum-counts.rds")

generate_education_spectrum_counts(
  input_file = RAW_DATA_FILE,
  output_file = education_counts_file,
  weighted = FALSE  # Use unweighted counts for binomial GAMs
)

cat("   ✓ Saved to:", education_counts_file, "\n\n")

# ==============================================================================
# 2. PhD Monthly Unemployment (for time series analysis)
# ==============================================================================

cat("2. Generating PhD monthly unemployment data...\n")

# Load raw data
cps_data <- readRDS(RAW_DATA_FILE)

# Process PhD unemployment
phd_monthly <- process_cps_data(cps_data)

# Save
phd_monthly_file <- file.path(OUTPUT_DIR, "phd-monthly-unemployment.rds")
saveRDS(phd_monthly, phd_monthly_file)

cat("   ✓ Rows:", nrow(phd_monthly), "\n")
cat("   ✓ Date range:",
    format(min(phd_monthly$date), "%Y-%m"), "to",
    format(max(phd_monthly$date), "%Y-%m"), "\n")
cat("   ✓ Saved to:", phd_monthly_file, "\n\n")

# ==============================================================================
# 3. Multi-Education Unemployment (for comparison analyses)
# ==============================================================================

cat("3. Generating multi-education unemployment data...\n")

# Define education levels for comparison
education_levels <- c(
  less_than_hs = 2,       # Less than high school
  high_school = 73,       # High school diploma
  some_college = 81,      # Some college
  bachelors = 111,        # Bachelor's degree
  masters = 123,          # Master's degree
  professional = 124,     # Professional degree
  phd = 125               # Doctoral degree
)

# Generate monthly data by education
multi_educ_data <- aggregate_monthly_by_education(
  cps_data,
  weight_var = "auto",  # Use ASECWT for March, WTFINL otherwise
  weighted = TRUE
)

# Save
multi_educ_file <- file.path(OUTPUT_DIR, "multi-education-unemployment.rds")
saveRDS(multi_educ_data, multi_educ_file)

cat("   ✓ Rows:", nrow(multi_educ_data), "\n")
cat("   ✓ Education levels:",
    length(unique(multi_educ_data$education)), "\n")
cat("   ✓ Saved to:", multi_educ_file, "\n\n")

# ==============================================================================
# 4. Data Summary
# ==============================================================================

cat("4. Generating data summary...\n")

# Get year range from data
year_range <- range(cps_data$YEAR)
month_range <- range(cps_data$MONTH[cps_data$YEAR == max(cps_data$YEAR)])

cat("   ✓ Data spans:", year_range[1], "to", year_range[2], "\n")
cat("   ✓ Latest month:", max(month_range), "/", max(year_range), "\n")
cat("   ✓ Total observations:", nrow(cps_data), "\n\n")

# ==============================================================================
# Summary
# ==============================================================================

cat("========================================\n")
cat("Data Generation Complete\n")
cat("========================================\n\n")

cat("Generated files:\n")
cat("  1.", basename(education_counts_file), "\n")
cat("  2.", basename(phd_monthly_file), "\n")
cat("  3.", basename(multi_educ_file), "\n\n")

cat("All derived datasets are up to date.\n")
cat("Use targets::tar_make() to regenerate with dependency tracking.\n")
