#' Demo: Data Completeness Report
#'
#' Demonstrates the completeness report functionality with actual CPS data.

# Load required libraries
library(here)

# Source functions
source(here("R", "data-completeness.R"))

# Load downloaded CPS data
cat("Loading CPS data from data-raw/ipums_data.rds...\n")
cps_data <- readRDS(here("data-raw", "ipums_data.rds"))

cat("\nData dimensions:", nrow(cps_data), "rows ×", ncol(cps_data), "columns\n")

# Generate completeness report
# Note: ASECWT is only present in March ASEC, so check core vars only
cat("\nGenerating completeness report for 2024...\n")
report <- generate_completeness_report(
  cps_data,
  expected_months = 1:12,
  min_obs = 100,
  required_vars = c("YEAR", "MONTH", "EMPSTAT", "EDUC")  # WTFINL checked separately
)

# Print the report
print_completeness_report(report)

# Quick validation check
cat("\n=== Quick Validation ===\n")
is_complete <- validate_data_completeness(
  cps_data,
  expected_months = 1:12,
  min_obs = 100,
  required_vars = c("YEAR", "MONTH", "EMPSTAT", "EDUC")
)

if (is_complete) {
  cat("✓ Data passed all completeness checks!\n")
} else {
  cat("✗ Data has completeness issues (see report above)\n")
}

# Save report to file
output_file <- here("data", "completeness_report_2024.csv")
write.csv(report, output_file, row.names = FALSE)
cat("\nReport saved to:", output_file, "\n")
