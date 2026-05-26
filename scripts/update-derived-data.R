#!/usr/bin/env Rscript
# Update Derived Data from Raw IPUMS CPS Data
# Processes raw microdata into analysis-ready datasets

library(data.table)
library(here)

RAW_DATA_FILE <- here("data-raw", "ipums_data.rds")
OUTPUT_DIR <- here("data")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("========================================\n")
cat("Generating Derived Datasets\n")
cat("========================================\n\n")

cat("Loading raw data...\n")
cps_data <- as.data.table(readRDS(RAW_DATA_FILE))
cat("  Rows:", nrow(cps_data), "\n")
cat("  Years:", min(cps_data$YEAR), "-", max(cps_data$YEAR), "\n\n")

# Education level definitions (IPUMS CPS codes)
# IPUMS CPS recoded education codes
# These are the actual codes present in the IPUMS extract
education_levels <- list(
  less_than_hs = c(2, 10, 20, 30, 40, 50, 60, 71),  # Less than high school
  high_school = 73,                                    # High school diploma / GED
  some_college = c(81, 91, 92),                        # Some college / associate
  bachelors = 111,                                     # Bachelor's degree
  masters = 123,                                       # Master's degree
  professional = 124,                                  # Professional degree
  phd = 125                                            # Doctoral degree
)

# Create education category mapping
cat("Creating education categories...\n")
educ_map <- data.table(
  EDUC = unlist(education_levels),
  education = rep(names(education_levels), times = sapply(education_levels, length))
)
cps_data[educ_map, education := i.education, on = "EDUC"]

# ==============================================================================
# 1. Education Spectrum Count Data (for binomial/quasi-binomial GAMs)
# ==============================================================================
cat("\n1. Generating education spectrum count data...\n")

# Filter to labor force (employed + unemployed)
in_lf <- cps_data[EMPSTAT %in% c(1, 2) & !is.na(education), .(
  n_employed = sum(EMPSTAT == 1),
  n_unemployed = sum(EMPSTAT == 2),
  n_total = .N
), by = .(YEAR, MONTH, education)]

# Add date
in_lf[, date := as.Date(ISOdate(YEAR, MONTH, 1))]
in_lf[, year_frac := YEAR + (MONTH - 0.5) / 12]

setcolorder(in_lf, c("date", "year_frac", "YEAR", "MONTH", "education",
                      "n_employed", "n_unemployed", "n_total"))

education_counts_file <- file.path(OUTPUT_DIR, "education-spectrum-counts.rds")
saveRDS(in_lf, education_counts_file)
cat("  ✓ Saved:", education_counts_file, "\n")
cat("  ✓ Education levels:", uniqueN(in_lf$education), "\n")
cat("  ✓ Date range:", format(min(in_lf$date), "%Y-%m"), "to", format(max(in_lf$date), "%Y-%m"), "\n")

# ==============================================================================
# 2. PhD Monthly Unemployment (for time series analysis)
# ==============================================================================
cat("\n2. Generating PhD monthly unemployment data...\n")

phd_data <- cps_data[education == "phd"]
phd_lf <- phd_data[EMPSTAT %in% c(1, 2)]

phd_monthly <- phd_lf[, .(
  n_employed = sum(EMPSTAT == 1),
  n_unemployed = sum(EMPSTAT == 2),
  n_total = .N,
  unemployment_rate = sum(EMPSTAT == 2) / .N
), by = .(YEAR, MONTH)]

phd_monthly[, `:=`(
  date = as.Date(ISOdate(YEAR, MONTH, 1)),
  time_index = .I
)]
setnames(phd_monthly, "n_total", "n_obs")

setcolorder(phd_monthly, c("date", "YEAR", "MONTH", "n_employed", "n_unemployed",
                            "n_obs", "unemployment_rate", "time_index"))

phd_monthly_file <- file.path(OUTPUT_DIR, "phd-monthly-unemployment.rds")
saveRDS(phd_monthly, phd_monthly_file)
cat("  ✓ Rows:", nrow(phd_monthly), "\n")
cat("  ✓ Date range:", format(min(phd_monthly$date), "%Y-%m"), "to",
    format(max(phd_monthly$date), "%Y-%m"), "\n")
cat("  ✓ Saved:", phd_monthly_file, "\n")

# ==============================================================================
# 3. Multi-Education Unemployment (for comparison analyses)
# ==============================================================================
cat("\n3. Generating multi-education unemployment data...\n")

lf_data <- cps_data[!is.na(education) & EMPSTAT %in% c(1, 2)]

multi_educ <- lf_data[, .(
  n_employed = sum(EMPSTAT == 1),
  n_unemployed = sum(EMPSTAT == 2),
  n_total = .N,
  unemployment_rate = sum(EMPSTAT == 2) / .N
), by = .(YEAR, MONTH, education)]

multi_educ[, date := as.Date(ISOdate(YEAR, MONTH, 1))]
multi_educ[, year_frac := YEAR + (MONTH - 0.5) / 12]

setcolorder(multi_educ, c("date", "year_frac", "YEAR", "MONTH", "education",
                           "n_employed", "n_unemployed", "n_total", "unemployment_rate"))

# Add weighted unemployment rate using WTFINL
cat("  Computing weighted unemployment rates...\n")
multi_educ_w <- lf_data[, .(
  unemp_rate_wtd = weighted.mean(EMPSTAT == 2, WTFINL, na.rm = TRUE)
), by = .(YEAR, MONTH, education)]

multi_educ <- merge(multi_educ, multi_educ_w, by = c("YEAR", "MONTH", "education"))

multi_educ_file <- file.path(OUTPUT_DIR, "multi-education-unemployment.rds")
saveRDS(multi_educ, multi_educ_file)
cat("  ✓ Rows:", nrow(multi_educ), "\n")
cat("  ✓ Education levels:", uniqueN(multi_educ$education), "\n")
cat("  ✓ Saved:", multi_educ_file, "\n")

# ==============================================================================
# Summary
# ==============================================================================
cat("\n========================================\n")
cat("Data Generation Complete\n")
cat("========================================\n\n")
cat("Generated files:\n")
cat("  1.", basename(education_counts_file), "\n")
cat("  2.", basename(phd_monthly_file), "\n")
cat("  3.", basename(multi_educ_file), "\n\n")
cat("Data spans:", min(cps_data$YEAR), "to", max(cps_data$YEAR), "\n")
cat("Latest month:", max(cps_data$MONTH[cps_data$YEAR == max(cps_data$YEAR)]),
    "/", max(cps_data$YEAR), "\n")
