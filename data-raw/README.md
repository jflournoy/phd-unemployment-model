# Raw Data Directory

This directory contains raw data files and data processing scripts.

## Quick Start

To download the full IPUMS CPS dataset (2000-2025):

```bash
# Set your IPUMS API key
export IPUMS_API_KEY='your-key-here'

# Run the download script
Rscript data-raw/download-full-dataset.R
```

See [QUICKSTART.md](QUICKSTART.md) for detailed instructions.

## Directory Structure

- **download-full-dataset.R** - Script to download full 2000-2025 CPS data
- **ipums-cps-config.R** - Configuration for CPS data downloads
- **DATA-REQUIREMENTS.md** - Detailed data requirements documentation
- **QUICKSTART.md** - Step-by-step download instructions
- **ipums_data.rds** - Downloaded raw CPS data (not in git, ~20GB)
- Processing scripts (`.R` files) should be named `01-process-*.R`, `02-clean-*.R`, etc.

## Data Processing Workflow

1. **Download data** using `download-full-dataset.R` (automated) or manual IPUMS extract
2. **Raw data** is saved as `ipums_data.rds` (ignored by git)
3. **Create processing script** to clean and structure data
4. **Save processed data** as RDS files in `../data/`
5. **Document** data sources and processing steps

## Example Processing Script

```r
# data-raw/01-process-ipums-cps.R

library(dplyr)
library(readr)
library(lubridate)

# Read raw IPUMS data
raw_data <- read_csv("data-raw/cps_00001.csv.gz")

# Process data
unemployment_data <- raw_data %>%
  filter(EDUC >= 116) %>%  # Filter for PhDs
  mutate(
    date = make_date(YEAR, MONTH, 1),
    unemployed = EMPSTAT == 2,
    in_labor_force = EMPSTAT <= 2
  ) %>%
  group_by(date) %>%
  summarise(
    unemp_rate = weighted.mean(unemployed, WTFINL),
    .groups = "drop"
  )

# Save processed data
saveRDS(unemployment_data, "data/phd_unemployment.rds")
```

## Data Documentation

For each dataset, create a corresponding `.md` file documenting:

- Data source and download date
- Sample restrictions
- Variable definitions
- Weighting scheme
- Processing steps
