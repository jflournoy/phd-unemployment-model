# Raw Data Directory

This directory contains raw data files and data processing scripts.

## Directory Structure

- Raw data files downloaded from IPUMS should be placed here
- Processing scripts (`.R` files) should be named `01-process-*.R`, `02-clean-*.R`, etc.
- Output processed data to `../data/` directory

## Data Processing Workflow

1. **Download data** from [IPUMS USA](https://usa.ipums.org/usa/index.shtml)
2. **Place raw files** in this directory (ignored by git)
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
