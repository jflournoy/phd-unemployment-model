# Data Acquisition Guide

## Data Source: IPUMS USA

Primary data source: [IPUMS USA](https://usa.ipums.org/usa/index.shtml)

## Required Variables

### Core Employment Variables

- **EMPSTAT**: Employment status
- **EDUC**: Educational attainment
- **DEGFIELD**: Field of degree (for identifying PhD holders)
- **YEAR**: Survey year
- **MONTH**: Survey month (for seasonal analysis)
- **SERIAL**: Household serial number
- **PERNUM**: Person number within household
- **WTFINL**: Person weight

### Demographic Controls (optional)

- **AGE**: Age
- **SEX**: Sex
- **RACE**: Race
- **HISPAN**: Hispanic origin

## Data Extraction Steps

1. **Register** at IPUMS USA website
2. **Select samples**: Monthly CPS data (ASEC or basic monthly)
3. **Select variables**: See list above
4. **Create extract**: Submit data extract request
5. **Download**: Once ready, download the data files
6. **Process**: Save raw data to `data-raw/` directory

## Data Processing Pipeline

Scripts in `data-raw/` should:

1. Read raw IPUMS data
2. Filter for relevant education levels (PhD vs baselines)
3. Calculate unemployment rates by group and time
4. Create time series objects
5. Save processed data to `data/` as RDS files

## Data Structure

Target structure for analysis:

```r
unemployment_data <- tibble(
  date = Date,              # Year-Month as Date object
  phd_unemp_rate = numeric, # PhD unemployment rate
  gen_unemp_rate = numeric, # General unemployment rate
  ms_unemp_rate = numeric,  # Master's unemployment rate
  ba_unemp_rate = numeric,  # Bachelor's unemployment rate
  # Additional fields as needed
)
```

## Data Documentation

Each processed dataset should include:

- Source information
- Processing date
- Variable definitions
- Sample restrictions
- Weighting scheme used
