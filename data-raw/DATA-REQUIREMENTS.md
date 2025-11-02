# Data Requirements for PhD Unemployment Analysis

## Overview

This project requires **monthly unemployment data from 2000-2025** to enable:

- Time series modeling with seasonal adjustment
- Comparison of PhD unemployment to baseline rates
- Detection of structural changes and economic cycles

## Data Source: IPUMS CPS

**Source**: [IPUMS Current Population Survey (CPS)](https://cps.ipums.org/cps/)

**Why CPS (not ACS)**:
- **Monthly frequency** - CPS surveys monthly, ACS is annual only
- **Employment focus** - CPS is designed for labor force statistics
- **Long time series** - CPS has consistent data back to 1962

## Time Coverage

**Required Range**: 2000-2025 (300+ monthly samples)

- **Start**: January 2000 (`cps200001`)
- **End**: Most recent available month in 2025 (`cps202501`, `cps202502`, etc.)
- **Total samples**: ~300 (25 years × 12 months)

**Note**: IPUMS CPS may not have all months available. Check availability at:
<https://cps.ipums.org/cps-action/samples/sample_ids>

## Required Variables

### Core Variables (REQUIRED)

| Variable | Description | Values | Purpose |
|----------|-------------|---------|---------|
| `YEAR` | Survey year | 2000-2025 | Time series index |
| `MONTH` | Survey month | 1-12 | Seasonal adjustment |
| `EMPSTAT` | Employment status | 1=Employed, 2=Unemployed, 3=Not in labor force | Outcome variable |
| `EDUC` | Education level | 111=Master's, 114=Professional, 116=Doctorate | Filter for PhDs |
| `WTFINL` | Person weight | Continuous | Population estimates |

### Demographic Variables (Recommended)

| Variable | Description | Purpose |
|----------|-------------|---------|
| `AGE` | Age in years | Disaggregation by career stage |
| `SEX` | Sex | Gender disparities in PhD unemployment |
| `RACE` | Race | Equity analysis |
| `HISPAN` | Hispanic origin | Equity analysis |

### Geographic Variables (Optional)

| Variable | Description | Purpose |
|----------|-------------|---------|
| `STATEFIP` | State FIPS code | Regional variation |
| `METRO` | Metropolitan status | Urban/rural differences |

## Education Coding (EDUC variable)

For PhD unemployment analysis:

- **PhDs**: `EDUC == 116` (Doctorate degree - research/scholarship)
- **Comparison groups**:
  - Master's: `EDUC == 111`
  - Professional degree: `EDUC == 114` (MD, JD, DDS, etc.)
  - Bachelor's: `EDUC == 110`

**Note**: IPUMS CPS uses consistent EDUC coding across years, but verify with data dictionary.

## Employment Status Coding (EMPSTAT variable)

- **Employed**: `EMPSTAT == 1`
- **Unemployed**: `EMPSTAT == 2`
- **Not in labor force**: `EMPSTAT == 3`

**Unemployment rate calculation**:
```r
unemployment_rate = (unemployed) / (employed + unemployed)
```

**Labor force participation**:
```r
lfp_rate = (employed + unemployed) / total_population
```

## Weighting

**Variable**: `WTFINL` (Final person weight)

**Why weights matter**:
- CPS uses complex sampling design
- Weights ensure nationally representative estimates
- Unweighted estimates will be biased

**Example weighted calculation**:
```r
library(dplyr)

unemployment_rate <- data %>%
  filter(EDUC == 116, EMPSTAT %in% 1:2) %>%  # PhDs in labor force
  group_by(YEAR, MONTH) %>%
  summarise(
    unemp_rate = weighted.mean(EMPSTAT == 2, WTFINL),
    .groups = "drop"
  )
```

## Data Size Estimates

**Individual extract**:
- 1 month ≈ 50-60 MB compressed
- 300 months ≈ 15-18 GB compressed
- Uncompressed ≈ 60-100 GB

**After filtering to PhDs**:
- PhDs are ~0.5-1% of population
- Final dataset ≈ 100-500 MB

**Recommendation**: Start with subset (2020-2025) to test workflow, then download full dataset.

## Extract Configuration

### Quick Start (Recent Data)

```r
source("data-raw/ipums-cps-config.R")

# Test with recent years only (faster download)
result <- download_cps_unemployment_data(
  start_year = 2020,
  end_year = 2025,
  variable_groups = c("time", "employment", "education", "weights")
)
```

### Full Dataset (2000-2025)

```r
source("data-raw/ipums-cps-config.R")

# Full dataset (WARNING: Large download, may take 30+ minutes)
result <- download_cps_unemployment_data(
  start_year = 2000,
  end_year = 2025,
  variable_groups = c("time", "employment", "education", "weights", "demographics")
)
```

### Manual Extract (IPUMS Web Interface)

If API download fails, you can create extract manually:

1. Go to <https://cps.ipums.org/cps/>
2. Select **Samples**:
   - Browse by year: 2000-2025
   - Select all monthly samples (Basic Monthly)
3. Select **Variables**:
   - Household: None needed
   - Person: `YEAR`, `MONTH`, `EMPSTAT`, `EDUC`, `AGE`, `SEX`, `WTFINL`
4. Create extract
5. Download when ready
6. Save to `data-raw/`

## Data Processing Workflow

After downloading raw data:

1. **Filter to PhDs**: `EDUC == 116`
2. **Filter to labor force**: `EMPSTAT %in% 1:2`
3. **Create date variable**: `date = make_date(YEAR, MONTH, 1)`
4. **Calculate weighted rates** by month
5. **Save processed data**: `data/phd_unemployment_monthly.rds`

See `data-raw/01-process-cps-data.R` (to be created) for processing pipeline.

## Alternative Data Sources

If IPUMS CPS is unavailable:

1. **BLS CPS Microdata**: <https://www.bls.gov/cps/data.htm>
   - Same data source, different interface
   - More complex to work with (fixed-width format)

2. **BLS Published Tables**: <https://www.bls.gov/charts/employment-situation/unemployment-rates-for-persons-25-years-and-older-by-educational-attainment.htm>
   - Pre-calculated unemployment by education
   - Lacks detailed PhD category
   - Cannot disaggregate by demographics

3. **NSF SESTAT**: <https://www.nsf.gov/statistics/sestat/>
   - Focuses on science/engineering PhDs
   - Biennial, not monthly
   - Better for career outcomes, not unemployment cycles

## Data Citation

When using IPUMS CPS data, cite as:

> Sarah Flood, Miriam King, Renae Rodgers, Steven Ruggles, J. Robert Warren, Daniel Backman, Annie Chen, Grace Cooper, Stephanie Richards, Megan Schouweiler, and Michael Westberry. IPUMS CPS: Version 11.0 [dataset]. Minneapolis, MN: IPUMS, 2023. <https://doi.org/10.18128/D030.V11.0>

## Questions and Support

- **IPUMS CPS Documentation**: <https://cps.ipums.org/cps/>
- **IPUMS Support**: <ipums@umn.edu>
- **Variable Definitions**: <https://cps.ipums.org/cps-action/variables/group>
- **Sample Availability**: <https://cps.ipums.org/cps-action/samples/sample_ids>
