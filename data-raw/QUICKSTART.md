# Quick Start: Getting IPUMS CPS Data

## Prerequisites

1. **IPUMS Account**: Register at <https://cps.ipums.org/>
2. **API Key**: Get from <https://account.ipums.org/api_keys>
3. **Set API Key** in R:

```r
ipumsr::set_ipums_api_key("your-key-here", save = TRUE)
```

Or add to `.Renviron`:
```bash
IPUMS_API_KEY=your-key-here
```

## Option 1: Test with Recent Data (Recommended First)

**Fast download (~5-10 min), good for testing workflow**

```r
# Load configuration
source("data-raw/ipums-cps-config.R")

# Download 2020-2025 only (6 years = 72 months)
result <- download_cps_unemployment_data(
  start_year = 2020,
  end_year = 2025,
  variable_groups = c("time", "employment", "education", "weights")
)

# Check what you got
data <- readRDS(result$file_path)
print(summary(data))
```

## Option 2: Full Dataset (2000-2025)

**Large download (~30-60 min), needed for seasonal adjustment**

```r
source("data-raw/ipums-cps-config.R")

# Full 25 years of monthly data
result <- download_cps_unemployment_data(
  start_year = 2000,
  end_year = 2025,
  variable_groups = c("time", "employment", "education", "weights", "demographics")
)
```

## Option 3: Manual Download (If API Fails)

1. Go to <https://cps.ipums.org/cps/>
2. **Select Samples**:
   - Click "Select Samples"
   - Browse by year: 2000-2025
   - Check all "Basic Monthly" samples
3. **Select Variables**:
   - Click "Select Variables"
   - Search and add: `YEAR`, `MONTH`, `EMPSTAT`, `EDUC`, `WTFINL`, `AGE`, `SEX`
4. **Create Extract**:
   - Click "Create Data Extract"
   - Add description: "PhD unemployment 2000-2025"
   - Submit
5. **Download**:
   - Wait for email notification (may take 30+ min)
   - Download `.dat` and `.xml` files to `data-raw/`
6. **Read in R**:
```r
library(ipumsr)
ddi <- read_ipums_ddi("data-raw/cps_00001.xml")
data <- read_ipums_micro(ddi)
saveRDS(data, "data-raw/cps_phd_unemployment_2000_2025.rds")
```

## What to Do After Download

See [DATA-REQUIREMENTS.md](DATA-REQUIREMENTS.md) for:
- Data filtering (PhDs only)
- Weighted calculations
- Monthly aggregation
- Seasonal adjustment preparation

## Troubleshooting

### "IPUMS_API_KEY not found"
```r
# Check if key is set
Sys.getenv("IPUMS_API_KEY")

# Set it for this session
Sys.setenv(IPUMS_API_KEY = "your-key-here")

# Or set permanently
ipumsr::set_ipums_api_key("your-key-here", save = TRUE)
```

### "Sample not available"
Some months may not be in IPUMS CPS. The script will fail with specific sample names. You can:
1. Use manual download to see available samples
2. Modify `generate_cps_samples()` to exclude unavailable months

### Extract takes too long
- Start with smaller date range (2020-2025)
- Reduce variables (minimal: `YEAR`, `MONTH`, `EMPSTAT`, `EDUC`, `WTFINL`)
- Large extracts can take 30-60+ minutes

### Download fails
- Check internet connection
- Check IPUMS system status
- Try manual download instead
- Contact IPUMS support: <ipums@umn.edu>

## Data Size Expectations

| Time Range | Samples | Compressed | Uncompressed | After PhD Filter |
|------------|---------|------------|--------------|------------------|
| 2020-2025  | ~72     | ~4 GB      | ~15 GB       | ~50-100 MB      |
| 2000-2025  | ~300    | ~18 GB     | ~80 GB       | ~200-500 MB     |

## Next Steps

After downloading data:

1. **Explore data**: Run summary statistics
2. **Filter to PhDs**: `EDUC == 116`
3. **Calculate unemployment rates**: By month, weighted
4. **Visualize**: Time series plots
5. **Model**: Seasonal adjustment, trend decomposition

See project README for full analysis workflow.
