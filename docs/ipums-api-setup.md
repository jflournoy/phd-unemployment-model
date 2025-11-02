# IPUMS API Setup Guide

This guide explains how to set up IPUMS API access for automated data downloads.

## Prerequisites

1. **IPUMS Account**: Register at [https://uma.pop.umn.edu/usa/user/new](https://uma.pop.umn.edu/usa/user/new)
2. **R Package**: The `ipumsr` package (automatically installed with this project)

## Getting Your API Key

1. Log in to your IPUMS account
2. Navigate to [https://account.ipums.org/api_keys](https://account.ipums.org/api_keys)
3. Click "Create new API key"
4. Copy the generated key (you won't be able to see it again!)

## Setting Up Your API Key

### Option 1: Save Permanently (Recommended)

In R, run:

```r
ipumsr::set_ipums_api_key("paste-your-key-here", save = TRUE)
```

This saves the key to your `.Renviron` file, making it available across all R sessions.

### Option 2: Set for Current Session

In R, run:

```r
Sys.setenv(IPUMS_API_KEY = "paste-your-key-here")
```

This only works for the current R session.

### Option 3: Manual .Renviron Setup

1. Edit your `.Renviron` file:
   ```r
   usethis::edit_r_environ()
   ```

2. Add this line:
   ```
   IPUMS_API_KEY=paste-your-key-here
   ```

3. Save and restart R

## Verifying Your Setup

Test your API key:

```r
# Check if key is set
Sys.getenv("IPUMS_API_KEY")

# Try a simple API call
ipumsr::get_sample_info("usa")
```

## Using the Download Function

Once your API key is set up:

```r
# Download real IPUMS data
result <- download_ipums_data(
  use_api = TRUE,
  samples = c("us2022a", "us2021a", "us2020a"),
  extract_description = "PhD unemployment time series"
)

# Read the data
data <- readRDS(result$file_path)
```

## Available IPUMS USA Samples

Common samples for unemployment analysis:

- `us2022a` - 2022 American Community Survey
- `us2021a` - 2021 American Community Survey
- `us2020a` - 2020 American Community Survey
- `us2019a` - 2019 American Community Survey

For Current Population Survey (CPS) monthly data:
- `cps2024_03s` - March 2024 CPS
- `cps2023_03s` - March 2023 CPS

View all available samples:

```r
ipumsr::get_sample_info("usa")
```

## Default Variables

The download function includes these variables by default:

- `YEAR` - Survey year
- `MONTH` - Survey month (for time series)
- `EMPSTAT` - Employment status
  - 1 = Employed
  - 2 = Unemployed
  - 3 = Not in labor force
- `EDUC` - Educational attainment
  - 116 = Doctorate degree
  - 115 = Professional degree
  - 114 = Master's degree
- `WTFINL` - Person weight for population estimates

## Customizing Your Extract

Specify different variables:

```r
result <- download_ipums_data(
  use_api = TRUE,
  samples = c("us2022a"),
  variables = c("YEAR", "MONTH", "EMPSTAT", "EDUC", "AGE", "SEX", "RACE", "WTFINL"),
  extract_description = "PhD unemployment by demographics"
)
```

## Extract Processing Time

- Small extracts (<10 variables, 1-2 samples): 2-5 minutes
- Medium extracts (5-10 samples): 5-15 minutes
- Large extracts (many years, many variables): 15-30 minutes

The function will wait automatically until your extract is ready.

## Troubleshooting

### "IPUMS API key not found" Error

Your API key isn't set. Follow the setup steps above.

### Extract Takes Too Long

Large extracts can take 30+ minutes. Consider:
- Reducing the number of samples
- Reducing the number of variables
- Running the download overnight for very large requests

### API Rate Limits

IPUMS may have rate limits. If you get rate limit errors:
- Wait a few minutes between requests
- Don't submit multiple large extracts simultaneously

## Data Storage and Updates

Downloaded data is saved to `data-raw/ipums_data.rds` by default.

To update your data:

```r
# Re-download with latest samples
result <- download_ipums_data(
  use_api = TRUE,
  samples = c("us2024a"),  # Latest year
  extract_description = "Updated PhD unemployment data"
)
```

## Security Notes

- **Never commit your API key** to version control
- The `.Renviron` file is in `.gitignore` by default
- Don't share your API key with others
- Each team member should get their own key

## Further Reading

- [IPUMS USA Documentation](https://usa.ipums.org/usa/)
- [ipumsr Package Documentation](https://tech.popdata.org/ipumsr/)
- [IPUMS API Documentation](https://developer.ipums.org/)
