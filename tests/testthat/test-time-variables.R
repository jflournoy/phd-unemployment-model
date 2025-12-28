test_that("monthly unemployment data has proper time variables", {
  # Load real CPS data
  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))
  phd_data <- filter_phd_holders(cps_data)
  monthly_rates <- aggregate_monthly_unemployment(phd_data, weight_var = "auto")

  # Should have year and month columns
  expect_true("year" %in% names(monthly_rates))
  expect_true("month" %in% names(monthly_rates))

  # Should have date column (Date class)
  expect_true("date" %in% names(monthly_rates))
  expect_s3_class(monthly_rates$date, "Date")

  # Should have time_index (sequential 1, 2, 3, ...)
  expect_true("time_index" %in% names(monthly_rates))
  expect_equal(monthly_rates$time_index, seq_len(nrow(monthly_rates)))

  # Date should be first day of each month
  # Extract month and year from date column
  date_months <- as.numeric(format(monthly_rates$date, "%m"))
  date_years <- as.numeric(format(monthly_rates$date, "%Y"))
  date_days <- as.numeric(format(monthly_rates$date, "%d"))

  # All dates should be first of month
  expect_true(all(date_days == 1))

  # Dates should match year and month columns
  expect_equal(date_months, monthly_rates$month)
  expect_equal(date_years, as.numeric(monthly_rates$year))  # Strip attributes
})

test_that("time variables are in correct sequence", {
  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))
  phd_data <- filter_phd_holders(cps_data)
  monthly_rates <- aggregate_monthly_unemployment(phd_data, weight_var = "auto")

  # Dates should be in chronological order
  expect_true(all(diff(monthly_rates$date) > 0))

  # Time index should be sequential with no gaps
  expect_equal(monthly_rates$time_index, 1:nrow(monthly_rates))

  # First observation should be January 2000
  expect_equal(monthly_rates$year[1], 2000)
  expect_equal(monthly_rates$month[1], 1)
  expect_equal(monthly_rates$date[1], as.Date("2000-01-01"))
  expect_equal(monthly_rates$time_index[1], 1)

  # Last observation should be in 2025 (month varies with data freshness)
  last_row <- nrow(monthly_rates)
  expect_equal(monthly_rates$year[last_row], 2025)
  expect_gte(monthly_rates$month[last_row], 1)  # At least January
  expect_lte(monthly_rates$month[last_row], 12)  # At most December

  # Date and time_index should be consistent with last row
  last_month <- monthly_rates$month[last_row]
  expected_date <- as.Date(paste("2025", last_month, "01", sep = "-"))
  expect_equal(monthly_rates$date[last_row], expected_date)
  expect_equal(monthly_rates$time_index[last_row], nrow(monthly_rates))
})

test_that("date column enables proper time series plotting", {
  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))
  phd_data <- filter_phd_holders(cps_data)
  monthly_rates <- aggregate_monthly_unemployment(phd_data, weight_var = "auto")

  # Date range should span full dataset
  date_range <- range(monthly_rates$date)
  expect_equal(date_range[1], as.Date("2000-01-01"))
  expect_gte(date_range[2], as.Date("2025-01-01"))  # At least 2025

  # Number of months between first and last date (at least 300 months = 25 years)
  months_elapsed <- as.numeric(difftime(date_range[2], date_range[1], units = "days")) / 30.44
  expect_gt(months_elapsed, 300)
  expect_lt(months_elapsed, 320)  # Allow for more months

  # Can calculate year fractions for plotting
  year_fraction <- monthly_rates$year + (monthly_rates$month - 0.5) / 12
  expect_gte(min(year_fraction), 2000)
  expect_lte(max(year_fraction), 2026)  # Allow for later months
})
