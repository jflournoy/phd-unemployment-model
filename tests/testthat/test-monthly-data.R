test_that("calculate_monthly_unemployment uses WTFINL for non-March months", {
  # Load real CPS data
  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  # Filter to PhD holders
  phd_data <- filter_phd_holders(cps_data)

  # Calculate monthly unemployment
  monthly_rates <- calculate_monthly_unemployment(phd_data)

  # Data spans 2000-2025 (through August 2025)
  # That's 25 complete years (2000-2024) plus 8 months (2025 Jan-Aug)
  expected_observations <- 25 * 12 + 8  # 308
  expect_equal(nrow(monthly_rates), expected_observations)

  # Count non-NA unemployment rates
  non_na_count <- sum(!is.na(monthly_rates$unemployment_rate))

  # Should have unemployment rates for ALL available months, not just March
  expect_equal(non_na_count, expected_observations)

  # Verify we have all 12 months represented in non-NA data
  months_with_data <- unique(monthly_rates$MONTH[!is.na(monthly_rates$unemployment_rate)])
  expect_equal(sort(months_with_data), 1:12)

  # Check that each complete year (2000-2024) has 12 months of data
  complete_years <- monthly_rates[monthly_rates$YEAR >= 2000 & monthly_rates$YEAR <= 2024, ]
  year_month_counts <- aggregate(
    !is.na(unemployment_rate) ~ YEAR,
    data = complete_years,
    FUN = sum
  )
  names(year_month_counts)[2] <- "months_with_data"

  # Every complete year should have all 12 months
  expect_true(all(year_month_counts$months_with_data == 12))

  # Check 2025 has 8 months (Jan-Aug)
  year_2025 <- monthly_rates[monthly_rates$YEAR == 2025, ]
  expect_equal(nrow(year_2025), 8)
  expect_equal(sort(year_2025$MONTH), 1:8)
})

test_that("calculate_monthly_unemployment uses ASECWT for March, WTFINL for other months", {
  # Load real CPS data
  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))
  phd_data <- filter_phd_holders(cps_data)

  # Get monthly unemployment rates
  monthly_rates <- calculate_monthly_unemployment(phd_data)

  # Check that we have data for both March (ASECWT) and non-March (WTFINL) months
  march_data <- monthly_rates[monthly_rates$MONTH == 3 & !is.na(monthly_rates$unemployment_rate), ]
  non_march_data <- monthly_rates[monthly_rates$MONTH != 3 & !is.na(monthly_rates$unemployment_rate), ]

  # Should have March data (25 complete years + 2025)
  expect_equal(nrow(march_data), 26)

  # Should have non-March data
  # 25 complete years (2000-2024) * 11 non-March months = 275
  # Plus 2025 Jan, Feb, Apr-Aug = 7 months
  # Total: 282
  expect_equal(nrow(non_march_data), 282)

  # All unemployment rates should be valid (between 0 and 1)
  expect_true(all(monthly_rates$unemployment_rate >= 0, na.rm = TRUE))
  expect_true(all(monthly_rates$unemployment_rate <= 1, na.rm = TRUE))
})

test_that("monthly unemployment rates are reasonable across all months", {
  # Load real CPS data
  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))
  phd_data <- filter_phd_holders(cps_data)
  monthly_rates <- calculate_monthly_unemployment(phd_data)

  # Remove NA values
  valid_rates <- monthly_rates[!is.na(monthly_rates$unemployment_rate), ]

  # Mean unemployment rate should be reasonable (0.5% - 5%)
  mean_rate <- mean(valid_rates$unemployment_rate)
  expect_gt(mean_rate, 0.005)
  expect_lt(mean_rate, 0.05)

  # Should have observations in each month
  expect_equal(length(unique(valid_rates$MONTH)), 12)

  # Sample size should be reasonable for PhD holders
  # (at least 100 per month on average)
  expect_gt(mean(valid_rates$n_obs), 100)
})
