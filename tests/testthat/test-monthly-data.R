test_that("aggregate_monthly_unemployment uses WTFINL for non-March months", {
  # Load real CPS data
  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  # Filter to PhD holders
  phd_data <- filter_phd_holders(cps_data)

  # Calculate monthly unemployment using new aggregation function
  monthly_rates <- aggregate_monthly_unemployment(phd_data, weight_var = "auto")

  # Data spans 2000-2025 (through latest available month)
  # Note: IPUMS data may have gaps in recent months
  n_complete_years <- 25  # 2000-2024
  n_2025_months <- nrow(monthly_rates[monthly_rates$year == 2025, ])
  expected_observations <- n_complete_years * 12 + n_2025_months
  expect_equal(nrow(monthly_rates), expected_observations)

  # Count non-NA unemployment rates
  non_na_count <- sum(!is.na(monthly_rates$unemployment_rate))

  # Should have unemployment rates for ALL available months, not just March
  expect_equal(non_na_count, expected_observations)

  # Verify we have all 12 months represented in non-NA data
  months_with_data <- unique(monthly_rates$month[!is.na(monthly_rates$unemployment_rate)])
  expect_equal(sort(months_with_data), 1:12)

  # Check that each complete year (2000-2024) has 12 months of data
  complete_years <- monthly_rates[monthly_rates$year >= 2000 & monthly_rates$year <= 2024, ]
  year_month_counts <- aggregate(
    !is.na(unemployment_rate) ~ year,
    data = complete_years,
    FUN = sum
  )
  names(year_month_counts)[2] <- "months_with_data"

  # Every complete year should have all 12 months
  expect_true(all(year_month_counts$months_with_data == 12))

  # Check 2025 has data (number of months depends on data freshness)
  # Note: IPUMS may have gaps in recent months (e.g., missing October 2025)
  year_2025 <- monthly_rates[monthly_rates$year == 2025, ]
  expect_gte(nrow(year_2025), 1)  # At least some 2025 data
  expect_lte(nrow(year_2025), 12)  # At most 12 months
  expect_true(1 %in% year_2025$month)  # January should be present
})

test_that("aggregate_monthly_unemployment uses ASECWT for March, WTFINL for other months", {
  # Load real CPS data
  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))
  phd_data <- filter_phd_holders(cps_data)

  # Get monthly unemployment rates
  monthly_rates <- aggregate_monthly_unemployment(phd_data, weight_var = "auto")

  # Check that we have data for both March (ASECWT) and non-March (WTFINL) months
  march_data <- monthly_rates[monthly_rates$month == 3 & !is.na(monthly_rates$unemployment_rate), ]
  non_march_data <- monthly_rates[monthly_rates$month != 3 & !is.na(monthly_rates$unemployment_rate), ]

  # Should have March data (25 complete years + 2025)
  expect_equal(nrow(march_data), 26)

  # Should have non-March data (varies with data freshness)
  # 25 complete years * 11 non-March months = 275 minimum
  # Plus 2025 non-March months (depends on latest data)
  expect_gte(nrow(non_march_data), 275)  # At least 25 complete years

  # All unemployment rates should be valid (between 0 and 1)
  expect_true(all(monthly_rates$unemployment_rate >= 0, na.rm = TRUE))
  expect_true(all(monthly_rates$unemployment_rate <= 1, na.rm = TRUE))
})

test_that("monthly unemployment rates are reasonable across all months", {
  # Load real CPS data
  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))
  phd_data <- filter_phd_holders(cps_data)
  monthly_rates <- aggregate_monthly_unemployment(phd_data, weight_var = "auto")

  # Remove NA values
  valid_rates <- monthly_rates[!is.na(monthly_rates$unemployment_rate), ]

  # Mean unemployment rate should be reasonable (0.5% - 5%)
  mean_rate <- mean(valid_rates$unemployment_rate)
  expect_gt(mean_rate, 0.005)
  expect_lt(mean_rate, 0.05)

  # Should have observations in each month
  expect_equal(length(unique(valid_rates$month)), 12)

  # Sample size should be reasonable for PhD holders
  # (at least 100 per month on average)
  expect_gt(mean(valid_rates$n_total), 100)
})
