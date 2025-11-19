#' Tests for Education Level Unemployment Comparison
#'
#' TDD RED Phase: Tests for comparing unemployment rates across educational
#' attainment strata using GAM seasonal models

library(testthat)

# Source required functions
source(here::here("R", "seasonal-gam.R"))
source(here::here("R", "data-processing.R"))
source(here::here("R", "education-comparison.R"))

test_that("compare_unemployment_by_education function exists", {
  expect_true(exists("compare_unemployment_by_education", mode = "function"))
})

test_that("compare_unemployment_by_education accepts data and education levels", {
  # This test will fail until we implement the function
  skip("Function not yet implemented")

  # Load real CPS data
  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  # Should accept CPS data and education codes
  result <- compare_unemployment_by_education(
    data = cps_data,
    education_levels = c(125, 123, 111)  # PhD, Masters, Bachelors
  )

  expect_type(result, "list")
})

test_that("compare_unemployment_by_education returns data for each education level", {
  skip("Function not yet implemented")

  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  result <- compare_unemployment_by_education(
    data = cps_data,
    education_levels = c(125, 123, 111)
  )

  expect_true("phd" %in% names(result))
  expect_true("masters" %in% names(result))
  expect_true("bachelors" %in% names(result))
})

test_that("compare_unemployment_by_education fits GAM for each education level", {
  skip("Function not yet implemented")

  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  result <- compare_unemployment_by_education(
    data = cps_data,
    education_levels = c(125, 123)  # PhD, Masters
  )

  # Should have monthly data for each level
  expect_true("monthly_data" %in% names(result$phd))
  expect_true("monthly_data" %in% names(result$masters))

  # Should have fitted GAM models
  expect_true("model" %in% names(result$phd))
  expect_true(inherits(result$phd$model, "gam"))
})

test_that("compare_unemployment_by_education extracts seasonal and trend components", {
  skip("Function not yet implemented")

  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  result <- compare_unemployment_by_education(
    data = cps_data,
    education_levels = c(125, 123)
  )

  # Should have seasonal components
  expect_true("seasonal" %in% names(result$phd))
  expect_equal(nrow(result$phd$seasonal), 12)  # 12 months

  # Should have trend components
  expect_true("trend" %in% names(result$phd))
  expect_gt(nrow(result$phd$trend), 0)
})

test_that("plot_education_comparison function exists", {
  expect_true(exists("plot_education_comparison", mode = "function"))
})

test_that("plot_education_comparison creates comparison plots", {
  skip("Function not yet implemented")

  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  comparison_result <- compare_unemployment_by_education(
    data = cps_data,
    education_levels = c(125, 123, 111)
  )

  plots <- plot_education_comparison(comparison_result)

  # Should return list of ggplot objects
  expect_type(plots, "list")
  expect_true(all(sapply(plots, function(p) inherits(p, "gg"))))

  # Should have key plots
  expect_true("timeseries" %in% names(plots))
  expect_true("seasonal" %in% names(plots))
  expect_true("trend" %in% names(plots))
})

test_that("compare_unemployment_by_education handles missing education codes gracefully", {
  skip("Function not yet implemented")

  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  # Should handle missing data for an education level
  result <- compare_unemployment_by_education(
    data = cps_data,
    education_levels = c(125, 999)  # 999 doesn't exist
  )

  expect_true("phd" %in% names(result))
  expect_false("999" %in% names(result))
})

test_that("compare_unemployment_by_education uses proper CPS weights", {
  skip("Function not yet implemented")

  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  result <- compare_unemployment_by_education(
    data = cps_data,
    education_levels = c(125)
  )

  # Verify weighted unemployment rate calculation
  # Should use WTFINL for regular months, ASECWT for March
  monthly_data <- result$phd$monthly_data

  expect_true("unemployment_rate" %in% names(monthly_data))
  expect_true(all(monthly_data$unemployment_rate >= 0))
  expect_true(all(monthly_data$unemployment_rate <= 1))
})

test_that("plot_education_comparison uses colors keyed to education labels, not internal names", {
  # TDD REFACTOR: Failing test to catch color mapping bug
  # Bug: colors are keyed to "phd", "masters", "bachelors" but plots use
  # "Doctorate Degree", "Master's Degree", "Bachelor's Degree"

  cps_data <- readRDS(here::here("data-raw", "ipums_data.rds"))

  comparison_result <- compare_unemployment_by_education(
    data = cps_data,
    education_levels = c(phd = 125, masters = 123, bachelors = 111)
  )

  # Custom palette - distinct colors to track if mapping works
  test_palette <- c("#FF0000", "#00FF00", "#0000FF")

  plots <- plot_education_comparison(comparison_result, palette = test_palette)

  # Verify all three plots use education labels in data
  for (plot_name in c("timeseries", "seasonal", "trend")) {
    plot_obj <- plots[[plot_name]]
    expect_true(inherits(plot_obj, "gg"), info = paste(plot_name, "should be a ggplot object"))

    # Check data uses education labels
    plot_data <- plot_obj$data
    if ("education" %in% names(plot_data)) {
      unique_education <- unique(plot_data$education)

      # Should use full labels from IPUMS codes, not internal names
      expected_labels <- c("Doctoral degree (PhD, EdD, etc.)",
                          "Master's degree (MA, MS, MEng, MEd, MSW, MBA, etc.)",
                          "Bachelor's degree")
      expect_true(all(unique_education %in% expected_labels),
                  info = paste(plot_name, "should use education labels from IPUMS codes, not internal names like 'phd'"))

      expect_false(any(unique_education %in% c("phd", "masters", "bachelors")),
                   info = paste(plot_name, "should not use internal names like 'phd'"))
    }
  }

  # Test that colors are actually being applied
  # Build the timeseries plot to ensure layers are generated
  ts_plot <- plots$timeseries
  built_plot <- ggplot2::ggplot_build(ts_plot)

  # Check that the plot has data with colors assigned
  expect_true(length(built_plot$data) > 0, info = "Plot should have data layers")

  # The first layer should have color aesthetic mapped
  layer_data <- built_plot$data[[1]]
  expect_true("colour" %in% names(layer_data), info = "Plot should have color aesthetic")

  # Verify that multiple distinct colors are used (one per education level)
  unique_colors <- unique(layer_data$colour)
  expect_equal(length(unique_colors), 3, info = "Should have 3 distinct colors for 3 education levels")
})
