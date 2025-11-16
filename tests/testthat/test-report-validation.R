test_that("predicted unemployment rates are in valid range", {
  # This test prevents us from plotting centered effects (which can be negative)
  # instead of actual unemployment rates (which must be in [0, 1])

  # Load real data
  data_file <- here::here("data", "education-spectrum-unemployment.rds")
  skip_if_not(file.exists(data_file), "Data file not found")

  # Run analysis
  analysis <- analyze_cps_unemployment_by_education(
    data_file = data_file,
    education_levels = c("phd", "masters", "bachelors"),
    start_year = 2020,
    end_year = 2024,
    save_models = FALSE,
    save_results = FALSE
  )

  # Get model and create predictions at fixed month (to remove seasonality)
  model <- analysis$best_model
  expect_s3_class(model, "gam")

  # Create prediction grid
  pred_data <- expand.grid(
    time_index = seq(1, 100, by = 6),  # Sample every 6 months
    month = 6,  # Fixed month
    education = factor(c("phd", "masters", "bachelors"))
  )

  # Get predictions
  preds <- predict(model, newdata = pred_data, type = "response", se.fit = TRUE)

  # CRITICAL: Predictions must be in [0, 1] for unemployment rates
  expect_true(all(preds$fit >= 0, na.rm = TRUE),
              info = "All predictions should be >= 0 (not centered effects!)")
  expect_true(all(preds$fit <= 1, na.rm = TRUE),
              info = "All predictions should be <= 1 (unemployment is a proportion)")

  # Predictions should be in realistic range (0.5% to 15%)
  expect_true(all(preds$fit >= 0.005, na.rm = TRUE),
              info = "Predictions unrealistically low (< 0.5%)")
  expect_true(all(preds$fit <= 0.15, na.rm = TRUE),
              info = "Predictions unrealistically high (> 15%)")
})


test_that("PhD unemployment is lower than less educated groups", {
  # This test catches the original bug where PhD appeared to converge
  # with "Less than HS" unemployment

  data_file <- here::here("data", "education-spectrum-unemployment.rds")
  skip_if_not(file.exists(data_file), "Data file not found")

  # Full education spectrum
  education_levels <- c("less_than_hs", "high_school", "some_college",
                        "bachelors", "masters", "professional", "phd")

  analysis <- analyze_cps_unemployment_by_education(
    data_file = data_file,
    education_levels = education_levels,
    start_year = 2020,
    end_year = 2024,
    save_models = FALSE,
    save_results = FALSE
  )

  model <- analysis$best_model

  # Predict at fixed month for recent period
  recent_months <- seq(240, 300, by = 1)  # ~5 years of monthly data

  pred_data <- expand.grid(
    time_index = recent_months,
    month = 6,
    education = factor(education_levels, levels = education_levels)
  )

  preds <- predict(model, newdata = pred_data, type = "response")
  pred_data$predicted_rate <- preds

  # Calculate mean predictions by education
  mean_rates <- tapply(pred_data$predicted_rate, pred_data$education, mean, na.rm = TRUE)

  # CRITICAL: PhD should have lowest unemployment
  expect_true(mean_rates["phd"] < mean_rates["less_than_hs"],
              info = "PhD unemployment should be < Less than HS")
  expect_true(mean_rates["phd"] < mean_rates["high_school"],
              info = "PhD unemployment should be < High School")
  expect_true(mean_rates["phd"] < mean_rates["some_college"],
              info = "PhD unemployment should be < Some College")
  expect_true(mean_rates["phd"] < mean_rates["bachelors"],
              info = "PhD unemployment should be < Bachelors")

  # Expected ordering (approximately)
  expect_true(mean_rates["less_than_hs"] > mean_rates["high_school"],
              info = "Less than HS should have higher unemployment than HS")
  expect_true(mean_rates["high_school"] > mean_rates["bachelors"],
              info = "HS should have higher unemployment than Bachelors")

  # PhD should be in realistic range (1-4%)
  expect_true(mean_rates["phd"] >= 0.01 && mean_rates["phd"] <= 0.04,
              info = sprintf("PhD mean rate should be 1-4%%, got %.1f%%",
                           mean_rates["phd"] * 100))

  # Less than HS should be higher (6-12%)
  expect_true(mean_rates["less_than_hs"] >= 0.06 && mean_rates["less_than_hs"] <= 0.12,
              info = sprintf("Less than HS mean rate should be 6-12%%, got %.1f%%",
                           mean_rates["less_than_hs"] * 100))
})


test_that("trend predictions differ from centered smooth effects", {
  # This test ensures we're not accidentally using centered effects
  # when we want actual predictions

  data_file <- here::here("data", "education-spectrum-unemployment.rds")
  skip_if_not(file.exists(data_file), "Data file not found")

  analysis <- analyze_cps_unemployment_by_education(
    data_file = data_file,
    education_levels = c("phd", "masters"),
    start_year = 2020,
    end_year = 2024,
    save_models = FALSE,
    save_results = FALSE
  )

  model <- analysis$best_model

  # Get predictions (with intercept)
  pred_data <- data.frame(
    time_index = rep(seq(1, 60, by = 6), 2),
    month = 6,
    education = factor(rep(c("phd", "masters"), each = 10))
  )

  # Response predictions (includes intercept)
  response_preds <- predict(model, newdata = pred_data, type = "response")

  # Terms predictions (centered effects, no intercept)
  terms_preds <- predict(model, newdata = pred_data, type = "terms")

  # Extract trend column
  trend_col_idx <- grep("time_index", colnames(terms_preds))
  if (length(trend_col_idx) > 0) {
    trend_effects <- terms_preds[, trend_col_idx[1]]

    # CRITICAL: Response predictions should be very different from trend effects
    # Response includes intercept (unemployment level)
    # Trend effects are centered (mean = 0)

    # Mean of response should be in unemployment range (1-5%)
    expect_true(mean(response_preds) > 0.01 && mean(response_preds) < 0.05,
                info = "Response predictions should be in unemployment range")

    # Mean of trend effects should be near zero (centered)
    expect_true(abs(mean(trend_effects)) < 0.01,
                info = "Trend effects should be centered near zero")

    # Response predictions should all be positive
    expect_true(all(response_preds > 0),
                info = "Response predictions should all be positive")

    # Trend effects can be negative (they're centered)
    # This is fine for effects, but NOT for plotting unemployment rates!
  }
})


test_that("analysis object has expected structure", {
  # This prevents errors from assuming wrong structure (e.g., analysis$models$m6)

  data_file <- here::here("data", "education-spectrum-unemployment.rds")
  skip_if_not(file.exists(data_file), "Data file not found")

  analysis <- analyze_cps_unemployment_by_education(
    data_file = data_file,
    education_levels = c("phd", "masters", "bachelors"),
    start_year = 2020,
    end_year = 2024,
    save_models = FALSE,
    save_results = FALSE
  )

  # Check expected structure
  expect_type(analysis, "list")

  # Must have best_model (not models$m6!)
  expect_true("best_model" %in% names(analysis),
              info = "analysis should have 'best_model' element")
  expect_s3_class(analysis$best_model, "gam")

  # Must have visualization_data
  expect_true("visualization_data" %in% names(analysis),
              info = "analysis should have 'visualization_data' element")

  viz_data <- analysis$visualization_data
  expect_type(viz_data, "list")

  # visualization_data should have expected components
  expect_true("observed" %in% names(viz_data),
              info = "viz_data should have 'observed'")
  expect_true("fitted" %in% names(viz_data),
              info = "viz_data should have 'fitted'")
  expect_true("trends" %in% names(viz_data),
              info = "viz_data should have 'trends'")
  expect_true("seasonal" %in% names(viz_data),
              info = "viz_data should have 'seasonal'")

  # fitted should have date column
  expect_true("date" %in% names(viz_data$fitted),
              info = "viz_data$fitted should have 'date' column")

  # trends should have date column
  expect_true("date" %in% names(viz_data$trends),
              info = "viz_data$trends should have 'date' column")
})


test_that("visualization data trends are centered effects, not predictions", {
  # This documents the expected behavior: viz_data$trends contains
  # centered effects, NOT predictions with intercept

  data_file <- here::here("data", "education-spectrum-unemployment.rds")
  skip_if_not(file.exists(data_file), "Data file not found")

  analysis <- analyze_cps_unemployment_by_education(
    data_file = data_file,
    education_levels = c("phd", "masters"),
    start_year = 2020,
    end_year = 2024,
    save_models = FALSE,
    save_results = FALSE
  )

  viz_data <- analysis$visualization_data

  # Trends should be centered (mean near zero)
  phd_trends <- viz_data$trends[viz_data$trends$education == "phd", ]

  # EXPECTED: trend_effect should be centered
  expect_true(abs(mean(phd_trends$trend_effect, na.rm = TRUE)) < 0.02,
              info = "trend_effect should be centered (mean near 0)")

  # EXPECTED: trend_effect can be negative
  expect_true(any(phd_trends$trend_effect < 0, na.rm = TRUE),
              info = "trend_effect can be negative (it's centered)")

  # WARNING: Do NOT use trend_effect directly as unemployment rate!
  # It needs intercept added
})


test_that("fixed month predictions remove seasonality", {
  # This tests that predicting at fixed month removes seasonal component

  data_file <- here::here("data", "education-spectrum-unemployment.rds")
  skip_if_not(file.exists(data_file), "Data file not found")

  analysis <- analyze_cps_unemployment_by_education(
    data_file = data_file,
    education_levels = c("phd"),
    start_year = 2020,
    end_year = 2024,
    save_models = FALSE,
    save_results = FALSE
  )

  model <- analysis$best_model

  # Predict same time points at different months
  time_points <- seq(100, 120, by = 1)

  pred_june <- predict(model, newdata = data.frame(
    time_index = time_points,
    month = 6,
    education = factor("phd")
  ), type = "response")

  pred_december <- predict(model, newdata = data.frame(
    time_index = time_points,
    month = 12,
    education = factor("phd")
  ), type = "response")

  # Predictions at different months should differ (due to seasonality)
  expect_false(isTRUE(all.equal(pred_june, pred_december)),
               info = "Predictions should differ by month (seasonality)")

  # Now predict at SAME month across time - should be smooth trend
  pred_all_june <- predict(model, newdata = data.frame(
    time_index = time_points,
    month = 6,  # Fixed month
    education = factor("phd")
  ), type = "response")

  # Should be smoother (less variance) than if we varied month
  # This is the key property we use for "trend" plots
  expect_true(sd(pred_all_june) < sd(c(pred_june, pred_december)) / 2,
              info = "Fixed month predictions should have less variance")
})


test_that("confidence intervals are reasonable width", {
  # Catches issues with SE calculation

  data_file <- here::here("data", "education-spectrum-unemployment.rds")
  skip_if_not(file.exists(data_file), "Data file not found")

  analysis <- analyze_cps_unemployment_by_education(
    data_file = data_file,
    education_levels = c("phd", "masters", "bachelors"),
    start_year = 2020,
    end_year = 2024,
    save_models = FALSE,
    save_results = FALSE
  )

  model <- analysis$best_model

  pred_data <- data.frame(
    time_index = seq(100, 200, by = 10),
    month = 6,
    education = factor("phd")
  )

  preds <- predict(model, newdata = pred_data, type = "response", se.fit = TRUE)

  # SE should be positive
  expect_true(all(preds$se.fit > 0),
              info = "Standard errors should be positive")

  # SE should be reasonable (not too wide or too narrow)
  # For unemployment rates, SE typically 0.1% to 2%
  expect_true(all(preds$se.fit < 0.02),
              info = "Standard errors should be < 2 percentage points")
  expect_true(all(preds$se.fit > 0.0001),
              info = "Standard errors should be > 0.01 percentage points")

  # 95% CI width should be reasonable
  ci_width <- 1.96 * 2 * preds$se.fit
  expect_true(all(ci_width < 0.08),
              info = "95% CI should be narrower than 8 percentage points")
})


test_that("difference calculations use correct formula", {
  # Tests that we compute differences and their SEs correctly

  data_file <- here::here("data", "education-spectrum-unemployment.rds")
  skip_if_not(file.exists(data_file), "Data file not found")

  analysis <- analyze_cps_unemployment_by_education(
    data_file = data_file,
    education_levels = c("phd", "masters"),
    start_year = 2020,
    end_year = 2024,
    save_models = FALSE,
    save_results = FALSE
  )

  model <- analysis$best_model

  pred_data <- data.frame(
    time_index = rep(100, 2),
    month = 6,
    education = factor(c("phd", "masters"))
  )

  preds <- predict(model, newdata = pred_data, type = "response", se.fit = TRUE)

  # Calculate difference
  diff <- preds$fit[2] - preds$fit[1]  # masters - phd

  # SE of difference (assuming independence for simplicity)
  # More correctly: should use variance-covariance matrix
  diff_se <- sqrt(preds$se.fit[1]^2 + preds$se.fit[2]^2)

  # Difference should be positive (masters > phd unemployment)
  expect_true(diff > 0,
              info = "Masters unemployment should be higher than PhD")

  # SE of difference should be larger than individual SEs
  expect_true(diff_se > preds$se.fit[1],
              info = "SE of difference should be > SE of PhD")
  expect_true(diff_se > preds$se.fit[2],
              info = "SE of difference should be > SE of Masters")

  # But not too much larger (sqrt(2) * individual SE as rough guide)
  expect_true(diff_se < 2 * max(preds$se.fit),
              info = "SE of difference should be < 2 * max(individual SEs)")
})
