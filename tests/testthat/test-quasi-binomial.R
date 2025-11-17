# Test Suite: Quasi-Binomial Family with CPS-Style Weights
# Date: 2025-11-17
# TDD Phase: RED → GREEN
# Related: Issue #13, docs/phase1-tdd-tests.md

# ==============================================================================
# RED PHASE TESTS (Tests 1.1 - 1.8)
# ==============================================================================

test_that("fit_seasonal_gam accepts cbind(n_unemployed, n_employed) response", {
  set.seed(123)

  # Simulate aggregated monthly data
  test_data <- data.frame(
    time_index = 1:60,
    month = rep(1:12, 5),
    year = rep(2020:2024, each = 12),
    n_unemployed = rpois(60, lambda = 50),
    n_employed = rpois(60, lambda = 950)
  )

  model <- fit_seasonal_gam(
    test_data,
    response_formula = quote(cbind(n_unemployed, n_employed)),
    family = "binomial"
  )

  expect_s3_class(model, "gam")
  expect_equal(model$family$family, "binomial")
})

test_that("cbind format recovers known latent probability", {
  set.seed(456)

  # All observations have true p = 0.10, varying sample sizes
  test_data <- data.frame(
    time_index = 1:30,
    month = rep(1:6, 5),
    n_unemployed = c(10, 20, 5, 15, 8, 25),  # Different counts
    n_employed = c(90, 180, 45, 135, 72, 225) # but same p=0.10
  )

  model <- fit_seasonal_gam(
    test_data,
    response_formula = quote(cbind(n_unemployed, n_employed)),
    family = "binomial",
    k_trend = 5,  # Low k for simple intercept-only test
    k_month = 6
  )

  preds <- predict(model, type = "response")

  # Should recover p ≈ 0.10 for all observations
  expect_true(all(abs(preds - 0.10) < 0.03),
              info = "Should recover latent probability within ±3pp tolerance")
  expect_equal(mean(preds), 0.10, tolerance = 0.02,
              info = "Mean prediction should be close to true p=0.10")
})

test_that("recovers different latent probabilities correctly", {
  set.seed(789)

  # First half: p=0.05, second half: p=0.20
  n1 <- 30
  test_data <- data.frame(
    time_index = 1:60,
    month = rep(1:12, 5)
  )

  # First 30: low unemployment (p=0.05)
  test_data$n_unemployed[1:n1] <- rbinom(n1, size = 100, prob = 0.05)
  test_data$n_employed[1:n1] <- 100 - test_data$n_unemployed[1:n1]

  # Last 30: high unemployment (p=0.20)
  test_data$n_unemployed[(n1+1):60] <- rbinom(30, size = 100, prob = 0.20)
  test_data$n_employed[(n1+1):60] <- 100 - test_data$n_unemployed[(n1+1):60]

  model <- fit_seasonal_gam(
    test_data,
    response_formula = quote(cbind(n_unemployed, n_employed)),
    family = "binomial",
    k_trend = 10
  )

  preds <- predict(model, type = "response")

  # First half should be near 0.05
  expect_true(mean(preds[1:n1]) < 0.10,
              info = "Low unemployment period should have predictions < 0.10")

  # Second half should be near 0.20
  expect_true(mean(preds[(n1+1):60]) > 0.15,
              info = "High unemployment period should have predictions > 0.15")

  # Should differ substantially
  expect_true(mean(preds[(n1+1):60]) - mean(preds[1:n1]) > 0.10,
              info = "Should detect 15pp difference between periods")
})

test_that("standard errors inversely proportional to sqrt(sample size)", {
  set.seed(1011)

  # Same p=0.05 but different sample sizes
  test_data <- data.frame(
    time_index = 1:60,
    month = rep(1:12, 5)
  )

  # Three groups: n=100, n=1000, n=10000
  sample_sizes <- rep(c(100, 1000, 10000), each = 20)
  true_p <- 0.05

  test_data$n_unemployed <- rbinom(60, size = sample_sizes, prob = true_p)
  test_data$n_employed <- sample_sizes - test_data$n_unemployed

  model <- fit_seasonal_gam(
    test_data,
    response_formula = quote(cbind(n_unemployed, n_employed)),
    family = "binomial",
    k_trend = 5,
    k_month = 6
  )

  preds <- predict(model, type = "response", se.fit = TRUE)

  # Group by sample size
  se_n100 <- mean(preds$se.fit[sample_sizes == 100])
  se_n1000 <- mean(preds$se.fit[sample_sizes == 1000])
  se_n10000 <- mean(preds$se.fit[sample_sizes == 10000])

  # SE should decrease with sqrt(n)
  # sqrt(10) ≈ 3.16, so expect ratio between 1.5 and 5 (allowing GAM smoothing effects)
  se_ratio_1000_100 <- se_n100 / se_n1000
  se_ratio_10000_1000 <- se_n1000 / se_n10000

  expect_true(se_ratio_1000_100 > 1.5 && se_ratio_1000_100 < 5,
              info = paste("SE ratio (100 vs 1000):", round(se_ratio_1000_100, 2),
                          "should be 1.5-5"))
  expect_true(se_ratio_10000_1000 > 1.5 && se_ratio_10000_1000 < 5,
              info = paste("SE ratio (1000 vs 10000):", round(se_ratio_10000_1000, 2),
                          "should be 1.5-5"))
})

test_that("binomial predictions never violate [0,1] bounds", {
  set.seed(1213)

  # Extreme rates and sample sizes to stress-test
  test_data <- data.frame(
    time_index = 1:100,
    month = rep(1:12, length.out = 100)
  )

  # Mix of very low and very high unemployment rates
  rates <- runif(100, min = 0.01, max = 0.25)
  sample_sizes <- rpois(100, lambda = 1500)
  sample_sizes <- pmax(sample_sizes, 50)  # Min 50

  test_data$n_unemployed <- rbinom(100, size = sample_sizes, prob = rates)
  test_data$n_employed <- sample_sizes - test_data$n_unemployed

  model <- fit_seasonal_gam(
    test_data,
    response_formula = quote(cbind(n_unemployed, n_employed)),
    family = "binomial"
  )

  preds <- predict(model, type = "response")

  expect_true(all(preds >= 0, na.rm = TRUE),
              info = "All predictions should be >= 0")
  expect_true(all(preds <= 1, na.rm = TRUE),
              info = "All predictions should be <= 1")
})

test_that("observations with larger sample sizes have more influence", {
  set.seed(1415)

  test_data <- data.frame(
    time_index = 1:100,
    month = rep(1:12, length.out = 100)
  )

  # First 50: high sample size (n≈5000), low unemployment (p≈0.03)
  # Last 50: low sample size (n≈100), high unemployment (p≈0.08)
  test_data$n_total <- c(rep(5000, 50), rep(100, 50))
  test_data$n_unemployed <- c(
    rbinom(50, size = 5000, prob = 0.03),
    rbinom(50, size = 100, prob = 0.08)
  )
  test_data$n_employed <- test_data$n_total - test_data$n_unemployed

  model <- fit_seasonal_gam(
    test_data,
    response_formula = quote(cbind(n_unemployed, n_employed)),
    family = "binomial",
    k_trend = 10
  )

  preds <- predict(model, type = "response")

  # High sample size observations (1:50) should be fitted closely
  true_rates <- test_data$n_unemployed / test_data$n_total
  mae_high_n <- mean(abs(preds[1:50] - true_rates[1:50]))

  expect_true(mae_high_n < 0.015,
              info = "High sample size obs should be fitted within 1.5pp")

  # Overall prediction should be pulled toward high-n observations
  expect_true(mean(preds) < 0.055,
              info = "Mean prediction closer to high-n rate (0.03) than low-n (0.08)")
})

test_that("CPS survey weight aggregation produces correct binomial counts", {
  set.seed(1617)

  # Simulate individual-level CPS data with survey weights
  individual_data <- data.frame(
    year = rep(2020:2022, each = 1200),
    month = rep(rep(1:12, each = 100), 3),
    is_unemployed = rbinom(3600, size = 1, prob = 0.04),
    survey_weight = rpois(3600, lambda = 1000)
  )

  # Aggregate to monthly counts (like real CPS processing)
  library(data.table)
  dt <- as.data.table(individual_data)
  monthly_data <- dt[, .(
    n_unemployed = sum(survey_weight * is_unemployed),
    n_employed = sum(survey_weight * (1 - is_unemployed)),
    time_index = .GRP
  ), by = .(year, month)]

  monthly_data <- as.data.frame(monthly_data)

  model <- fit_seasonal_gam(
    monthly_data,
    response_formula = quote(cbind(n_unemployed, n_employed)),
    family = "binomial",
    k_trend = 10,
    k_month = 12
  )

  preds <- predict(model, type = "response")

  expect_equal(mean(preds), 0.04, tolerance = 0.08,
              info = "Should recover aggregated unemployment rate within 8%")

  expect_true(all(preds > 0.002 & preds < 0.15),
              info = "Predictions in reasonable range around 4%")
})

test_that("parameter recovery improves with proper binomial specification", {
  set.seed(1819)

  # True parameters
  true_baseline <- 0.04
  true_seasonal_amplitude <- 0.01

  test_data <- data.frame(
    time_index = 1:120,
    month = rep(1:12, 10),
    year = rep(2015:2024, each = 12)
  )

  # True rate varies seasonally
  true_rate <- true_baseline +
    true_seasonal_amplitude * sin(2 * pi * test_data$month / 12)

  # Realistic CPS-style sample sizes (varying 800-3000 per month)
  test_data$n_total <- rpois(120, lambda = 1800)
  test_data$n_total <- pmax(test_data$n_total, 500)

  # Generate binomial counts
  test_data$n_unemployed <- rbinom(120,
                                    size = test_data$n_total,
                                    prob = true_rate)
  test_data$n_employed <- test_data$n_total - test_data$n_unemployed

  model <- fit_seasonal_gam(
    test_data,
    response_formula = quote(cbind(n_unemployed, n_employed)),
    family = "binomial",
    k_trend = 5,
    k_month = 10
  )

  # Predict at baseline month
  newdata <- data.frame(time_index = 60, month = 6)
  pred_baseline <- predict(model, newdata = newdata, type = "response")

  expect_equal(as.numeric(pred_baseline), true_baseline, tolerance = 0.015,
              info = "Should recover baseline within 1.5pp")

  # Check seasonal amplitude recovery
  preds_by_month <- sapply(1:12, function(m) {
    predict(model, newdata = data.frame(time_index = 60, month = m),
            type = "response")
  })

  recovered_amplitude <- (max(preds_by_month) - min(preds_by_month)) / 2
  expect_equal(recovered_amplitude, true_seasonal_amplitude, tolerance = 0.01,
              info = "Should recover seasonal amplitude within 1.0pp")
})

# ==============================================================================
# GREEN PHASE TESTS (Tests 1.9 - 1.12)
# Validation tests to ensure implementation works correctly
# ==============================================================================

test_that("binomial family produces different fit than Gaussian", {
  set.seed(2021)

  test_data <- data.frame(
    time_index = 1:60,
    month = rep(1:12, 5),
    n_unemployed = rbinom(60, size = 1000, prob = 0.05)
  )
  test_data$n_employed <- 1000 - test_data$n_unemployed
  test_data$unemployment_rate <- test_data$n_unemployed / 1000

  model_binomial <- fit_seasonal_gam(
    test_data,
    response_formula = quote(cbind(n_unemployed, n_employed)),
    family = "binomial"
  )

  model_gaussian <- fit_seasonal_gam(
    test_data,
    response_formula = quote(unemployment_rate),
    family = "gaussian"
  )

  preds_binom <- predict(model_binomial, type = "response")
  preds_gauss <- predict(model_gaussian, type = "response")

  expect_true(cor(preds_binom, preds_gauss) > 0.9,
              info = "Predictions should be correlated")
  expect_false(isTRUE(all.equal(preds_binom, preds_gauss, tolerance = 0.0001)),
               info = "Predictions should differ between families")
})

test_that("accepts unemployment_rate format for backward compatibility", {
  set.seed(2223)

  test_data <- data.frame(
    time_index = 1:60,
    month = rep(1:12, 5),
    unemployment_rate = runif(60, 0.02, 0.08)
  )

  model <- fit_seasonal_gam(
    test_data,
    response_formula = quote(unemployment_rate),
    family = "gaussian"
  )

  expect_s3_class(model, "gam")
  expect_equal(model$family$family, "gaussian")
})

test_that("binomial works with factor smooth (by=education)", {
  skip("Factor smooth implementation pending - Phase 1 feature 1.11")

  set.seed(2425)

  educations <- c("phd", "masters", "bachelors")
  test_data <- expand.grid(
    time_index = 1:60,
    education = educations
  )
  test_data$month <- rep(1:12, length.out = nrow(test_data))
  test_data$education <- factor(test_data$education, levels = educations)

  # Different unemployment rates by education
  base_rates <- c(phd = 0.03, masters = 0.04, bachelors = 0.05)
  test_data$true_rate <- base_rates[as.character(test_data$education)]

  test_data$n_total <- 1000
  test_data$n_unemployed <- rbinom(nrow(test_data),
                                    size = 1000,
                                    prob = test_data$true_rate)
  test_data$n_employed <- test_data$n_total - test_data$n_unemployed

  model <- fit_seasonal_gam(
    test_data,
    response_formula = quote(cbind(n_unemployed, n_employed)),
    family = "binomial",
    factor_smooth = TRUE,
    by_variable = "education",
    k_trend = 10,
    k_month = 12
  )

  expect_s3_class(model, "gam")

  smooth_labels <- sapply(model$smooth, function(s) s$label)
  expect_true(any(grepl("education", smooth_labels)),
              info = "Should have education factor smooth")

  preds <- predict(model, type = "response")
  expect_true(all(preds >= 0 & preds <= 1))
})

test_that("binomial works with factor smooth + shared smoothing parameters", {
  skip("Factor smooth + shared smoothing implementation pending - Phase 1 feature 1.12")

  set.seed(2627)

  educations <- c("phd", "masters", "bachelors")
  test_data <- expand.grid(
    time_index = 1:60,
    education = educations
  )
  test_data$month <- rep(1:12, length.out = nrow(test_data))
  test_data$year <- 2020 + (test_data$time_index - 1) %/% 12
  test_data$education <- factor(test_data$education, levels = educations)

  # Generate binomial data
  base_rates <- c(phd = 0.025, masters = 0.035, bachelors = 0.045)
  test_data$true_rate <- base_rates[as.character(test_data$education)] +
    0.005 * sin(2 * pi * test_data$month / 12)

  test_data$n_total <- 1000
  test_data$n_unemployed <- rbinom(nrow(test_data),
                                    size = 1000,
                                    prob = test_data$true_rate)
  test_data$n_employed <- test_data$n_total - test_data$n_unemployed

  models <- fit_nested_model_sequence(
    test_data,
    response_formula = quote(cbind(n_unemployed, n_employed)),
    family = "binomial",
    shared_wiggliness = TRUE
  )

  model_m6 <- models$m6

  expect_s3_class(model_m6, "gam")

  has_id_params <- sapply(model_m6$smooth, function(s) !is.null(s$id))
  expect_true(any(has_id_params),
              info = "Should have smooths with id= parameter")

  preds <- predict(model_m6, type = "response")
  expect_true(all(preds >= 0 & preds <= 1))

  expect_true(summary(model_m6)$dev.expl > 0.5,
              info = "Should explain >50% deviance")
})
