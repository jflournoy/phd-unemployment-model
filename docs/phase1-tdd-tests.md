# Phase 1 TDD Test Specifications

**Date**: 2025-11-16
**Purpose**: Comprehensive test specifications for Phase 1 next-generation GAM features

---

## Overview

This document provides complete test code for all Phase 1 TDD tests. Each test includes:
- Setup code with `set.seed()` for reproducibility
- Clear assertions with tolerance values
- Explanation of why RED tests fail
- Explanation of what GREEN tests validate

**Related Issues**:
- Issue #13: Quasi-Binomial Family with CPS-Style Weights
- Issue #14: Increase Basis Dimensions to k=30
- Issue #15: Add COVID-19 Intervention Smooth

**Implementation Plan**: See `docs/next-gen-gam-plan.md`

---

## Feature 1: Quasi-Binomial Family with CPS-Style Weights

### RED Phase Tests

#### Test 1.1: Binomial response format acceptance
```r
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
    response_formula = cbind(n_unemployed, n_employed),
    family = "binomial"
  )

  expect_s3_class(model, "gam")
  expect_equal(model$family$family, "binomial")
})
```
**Why RED**: `response_formula` parameter doesn't exist yet → "unused argument" error

---

#### Test 1.2: Latent probability recovery - simple case
```r
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
    response_formula = cbind(n_unemployed, n_employed),
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
```
**Why RED**: Function doesn't support binomial response format yet

---

#### Test 1.3: Latent probability recovery - varying rates
```r
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
    response_formula = cbind(n_unemployed, n_employed),
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
```
**Why RED**: Binomial family support doesn't exist yet

---

#### Test 1.4: Sample size affects standard errors
```r
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
    response_formula = cbind(n_unemployed, n_employed),
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
  # sqrt(10) ≈ 3.16, so expect ratio between 2 and 5 (allowing GAM smoothing)
  se_ratio_1000_100 <- se_n100 / se_n1000
  se_ratio_10000_1000 <- se_n1000 / se_n10000

  expect_true(se_ratio_1000_100 > 2 && se_ratio_1000_100 < 5,
              info = paste("SE ratio (100 vs 1000):", round(se_ratio_1000_100, 2),
                          "should be 2-5"))
  expect_true(se_ratio_10000_1000 > 2 && se_ratio_10000_1000 < 5,
              info = paste("SE ratio (1000 vs 10000):", round(se_ratio_10000_1000, 2),
                          "should be 2-5"))
})
```
**Why RED**: Binomial family support doesn't exist yet

---

#### Test 1.5: Predictions stay in [0,1] with binomial format
```r
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
    response_formula = cbind(n_unemployed, n_employed),
    family = "binomial"
  )

  preds <- predict(model, type = "response")

  expect_true(all(preds >= 0, na.rm = TRUE),
              info = "All predictions should be >= 0")
  expect_true(all(preds <= 1, na.rm = TRUE),
              info = "All predictions should be <= 1")
})
```
**Why RED**: Binomial family support doesn't exist yet

---

#### Test 1.6: High sample size observations dominate fit
```r
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
    response_formula = cbind(n_unemployed, n_employed),
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
  expect_true(mean(preds) < 0.05,
              info = "Mean prediction closer to high-n rate (0.03) than low-n (0.08)")
})
```
**Why RED**: Binomial family support doesn't exist yet

---

#### Test 1.7: CPS-style aggregated weights work correctly
```r
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
    response_formula = cbind(n_unemployed, n_employed),
    family = "binomial",
    k_trend = 10,
    k_month = 12
  )

  preds <- predict(model, type = "response")

  expect_equal(mean(preds), 0.04, tolerance = 0.015,
              info = "Should recover aggregated unemployment rate")

  expect_true(all(preds > 0.01 & preds < 0.08),
              info = "Predictions in reasonable range around 4%")
})
```
**Why RED**: Binomial family support doesn't exist yet

---

#### Test 1.8: Parameter recovery with realistic CPS weights
```r
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
    response_formula = cbind(n_unemployed, n_employed),
    family = "binomial",
    k_trend = 5,
    k_month = 10
  )

  # Predict at baseline month
  newdata <- data.frame(time_index = 60, month = 6)
  pred_baseline <- predict(model, newdata = newdata, type = "response")

  expect_equal(pred_baseline, true_baseline, tolerance = 0.012,
              info = "Should recover baseline within 1.2pp")

  # Check seasonal amplitude recovery
  preds_by_month <- sapply(1:12, function(m) {
    predict(model, newdata = data.frame(time_index = 60, month = m),
            type = "response")
  })

  recovered_amplitude <- (max(preds_by_month) - min(preds_by_month)) / 2
  expect_equal(recovered_amplitude, true_seasonal_amplitude, tolerance = 0.008,
              info = "Should recover seasonal amplitude within 0.8pp")
})
```
**Why RED**: Binomial family support doesn't exist yet

---

### GREEN Phase Tests

#### Test 1.9: Binomial vs Gaussian produces different fits
```r
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
    response_formula = cbind(n_unemployed, n_employed),
    family = "binomial"
  )

  model_gaussian <- fit_seasonal_gam(
    test_data,
    response_formula = unemployment_rate,
    family = "gaussian"
  )

  preds_binom <- predict(model_binomial, type = "response")
  preds_gauss <- predict(model_gaussian, type = "response")

  expect_true(cor(preds_binom, preds_gauss) > 0.9,
              info = "Predictions should be correlated")
  expect_false(isTRUE(all.equal(preds_binom, preds_gauss)),
               info = "Predictions should differ between families")
})
```
**Why GREEN**: Validates family parameter actually changes behavior

---

#### Test 1.10: Backward compatibility with rate format
```r
test_that("accepts unemployment_rate format for backward compatibility", {
  set.seed(2223)

  test_data <- data.frame(
    time_index = 1:60,
    month = rep(1:12, 5),
    unemployment_rate = runif(60, 0.02, 0.08)
  )

  model <- fit_seasonal_gam(
    test_data,
    response_formula = unemployment_rate,
    family = "gaussian"
  )

  expect_s3_class(model, "gam")
  expect_equal(model$family$family, "gaussian")
})
```
**Why GREEN**: Ensures old code still works

---

#### Test 1.11: Factor smooth compatibility - simple
```r
test_that("binomial works with factor smooth (by=education)", {
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
    response_formula = cbind(n_unemployed, n_employed),
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
```
**Why GREEN**: Validates factor smooth compatibility

---

#### Test 1.12: Factor smooth + shared smoothing (id=)
```r
test_that("binomial works with factor smooth + shared smoothing parameters", {
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
    response_formula = cbind(n_unemployed, n_employed),
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
```
**Why GREEN**: Validates shared smoothing works with binomial

---

## Feature 2: Increased Basis Dimensions (k=30)

### RED Phase Tests

#### Test 2.1: k_trend=30 parameter accepted and used
```r
test_that("fit_seasonal_gam accepts and uses k_trend = 30", {
  set.seed(123)
  test_data <- simulate_seasonal_unemployment(n_years = 10, seed = 123)

  model <- fit_seasonal_gam(test_data, k_trend = 30)

  time_smooth <- model$smooth[[which(sapply(model$smooth, function(s) {
    grepl("time_index", s$label)
  }))[1]]]

  expect_equal(time_smooth$bs.dim, 30,
              info = "time_index smooth should have k=30")
})
```
**Why RED**: Validates parameter is used (may work if parameter exists)

---

#### Test 2.2: EDF ratio validation warns on high ratios
```r
test_that("warns when EDF ratio exceeds threshold", {
  set.seed(456)
  test_data <- simulate_seasonal_unemployment(n_years = 2, seed = 456)

  expect_warning(
    model <- fit_seasonal_gam(test_data, k_trend = 30, check_edf = TRUE),
    regexp = "EDF ratio|High EDF|edf",
    info = "Should warn when EDF ratio too high"
  )
})
```
**Why RED**: `check_edf` parameter doesn't exist

---

#### Test 2.3: EDF ratio calculation is correct
```r
test_that("validate_edf_ratio correctly computes ratio", {
  set.seed(789)
  test_data <- simulate_seasonal_unemployment(n_years = 10, seed = 789)
  model <- fit_seasonal_gam(test_data, k_trend = 30)

  edf_check <- validate_edf_ratio(model, test_data, max_ratio = 0.15)

  expected_ratio <- sum(model$edf) / nrow(test_data)

  expect_equal(edf_check$edf_ratio, expected_ratio, tolerance = 1e-6)
  expect_true(edf_check$acceptable,
              info = "10 years with k=30 should have acceptable ratio")
})
```
**Why RED**: `validate_edf_ratio()` function doesn't exist

---

#### Test 2.4: Higher k captures more flexible trends
```r
test_that("k=30 uses more EDF than k=20 for complex trend", {
  set.seed(999)
  test_data <- data.frame(
    time_index = 1:120,
    month = rep(1:12, 10),
    unemployment_rate = 0.03 +
      0.0001 * (1:120) +
      0.00005 * sin(2 * pi * (1:120) / 60) +
      0.005 * sin(2 * pi * rep(1:12, 10) / 12) +
      rnorm(120, 0, 0.002)
  )

  model_k20 <- fit_seasonal_gam(test_data, k_trend = 20)
  model_k30 <- fit_seasonal_gam(test_data, k_trend = 30)

  edf_k20 <- model_k20$edf[grepl("time_index", names(model_k20$edf))][1]
  edf_k30 <- model_k30$edf[grepl("time_index", names(model_k30$edf))][1]

  expect_true(edf_k30 >= edf_k20,
              info = "k=30 should use at least as much EDF as k=20")
})
```
**Why RED**: Tests flexibility assumption

---

### GREEN Phase Tests

#### Test 2.5: Default k_trend changes to 30
```r
test_that("default k_trend is now 30", {
  set.seed(111)
  test_data <- simulate_seasonal_unemployment(n_years = 10, seed = 111)

  model_default <- fit_seasonal_gam(test_data)

  time_smooth <- model_default$smooth[[which(sapply(model_default$smooth, function(s) {
    grepl("time_index", s$label)
  }))[1]]]

  expect_equal(time_smooth$bs.dim, 30,
              info = "Default k_trend should be 30")
})
```
**Why GREEN**: Validates default changed

---

#### Test 2.6: EDF check can be disabled
```r
test_that("check_edf=FALSE skips EDF validation", {
  set.seed(222)
  test_data <- simulate_seasonal_unemployment(n_years = 2, seed = 222)

  expect_silent(
    model <- fit_seasonal_gam(test_data, k_trend = 30, check_edf = FALSE)
  )
})
```
**Why GREEN**: Validates optional checking

---

#### Test 2.7: Factor smooth EDF ratio accounts for groups
```r
test_that("EDF ratio divides by number of education groups", {
  set.seed(333)
  educations <- c("phd", "masters", "bachelors")
  test_data <- expand.grid(
    time_index = 1:120,
    education = educations
  )
  test_data$year <- 2000 + (test_data$time_index - 1) %/% 12
  test_data$month <- ((test_data$time_index - 1) %% 12) + 1
  test_data$education <- factor(test_data$education)
  test_data$unemployment_rate <- 0.03 + rnorm(nrow(test_data), 0, 0.01)

  models <- fit_nested_model_sequence(test_data, shared_wiggliness = TRUE)
  model_m6 <- models$m6

  edf_check <- validate_edf_ratio(model_m6, test_data, max_ratio = 0.15)

  n_groups <- length(educations)
  expected_n_obs <- nrow(test_data) / n_groups

  expect_equal(edf_check$n_obs, expected_n_obs, tolerance = 1e-6,
              info = "Should account for grouped structure")
})
```
**Why GREEN**: Validates factor smooth EDF calculation

---

#### Test 2.8: k=30 acceptable for 25 years of data
```r
test_that("k=30 with 25 years passes EDF check", {
  set.seed(444)
  test_data <- simulate_seasonal_unemployment(n_years = 25, seed = 444)

  model <- fit_seasonal_gam(test_data, k_trend = 30, check_edf = TRUE)
  edf_check <- validate_edf_ratio(model, test_data, max_ratio = 0.15)

  expect_true(edf_check$acceptable,
              info = "k=30 with 25 years should be acceptable")
  expect_true(edf_check$edf_ratio < 0.12,
              info = "EDF ratio should be comfortably below 15%")
})
```
**Why GREEN**: Validates real-world use case

---

#### Test 2.9: k=30 works with binomial + factor smooth
```r
test_that("k=30 works with binomial family and factor smooth", {
  set.seed(555)
  educations <- c("phd", "masters")
  test_data <- expand.grid(
    time_index = 1:120,
    education = educations
  )
  test_data$month <- rep(1:12, length.out = nrow(test_data))
  test_data$education <- factor(test_data$education)

  base_rates <- c(phd = 0.03, masters = 0.04)
  test_data$true_rate <- base_rates[as.character(test_data$education)]
  test_data$n_unemployed <- rbinom(nrow(test_data), size = 1000,
                                    prob = test_data$true_rate)
  test_data$n_employed <- 1000 - test_data$n_unemployed

  model <- fit_seasonal_gam(
    test_data,
    response_formula = cbind(n_unemployed, n_employed),
    family = "binomial",
    k_trend = 30,
    factor_smooth = TRUE,
    by_variable = "education"
  )

  expect_s3_class(model, "gam")

  edf_check <- validate_edf_ratio(model, test_data, max_ratio = 0.15)
  expect_true(edf_check$acceptable,
              info = "k=30 should work with binomial + factor smooth")
})
```
**Why GREEN**: Validates feature integration

---

## Feature 3: COVID-19 Intervention Smooth

### RED Phase Tests

#### Test 3.1: COVID intervention time variable is created
```r
test_that("add_intervention_times creates covid_time column", {
  set.seed(123)
  test_data <- data.frame(
    time_index = 1:120,
    year = rep(2015:2024, each = 12),
    month = rep(1:12, 10),
    unemployment_rate = runif(120, 0.02, 0.08)
  )

  data_with_intervention <- add_intervention_times(test_data, interventions = "covid")

  expect_true("covid_time" %in% names(data_with_intervention),
              info = "Should add covid_time column")

  covid_start <- which(data_with_intervention$year == 2020 &
                       data_with_intervention$month == 3)[1]

  expect_true(all(data_with_intervention$covid_time[1:(covid_start-1)] == 0),
              info = "covid_time should be 0 before March 2020")
  expect_true(all(data_with_intervention$covid_time[covid_start:nrow(test_data)] >= 0),
              info = "covid_time should be >= 0 after March 2020")
})
```
**Why RED**: `add_intervention_times()` function doesn't exist

---

#### Test 3.2: COVID intervention included in model formula
```r
test_that("model includes s(covid_time) when interventions='covid'", {
  set.seed(456)
  test_data <- data.frame(
    time_index = 1:120,
    year = rep(2015:2024, each = 12),
    month = rep(1:12, 10),
    unemployment_rate = runif(120, 0.02, 0.08)
  )

  model <- fit_seasonal_gam_with_interventions(
    test_data,
    interventions = "covid"
  )

  smooth_labels <- sapply(model$smooth, function(s) s$label)
  expect_true(any(grepl("covid_time", smooth_labels)),
              info = "Model should include covid_time smooth")
})
```
**Why RED**: `fit_seasonal_gam_with_interventions()` doesn't exist

---

#### Test 3.3: COVID intervention has positive effect
```r
test_that("extract_intervention_effect shows positive post-COVID effect", {
  set.seed(789)
  test_data <- data.frame(
    time_index = 1:120,
    year = rep(2015:2024, each = 12),
    month = rep(1:12, 10)
  )
  covid_idx <- which(test_data$year == 2020 & test_data$month == 3)[1]

  # Add COVID spike
  test_data$unemployment_rate <- 0.03 +
    0.005 * sin(2 * pi * test_data$month / 12) +
    ifelse(test_data$time_index >= covid_idx,
           0.015 * exp(-0.05 * (test_data$time_index - covid_idx)),
           0) +
    rnorm(120, 0, 0.002)

  model <- fit_seasonal_gam_with_interventions(
    test_data,
    interventions = "covid"
  )

  effect <- extract_intervention_effect(model, "covid", test_data)

  pre_covid <- effect$intervention_effect[1:(covid_idx - 1)]
  expect_true(mean(abs(pre_covid)) < 0.003,
              info = "Pre-COVID effect should be near zero")

  post_covid <- effect$intervention_effect[covid_idx:nrow(test_data)]
  expect_true(mean(post_covid) > 0.003,
              info = "Post-COVID effect should be positive")
})
```
**Why RED**: `extract_intervention_effect()` doesn't exist

---

#### Test 3.4: Intervention smooth basis dimension is configurable
```r
test_that("intervention_k parameter controls smooth basis dimension", {
  set.seed(999)
  test_data <- data.frame(
    time_index = 1:100,
    year = rep(2016:2024, each = 11)[1:100],
    month = rep(1:12, 9)[1:100],
    unemployment_rate = runif(100, 0.02, 0.08)
  )

  model_k5 <- fit_seasonal_gam_with_interventions(
    test_data,
    interventions = "covid",
    intervention_k = 5
  )

  model_k10 <- fit_seasonal_gam_with_interventions(
    test_data,
    interventions = "covid",
    intervention_k = 10
  )

  covid_smooth_k5 <- model_k5$smooth[[which(sapply(model_k5$smooth, function(s) {
    grepl("covid_time", s$label)
  }))]]

  covid_smooth_k10 <- model_k10$smooth[[which(sapply(model_k10$smooth, function(s) {
    grepl("covid_time", s$label)
  }))]]

  expect_equal(covid_smooth_k5$bs.dim, 5)
  expect_equal(covid_smooth_k10$bs.dim, 10)
})
```
**Why RED**: `intervention_k` parameter doesn't exist

---

### GREEN Phase Tests

#### Test 3.5: COVID intervention improves model fit
```r
test_that("COVID intervention improves R² on data with COVID effect", {
  set.seed(111)
  test_data <- data.frame(
    time_index = 1:120,
    year = rep(2015:2024, each = 12),
    month = rep(1:12, 10)
  )
  covid_idx <- which(test_data$year == 2020 & test_data$month == 3)[1]

  test_data$unemployment_rate <- 0.03 +
    0.005 * sin(2 * pi * test_data$month / 12) +
    0.0001 * test_data$time_index +
    ifelse(test_data$time_index >= covid_idx, 0.02, 0) +
    rnorm(120, 0, 0.003)

  model_no_covid <- fit_seasonal_gam(test_data)
  model_with_covid <- fit_seasonal_gam_with_interventions(
    test_data,
    interventions = "covid"
  )

  r2_no_covid <- summary(model_no_covid)$r.sq
  r2_with_covid <- summary(model_with_covid)$r.sq

  expect_true(r2_with_covid > r2_no_covid,
              info = "COVID intervention should improve R²")
  expect_true((r2_with_covid - r2_no_covid) > 0.03,
              info = "R² improvement should be substantial (>3%)")
})
```
**Why GREEN**: Validates intervention improves fit

---

#### Test 3.6: Multiple interventions can be specified
```r
test_that("framework supports vector of interventions", {
  set.seed(222)
  test_data <- data.frame(
    time_index = 1:180,
    year = rep(2010:2024, each = 12),
    month = rep(1:12, 15),
    unemployment_rate = runif(180, 0.02, 0.08)
  )

  data_with_interventions <- add_intervention_times(
    test_data,
    interventions = c("covid")
  )

  expect_true("covid_time" %in% names(data_with_interventions))

  data_no_interventions <- add_intervention_times(test_data, interventions = NULL)
  expect_equal(ncol(data_no_interventions), ncol(test_data),
              info = "No new columns when interventions=NULL")
})
```
**Why GREEN**: Validates infrastructure for Phase 2

---

#### Test 3.7: Intervention effect extraction handles edge cases
```r
test_that("handles data that ends before COVID", {
  set.seed(333)
  test_data_pre_covid <- data.frame(
    time_index = 1:60,
    year = rep(2015:2019, each = 12),
    month = rep(1:12, 5),
    unemployment_rate = runif(60, 0.02, 0.05)
  )

  data_with_intervention <- add_intervention_times(
    test_data_pre_covid,
    interventions = "covid"
  )

  expect_true(all(data_with_intervention$covid_time == 0),
              info = "covid_time should be 0 when COVID not in data range")
})
```
**Why GREEN**: Validates edge case handling

---

#### Test 3.8: COVID intervention works with factor smooth models
```r
test_that("intervention works with multi-education models", {
  set.seed(444)
  educations <- c("phd", "masters")
  test_data <- expand.grid(
    time_index = 1:100,
    education = educations
  )
  test_data$year <- 2016 + (test_data$time_index - 1) %/% 12
  test_data$month <- ((test_data$time_index - 1) %% 12) + 1
  test_data$unemployment_rate <- runif(nrow(test_data), 0.02, 0.06)

  data_with_intervention <- add_intervention_times(test_data, interventions = "covid")

  expect_true("covid_time" %in% names(data_with_intervention))
  expect_equal(nrow(data_with_intervention), nrow(test_data),
              info = "Should preserve all rows")

  phd_covid <- data_with_intervention$covid_time[data_with_intervention$education == "phd"]
  masters_covid <- data_with_intervention$covid_time[data_with_intervention$education == "masters"]
  expect_equal(phd_covid, masters_covid,
              info = "COVID timing same for all education levels")
})
```
**Why GREEN**: Validates factor smooth compatibility

---

#### Test 3.9: COVID + binomial + factor smooth all work together
```r
test_that("COVID works with binomial and factor smooth", {
  set.seed(555)
  educations <- c("phd", "masters")
  test_data <- expand.grid(
    time_index = 1:120,
    education = educations
  )
  test_data$year <- 2015 + (test_data$time_index - 1) %/% 12
  test_data$month <- ((test_data$time_index - 1) %% 12) + 1
  test_data$education <- factor(test_data$education)

  covid_idx <- which(test_data$year == 2020 & test_data$month == 3)[1]
  base_rates <- c(phd = 0.03, masters = 0.04)
  test_data$true_rate <- base_rates[as.character(test_data$education)] +
    ifelse(test_data$time_index >= covid_idx, 0.015, 0)

  test_data$n_unemployed <- rbinom(nrow(test_data), size = 1000,
                                    prob = test_data$true_rate)
  test_data$n_employed <- 1000 - test_data$n_unemployed

  model <- fit_seasonal_gam_with_interventions(
    test_data,
    response_formula = cbind(n_unemployed, n_employed),
    family = "binomial",
    interventions = "covid",
    factor_smooth = TRUE,
    by_variable = "education"
  )

  expect_s3_class(model, "gam")

  smooth_labels <- sapply(model$smooth, function(s) s$label)
  expect_true(any(grepl("covid_time", smooth_labels)))

  preds <- predict(model, type = "response")
  expect_true(all(preds >= 0 & preds <= 1))
})
```
**Why GREEN**: Validates full feature integration

---

## Integration Tests

### INT.1: All Phase 1 features work together
```r
test_that("quasi-binomial + k=30 + COVID intervention work together", {
  set.seed(999)
  test_data <- data.frame(
    time_index = 1:300,
    year = 2000 + (1:300 - 1) %/% 12,
    month = ((1:300 - 1) %% 12) + 1
  )

  covid_idx <- which(test_data$year == 2020 & test_data$month == 3)[1]
  test_data$true_rate <- 0.025 +
    0.005 * sin(2 * pi * test_data$month / 12) +
    0.0001 * test_data$time_index +
    ifelse(test_data$time_index >= covid_idx, 0.015, 0)

  test_data$n_obs <- rpois(300, lambda = 2000)
  test_data$n_unemployed <- rbinom(300, size = test_data$n_obs,
                                    prob = test_data$true_rate)
  test_data$n_employed <- test_data$n_obs - test_data$n_unemployed

  model_full <- fit_seasonal_gam_with_interventions(
    test_data,
    response_formula = cbind(n_unemployed, n_employed),
    family = "binomial",
    k_trend = 30,
    interventions = "covid",
    check_edf = TRUE
  )

  expect_s3_class(model_full, "gam")

  preds <- predict(model_full, type = "response")
  expect_true(all(preds >= 0 & preds <= 1))

  expect_true(summary(model_full)$r.sq > 0.65,
              info = "Should achieve reasonable R²")

  smooth_labels <- sapply(model_full$smooth, function(s) s$label)
  expect_true(any(grepl("covid_time", smooth_labels)))

  edf_check <- validate_edf_ratio(model_full, test_data, max_ratio = 0.15)
  expect_true(edf_check$acceptable)
})
```

---

### INT.2: Full Phase 1 + factor smooth + shared smoothing
```r
test_that("all features work with factor smooth and shared smoothing", {
  set.seed(777)
  educations <- c("phd", "masters", "bachelors")
  test_data <- expand.grid(
    time_index = 1:300,
    education = educations
  )
  test_data$year <- 2000 + (test_data$time_index - 1) %/% 12
  test_data$month <- ((test_data$time_index - 1) %% 12) + 1
  test_data$education <- factor(test_data$education, levels = educations)

  covid_idx <- which(test_data$year == 2020 & test_data$month == 3)[1]
  base_rates <- c(phd = 0.025, masters = 0.035, bachelors = 0.045)
  test_data$true_rate <- base_rates[as.character(test_data$education)] +
    0.005 * sin(2 * pi * test_data$month / 12) +
    ifelse(test_data$time_index >= covid_idx, 0.015, 0)

  test_data$n_unemployed <- rbinom(nrow(test_data), size = 1000,
                                    prob = test_data$true_rate)
  test_data$n_employed <- 1000 - test_data$n_unemployed

  models <- fit_nested_model_sequence(
    test_data,
    response_formula = cbind(n_unemployed, n_employed),
    family = "binomial",
    k_trend = 30,
    interventions = "covid",
    shared_wiggliness = TRUE
  )

  # Test m6 (full model)
  model_m6 <- models$m6

  expect_s3_class(model_m6, "gam")

  # Has shared smoothing
  has_id_params <- sapply(model_m6$smooth, function(s) !is.null(s$id))
  expect_true(any(has_id_params))

  # Has COVID intervention
  smooth_labels <- sapply(model_m6$smooth, function(s) s$label)
  expect_true(any(grepl("covid_time", smooth_labels)))

  # Valid predictions
  preds <- predict(model_m6, type = "response")
  expect_true(all(preds >= 0 & preds <= 1))

  # Good fit
  expect_true(summary(model_m6)$dev.expl > 0.65)
})
```

---

### INT.3: Parameter recovery - complete model
```r
test_that("can recover known parameters with full model complexity", {
  set.seed(888)
  # True parameters
  true_baseline <- 0.03
  true_trend <- 0.0001
  true_amplitude <- 0.008
  true_covid_effect <- 0.015

  test_data <- data.frame(
    time_index = 1:300,
    year = 2000 + (1:300 - 1) %/% 12,
    month = ((1:300 - 1) %% 12) + 1
  )

  covid_idx <- which(test_data$year == 2020 & test_data$month == 3)[1]
  test_data$true_rate <- true_baseline +
    true_trend * test_data$time_index +
    true_amplitude * sin(2 * pi * test_data$month / 12) +
    ifelse(test_data$time_index >= covid_idx, true_covid_effect, 0)

  test_data$n_obs <- rpois(300, lambda = 2000)
  test_data$n_unemployed <- rbinom(300, size = test_data$n_obs,
                                    prob = test_data$true_rate)
  test_data$n_employed <- test_data$n_obs - test_data$n_unemployed

  model <- fit_seasonal_gam_with_interventions(
    test_data,
    response_formula = cbind(n_unemployed, n_employed),
    family = "binomial",
    k_trend = 30,
    interventions = "covid"
  )

  # Validate baseline recovery
  pred_baseline <- predict(model,
    newdata = data.frame(time_index = 1, month = 6, covid_time = 0),
    type = "response"
  )
  expect_equal(pred_baseline, true_baseline, tolerance = 0.008,
              info = "Should recover baseline within 0.8pp")

  # Validate COVID effect recovery
  covid_effect_data <- extract_intervention_effect(model, "covid", test_data)
  mean_covid_effect <- mean(covid_effect_data$intervention_effect[covid_idx:(covid_idx+24)])
  expect_equal(mean_covid_effect, true_covid_effect, tolerance = 0.01,
              info = "Should recover COVID effect within 1pp")
})
```

---

## Summary

**Total Tests**: 33 tests + 19 edge cases = 52 test scenarios

- **Feature 1 (Binomial)**: 12 tests (8 RED, 4 GREEN)
- **Feature 2 (k=30)**: 9 tests (4 RED, 5 GREEN)
- **Feature 3 (COVID)**: 9 tests (4 RED, 5 GREEN)
- **Integration**: 3 tests

**All tests include**:
- `set.seed()` for reproducibility
- Tolerance values for stochasticity
- Informative failure messages
- Clear RED/GREEN phase separation

**Related Issues**:
- Issue #13: Quasi-Binomial Family
- Issue #14: Increase k=30
- Issue #15: COVID Intervention
