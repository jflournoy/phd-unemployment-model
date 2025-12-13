# Tests for binomial and quasi-binomial GAM evaluation on real CPS data
# Following TDD RED-GREEN-REFACTOR pattern
#
# Purpose: Evaluate factor smooth GAMs using binomial and quasi-binomial families
# for real CPS unemployment data, incorporating learnings from quasi-binomial validation:
# - Binomial: Narrower PIs, more wiggly fits
# - Quasi-binomial: Wider PIs, smoother fits (higher REML λ)
# - Neither is "parameter recovery" - this is model comparison on real data

library(testthat)
library(mgcv)

# ==============================================================================
# Test Suite 1: Overdispersion Detection
# ==============================================================================

test_that("check_overdispersion detects overdispersion in binomial count data", {
  # Create sample count data with overdispersion
  # (more variance than binomial would predict)
  n <- 100
  n_total <- rep(1000, n)

  # Generate overdispersed data (variance > n*p*(1-p))
  true_p <- 0.05
  phi <- 20  # Overdispersion parameter

  # Beta-binomial-like data
  set.seed(123)
  alpha <- true_p * phi
  beta <- (1 - true_p) * phi
  prob_vec <- rbeta(n, alpha, beta)
  n_success <- rbinom(n, size = n_total, prob = prob_vec)

  test_data <- data.frame(
    n_success = n_success,
    n_total = n_total,
    unemployment_rate = n_success / n_total
  )

  # Function should detect overdispersion
  result <- check_overdispersion(test_data, success_col = "n_success", total_col = "n_total")

  # Should return a list with detection results
  expect_type(result, "list")
  expect_true("overdispersed" %in% names(result))
  expect_true("dispersion_parameter" %in% names(result))
  expect_true("pearson_chisq" %in% names(result))
  expect_true("df_residual" %in% names(result))

  # Should detect overdispersion (phi > 1)
  expect_true(result$overdispersed)
  expect_gt(result$dispersion_parameter, 1)
})

test_that("check_overdispersion does not detect overdispersion in binomial data", {
  # Create sample count data WITHOUT overdispersion
  n <- 100
  n_total <- rep(1000, n)
  true_p <- 0.05

  # Pure binomial data
  set.seed(456)
  n_success <- rbinom(n, size = n_total, prob = true_p)

  test_data <- data.frame(
    n_success = n_success,
    n_total = n_total
  )

  # Function should NOT detect overdispersion
  result <- check_overdispersion(test_data, success_col = "n_success", total_col = "n_total")

  # Should not detect overdispersion (phi ≈ 1)
  expect_false(result$overdispersed)
  expect_lte(result$dispersion_parameter, 2)  # Allow some sampling variability
})

test_that("check_overdispersion returns dispersion parameter estimate", {
  # Generate data with known dispersion
  n <- 150
  n_total <- rep(500, n)
  true_p <- 0.04
  phi <- 15

  set.seed(789)
  alpha <- true_p * phi
  beta <- (1 - true_p) * phi
  prob_vec <- rbeta(n, alpha, beta)
  n_success <- rbinom(n, size = n_total, prob = prob_vec)

  test_data <- data.frame(
    n_success = n_success,
    n_total = n_total
  )

  result <- check_overdispersion(test_data, success_col = "n_success", total_col = "n_total")

  # Should estimate dispersion parameter close to true value
  # (won't be exact due to sampling, but should be in right ballpark)
  expect_gt(result$dispersion_parameter, 5)  # Should be > 1
  expect_type(result$dispersion_parameter, "double")
})


# ==============================================================================
# Test Suite 2: Fitting Binomial and Quasi-Binomial Factor Smooth GAMs
# ==============================================================================

test_that("fit_factor_smooth_binomial fits binomial GAM with factor smooths", {
  # Create simulated count data for multiple education levels
  # (We'll use simulated data for testing, real data for actual analysis)

  n_months <- 120
  education_levels <- c("phd", "masters", "bachelors")

  # Generate count data
  set.seed(111)
  sim_data <- do.call(rbind, lapply(education_levels, function(educ) {
    time_index <- seq_len(n_months)
    month <- ((time_index - 1) %% 12) + 1

    # Logit-scale linear predictor
    baseline <- if (educ == "phd") -3.0 else if (educ == "masters") -2.8 else -2.6
    seasonal_amp <- if (educ == "phd") 0.2 else if (educ == "masters") 0.4 else 0.6
    seasonal_effect <- seasonal_amp * sin(2 * pi * month / 12)

    logit_p <- baseline + seasonal_effect
    prob <- plogis(logit_p)

    n_total <- round(runif(n_months, 800, 1200))
    n_unemployed <- rbinom(n_months, size = n_total, prob = prob)

    data.frame(
      time_index = time_index,
      month = month,
      education = educ,
      n_unemployed = n_unemployed,
      n_total = n_total,
      stringsAsFactors = FALSE
    )
  }))

  sim_data$education <- factor(sim_data$education, levels = education_levels)

  # Fit binomial factor smooth GAM
  model <- fit_factor_smooth_binomial(
    data = sim_data,
    formula_type = "full",
    family_type = "binomial"
  )

  # Should return a gam object
  expect_s3_class(model, "gam")

  # Should use binomial family
  expect_equal(model$family$family, "binomial")

  # Should have metadata
  expect_equal(attr(model, "formula_type"), "full")
  expect_equal(attr(model, "family_type"), "binomial")
})

test_that("fit_factor_smooth_binomial fits quasi-binomial GAM with factor smooths", {
  # Create simulated count data with overdispersion
  n_months <- 120
  education_levels <- c("phd", "masters")

  set.seed(222)
  sim_data <- do.call(rbind, lapply(education_levels, function(educ) {
    time_index <- seq_len(n_months)
    month <- ((time_index - 1) %% 12) + 1

    # Logit-scale linear predictor with overdispersion
    baseline <- if (educ == "phd") -3.0 else -2.7
    seasonal_amp <- if (educ == "phd") 0.3 else 0.5
    seasonal_effect <- seasonal_amp * sin(2 * pi * month / 12)

    logit_p <- baseline + seasonal_effect
    prob_mean <- plogis(logit_p)

    # Add beta-binomial overdispersion
    phi <- 20
    alpha <- prob_mean * phi
    beta <- (1 - prob_mean) * phi
    prob_vec <- rbeta(n_months, alpha, beta)

    n_total <- round(runif(n_months, 800, 1200))
    n_unemployed <- rbinom(n_months, size = n_total, prob = prob_vec)

    data.frame(
      time_index = time_index,
      month = month,
      education = educ,
      n_unemployed = n_unemployed,
      n_total = n_total,
      stringsAsFactors = FALSE
    )
  }))

  sim_data$education <- factor(sim_data$education, levels = education_levels)

  # Fit quasi-binomial factor smooth GAM
  model <- fit_factor_smooth_binomial(
    data = sim_data,
    formula_type = "full",
    family_type = "quasibinomial"
  )

  # Should return a gam object
  expect_s3_class(model, "gam")

  # Should use quasibinomial family
  expect_equal(model$family$family, "quasibinomial")

  # Should have metadata
  expect_equal(attr(model, "formula_type"), "full")
  expect_equal(attr(model, "family_type"), "quasibinomial")
})

test_that("fit_factor_smooth_binomial validates input data has count columns", {
  # Create data without required columns
  bad_data <- data.frame(
    time_index = 1:100,
    month = rep(1:12, length.out = 100),
    education = rep(c("phd", "masters"), each = 50)
  )

  # Should error with informative message
  expect_error(
    fit_factor_smooth_binomial(
      data = bad_data,
      formula_type = "full",
      family_type = "binomial"
    ),
    regexp = "n_unemployed|n_total|count"
  )
})


# ==============================================================================
# Test Suite 3: Comparing Binomial vs Quasi-Binomial Models
# ==============================================================================

test_that("compare_binomial_quasibinomial fits both families and compares", {
  # Create overdispersed count data
  n_months <- 100
  education_levels <- c("phd", "masters")

  set.seed(333)
  sim_data <- do.call(rbind, lapply(education_levels, function(educ) {
    time_index <- seq_len(n_months)
    month <- ((time_index - 1) %% 12) + 1

    baseline <- if (educ == "phd") -3.0 else -2.8
    seasonal_amp <- if (educ == "phd") 0.25 else 0.35
    seasonal_effect <- seasonal_amp * sin(2 * pi * month / 12)

    logit_p <- baseline + seasonal_effect
    prob_mean <- plogis(logit_p)

    # Beta-binomial overdispersion
    phi <- 25
    alpha <- prob_mean * phi
    beta <- (1 - prob_mean) * phi
    prob_vec <- rbeta(n_months, alpha, beta)

    n_total <- round(runif(n_months, 1000, 1500))
    n_unemployed <- rbinom(n_months, size = n_total, prob = prob_vec)

    data.frame(
      time_index = time_index,
      month = month,
      education = educ,
      n_unemployed = n_unemployed,
      n_total = n_total,
      stringsAsFactors = FALSE
    )
  }))

  sim_data$education <- factor(sim_data$education, levels = education_levels)

  # Compare both families
  comparison <- compare_binomial_quasibinomial(
    data = sim_data,
    formula_type = "full"
  )

  # Should return a list with both models
  expect_type(comparison, "list")
  expect_true("binomial_model" %in% names(comparison))
  expect_true("quasibinomial_model" %in% names(comparison))
  expect_true("comparison_table" %in% names(comparison))

  # Both should be gam objects
  expect_s3_class(comparison$binomial_model, "gam")
  expect_s3_class(comparison$quasibinomial_model, "gam")

  # Comparison table should have key metrics
  expect_s3_class(comparison$comparison_table, "data.frame")
  expect_true("model" %in% names(comparison$comparison_table))
  expect_true("deviance" %in% names(comparison$comparison_table))
  expect_true("dispersion" %in% names(comparison$comparison_table))
})

test_that("compare_binomial_quasibinomial shows different smoothing parameters", {
  # Based on learnings: quasi-binomial λ ~64,000× larger than binomial

  # Create overdispersed data
  n_months <- 120
  set.seed(444)

  sim_data <- data.frame(
    time_index = rep(seq_len(n_months), times = 2),
    month = rep(((seq_len(n_months) - 1) %% 12) + 1, times = 2),
    education = rep(c("phd", "masters"), each = n_months)
  )

  # Generate with overdispersion
  for (educ in c("phd", "masters")) {
    idx <- sim_data$education == educ
    baseline <- if (educ == "phd") -3.2 else -3.0

    logit_p <- baseline + 0.3 * sin(2 * pi * sim_data$month[idx] / 12)
    prob_mean <- plogis(logit_p)

    phi <- 30
    prob_vec <- rbeta(sum(idx), prob_mean * phi, (1 - prob_mean) * phi)

    sim_data$n_total[idx] <- round(runif(sum(idx), 800, 1200))
    sim_data$n_unemployed[idx] <- rbinom(sum(idx), size = sim_data$n_total[idx], prob = prob_vec)
  }

  sim_data$education <- factor(sim_data$education)

  # Compare models
  comparison <- compare_binomial_quasibinomial(
    data = sim_data,
    formula_type = "full"
  )

  # Extract smoothing parameters
  sp_binomial <- comparison$binomial_model$sp
  sp_quasibinomial <- comparison$quasibinomial_model$sp

  # Quasi-binomial should have MUCH larger smoothing parameters
  # (due to scale parameter in REML)
  expect_gt(
    mean(sp_quasibinomial),
    mean(sp_binomial),
    label = "Quasi-binomial should have larger smoothing parameters than binomial"
  )

  # Should be at least 10× larger (conservative check, often ~1000× or more)
  expect_gt(
    mean(sp_quasibinomial) / mean(sp_binomial),
    10,
    label = "Quasi-binomial λ should be substantially larger (10×+ factor)"
  )
})

test_that("compare_binomial_quasibinomial comparison table includes dispersion", {
  # Quick test to ensure dispersion parameter is reported
  n_months <- 80
  set.seed(555)

  sim_data <- data.frame(
    time_index = seq_len(n_months),
    month = ((seq_len(n_months) - 1) %% 12) + 1,
    education = "phd",
    n_total = round(runif(n_months, 1000, 1500))
  )

  # Simple binomial data (no overdispersion)
  logit_p <- -3.0 + 0.2 * sin(2 * pi * sim_data$month / 12)
  sim_data$n_unemployed <- rbinom(n_months, size = sim_data$n_total, prob = plogis(logit_p))
  sim_data$education <- factor(sim_data$education)

  comparison <- compare_binomial_quasibinomial(
    data = sim_data,
    formula_type = "shared"
  )

  # Comparison table should report dispersion
  expect_true("dispersion" %in% names(comparison$comparison_table))

  # Binomial dispersion should be 1 (by definition)
  binom_row <- comparison$comparison_table[comparison$comparison_table$model == "binomial", ]
  expect_equal(binom_row$dispersion, 1)

  # Quasi-binomial dispersion should be estimated from data
  quasi_row <- comparison$comparison_table[comparison$comparison_table$model == "quasibinomial", ]
  expect_type(quasi_row$dispersion, "double")
  expect_gt(quasi_row$dispersion, 0)
})


# ==============================================================================
# Test Suite 4: Extracting Predictions and Intervals
# ==============================================================================

test_that("extract_prediction_intervals extracts PIs for binomial model", {
  # Create simple binomial data
  n_months <- 100
  set.seed(666)

  test_data <- data.frame(
    time_index = seq_len(n_months),
    month = ((seq_len(n_months) - 1) %% 12) + 1,
    education = "phd",
    n_total = rep(1000, n_months)
  )

  logit_p <- -3.0 + 0.25 * sin(2 * pi * test_data$month / 12)
  test_data$n_unemployed <- rbinom(n_months, size = test_data$n_total, prob = plogis(logit_p))
  test_data$education <- factor(test_data$education)

  # Fit binomial model
  model <- fit_factor_smooth_binomial(
    data = test_data,
    formula_type = "shared",
    family_type = "binomial"
  )

  # Extract prediction intervals
  pi_results <- extract_prediction_intervals(
    model,
    newdata = test_data,
    alpha = 0.05
  )

  # Should return a data frame
  expect_s3_class(pi_results, "data.frame")

  # Required columns
  required_cols <- c("fit", "se_fit", "pi_lower", "pi_upper")
  expect_true(all(required_cols %in% names(pi_results)))

  # Should have same number of rows as input data
  expect_equal(nrow(pi_results), nrow(test_data))

  # PIs should be valid (lower < upper)
  expect_true(all(pi_results$pi_lower < pi_results$pi_upper))

  # All predictions should be in (0, 1) range (on response scale)
  expect_true(all(pi_results$fit >= 0 & pi_results$fit <= 1))
  expect_true(all(pi_results$pi_lower >= 0 & pi_results$pi_lower <= 1))
  expect_true(all(pi_results$pi_upper >= 0 & pi_results$pi_upper <= 1))
})

test_that("extract_prediction_intervals shows wider intervals for quasi-binomial", {
  # Based on learnings: quasi-binomial should have wider PIs than binomial

  # Create overdispersed data
  n_months <- 120
  set.seed(777)

  test_data <- data.frame(
    time_index = seq_len(n_months),
    month = ((seq_len(n_months) - 1) %% 12) + 1,
    education = "phd",
    n_total = rep(1000, n_months)
  )

  # Beta-binomial data
  logit_p <- -3.0 + 0.3 * sin(2 * pi * test_data$month / 12)
  prob_mean <- plogis(logit_p)
  phi <- 20
  prob_vec <- rbeta(n_months, prob_mean * phi, (1 - prob_mean) * phi)
  test_data$n_unemployed <- rbinom(n_months, size = test_data$n_total, prob = prob_vec)
  test_data$education <- factor(test_data$education)

  # Fit both models
  model_binom <- fit_factor_smooth_binomial(test_data, "shared", "binomial")
  model_quasi <- fit_factor_smooth_binomial(test_data, "shared", "quasibinomial")

  # Extract PIs
  pi_binom <- extract_prediction_intervals(model_binom, test_data, alpha = 0.05)
  pi_quasi <- extract_prediction_intervals(model_quasi, test_data, alpha = 0.05)

  # Quasi-binomial PIs should be wider
  width_binom <- mean(pi_binom$pi_upper - pi_binom$pi_lower)
  width_quasi <- mean(pi_quasi$pi_upper - pi_quasi$pi_lower)

  expect_gt(
    width_quasi,
    width_binom,
    label = "Quasi-binomial PIs should be wider than binomial PIs"
  )
})

test_that("extract_prediction_intervals uses correct variance for binomial PIs", {
  # PIs should add binomial sampling variance: var_pred = se.fit^2 + 1/(n*p*(1-p))

  n_months <- 50
  set.seed(888)

  test_data <- data.frame(
    time_index = seq_len(n_months),
    month = ((seq_len(n_months) - 1) %% 12) + 1,
    education = "phd",
    n_total = rep(1000, n_months)
  )

  logit_p <- rep(-3.0, n_months)  # Constant for simplicity
  test_data$n_unemployed <- rbinom(n_months, size = test_data$n_total, prob = plogis(logit_p))
  test_data$education <- factor(test_data$education)

  model <- fit_factor_smooth_binomial(test_data, "shared", "binomial")

  # Get predictions
  pred <- predict(model, newdata = test_data, type = "link", se.fit = TRUE)

  # Calculate PIs manually
  p_hat <- plogis(pred$fit)
  n_total <- test_data$n_total

  # Variance on link scale (delta method)
  var_link <- pred$se.fit^2

  # Sampling variance contribution
  # For binomial: var(p_hat) ≈ p*(1-p)/n on response scale
  # On link scale: var(logit(p)) ≈ var(p) / (p*(1-p))^2 ≈ 1/(n*p*(1-p))
  var_sampling <- 1 / (n_total * p_hat * (1 - p_hat))

  # Total variance
  var_pred <- var_link + var_sampling
  se_pred <- sqrt(var_pred)

  # PIs on link scale
  z <- qnorm(0.975)
  pi_lower_manual <- plogis(pred$fit - z * se_pred)
  pi_upper_manual <- plogis(pred$fit + z * se_pred)

  # Extract PIs using function
  pi_results <- extract_prediction_intervals(model, test_data, alpha = 0.05)

  # Should match manual calculation (within numerical tolerance)
  expect_equal(as.vector(pi_results$pi_lower), as.vector(pi_lower_manual), tolerance = 0.01)
  expect_equal(as.vector(pi_results$pi_upper), as.vector(pi_upper_manual), tolerance = 0.01)
})


# ==============================================================================
# Test Suite 5: Evaluating Factor Smooth Effects
# ==============================================================================

test_that("evaluate_factor_smooth extracts education-specific effects", {
  # Create multi-education data
  n_months <- 100
  education_levels <- c("phd", "masters", "bachelors")

  set.seed(999)
  sim_data <- do.call(rbind, lapply(education_levels, function(educ) {
    time_index <- seq_len(n_months)
    month <- ((time_index - 1) %% 12) + 1

    # Different seasonal patterns by education
    baseline <- if (educ == "phd") -3.0 else if (educ == "masters") -2.8 else -2.6
    seasonal_amp <- if (educ == "phd") 0.2 else if (educ == "masters") 0.4 else 0.6

    logit_p <- baseline + seasonal_amp * sin(2 * pi * month / 12)

    n_total <- rep(1000, n_months)
    n_unemployed <- rbinom(n_months, size = n_total, prob = plogis(logit_p))

    data.frame(
      time_index = time_index,
      month = month,
      education = educ,
      n_unemployed = n_unemployed,
      n_total = n_total,
      stringsAsFactors = FALSE
    )
  }))

  sim_data$education <- factor(sim_data$education, levels = education_levels)

  # Fit full model
  model <- fit_factor_smooth_binomial(sim_data, "full", "binomial")

  # Evaluate factor smooth effects
  effects <- evaluate_factor_smooth(model, education_levels = education_levels)

  # Should return a list with effects for each education level
  expect_type(effects, "list")
  expect_equal(length(effects), length(education_levels))
  expect_equal(names(effects), education_levels)

  # Each element should be a data frame with predictions
  for (educ in education_levels) {
    expect_s3_class(effects[[educ]], "data.frame")
    expect_true("time_index" %in% names(effects[[educ]]) || "month" %in% names(effects[[educ]]))
    expect_true("fit" %in% names(effects[[educ]]))
    expect_true("se" %in% names(effects[[educ]]))
  }
})

test_that("evaluate_factor_smooth identifies differences between education levels", {
  # Create data where PhD and Masters have VERY different seasonal patterns
  n_months <- 120
  set.seed(1010)

  sim_data <- do.call(rbind, lapply(c("phd", "masters"), function(educ) {
    time_index <- seq_len(n_months)
    month <- ((time_index - 1) %% 12) + 1

    # PhD: small seasonal, Masters: large seasonal
    baseline <- -3.0
    seasonal_amp <- if (educ == "phd") 0.1 else 0.8  # 8× difference!

    logit_p <- baseline + seasonal_amp * sin(2 * pi * month / 12)

    n_total <- rep(1000, n_months)
    n_unemployed <- rbinom(n_months, size = n_total, prob = plogis(logit_p))

    data.frame(
      time_index = time_index,
      month = month,
      education = educ,
      n_unemployed = n_unemployed,
      n_total = n_total,
      stringsAsFactors = FALSE
    )
  }))

  sim_data$education <- factor(sim_data$education)

  model <- fit_factor_smooth_binomial(sim_data, "full", "binomial")
  effects <- evaluate_factor_smooth(model, education_levels = c("phd", "masters"))

  # Compute seasonal range for each education level
  # (This is a proxy for seasonal amplitude)
  range_phd <- max(effects$phd$fit) - min(effects$phd$fit)
  range_masters <- max(effects$masters$fit) - min(effects$masters$fit)

  # Masters should have much larger range than PhD
  expect_gt(
    range_masters,
    range_phd,
    label = "Masters should have larger seasonal range than PhD"
  )

  # Should be at least 2× larger (conservative, true difference is 8×)
  expect_gt(
    range_masters / range_phd,
    2,
    label = "Masters range should be substantially larger"
  )
})


# ==============================================================================
# Test Suite 6: Integration Test - Full Workflow
# ==============================================================================

test_that("INTEGRATION: full workflow for comparing binomial vs quasi-binomial on real-like data", {
  skip("Requires real CPS data - implement after GREEN phase")

  # This test will use actual CPS data structure:
  # 1. Check for overdispersion
  # 2. Fit both binomial and quasi-binomial
  # 3. Compare smoothing parameters
  # 4. Extract PIs and compare widths
  # 5. Evaluate education-specific effects
  # 6. Generate comparison report
})
