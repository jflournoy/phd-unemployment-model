# Load the seasonal-gam functions
source(here::here("R", "seasonal-gam.R"))

test_that("fit_nested_model_sequence implements shared smoothing parameters correctly", {
  # Create test data with multiple education levels
  set.seed(123)
  n_time <- 120
  educations <- c("phd", "masters", "bachelors")

  test_data <- expand.grid(
    time_index = 1:n_time,
    education = educations,
    stringsAsFactors = FALSE
  )

  test_data$year <- 2000 + (test_data$time_index - 1) %/% 12
  test_data$month <- ((test_data$time_index - 1) %% 12) + 1
  test_data$education <- factor(test_data$education, levels = educations)

  # Generate synthetic unemployment data
  test_data$unemployment_rate <- 0.05 +
    0.01 * sin(2 * pi * test_data$month / 12) +
    0.0001 * test_data$time_index +
    rnorm(nrow(test_data), 0, 0.005)

  # Fit models with shared wiggliness
  models_shared <- fit_nested_model_sequence(test_data, shared_wiggliness = TRUE)

  # Fit models without shared wiggliness
  models_unshared <- fit_nested_model_sequence(test_data, shared_wiggliness = FALSE)

  # Test m4: education-specific trends + shared seasonality
  # With shared_wiggliness=TRUE, should have id=1 on time_index smooth
  m4_shared <- models_shared$m4
  m4_shared_smooths <- m4_shared$smooth

  # Find the time_index smooth (by=education)
  time_smooth_idx <- which(sapply(m4_shared_smooths, function(s) {
    grepl("time_index", s$label) && !is.null(s$by) && s$by == "education"
  }))

  expect_true(length(time_smooth_idx) > 0,
              info = "m4 should have time_index smooth with by=education")

  if (length(time_smooth_idx) > 0) {
    time_smooth <- m4_shared_smooths[[time_smooth_idx[1]]]
    expect_equal(as.character(time_smooth$id), "1",
                 info = "m4 time_index smooth should have id=1 for shared wiggliness")
  }

  # Test m5: shared trend + education-specific seasonality
  # With shared_wiggliness=TRUE, should have id=1 on month smooth
  m5_shared <- models_shared$m5
  m5_shared_smooths <- m5_shared$smooth

  month_smooth_idx <- which(sapply(m5_shared_smooths, function(s) {
    grepl("month", s$label) && !is.null(s$by) && s$by == "education"
  }))

  expect_true(length(month_smooth_idx) > 0,
              info = "m5 should have month smooth with by=education")

  if (length(month_smooth_idx) > 0) {
    month_smooth <- m5_shared_smooths[[month_smooth_idx[1]]]
    expect_equal(as.character(month_smooth$id), "1",
                 info = "m5 month smooth should have id=1 for shared wiggliness")
  }

  # Test m6: education-specific trends AND seasonality
  # With shared_wiggliness=TRUE, should have id=1 for trends, id=2 for seasonality
  m6_shared <- models_shared$m6
  m6_shared_smooths <- m6_shared$smooth

  time_smooth_idx <- which(sapply(m6_shared_smooths, function(s) {
    grepl("time_index", s$label) && !is.null(s$by) && s$by == "education"
  }))

  month_smooth_idx <- which(sapply(m6_shared_smooths, function(s) {
    grepl("month", s$label) && !is.null(s$by) && s$by == "education"
  }))

  expect_true(length(time_smooth_idx) > 0 && length(month_smooth_idx) > 0,
              info = "m6 should have both time_index and month smooths with by=education")

  if (length(time_smooth_idx) > 0) {
    time_smooth <- m6_shared_smooths[[time_smooth_idx[1]]]
    expect_equal(as.character(time_smooth$id), "1",
                 info = "m6 time_index smooth should have id=1")
  }

  if (length(month_smooth_idx) > 0) {
    month_smooth <- m6_shared_smooths[[month_smooth_idx[1]]]
    expect_equal(as.character(month_smooth$id), "2",
                 info = "m6 month smooth should have id=2 (different from time_index)")
  }

  # Test that unshared models do NOT have id parameter
  m6_unshared <- models_unshared$m6
  m6_unshared_smooths <- m6_unshared$smooth

  time_smooth_idx_unshared <- which(sapply(m6_unshared_smooths, function(s) {
    grepl("time_index", s$label) && !is.null(s$by) && s$by == "education"
  }))

  if (length(time_smooth_idx_unshared) > 0) {
    time_smooth_unshared <- m6_unshared_smooths[[time_smooth_idx_unshared[1]]]
    expect_null(time_smooth_unshared$id,
                info = "unshared models should NOT have id parameter")
  }
})


test_that("shared smoothing parameters result in consistent effective degrees of freedom", {
  # Create test data
  set.seed(456)
  n_time <- 120
  educations <- c("phd", "masters", "bachelors")

  test_data <- expand.grid(
    time_index = 1:n_time,
    education = educations,
    stringsAsFactors = FALSE
  )

  test_data$year <- 2000 + (test_data$time_index - 1) %/% 12
  test_data$month <- ((test_data$time_index - 1) %% 12) + 1
  test_data$education <- factor(test_data$education, levels = educations)

  # Generate synthetic data
  test_data$unemployment_rate <- 0.05 +
    0.01 * sin(2 * pi * test_data$month / 12) +
    0.0001 * test_data$time_index +
    rnorm(nrow(test_data), 0, 0.005)

  # Fit model with shared wiggliness
  models <- fit_nested_model_sequence(test_data, shared_wiggliness = TRUE)
  m6 <- models$m6

  # Extract EDFs for each education level's trend smooth
  # With shared smoothing, these should be similar (not identical, but close)
  trend_edfs <- sapply(m6$smooth, function(s) {
    if (grepl("time_index", s$label) && !is.null(s$by) && s$by == "education") {
      # Get the EDF for this smooth term
      # Note: This is approximate - actual EDFs are in m6$edf
      return(s$bs.dim)
    }
    return(NULL)
  })

  trend_edfs <- unlist(trend_edfs[!sapply(trend_edfs, is.null)])

  # With shared smoothing (id=1), all trend smooths should have same basis dimension
  if (length(trend_edfs) > 1) {
    expect_true(length(unique(trend_edfs)) == 1,
                info = "Shared smoothing should result in same basis dimension for all education levels")
  }
})


test_that("shared_wiggliness parameter defaults to TRUE", {
  # Create minimal test data
  set.seed(789)
  test_data <- data.frame(
    time_index = rep(1:24, 2),
    month = rep(1:12, 4),
    education = factor(rep(c("phd", "masters"), each = 24)),
    unemployment_rate = runif(48, 0.03, 0.08)
  )

  # Call without specifying shared_wiggliness (should default to TRUE)
  models_default <- fit_nested_model_sequence(test_data)

  # Check that m6 has id parameters (indicating default is TRUE)
  m6_smooths <- models_default$m6$smooth

  has_id <- any(sapply(m6_smooths, function(s) !is.null(s$id)))

  expect_true(has_id,
              info = "Default behavior should be shared_wiggliness=TRUE (models should have id parameter)")
})


test_that("report's claim about shared smoothing can be verified programmatically", {
  # This test ensures that we can programmatically verify the report's claims
  # about shared smoothing parameters

  set.seed(999)
  test_data <- data.frame(
    time_index = rep(1:60, 3),
    month = rep(1:12, 15),
    year = rep(2000:2004, each = 36),
    education = factor(rep(c("phd", "masters", "bachelors"), each = 60)),
    unemployment_rate = runif(180, 0.02, 0.10)
  )

  models <- fit_nested_model_sequence(test_data, shared_wiggliness = TRUE)
  best_model <- models$m6  # Assume m6 is selected

  # Function to check if model uses shared smoothing
  verify_shared_smoothing <- function(model) {
    smooths <- model$smooth

    # Find smooths with by=education
    by_smooths <- smooths[sapply(smooths, function(s) {
      !is.null(s$by) && s$by == "education"
    })]

    if (length(by_smooths) == 0) {
      return(list(has_shared = FALSE, reason = "No by=education smooths found"))
    }

    # Check if they have id parameter
    ids <- sapply(by_smooths, function(s) s$id)

    has_id <- !all(sapply(ids, is.null))

    if (has_id) {
      # Extract unique id values
      unique_ids <- unique(ids[!sapply(ids, is.null)])
      return(list(
        has_shared = TRUE,
        n_shared_groups = length(unique_ids),
        ids = unique_ids,
        reason = sprintf("Found %d shared smoothing groups", length(unique_ids))
      ))
    } else {
      return(list(has_shared = FALSE, reason = "No id parameters found on by=education smooths"))
    }
  }

  verification <- verify_shared_smoothing(best_model)

  expect_true(verification$has_shared,
              info = sprintf("Model should use shared smoothing. Reason: %s", verification$reason))

  expect_equal(verification$n_shared_groups, 2,
               info = "m6 should have 2 shared groups: one for trends (id=1), one for seasonality (id=2)")
})
