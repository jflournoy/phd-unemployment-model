#' Extract and Visualize Education Main Effects
#'
#' Extracts the main education effects from a fitted GAM model and returns
#' a data frame suitable for visualization.
#'
#' @param model_result Result from fit_education_binomial_gam()
#' @param reference_level Reference education level for comparison (default: "bachelors")
#'
#' @return Data frame with education levels, estimates, and standard errors
#'
#' @export
extract_education_effects <- function(model_result, reference_level = "bachelors") {
  model <- model_result$model
  coefs <- model$coefficients
  ses <- sqrt(diag(model$Vp))

  # Education coefficients (relative to first level which is intercept)
  edu_levels <- unique(model_result$data$education)
  edu_effects <- data.frame()

  for (i in seq_along(edu_levels)) {
    level <- edu_levels[i]
    if (i == 1) {
      # First level is the reference/intercept
      effect <- 0
      se <- 0
    } else {
      # Other levels have education parameter
      param_name <- paste0("education", level)
      if (param_name %in% names(coefs)) {
        effect <- coefs[param_name]
        se <- ses[which(names(coefs) == param_name)]
      } else {
        effect <- NA
        se <- NA
      }
    }

    edu_effects <- rbind(edu_effects, data.frame(
      education = level,
      effect = effect,
      se = se,
      ci_lower = effect - 1.96 * se,
      ci_upper = effect + 1.96 * se
    ))
  }

  rownames(edu_effects) <- NULL
  edu_effects
}

#' Extract Shock Dynamics Marginal Effects
#'
#' Extracts shock-specific time dynamics using the fuzzy impulse model approach.
#' The model learns how unemployment responds to continuous shock intensity.
#' Shock effects are computed as fitted(actual intensity) - fitted(intensity=0).
#'
#' @param model_result Result from fit_education_binomial_gam()
#' @param shock_type Character. Either "2008_2009" or "2020"
#' @param education_level Character. Education level to extract (default: "phd")
#' @param extend_years How many years past the acute phase to show decay (default: 5)
#'
#' @return Data frame with time indices, observed and counterfactual rates,
#'   shock intensities, and shock effects with confidence intervals
#'
#' @export
extract_shock_effects <- function(model_result, shock_type = "2008_2009",
                                   education_level = "phd",
                                   extend_years = 5) {
  model <- model_result$model
  data <- model_result$data
  preds <- model_result$predictions

  # Define time range based on shock type
  # Include extended range to show persistence/decay
  if (shock_type == "2008_2009") {
    start_year <- 2007
    end_year <- 2010 + extend_years  # Show decay through 2015
    onset <- 2007.5
    peak <- 2009.75
    halflife <- 2.5
  } else if (shock_type == "2020") {
    start_year <- 2020
    end_year <- 2021 + extend_years  # Show decay through 2026
    onset <- 2020.17
    peak <- 2020.33
    halflife <- 1.5
  } else {
    stop("shock_type must be '2008_2009' or '2020'")
  }

  # Create fine prediction grid (monthly) for the extended range
  year_seq <- seq(start_year, min(end_year, max(data$year)), by = 1/12)

  shock_effects <- data.frame()

  for (year_frac in year_seq) {
    # Convert year to time_index
    time_idx <- (year_frac - 2000) * 12 + 1

    # Calculate shock intensities for this time point
    if (shock_type == "2008_2009") {
      shock_2008_intensity <- create_shock_impulse(year_frac, onset, peak, halflife)
      shock_2020_intensity <- 0
    } else {
      shock_2008_intensity <- 0
      shock_2020_intensity <- create_shock_impulse(year_frac, onset, peak, halflife)
    }

    # Create prediction data for ACTUAL scenario (with shock)
    actual_data <- data.frame(
      education = factor(education_level, levels = levels(data$education)),
      time_index = time_idx,
      month = 6,  # Mid-year for seasonal average
      shock_2008_intensity = shock_2008_intensity,
      shock_2020_intensity = shock_2020_intensity
    )

    # Create prediction data for COUNTERFACTUAL (no shock)
    counterfactual_data <- data.frame(
      education = factor(education_level, levels = levels(data$education)),
      time_index = time_idx,
      month = 6,
      shock_2008_intensity = 0,
      shock_2020_intensity = 0
    )

    # Get predictions from model
    actual_pred <- mgcv::predict.bam(model, newdata = actual_data,
                                      type = "response", se.fit = TRUE)
    counterfactual_pred <- mgcv::predict.bam(model, newdata = counterfactual_data,
                                              type = "response", se.fit = TRUE)

    # Find nearest observed data point for this time index (if available)
    nearest_idx <- which.min(abs(preds$time_index - time_idx) +
                              ifelse(preds$education == education_level, 0, 1000))
    observed_rate <- if (length(nearest_idx) > 0 &&
                         abs(preds$time_index[nearest_idx] - time_idx) < 1) {
      preds$observed_rate[nearest_idx]
    } else {
      NA
    }

    # Shock effect is actual - counterfactual
    fitted_rate <- as.numeric(actual_pred$fit)
    counterfactual_rate <- as.numeric(counterfactual_pred$fit)
    shock_effect <- fitted_rate - counterfactual_rate

    # Standard error approximation
    se_effect <- sqrt(as.numeric(actual_pred$se.fit)^2 +
                       as.numeric(counterfactual_pred$se.fit)^2)

    # Current shock intensity for this time point
    current_intensity <- if (shock_type == "2008_2009") {
      shock_2008_intensity
    } else {
      shock_2020_intensity
    }

    shock_effects <- rbind(shock_effects, data.frame(
      year = year_frac,
      time_index = time_idx,
      education = education_level,
      shock_type = shock_type,
      shock_intensity = current_intensity,
      observed_rate = observed_rate,
      fitted_rate = fitted_rate,
      counterfactual_rate = counterfactual_rate,
      shock_effect = shock_effect,
      se = se_effect,
      ci_lower = shock_effect - 1.96 * se_effect,
      ci_upper = shock_effect + 1.96 * se_effect
    ))
  }

  rownames(shock_effects) <- NULL
  shock_effects
}

#' Extract Seasonal Marginal Effects
#'
#' Extracts seasonal patterns by examining unemployment across months
#' at a fixed time point (excluding time trends). Uses zero shock intensities
#' for clean seasonal estimation.
#'
#' @param model_result Result from fit_education_binomial_gam()
#' @param time_point Time index for extraction (default: 200 = mid-series)
#' @param education_level Education level to extract (default: "phd")
#'
#' @return Data frame with months and seasonal unemployment effects
#'
#' @export
extract_seasonal_effects <- function(model_result, time_point = 200, education_level = "phd") {
  model <- model_result$model
  data <- model_result$data

  # Year corresponding to time_point
  year <- 2000 + floor((time_point - 1) / 12)

  seasonal_effects <- data.frame()

  # Create fine monthly grid for smooth seasonal curves (weekly interpolation within months)
  month_seq <- seq(1, 12, by = 1/4.29)  # ~4 weeks per month for smooth appearance

  for (month_val in month_seq) {
    pred_data <- data.frame(
      education = factor(education_level, levels = levels(data$education)),
      time_index = time_point,
      month = month_val,
      # Use zero shock intensity for clean seasonal estimation
      shock_2008_intensity = 0,
      shock_2020_intensity = 0
    )

    # Handle bam() models - use mgcv::predict.bam explicitly
    preds <- mgcv::predict.bam(model, newdata = pred_data,
                                type = "response", se.fit = TRUE)

    # Round month to nearest integer for display (1-12)
    month_int <- round(month_val)
    if (month_int < 1) month_int <- 1
    if (month_int > 12) month_int <- 12

    seasonal_effects <- rbind(seasonal_effects, data.frame(
      month = month_val,
      month_name = month.abb[month_int],
      unemployment_rate = as.numeric(preds$fit),
      se = as.numeric(preds$se.fit),
      ci_lower = as.numeric(preds$fit) - 1.96 * as.numeric(preds$se.fit),
      ci_upper = as.numeric(preds$fit) + 1.96 * as.numeric(preds$se.fit)
    ))
  }

  rownames(seasonal_effects) <- NULL
  seasonal_effects
}

#' Create Marginal Effects Visualization Panel
#'
#' Creates a comprehensive multi-panel plot showing:
#' - Education main effects
#' - 2008-2009 shock dynamics
#' - 2020 shock dynamics
#' - Seasonal effects
#'
#' @param model_result Result from fit_education_binomial_gam()
#'
#' @return A ggplot object with four panels
#'
#' @export
plot_marginal_effects <- function(model_result) {
  library(ggplot2)
  library(gridExtra)

  # Extract effects
  edu_effects <- extract_education_effects(model_result)
  shock_2008_effects <- extract_shock_effects(model_result, "2008_2009")
  shock_2020_effects <- extract_shock_effects(model_result, "2020")
  seasonal_effects <- extract_seasonal_effects(model_result)

  # Plot 1: Education Main Effects
  p1 <- ggplot(edu_effects, aes(x = reorder(education, effect), y = effect)) +
    geom_point(size = 3, color = "#2E86AB") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "#2E86AB") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    coord_flip() +
    labs(
      title = "Education Main Effects",
      x = "Education Level",
      y = "Log-odds Effect (relative to reference)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  # Plot 2: 2008-2009 Shock Dynamics
  # Show both observed-counterfactual and the counterfactual baseline
  p2 <- ggplot(shock_2008_effects, aes(x = year)) +
    geom_ribbon(
      aes(ymin = ci_lower * 100, ymax = ci_upper * 100),
      alpha = 0.2, fill = "#A23B72"
    ) +
    geom_line(aes(y = shock_effect * 100), color = "#A23B72", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = "2008-2009 Financial Crisis Dynamics (PhD)",
      subtitle = "Shock effect = observed - counterfactual",
      x = "Year",
      y = "Unemployment Rate Effect (pp)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  # Plot 3: 2020 Shock Dynamics
  p3 <- ggplot(shock_2020_effects, aes(x = year)) +
    geom_ribbon(
      aes(ymin = ci_lower * 100, ymax = ci_upper * 100),
      alpha = 0.2, fill = "#C73E1D"
    ) +
    geom_line(aes(y = shock_effect * 100), color = "#C73E1D", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = "2020 COVID-19 Pandemic Dynamics (PhD)",
      subtitle = "Shock effect = observed - counterfactual",
      x = "Year",
      y = "Unemployment Rate Effect (pp)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  # Plot 4: Seasonal Effects
  p4 <- ggplot(seasonal_effects, aes(x = month, y = unemployment_rate * 100)) +
    geom_point(color = "#6A994E", size = 2) +
    geom_line(color = "#6A994E") +
    geom_ribbon(
      aes(ymin = ci_lower * 100, ymax = ci_upper * 100),
      alpha = 0.2, fill = "#6A994E"
    ) +
    scale_x_continuous(
      breaks = 1:12,
      labels = month.abb
    ) +
    labs(
      title = "Seasonal Effects (PhD level)",
      x = "Month",
      y = "Unemployment Rate (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  # Combine plots
  gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
}
