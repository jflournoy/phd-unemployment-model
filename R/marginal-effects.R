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
#' Extracts shock-specific time dynamics by comparing predictions
#' during shock and non-shock periods.
#'
#' @param model_result Result from fit_education_binomial_gam()
#' @param shock_type Character. Either "2008_2009" or "2020"
#'
#' @return Data frame with time indices, shock status, and fitted values
#'
#' @export
extract_shock_effects <- function(model_result, shock_type = "2008_2009") {
  model <- model_result$model
  data <- model_result$data

  if (shock_type == "2008_2009") {
    shock_col <- "shock_2008_2009"
    period_years <- 2007:2010
    shock_indicator <- 1
  } else if (shock_type == "2020") {
    shock_col <- "shock_2020"
    period_years <- 2019:2021
    shock_indicator <- 1
  } else {
    stop("shock_type must be '2008_2009' or '2020'")
  }

  # Create fine prediction grid for smooth shock curves (weekly granularity)
  # Use continuous sequence across entire period rather than just observed data
  min_time <- min(data$time_index[data$year == min(period_years)])
  max_time <- max(data$time_index[data$year == max(period_years)])
  time_indices <- seq(min_time, max_time, by = 0.25)  # Weekly for very smooth curves

  shock_effects <- data.frame()

  for (time_idx in time_indices) {
    # Use fractional year for smooth x-axis spacing (not integer grouping)
    year_frac <- 2000 + (time_idx - 1) / 12

    # Prediction with shock
    pred_shock <- data.frame(
      education = "phd",
      time_index = time_idx,
      month = 6,
      shock_2008_2009 = if (shock_type == "2008_2009") 1 else 0,
      shock_2020 = if (shock_type == "2020") 1 else 0
    )

    # Prediction without shock (counterfactual)
    pred_no_shock <- data.frame(
      education = "phd",
      time_index = time_idx,
      month = 6,
      shock_2008_2009 = 0,
      shock_2020 = 0
    )

    fit_shock <- predict(model, newdata = pred_shock, type = "response", se.fit = TRUE)
    fit_no_shock <- predict(model, newdata = pred_no_shock, type = "response", se.fit = TRUE)

    shock_effect <- fit_shock$fit - fit_no_shock$fit

    shock_effects <- rbind(shock_effects, data.frame(
      year = year_frac,
      time_index = time_idx,
      shock_period = shock_type,
      unemployment_rate_with_shock = fit_shock$fit,
      unemployment_rate_no_shock = fit_no_shock$fit,
      shock_effect = shock_effect,
      se = sqrt(fit_shock$se.fit^2 + fit_no_shock$se.fit^2)
    ))
  }

  rownames(shock_effects) <- NULL
  shock_effects
}

#' Extract Seasonal Marginal Effects
#'
#' Extracts seasonal patterns by examining unemployment across months
#' at a fixed time point (excluding time trends).
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
      shock_2008_2009 = 0,
      shock_2020 = 0
    )

    preds <- predict(model, newdata = pred_data, type = "response", se.fit = TRUE)

    # Round month to nearest integer for display (1-12)
    month_int <- round(month_val)
    if (month_int < 1) month_int <- 1
    if (month_int > 12) month_int <- 12

    seasonal_effects <- rbind(seasonal_effects, data.frame(
      month = month_val,
      month_name = month.abb[month_int],
      unemployment_rate = preds$fit,
      se = preds$se.fit,
      ci_lower = preds$fit - 1.96 * preds$se.fit,
      ci_upper = preds$fit + 1.96 * preds$se.fit
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
  p2 <- ggplot(shock_2008_effects, aes(x = year, y = shock_effect * 100)) +
    geom_point(color = "#A23B72", size = 2) +
    geom_line(color = "#A23B72") +
    geom_ribbon(
      aes(ymin = (shock_effect - 1.96 * se) * 100, ymax = (shock_effect + 1.96 * se) * 100),
      alpha = 0.2, fill = "#A23B72"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = "2008-2009 Financial Crisis Dynamics",
      x = "Year",
      y = "Unemployment Rate Effect (%)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  # Plot 3: 2020 Shock Dynamics
  p3 <- ggplot(shock_2020_effects, aes(x = year, y = shock_effect * 100)) +
    geom_point(color = "#C73E1D", size = 2) +
    geom_line(color = "#C73E1D") +
    geom_ribbon(
      aes(ymin = (shock_effect - 1.96 * se) * 100, ymax = (shock_effect + 1.96 * se) * 100),
      alpha = 0.2, fill = "#C73E1D"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = "2020 COVID-19 Pandemic Dynamics",
      x = "Year",
      y = "Unemployment Rate Effect (%)"
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
