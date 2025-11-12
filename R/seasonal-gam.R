#' Simulate Seasonal Unemployment Time Series
#'
#' Generates synthetic unemployment rate data with trend and seasonal components
#' for testing and model validation.
#'
#' @param n_years Integer. Number of years to simulate (default: 5)
#' @param baseline_rate Numeric. Baseline unemployment rate (default: 0.02)
#' @param trend_slope Numeric. Linear trend slope per time unit (default: 0.001)
#' @param seasonal_amplitude Numeric. Amplitude of seasonal variation (default: 0.005)
#' @param noise_sd Numeric. Standard deviation of random noise (default: 0.002)
#' @param seed Integer. Random seed for reproducibility (default: NULL)
#'
#' @return Data frame with columns:
#'   \item{time_index}{Sequential time index (1 to n_months)}
#'   \item{year}{Year}
#'   \item{month}{Month (1-12)}
#'   \item{unemployment_rate}{Simulated unemployment rate}
#'
#' @details
#' The simulation model is:
#' \deqn{y_t = baseline + trend * t + amplitude * sin(2\pi * month / 12) + \epsilon}
#'
#' Where:
#' - Trend captures long-term changes
#' - Seasonal component uses sinusoidal pattern (peak in summer months)
#' - Noise adds realistic variation
#'
#' @examples
#' \dontrun{
#' # Simulate 5 years with moderate seasonality
#' sim_data <- simulate_seasonal_unemployment(
#'   n_years = 5,
#'   seasonal_amplitude = 0.005
#' )
#'
#' # Strong seasonality, no trend
#' sim_data <- simulate_seasonal_unemployment(
#'   n_years = 10,
#'   trend_slope = 0,
#'   seasonal_amplitude = 0.01
#' )
#' }
#'
#' @export
simulate_seasonal_unemployment <- function(n_years = 5,
                                           baseline_rate = 0.02,
                                           trend_slope = 0.001,
                                           seasonal_amplitude = 0.005,
                                           noise_sd = 0.002,
                                           seed = NULL) {
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Create time grid
  n_months <- n_years * 12
  time_index <- 1:n_months

  # Calculate year and month
  year <- rep(1:n_years, each = 12)
  month <- rep(1:12, times = n_years)

  # Generate seasonal component (sinusoidal with peak in summer)
  # Phase shift to put peak around June-July (month 6-7)
  seasonal_component <- seasonal_amplitude * sin(2 * pi * (month - 3) / 12)

  # Generate trend component
  trend_component <- trend_slope * time_index

  # Combine components
  unemployment_rate <- baseline_rate + trend_component + seasonal_component + rnorm(n_months, 0, noise_sd)

  # Ensure rates are in valid range [0, 1]
  unemployment_rate <- pmax(0, pmin(1, unemployment_rate))

  # Create data frame
  data.frame(
    time_index = time_index,
    year = year,
    month = month,
    unemployment_rate = unemployment_rate
  )
}

#' Fit Seasonal GAM with Cyclic Splines
#'
#' Fits a Generalized Additive Model for seasonal unemployment data using
#' mgcv with cyclic splines for month and smooth trend.
#'
#' @param data Data frame. Must contain: time_index, month, unemployment_rate
#' @param k_month Integer. Basis dimension for cyclic month spline (default: 12)
#' @param k_trend Integer. Basis dimension for trend spline (default: 20)
#' @param family Character. Model family (default: "gaussian")
#'
#' @return mgcv GAM model object
#'
#' @details
#' Model formula:
#' \code{unemployment_rate ~ s(time_index, bs = "cr", k = k_trend) + s(month, bs = "cc", k = k_month)}
#'
#' Where:
#' - \code{bs = "cr"}: Cubic regression spline for trend (flexible, non-linear)
#' - \code{bs = "cc"}: Cyclic cubic spline for month (ensures Dec→Jan continuity)
#' - \code{k}: Number of basis functions (controls smoothness)
#'
#' The cyclic constraint ensures the seasonal pattern wraps smoothly from
#' December to January, which is essential for modeling repeating annual cycles.
#'
#' @examples
#' \dontrun{
#' sim_data <- simulate_seasonal_unemployment(n_years = 5)
#' model <- fit_seasonal_gam(sim_data)
#' summary(model)
#' plot(model, pages = 1)
#' }
#'
#' @export
fit_seasonal_gam <- function(data,
                              k_month = 12,
                              k_trend = 20,
                              family = "gaussian") {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  required_vars <- c("time_index", "month", "unemployment_rate")
  missing_vars <- setdiff(required_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("data missing required variables: ", paste(missing_vars, collapse = ", "))
  }

  # Check if mgcv package is available
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("mgcv package required. Install with: install.packages('mgcv')")
  }

  # Fit GAM with cyclic spline for month and smooth trend
  # bs = "cc" = cyclic cubic spline (ensures Dec-Jan continuity)
  # bs = "cr" = cubic regression spline (flexible trend)
  model <- mgcv::gam(
    unemployment_rate ~ s(time_index, bs = "cr", k = k_trend) +
                        s(month, bs = "cc", k = k_month),
    data = data,
    family = family,
    method = "REML"  # Restricted Maximum Likelihood for smoothness selection
  )

  return(model)
}

#' Extract Seasonal Component from GAM
#'
#' Extracts the estimated seasonal effects (month-specific deviations) from
#' a fitted GAM model.
#'
#' @param model mgcv GAM model object from fit_seasonal_gam()
#' @param data Data frame. Original data used to fit model
#'
#' @return Data frame with columns:
#'   \item{month}{Month (1-12)}
#'   \item{seasonal_effect}{Estimated seasonal effect}
#'   \item{se}{Standard error of the effect}
#'
#' @details
#' Uses mgcv::predict.gam() with type = "terms" to extract the smooth
#' component corresponding to the cyclic month effect.
#'
#' @examples
#' \dontrun{
#' sim_data <- simulate_seasonal_unemployment(n_years = 5)
#' model <- fit_seasonal_gam(sim_data)
#' seasonal <- extract_seasonal_component(model, sim_data)
#' plot(seasonal$month, seasonal$seasonal_effect, type = "b")
#' }
#'
#' @export
extract_seasonal_component <- function(model, data) {
  if (!inherits(model, "gam")) {
    stop("model must be a GAM object from mgcv")
  }

  # Create prediction grid for all 12 months
  # Use mean time_index to isolate seasonal effect
  pred_data <- data.frame(
    month = 1:12,
    time_index = mean(data$time_index)
  )

  # Get predictions with standard errors
  preds <- predict(model, newdata = pred_data, type = "terms", se.fit = TRUE)

  # Extract month component (second smooth term)
  month_idx <- which(grepl("month", colnames(preds$fit)))
  seasonal_effect <- preds$fit[, month_idx]
  se <- preds$se.fit[, month_idx]

  # Return as data frame
  data.frame(
    month = 1:12,
    seasonal_effect = seasonal_effect,
    se = se
  )
}

#' Extract Trend Component from GAM
#'
#' Extracts the estimated time trend from a fitted GAM model.
#'
#' @param model mgcv GAM model object from fit_seasonal_gam()
#' @param data Data frame. Original data used to fit model
#' @param absolute Logical. If TRUE (default), returns absolute trend values
#'   (intercept + trend component). If FALSE, returns centered trend component
#'   (deviations from intercept).
#'
#' @return Data frame with columns:
#'   \item{time_index}{Time index}
#'   \item{trend_effect}{Estimated trend effect (absolute or centered)}
#'   \item{se}{Standard error of the effect}
#'
#' @details
#' Uses mgcv::predict.gam() to extract the smooth component corresponding to
#' the time trend. By default, returns absolute values (intercept + trend)
#' which are easier to interpret and compare with true values.
#'
#' GAM smooths are sum-to-zero constrained, so the centered trend component
#' oscillates around 0. The absolute trend shows the actual predicted values
#' over time (averaged over seasonal variation).
#'
#' @examples
#' \dontrun{
#' sim_data <- simulate_seasonal_unemployment(n_years = 5)
#' model <- fit_seasonal_gam(sim_data)
#'
#' # Absolute trend (default) - easier to interpret
#' trend_abs <- extract_trend_component(model, sim_data)
#' plot(trend_abs$time_index, trend_abs$trend_effect, type = "l")
#'
#' # Centered trend - shows deviations from intercept
#' trend_centered <- extract_trend_component(model, sim_data, absolute = FALSE)
#' }
#'
#' @export
extract_trend_component <- function(model, data, absolute = TRUE) {
  if (!inherits(model, "gam")) {
    stop("model must be a GAM object from mgcv")
  }

  time_points <- unique(data$time_index)

  if (absolute) {
    # Get absolute trend by averaging predictions over all months
    # This removes seasonal variation and gives baseline + trend
    trend_effect <- numeric(length(time_points))
    se_squared_sum <- numeric(length(time_points))

    for (i in seq_along(time_points)) {
      pred_grid <- data.frame(
        time_index = time_points[i],
        month = 1:12
      )
      preds <- predict(model, newdata = pred_grid, se.fit = TRUE)
      trend_effect[i] <- mean(preds$fit)
      # SE of mean: sqrt(sum(se^2)) / n
      se_squared_sum[i] <- sum(preds$se.fit^2)
    }

    se <- sqrt(se_squared_sum) / 12
  } else {
    # Get centered trend component only (deviations from intercept)
    # Use representative month to isolate trend
    pred_data <- data.frame(
      time_index = time_points,
      month = 6  # Mid-year month
    )
    preds <- predict(model, newdata = pred_data, type = "terms", se.fit = TRUE)
    trend_idx <- which(grepl("time_index", colnames(preds$fit)))
    trend_effect <- preds$fit[, trend_idx]
    se <- preds$se.fit[, trend_idx]
  }

  # Return as data frame
  data.frame(
    time_index = time_points,
    trend_effect = trend_effect,
    se = se
  )
}

#' Compare Different Model Formulations
#'
#' Fits multiple GAM formulations and compares their performance using
#' AIC, BIC, R², and effective degrees of freedom.
#'
#' @param data Data frame with time series data
#'
#' @return Data frame with model comparison metrics, sorted by AIC (best first)
#'
#' @details
#' Compares the following models:
#' 1. Cyclic spline for month + smooth trend (recommended)
#' 2. Fourier basis for month + smooth trend
#' 3. Factor smooth (random effect) for month + smooth trend
#' 4. Tensor product smooth (trend-season interaction)
#' 5. Linear trend + cyclic month (simpler baseline)
#'
#' @examples
#' \dontrun{
#' sim_data <- simulate_seasonal_unemployment(n_years = 5)
#' comparison <- compare_model_formulations(sim_data)
#' print(comparison)
#' }
#'
#' @export
compare_model_formulations <- function(data) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("mgcv package required")
  }

  # Model 1: Cyclic spline + smooth trend (recommended)
  m1 <- mgcv::gam(unemployment_rate ~ s(time_index, bs = "cr", k = 20) +
                                       s(month, bs = "cc", k = 12),
                   data = data, method = "REML")

  # Model 2: Fourier basis + smooth trend
  m2 <- mgcv::gam(unemployment_rate ~ s(time_index, bs = "cr", k = 20) +
                                       s(month, bs = "fs", k = 6),
                   data = data, method = "REML")

  # Model 3: Random effect for month + smooth trend
  m3 <- mgcv::gam(unemployment_rate ~ s(time_index, bs = "cr", k = 20) +
                                       s(month, bs = "re"),
                   data = data, method = "REML")

  # Model 4: Tensor product (interaction between trend and season)
  m4 <- mgcv::gam(unemployment_rate ~ te(time_index, month, k = c(10, 12)),
                   data = data, method = "REML")

  # Model 5: Linear trend + cyclic month (simpler)
  m5 <- mgcv::gam(unemployment_rate ~ time_index + s(month, bs = "cc", k = 12),
                   data = data, method = "REML")

  # Extract metrics for each model
  models <- list(m1, m2, m3, m4, m5)
  model_names <- c(
    "Cyclic + Smooth Trend",
    "Fourier + Smooth Trend",
    "Random Effect + Smooth Trend",
    "Tensor Product",
    "Linear + Cyclic"
  )

  results <- data.frame(
    model_name = model_names,
    aic = sapply(models, AIC),
    bic = sapply(models, BIC),
    r_squared = sapply(models, function(m) summary(m)$r.sq),
    edf = sapply(models, function(m) sum(m$edf)),
    deviance_explained = sapply(models, function(m) summary(m)$dev.expl * 100)
  )

  # Rank by AIC (lower is better)
  results$rank <- rank(results$aic)
  results <- results[order(results$aic), ]

  return(results)
}

#' Plot Seasonal Decomposition
#'
#' Creates diagnostic plot showing observed data, fitted values, trend component,
#' and seasonal component from a GAM model.
#'
#' @param model mgcv GAM model object
#' @param data Data frame. Original data used to fit model
#'
#' @return NULL (creates plot)
#'
#' @details
#' Creates a 3-panel plot:
#' 1. Observed vs fitted values over time
#' 2. Estimated time trend with confidence bands
#' 3. Estimated seasonal pattern by month with confidence bands
#'
#' @examples
#' \dontrun{
#' sim_data <- simulate_seasonal_unemployment(n_years = 5)
#' model <- fit_seasonal_gam(sim_data)
#' plot_seasonal_decomposition(model, sim_data)
#' }
#'
#' @export
plot_seasonal_decomposition <- function(model, data) {
  if (!inherits(model, "gam")) {
    stop("model must be a GAM object from mgcv")
  }

  # Set up 3-panel plot
  par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

  # Panel 1: Observed vs fitted
  fitted_vals <- fitted(model)
  plot(data$time_index, data$unemployment_rate,
       type = "l", col = "gray50", lwd = 1,
       xlab = "Time Index", ylab = "Unemployment Rate",
       main = "Observed vs Fitted Values")
  lines(data$time_index, fitted_vals, col = "blue", lwd = 2)
  legend("topright", legend = c("Observed", "Fitted"),
         col = c("gray50", "blue"), lwd = c(1, 2), cex = 0.8)

  # Panel 2: Trend component
  trend <- extract_trend_component(model, data)
  plot(trend$time_index, trend$trend_effect,
       type = "l", col = "darkgreen", lwd = 2,
       xlab = "Time Index", ylab = "Trend Effect",
       main = "Estimated Time Trend")
  # Add confidence bands
  lines(trend$time_index, trend$trend_effect + 2 * trend$se, lty = 2, col = "darkgreen")
  lines(trend$time_index, trend$trend_effect - 2 * trend$se, lty = 2, col = "darkgreen")

  # Panel 3: Seasonal component
  seasonal <- extract_seasonal_component(model, data)
  plot(seasonal$month, seasonal$seasonal_effect,
       type = "b", col = "red", lwd = 2, pch = 19,
       xlab = "Month", ylab = "Seasonal Effect",
       main = "Estimated Seasonal Pattern",
       xaxt = "n")
  axis(1, at = 1:12, labels = month.abb)
  # Add confidence bands
  arrows(seasonal$month, seasonal$seasonal_effect - 2 * seasonal$se,
         seasonal$month, seasonal$seasonal_effect + 2 * seasonal$se,
         length = 0.05, angle = 90, code = 3, col = "red")

  # Reset plotting parameters
  par(mfrow = c(1, 1))

  invisible(NULL)
}

#' Plot Seasonal Decomposition using ggplot2
#'
#' Creates diagnostic plots showing observed vs fitted values, trend component,
#' and seasonal pattern using ggplot2 for better customization and appearance.
#'
#' @param model mgcv GAM model object
#' @param data Data frame. Original data used to fit model
#' @param true_params Optional list with true parameter values for comparison:
#'   \item{baseline}{True baseline unemployment rate}
#'   \item{trend_slope}{True linear trend slope}
#'   \item{seasonal_amplitude}{True seasonal amplitude}
#'
#' @return List of three ggplot objects:
#'   \item{observed_fitted}{Observed vs fitted values}
#'   \item{trend}{Trend component with confidence bands (absolute values)}
#'   \item{seasonal}{Seasonal pattern with error bars}
#'
#' @details
#' This is the ggplot2 version of plot_seasonal_decomposition(). It returns
#' a list of ggplot objects that can be combined with patchwork or gridExtra,
#' or displayed individually for customization.
#'
#' The trend plot now shows ABSOLUTE values (intercept + trend component) by
#' default, making it easier to compare with true values and interpret results.
#' If true_params are provided, they will be overlaid for visual comparison.
#'
#' @examples
#' \dontrun{
#' sim_data <- simulate_seasonal_unemployment(n_years = 5)
#' model <- fit_seasonal_gam(sim_data)
#' plots <- plot_seasonal_decomposition_ggplot(model, sim_data)
#'
#' # Display all three panels
#' library(patchwork)
#' plots[[1]] / plots[[2]] / plots[[3]]
#'
#' # With true parameters for validation
#' true_params <- list(baseline = 0.02, trend_slope = 0.001, seasonal_amplitude = 0.005)
#' plots <- plot_seasonal_decomposition_ggplot(model, sim_data, true_params)
#' }
#'
#' @export
plot_seasonal_decomposition_ggplot <- function(model, data, true_params = NULL) {
  if (!inherits(model, "gam")) {
    stop("model must be a GAM object from mgcv")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required. Install with: install.packages('ggplot2')")
  }

  # Extract components
  fitted_vals <- fitted(model)
  trend <- extract_trend_component(model, data)
  seasonal <- extract_seasonal_component(model, data)

  # Determine if we should use date or time_index for x-axis
  use_date <- "date" %in% names(data)

  # Panel 1: Observed vs fitted
  if (use_date) {
    panel1_data <- data.frame(
      date = data$date,
      time_index = data$time_index,
      observed = data$unemployment_rate,
      fitted = fitted_vals
    )
    panel1 <- ggplot2::ggplot(panel1_data, ggplot2::aes(x = date)) +
      ggplot2::geom_line(ggplot2::aes(y = observed, color = "Observed"), linewidth = 0.5) +
      ggplot2::geom_line(ggplot2::aes(y = fitted, color = "Fitted"), linewidth = 1) +
      ggplot2::scale_color_manual(values = c("Observed" = "gray50", "Fitted" = "blue")) +
      ggplot2::labs(
        title = "Observed vs Fitted Values",
        x = "Date",
        y = "Unemployment Rate",
        color = NULL
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "top")
  } else {
    panel1_data <- data.frame(
      time_index = data$time_index,
      observed = data$unemployment_rate,
      fitted = fitted_vals
    )
    panel1 <- ggplot2::ggplot(panel1_data, ggplot2::aes(x = time_index)) +
      ggplot2::geom_line(ggplot2::aes(y = observed, color = "Observed"), linewidth = 0.5) +
      ggplot2::geom_line(ggplot2::aes(y = fitted, color = "Fitted"), linewidth = 1) +
      ggplot2::scale_color_manual(values = c("Observed" = "gray50", "Fitted" = "blue")) +
      ggplot2::labs(
        title = "Observed vs Fitted Values",
        x = "Time Index",
        y = "Unemployment Rate",
        color = NULL
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "top")
  }

  # Panel 2: Trend component (absolute values for clarity)
  # Match date to trend data if available
  if (use_date) {
    trend_dates <- data$date[match(trend$time_index, data$time_index)]
    panel2_data <- data.frame(
      date = trend_dates,
      time_index = trend$time_index,
      trend_effect = trend$trend_effect,
      lower = trend$trend_effect - 2 * trend$se,
      upper = trend$trend_effect + 2 * trend$se
    )

    panel2 <- ggplot2::ggplot(panel2_data, ggplot2::aes(x = date, y = trend_effect)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                          fill = "darkgreen", alpha = 0.2) +
      ggplot2::geom_line(ggplot2::aes(color = "Estimated"), linewidth = 1)

    # Add true trend if provided
    if (!is.null(true_params) && "trend_slope" %in% names(true_params) &&
        "baseline" %in% names(true_params)) {
      true_trend_data <- data.frame(
        date = trend_dates,
        time_index = trend$time_index,
        true_trend = true_params$baseline + true_params$trend_slope * trend$time_index
      )
      panel2 <- panel2 +
        ggplot2::geom_line(data = true_trend_data,
                          ggplot2::aes(x = date, y = true_trend, color = "True"),
                          linewidth = 1, linetype = "dashed")
    }

    panel2 <- panel2 +
      ggplot2::scale_color_manual(values = c("Estimated" = "darkgreen", "True" = "red")) +
      ggplot2::labs(
        title = "Estimated Time Trend (Absolute Values)",
        x = "Date",
        y = "Unemployment Rate",
        color = NULL
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "top")
  } else {
    panel2_data <- data.frame(
      time_index = trend$time_index,
      trend_effect = trend$trend_effect,
      lower = trend$trend_effect - 2 * trend$se,
      upper = trend$trend_effect + 2 * trend$se
    )

    panel2 <- ggplot2::ggplot(panel2_data, ggplot2::aes(x = time_index, y = trend_effect)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                          fill = "darkgreen", alpha = 0.2) +
      ggplot2::geom_line(ggplot2::aes(color = "Estimated"), linewidth = 1)

    # Add true trend if provided
    if (!is.null(true_params) && "trend_slope" %in% names(true_params) &&
        "baseline" %in% names(true_params)) {
      true_trend_data <- data.frame(
        time_index = trend$time_index,
        true_trend = true_params$baseline + true_params$trend_slope * trend$time_index
      )
      panel2 <- panel2 +
        ggplot2::geom_line(data = true_trend_data,
                          ggplot2::aes(x = time_index, y = true_trend, color = "True"),
                          linewidth = 1, linetype = "dashed")
    }

    panel2 <- panel2 +
      ggplot2::scale_color_manual(values = c("Estimated" = "darkgreen", "True" = "red")) +
      ggplot2::labs(
        title = "Estimated Time Trend (Absolute Values)",
        x = "Time Index",
        y = "Unemployment Rate",
        color = NULL
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "top")
  }

  # Panel 3: Seasonal component
  panel3_data <- data.frame(
    month = seasonal$month,
    month_name = factor(month.abb[seasonal$month], levels = month.abb),
    seasonal_effect = seasonal$seasonal_effect,
    lower = seasonal$seasonal_effect - 2 * seasonal$se,
    upper = seasonal$seasonal_effect + 2 * seasonal$se
  )

  panel3 <- ggplot2::ggplot(panel3_data, ggplot2::aes(x = month_name, y = seasonal_effect)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                          width = 0.2, color = "red") +
    ggplot2::geom_line(ggplot2::aes(group = 1), color = "red", linewidth = 1) +
    ggplot2::geom_point(color = "red", size = 3)

  # Add true amplitude bounds if provided
  if (!is.null(true_params) && "seasonal_amplitude" %in% names(true_params)) {
    true_amp <- true_params$seasonal_amplitude
    panel3 <- panel3 +
      ggplot2::geom_hline(yintercept = c(-true_amp, true_amp),
                         linetype = "dotted", color = "blue", linewidth = 0.8) +
      ggplot2::annotate("text", x = 1, y = true_amp, label = "True amplitude",
                       vjust = -0.5, hjust = 0, color = "blue", size = 3)
  }

  panel3 <- panel3 +
    ggplot2::labs(
      title = "Estimated Seasonal Pattern",
      x = "Month",
      y = "Seasonal Effect"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # Return list of plots
  list(
    observed_fitted = panel1,
    trend = panel2,
    seasonal = panel3
  )
}

#' Create Time Series ggplot Visualization
#'
#' Creates two-panel time series visualization showing observed vs fitted
#' unemployment rates over time and the seasonal pattern across months.
#'
#' @param data Data frame with unemployment rates and fitted values
#' @param seasonal_effects Data frame with seasonal effects by month
#'
#' @return List of two ggplot objects:
#'   \item{timeseries}{Observed vs fitted time series}
#'   \item{seasonal}{Seasonal pattern with confidence intervals}
#'
#' @details
#' This function creates publication-quality time series plots using ggplot2.
#' The data frame should have columns: date, unemployment_rate, fitted.
#' The seasonal_effects frame should have: month, seasonal_effect, se.
#'
#' @examples
#' \dontrun{
#' # After fitting model and extracting components
#' plots <- create_timeseries_ggplot(phd_monthly, seasonal_effects)
#'
#' # Display both panels
#' library(patchwork)
#' plots[[1]] / plots[[2]]
#' }
#'
#' @export
create_timeseries_ggplot <- function(data, seasonal_effects) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required. Install with: install.packages('ggplot2')")
  }

  # Check for required columns
  if (!"date" %in% names(data)) {
    stop("data must contain 'date' column")
  }
  if (!"unemployment_rate" %in% names(data)) {
    stop("data must contain 'unemployment_rate' column")
  }
  if (!"fitted" %in% names(data)) {
    stop("data must contain 'fitted' column")
  }

  # Panel 1: Time series with observed and fitted
  ts_data <- data.frame(
    date = data$date,
    observed = data$unemployment_rate,
    fitted = data$fitted
  )

  panel1 <- ggplot2::ggplot(ts_data, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = observed, color = "Observed"), linewidth = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = fitted, color = "GAM Fitted"),
                      linewidth = 0.8, linetype = "dashed") +
    ggplot2::scale_color_manual(values = c("Observed" = "black", "GAM Fitted" = "red")) +
    ggplot2::labs(
      title = "PhD Unemployment Rate: Observed vs Fitted (Monthly Data 2000-2025)",
      x = "Date",
      y = "Unemployment Rate",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      plot.title = ggplot2::element_text(size = 11)
    )

  # Panel 2: Seasonal pattern
  seasonal_data <- data.frame(
    month = seasonal_effects$month,
    month_name = factor(month.abb[seasonal_effects$month], levels = month.abb),
    seasonal_effect = seasonal_effects$seasonal_effect,
    lower = seasonal_effects$seasonal_effect - 2 * seasonal_effects$se,
    upper = seasonal_effects$seasonal_effect + 2 * seasonal_effects$se
  )

  panel2 <- ggplot2::ggplot(seasonal_data, ggplot2::aes(x = month_name, y = seasonal_effect)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                          width = 0.2, color = "blue", linewidth = 0.5) +
    ggplot2::geom_line(ggplot2::aes(group = 1), color = "blue", linewidth = 1) +
    ggplot2::geom_point(color = "blue", size = 3) +
    ggplot2::labs(
      title = "Seasonal Pattern in PhD Unemployment",
      x = "Month",
      y = "Seasonal Effect"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # Return list of plots
  list(
    timeseries = panel1,
    seasonal = panel2
  )
}

#' Validate Parameter Recovery
#'
#' Checks whether a fitted GAM model successfully recovers known parameters
#' from simulated data. This is critical for model validation.
#'
#' @param model mgcv GAM model object
#' @param data Data frame. Original data used to fit model
#' @param true_params List with true parameter values:
#'   \item{baseline}{True baseline unemployment rate}
#'   \item{trend_slope}{True linear trend slope (optional)}
#'   \item{seasonal_amplitude}{True seasonal amplitude (optional)}
#'
#' @return Data frame with validation results for each parameter:
#'   \item{parameter}{Parameter name}
#'   \item{true_value}{True parameter value}
#'   \item{estimated_value}{Estimated parameter value from model}
#'   \item{error}{Absolute error (estimated - true)}
#'   \item{relative_error}{Relative error as percentage}
#'   \item{recovered}{Logical indicating if parameter was successfully recovered}
#'
#' @details
#' Recovery criteria:
#' - Baseline: within 10% of true value
#' - Trend slope: within 20% of true value (allows for nonlinear smooths)
#' - Seasonal amplitude: within 25% of true value (peak-to-trough / 2)
#'
#' Use this function to validate that your model can recover known parameters
#' before applying it to real data where true values are unknown.
#'
#' @examples
#' \dontrun{
#' # Simulate data with known parameters
#' true_params <- list(
#'   baseline = 0.025,
#'   trend_slope = 0.0005,
#'   seasonal_amplitude = 0.008
#' )
#'
#' sim_data <- simulate_seasonal_unemployment(
#'   n_years = 10,
#'   baseline_rate = true_params$baseline,
#'   trend_slope = true_params$trend_slope,
#'   seasonal_amplitude = true_params$seasonal_amplitude
#' )
#'
#' model <- fit_seasonal_gam(sim_data)
#' recovery <- validate_parameter_recovery(model, sim_data, true_params)
#' print(recovery)
#'
#' # Check if all parameters recovered successfully
#' if (all(recovery$recovered)) {
#'   cat("✓ All parameters successfully recovered!\n")
#' }
#' }
#'
#' @export
validate_parameter_recovery <- function(model, data, true_params) {
  if (!inherits(model, "gam")) {
    stop("model must be a GAM object from mgcv")
  }

  results <- data.frame(
    parameter = character(),
    true_value = numeric(),
    estimated_value = numeric(),
    error = numeric(),
    relative_error = numeric(),
    recovered = logical(),
    stringsAsFactors = FALSE
  )

  # 1. Validate baseline recovery
  if ("baseline" %in% names(true_params)) {
    true_baseline <- true_params$baseline

    # CORRECT METHOD: Average predictions at t=min across all months
    # This removes seasonal variation and evaluates at the start of the series
    # (GAM smooths are sum-to-zero, so mean(fitted) includes trend at mean(t))
    pred_grid <- expand.grid(
      time_index = min(data$time_index),
      month = 1:12
    )
    preds <- predict(model, newdata = pred_grid)
    estimated_baseline <- mean(preds)

    error <- estimated_baseline - true_baseline
    rel_error <- abs(error / true_baseline) * 100

    # Recovery criterion: within 10%
    recovered <- rel_error < 10

    results <- rbind(results, data.frame(
      parameter = "baseline",
      true_value = true_baseline,
      estimated_value = estimated_baseline,
      error = error,
      relative_error = rel_error,
      recovered = recovered
    ))
  }

  # 2. Validate trend slope recovery
  if ("trend_slope" %in% names(true_params)) {
    true_slope <- true_params$trend_slope

    # Extract trend component (use centered for slope estimation)
    trend <- extract_trend_component(model, data, absolute = FALSE)

    # Estimate slope from trend (may not be exactly linear)
    time_points <- trend$time_index
    trend_effects <- trend$trend_effect
    fit_lm <- lm(trend_effects ~ time_points)
    estimated_slope <- coef(fit_lm)[2]

    error <- estimated_slope - true_slope

    # Handle zero slope case
    if (true_slope == 0) {
      # For zero slope, use absolute error criterion
      recovered <- abs(error) < 0.0001
      rel_error <- NA  # Relative error undefined for zero
    } else {
      rel_error <- abs(error / true_slope) * 100
      # Recovery criterion: within 20% (allows for nonlinear smooths)
      recovered <- rel_error < 20
    }

    results <- rbind(results, data.frame(
      parameter = "trend_slope",
      true_value = true_slope,
      estimated_value = estimated_slope,
      error = error,
      relative_error = rel_error,
      recovered = recovered
    ))
  }

  # 3. Validate seasonal amplitude recovery
  if ("seasonal_amplitude" %in% names(true_params)) {
    true_amplitude <- true_params$seasonal_amplitude

    # Extract seasonal component
    seasonal <- extract_seasonal_component(model, data)

    # Calculate amplitude as half the peak-to-trough range
    estimated_amplitude <- (max(seasonal$seasonal_effect) - min(seasonal$seasonal_effect)) / 2

    error <- estimated_amplitude - true_amplitude

    # Handle zero amplitude case
    if (true_amplitude == 0) {
      # For zero amplitude, use absolute error criterion
      recovered <- abs(error) < 0.001
      rel_error <- NA  # Relative error undefined for zero
    } else {
      rel_error <- abs(error / true_amplitude) * 100
      # Recovery criterion: within 25% (seasonal patterns can be complex)
      recovered <- rel_error < 25
    }

    results <- rbind(results, data.frame(
      parameter = "seasonal_amplitude",
      true_value = true_amplitude,
      estimated_value = estimated_amplitude,
      error = error,
      relative_error = rel_error,
      recovered = recovered
    ))
  }

  return(results)
}

#' Validate Parameter Recovery Using Coverage Probability
#'
#' Tests whether model uncertainty intervals have correct coverage properties.
#' This is a more rigorous validation than point estimates.
#'
#' @param model mgcv GAM model object (fitted to ONE reference dataset)
#' @param data Data frame. Original data used to fit the reference model
#' @param true_params List with true parameter values used to generate data
#' @param n_sims Integer. Number of simulations for coverage testing (default: 100)
#' @param tolerance Numeric. Acceptable deviation from nominal coverage (default: 0.10)
#'
#' @return Data frame with coverage statistics:
#'   \item{parameter}{Parameter name}
#'   \item{coverage_rate}{Empirical coverage rate from simulations}
#'   \item{target_coverage}{Target coverage rate (0.95)}
#'   \item{meets_target}{Logical indicating if coverage is within tolerance}
#'
#' @details
#' Tests three types of coverage using the CORRECT procedure:
#'
#' 1. **Prediction Intervals (Test 1 approach):**
#'    - Uses the REFERENCE model (fitted once to reference data)
#'    - Generates n_sims NEW datasets from the same DGP
#'    - Checks if reference model's prediction intervals contain new observations
#'
#' 2. **Baseline CI (Test 2 approach):**
#'    - For each of n_sims experiments:
#'      * Generate NEW dataset from DGP
#'      * Fit NEW model to this dataset
#'      * Check if NEW model's baseline CI contains true baseline
#'
#' 3. **Amplitude CI (Test 3 approach):**
#'    - For each of n_sims experiments:
#'      * Generate NEW dataset from DGP
#'      * Fit NEW model to this dataset
#'      * Check if NEW model's amplitude CI contains true amplitude
#'
#' This is the correct way to validate model calibration. A well-calibrated model
#' should have empirical coverage close to the nominal level (e.g., 95%).
#'
#' @examples
#' \dontrun{
#' # Simulate reference data with known parameters
#' true_params <- list(baseline = 0.025, seasonal_amplitude = 0.008)
#' sim_data <- simulate_seasonal_unemployment(n_years = 10,
#'                                            baseline_rate = 0.025,
#'                                            trend_slope = 0,  # MUST match defaults
#'                                            seasonal_amplitude = 0.008)
#' model <- fit_seasonal_gam(sim_data)
#' coverage <- validate_parameter_recovery_coverage(model, sim_data, true_params, n_sims = 100)
#' print(coverage)
#' }
#'
#' @export
validate_parameter_recovery_coverage <- function(model, data, true_params,
                                                 n_sims = 100, tolerance = 0.10) {
  if (!inherits(model, "gam")) {
    stop("model must be a GAM object from mgcv")
  }

  results <- data.frame(
    parameter = character(),
    coverage_rate = numeric(),
    target_coverage = numeric(),
    meets_target = logical(),
    stringsAsFactors = FALSE
  )

  # Get DGP parameters (with defaults matching function expectations)
  baseline_rate <- if ("baseline" %in% names(true_params)) true_params$baseline else 0.025
  trend_slope <- if ("trend_slope" %in% names(true_params)) true_params$trend_slope else 0
  seasonal_amplitude <- if ("seasonal_amplitude" %in% names(true_params)) true_params$seasonal_amplitude else 0.005
  noise_sd <- sd(residuals(model))

  # ===== TEST 1: PREDICTION INTERVAL COVERAGE =====
  # Use REFERENCE model's intervals, generate NEW data from same DGP
  # This tests: "Do the reference model's 95% PIs contain 95% of new observations?"

  # Calculate prediction intervals from REFERENCE model (once)
  pred_ref <- predict(model, se.fit = TRUE)
  pred_sd <- sqrt(pred_ref$se.fit^2 + noise_sd^2)
  lower_pi <- pred_ref$fit - 1.96 * pred_sd
  upper_pi <- pred_ref$fit + 1.96 * pred_sd

  # Test on many new datasets
  total_coverage_count <- 0
  total_observations <- 0

  for (i in 1:n_sims) {
    # Generate NEW dataset from same DGP
    new_data <- simulate_seasonal_unemployment(
      n_years = nrow(data) / 12,
      baseline_rate = baseline_rate,
      trend_slope = trend_slope,
      seasonal_amplitude = seasonal_amplitude,
      noise_sd = noise_sd,
      seed = i * 12345
    )

    # Check if reference model's PIs contain new observations
    within_interval <- (new_data$unemployment_rate >= lower_pi) &
                       (new_data$unemployment_rate <= upper_pi)
    total_coverage_count <- total_coverage_count + sum(within_interval)
    total_observations <- total_observations + nrow(new_data)
  }

  pred_coverage <- total_coverage_count / total_observations
  results <- rbind(results, data.frame(
    parameter = "prediction_interval",
    coverage_rate = pred_coverage,
    target_coverage = 0.95,
    meets_target = abs(pred_coverage - 0.95) <= tolerance
  ))

  # ===== TEST 2: BASELINE CI COVERAGE =====
  # For each simulation: fit NEW model, check if NEW model's CI contains true baseline
  if ("baseline" %in% names(true_params)) {
    baseline_coverage_count <- 0

    for (i in 1:n_sims) {
      sim_data <- simulate_seasonal_unemployment(
        n_years = nrow(data) / 12,
        baseline_rate = baseline_rate,
        trend_slope = trend_slope,
        seasonal_amplitude = seasonal_amplitude,
        noise_sd = noise_sd,
        seed = i * 123
      )

      sim_model <- fit_seasonal_gam(sim_data, k_month = 10, k_trend = 15)

      # Estimate baseline using correct method
      # Average predictions at t=min across all months to remove seasonal variation
      pred_grid <- expand.grid(
        time_index = min(sim_data$time_index),
        month = 1:12
      )
      preds <- predict(sim_model, newdata = pred_grid, se.fit = TRUE)
      baseline_est <- mean(preds$fit)

      # Proper SE calculation using variance-covariance matrix
      # Get linear predictor matrix for computing SE of mean
      Xp <- predict(sim_model, newdata = pred_grid, type = "lpmatrix")
      # Mean is a linear combination with weights 1/12
      weights <- rep(1/12, nrow(pred_grid))
      mean_Xp <- colSums(Xp * weights)
      # SE: sqrt(mean_Xp^T %*% Vcov %*% mean_Xp)
      baseline_se <- sqrt(mean_Xp %*% vcov(sim_model) %*% mean_Xp)[1, 1]

      # 95% CI
      baseline_lower <- baseline_est - 1.96 * baseline_se
      baseline_upper <- baseline_est + 1.96 * baseline_se

      # Check if true baseline in CI
      if (true_params$baseline >= baseline_lower &&
          true_params$baseline <= baseline_upper) {
        baseline_coverage_count <- baseline_coverage_count + 1
      }
    }

    baseline_coverage <- baseline_coverage_count / n_sims
    results <- rbind(results, data.frame(
      parameter = "baseline_CI",
      coverage_rate = baseline_coverage,
      target_coverage = 0.95,
      meets_target = abs(baseline_coverage - 0.95) <= tolerance
    ))
  }

  # ===== TEST 3: SEASONAL AMPLITUDE CI COVERAGE =====
  # For each simulation: fit NEW model, check if CI contains true amplitude
  if ("seasonal_amplitude" %in% names(true_params)) {
    amplitude_coverage_count <- 0

    for (i in 1:n_sims) {
      sim_data <- simulate_seasonal_unemployment(
        n_years = nrow(data) / 12,
        baseline_rate = baseline_rate,
        trend_slope = trend_slope,  # Must include trend for consistency
        seasonal_amplitude = seasonal_amplitude,
        noise_sd = noise_sd,
        seed = i * 456
      )

      sim_model <- fit_seasonal_gam(sim_data, k_month = 10, k_trend = 15)

      # Extract seasonal effects
      seasonal <- extract_seasonal_component(sim_model, sim_data)

      # Estimate amplitude
      amplitude_est <- (max(seasonal$seasonal_effect) -
                        min(seasonal$seasonal_effect)) / 2

      # Proper SE calculation using variance-covariance matrix
      # Need to get linear predictor matrix for the two months
      max_month <- seasonal$month[which.max(seasonal$seasonal_effect)]
      min_month <- seasonal$month[which.min(seasonal$seasonal_effect)]

      # Create prediction grid for these two months at mean time
      pred_grid_amplitude <- data.frame(
        time_index = mean(sim_data$time_index),
        month = c(max_month, min_month)
      )

      # Get linear predictor matrix
      Xp <- predict(sim_model, newdata = pred_grid_amplitude, type = "lpmatrix")

      # Amplitude = (max - min) / 2, so we want SE of: 0.5 * (pred1 - pred2)
      # This is a linear combination with weights [0.5, -0.5]
      contrast_weights <- c(0.5, -0.5)
      contrast_Xp <- colSums(Xp * contrast_weights)

      # SE: sqrt(contrast_Xp^T %*% Vcov %*% contrast_Xp)
      amplitude_se <- sqrt(contrast_Xp %*% vcov(sim_model) %*% contrast_Xp)[1, 1]

      # 95% CI
      amplitude_lower <- amplitude_est - 1.96 * amplitude_se
      amplitude_upper <- amplitude_est + 1.96 * amplitude_se

      # Check if true amplitude in CI
      if (true_params$seasonal_amplitude >= amplitude_lower &&
          true_params$seasonal_amplitude <= amplitude_upper) {
        amplitude_coverage_count <- amplitude_coverage_count + 1
      }
    }

    amplitude_coverage <- amplitude_coverage_count / n_sims
    results <- rbind(results, data.frame(
      parameter = "seasonal_amplitude_CI",
      coverage_rate = amplitude_coverage,
      target_coverage = 0.95,
      meets_target = abs(amplitude_coverage - 0.95) <= tolerance
    ))
  }

  return(results)
}
