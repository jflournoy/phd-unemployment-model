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
#'
#' @return Data frame with columns:
#'   \item{time_index}{Time index}
#'   \item{trend_effect}{Estimated trend effect}
#'   \item{se}{Standard error of the effect}
#'
#' @details
#' Uses mgcv::predict.gam() with type = "terms" to extract the smooth
#' component corresponding to the time trend.
#'
#' @examples
#' \dontrun{
#' sim_data <- simulate_seasonal_unemployment(n_years = 5)
#' model <- fit_seasonal_gam(sim_data)
#' trend <- extract_trend_component(model, sim_data)
#' plot(trend$time_index, trend$trend_effect, type = "l")
#' }
#'
#' @export
extract_trend_component <- function(model, data) {
  if (!inherits(model, "gam")) {
    stop("model must be a GAM object from mgcv")
  }

  # Create prediction grid for all time points
  # Use representative month (e.g., June = 6) to isolate trend
  pred_data <- data.frame(
    time_index = unique(data$time_index),
    month = 6  # Mid-year month to avoid seasonal extremes
  )

  # Get predictions with standard errors
  preds <- predict(model, newdata = pred_data, type = "terms", se.fit = TRUE)

  # Extract trend component (first smooth term)
  trend_idx <- which(grepl("time_index", colnames(preds$fit)))
  trend_effect <- preds$fit[, trend_idx]
  se <- preds$se.fit[, trend_idx]

  # Return as data frame
  data.frame(
    time_index = unique(data$time_index),
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
