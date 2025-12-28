#' Create Fuzzy Impulse Function for Economic Shocks
#'
#' Creates a continuous shock intensity variable with gradual onset, peak, and
#' exponential decay. This captures the idea that shocks don't have sharp boundaries
#' and their effects persist well beyond the acute phase.
#'
#' @param year Numeric vector of years (can include fractions for months)
#' @param onset_year Year when shock begins
#' @param peak_year Year of maximum shock intensity
#' @param decay_halflife Half-life of exponential decay in years
#' @param max_intensity Maximum intensity at peak (default 1)
#'
#' @return Numeric vector of shock intensities (0 to max_intensity)
#'
#' @details
#' The impulse function has three phases:
#' 1. **Pre-onset**: intensity = 0
#' 2. **Rising phase**: linear rise from onset to peak
#' 3. **Decay phase**: exponential decay from peak with specified half-life
#'
#' For the 2008 financial crisis:
#' - Onset ~2007.5 (subprime mortgage issues)
#' - Peak ~2009.5 (unemployment peaked late)
#' - Decay half-life ~2.5 years (slow recovery)
#'
#' For the 2020 COVID pandemic:
#' - Onset ~2020.25 (March 2020)
#' - Peak ~2020.33 (April 2020, very sharp)
#' - Decay half-life ~1.5 years (faster recovery)
#'
#' @export
create_shock_impulse <- function(year, onset_year, peak_year, decay_halflife,
                                  max_intensity = 1) {
  intensity <- numeric(length(year))

  for (i in seq_along(year)) {
    y <- year[i]
    if (y < onset_year) {
      # Pre-onset: no shock
      intensity[i] <- 0
    } else if (y <= peak_year) {
      # Rising phase: linear increase from 0 to max
      intensity[i] <- max_intensity * (y - onset_year) / (peak_year - onset_year)
    } else {
      # Decay phase: exponential falloff
      years_since_peak <- y - peak_year
      decay_rate <- log(2) / decay_halflife
      intensity[i] <- max_intensity * exp(-decay_rate * years_since_peak)
    }
  }

  intensity
}


#' Fit Quasi-Binomial GAM Model Across Education Levels
#'
#' Fits a quasi-binomial GAM to unemployment count data across all education levels,
#' with education-specific trends, shock dynamics, and seasonal patterns.
#'
#' @param data Data frame with columns: n_unemployed, n_employed, time_index, month, education
#' @param education_levels Character vector of education levels to include.
#'   If NULL (default), uses all levels in data.
#' @param use_quasi Logical. If TRUE (default), uses quasi-binomial family to account for overdispersion.
#'   If FALSE, uses standard binomial family.
#' @param time_k Numeric. Basis dimension for time_index smooth (default 150).
#'
#' @return List containing:
#'   - model: Fitted GAM object
#'   - formula: Model formula used
#'   - data: Data with shock intensity variables added
#'   - summary_stats: Model summary statistics including dispersion
#'   - predictions: Data frame with fitted values and counterfactual predictions
#'   - convergence_info: Convergence diagnostics
#'
#' @details
#' The model uses year-based adaptive smooths with clean time/season separation:
#'
#' cbind(n_unemployed, n_employed) ~
#'   education +
#'   s(year_frac, bs="ad", k=30) +
#'   s(year_frac, education, bs="fs", k=20) +
#'   s(month, bs="cc", k=12) +
#'   s(month, education, bs="fs", k=12, xt=list(bs="cc"))
#'
#' Key design choices:
#'
#' **Fixed effects for education:** Parametric terms capture baseline unemployment
#' differences across 7 education levels.
#'
#' **Year-based adaptive smooth (bs="ad"):** Using `year_frac` (year + month/12)
#' instead of monthly `time_index` prevents seasonality from leaking into the
#' time trend. Wiggliness varies automatically during volatile periods.
#'
#' **Education deviations with partial pooling:** `s(year_frac, education, bs="fs")`
#' models education-specific deviations from the global trend. Factor smooths
#' shrink curves toward a common shape, borrowing strength across levels.
#'
#' **Strong seasonality:** Separate global seasonal effect `s(month, bs="cc")` plus
#' education-specific seasonal deviations. Clean separation ensures no confounding.
#'
#' **Family:** Quasi-binomial accounts for overdispersion common in count data.
#'
#' @examples
#' \dontrun{
#' data <- readRDS("data/education-spectrum-counts.rds")
#' result <- fit_education_binomial_gam(data)
#' summary(result$model)
#' }
#'
#' @export
fit_education_binomial_gam <- function(data,
                                        education_levels = NULL,
                                        use_quasi = TRUE,
                                        time_k = 150) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  required_cols <- c("n_unemployed", "n_employed", "time_index", "month", "education")
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
  }

  # Filter to specified education levels if provided
  if (!is.null(education_levels)) {
    data <- data[data$education %in% education_levels, ]
  }

  # Ensure factors are properly set
  data$education <- factor(data$education)
  data$month <- as.integer(data$month)
  data$time_index <- as.numeric(data$time_index)

  # Add year if not present (for reference)
  if (!"year" %in% names(data)) {
    # Estimate year from time_index if possible (assumes monthly data starting 2000)
    data$year <- 2000 + floor((data$time_index - 1) / 12)
  }

  # Calculate precise year with month fraction
  data$year_frac <- data$year + (data$month - 0.5) / 12

  # Year-based adaptive smooth with clean time/season separation
  #
  # Model structure:
  # - education: Fixed effects for baseline education differences
  # - s(year_frac, bs="ad"): Adaptive year-based smooth - wiggliness varies
  #   (automatically more flexible during crisis periods like 2008, 2020)
  #   Using year_frac instead of time_index prevents seasonality leakage
  # - s(year_frac, education, bs="fs"): Education-specific year deviations
  #   with partial pooling (shrinkage toward common shape)
  # - s(month, bs="cc"): Global seasonal pattern (cyclic)
  # - s(month, education, bs="fs"): Education-specific seasonal deviations
  #
  # Key features:
  # - Clean separation of time trend and seasonality (no confounding)
  # - No explicit COVID/shock modeling - adaptive basis handles it
  # - Partial pooling via bs="fs" for education deviations
  # - Strong seasonality: separate global + education-specific terms
  formula <- cbind(n_unemployed, n_employed) ~
    # Fixed effects: education level baselines
    education +
    # Adaptive year-based smooth (clean separation from seasonality)
    s(year_frac, bs = "ad", k = 30) +
    # Education-specific year deviations with partial pooling
    s(year_frac, education, bs = "fs", k = 20) +
    # Global seasonality (cyclic)
    s(month, bs = "cc", k = 12) +
    # Education-specific seasonal deviations
    s(month, education, bs = "fs", k = 12, xt = list(bs = "cc"))

  # Fit model with specified family
  # Note: Always use quasibinomial for large datasets with overdispersion
  family_obj <- if (use_quasi) quasibinomial() else binomial()

  # Use bam() for large dataset performance with optimizations:
  # - discrete=TRUE: Fast discrete approximation for factor smooths
  # - nthreads=4: Parallel computation (adjust based on available cores)
  # - method="fREML": Fast REML for speed (20-30% faster than REML)
  model <- mgcv::bam(
    formula,
    data = data,
    family = family_obj,
    method = "fREML",
    discrete = TRUE,
    nthreads = 4,
    control = list(maxit = 500)
  )

  # Extract diagnostics
  dispersion <- summary(model)$dispersion
  convergence_info <- list(
    converged = model$converged,
    gcv_score = model$gcv.ubre
  )

  # Generate predictions
  preds_se <- predict(model, type = "response", se.fit = TRUE)

  # Observed unemployment rate
  observed_rate <- data$n_unemployed / (data$n_unemployed + data$n_employed)

  predictions <- data.frame(
    education = data$education,
    year = data$year,
    year_frac = data$year_frac,
    time_index = data$time_index,
    month = data$month,
    n_unemployed = data$n_unemployed,
    n_employed = data$n_employed,
    observed_rate = observed_rate,
    fitted_prob = as.numeric(preds_se$fit),
    se = as.numeric(preds_se$se.fit),
    ci_lower = pmax(0, as.numeric(preds_se$fit - 1.96 * preds_se$se.fit)),
    ci_upper = pmin(1, as.numeric(preds_se$fit + 1.96 * preds_se$se.fit)),
    fitted_unemployed = as.numeric(preds_se$fit * (data$n_unemployed + data$n_employed)),
    residuals = residuals(model, type = "deviance")
  )

  # Return comprehensive result
  list(
    model = model,
    formula = formula,
    data = data,
    family_type = if (use_quasi) "quasi-binomial" else "binomial",
    summary_stats = list(
      n_observations = nrow(data),
      n_education_levels = length(unique(data$education)),
      n_months = length(unique(data$month)),
      time_span = paste(range(data$year), collapse = "-"),
      dispersion = dispersion,
      deviance_explained = summary(model)$dev.expl
    ),
    predictions = predictions,
    convergence_info = convergence_info,
    call = match.call()
  )
}


#' Extract Education-Specific Unemployment Estimates
#'
#' Extract fitted unemployment rates for each education level at a specific time point.
#' Uses the fuzzy impulse function to calculate shock intensities for that time.
#'
#' @param model_result Result from fit_education_binomial_gam()
#' @param time_point Numeric. Time index for prediction (1-308 for monthly data 2000-2025)
#' @param month Numeric. Month (1-12) for prediction
#' @param counterfactual Logical. If TRUE, predict baseline (no shock) scenario.
#'   If FALSE (default), predict with actual shock intensities for that time.
#'
#' @return Data frame with unemployment estimates and standard errors for each education level
#'
#' @export
predict_education_unemployment <- function(model_result, time_point = 308, month = 6,
                                            counterfactual = FALSE) {
  model <- model_result$model
  education_levels <- levels(model_result$data$education)

  # Infer year from time_point (assumes monthly data starting 2000)
  year <- 2000 + floor((time_point - 1) / 12)
  year_frac <- year + (month - 0.5) / 12

  # Calculate shock intensities using the fuzzy impulse function
  # (or set to 0 for counterfactual)
  if (counterfactual) {
    shock_2008 <- 0
    shock_2020 <- 0
  } else {
    shock_2008 <- create_shock_impulse(year_frac, 2007.5, 2009.75, 2.5)
    shock_2020 <- create_shock_impulse(year_frac, 2020.17, 2020.33, 1.5)
  }

  # Create prediction data with shock intensities
  pred_data <- data.frame(
    education = factor(education_levels, levels = education_levels),
    time_index = time_point,
    month = month,
    shock_2008_intensity = shock_2008,
    shock_2020_intensity = shock_2020
  )

  # Generate predictions with standard errors
  preds <- mgcv::predict.bam(model, newdata = pred_data, type = "response", se.fit = TRUE)

  result <- data.frame(
    education = education_levels,
    time_point = time_point,
    year = year,
    month = month,
    shock_2008_intensity = shock_2008,
    shock_2020_intensity = shock_2020,
    unemployment_rate = as.numeric(preds$fit),
    se = as.numeric(preds$se.fit),
    ci_lower = pmax(0, as.numeric(preds$fit) - 1.96 * as.numeric(preds$se.fit)),
    ci_upper = pmin(1, as.numeric(preds$fit) + 1.96 * as.numeric(preds$se.fit))
  )

  result
}
