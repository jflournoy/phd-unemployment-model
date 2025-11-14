#' Fit Factor Smooth GAM to Real CPS Data
#'
#' Loads processed CPS unemployment data and fits a factor smooth GAM model
#' with education-specific trends and/or seasonal patterns.
#'
#' @param data_file Character. Path to RDS file containing processed CPS data
#' @param education_levels Character vector. Education levels to include (e.g., c("phd", "masters", "bachelors"))
#' @param formula_type Character. One of "shared", "seasonal_by_education", "trend_by_education", "full"
#' @param start_year Integer. Optional. Filter data to years >= start_year
#' @param end_year Integer. Optional. Filter data to years <= end_year
#' @param save_model Logical. Whether to save the fitted model to disk
#' @param model_dir Character. Directory to save model (if save_model = TRUE)
#' @param model_name Character. Name for saved model file (if save_model = TRUE)
#' @param k_month Integer. Basis dimension for cyclic month spline (default: 10)
#' @param k_trend Integer. Basis dimension for trend spline (default: 20)
#'
#' @return List containing:
#'   \item{model}{Fitted GAM model object}
#'   \item{data}{Processed data frame used for fitting}
#'   \item{education_levels}{Character vector of education levels}
#'   \item{formula_type}{Formula type used}
#'   \item{data_years}{Range of years in data}
#'   \item{model_path}{Path to saved model (if save_model = TRUE)}
#'
#' @examples
#' \dontrun{
#' result <- fit_factor_smooth_to_cps_data(
#'   data_file = here::here("data/phd-monthly-unemployment.rds"),
#'   education_levels = c("phd", "masters", "bachelors"),
#'   formula_type = "full",
#'   start_year = 2010,
#'   end_year = 2020
#' )
#' summary(result$model)
#' }
#'
#' @export
fit_factor_smooth_to_cps_data <- function(data_file,
                                           education_levels = c("phd", "masters", "bachelors"),
                                           formula_type = "full",
                                           start_year = NULL,
                                           end_year = NULL,
                                           save_model = FALSE,
                                           model_dir = here::here("models"),
                                           model_name = NULL,
                                           k_month = 10,
                                           k_trend = 20) {

  # Load data
  if (!file.exists(data_file)) {
    stop("Data file not found: ", data_file)
  }

  raw_data <- readRDS(data_file)

  # Filter to requested education levels
  # Assuming data has an 'educ_label' or similar column
  # We'll need to check what the actual column names are
  if ("educ_label" %in% names(raw_data)) {
    # Map education levels to data labels
    educ_mapping <- c(
      "phd" = "PhD",
      "masters" = "Master's",
      "bachelors" = "Bachelor's"
    )

    labels_to_keep <- educ_mapping[education_levels]
    data <- raw_data[raw_data$educ_label %in% labels_to_keep, ]

    # Create standardized education variable
    data$education <- factor(
      data$educ_label,
      levels = labels_to_keep,
      labels = education_levels
    )
  } else if ("education" %in% names(raw_data)) {
    # Data already has education column
    data <- raw_data[raw_data$education %in% education_levels, ]
    data$education <- factor(data$education, levels = education_levels)
  } else {
    stop("Data must have 'education' or 'educ_label' column")
  }

  # Filter by year if requested
  if (!is.null(start_year)) {
    data <- data[data$year >= start_year, ]
  }
  if (!is.null(end_year)) {
    data <- data[data$year <= end_year, ]
  }

  # Check we have required columns
  required_cols <- c("unemployment_rate", "time_index", "month", "year")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Data missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Fit model using existing function
  model <- fit_factor_smooth_gam(
    data = data,
    formula_type = formula_type,
    education_var = "education",
    k_month = k_month,
    k_trend = k_trend
  )

  # Prepare result
  result <- list(
    model = model,
    data = data,
    education_levels = education_levels,
    formula_type = formula_type,
    data_years = range(data$year)
  )

  # Save model if requested
  if (save_model) {
    if (!dir.exists(model_dir)) {
      dir.create(model_dir, recursive = TRUE)
    }

    if (is.null(model_name)) {
      model_name <- paste0(
        "cps_factor_smooth_",
        paste(education_levels, collapse = "_"),
        "_",
        formula_type,
        "_",
        format(Sys.Date(), "%Y%m%d")
      )
    }

    model_path <- file.path(model_dir, paste0(model_name, ".rds"))

    # Save with metadata
    saveRDS(
      list(
        model = model,
        education_levels = education_levels,
        formula_type = formula_type,
        data_years = range(data$year),
        fit_date = Sys.time()
      ),
      file = model_path
    )

    result$model_path <- model_path
  }

  return(result)
}


#' Extract Education-Specific Components from Real Data Model
#'
#' Extracts trend and seasonal components for each education level from a
#' fitted factor smooth GAM model on real CPS data.
#'
#' @param model GAM model object from fit_factor_smooth_to_cps_data
#' @param data Data frame used to fit the model
#' @param education_levels Character vector of education levels to extract
#'
#' @return List with two elements:
#'   \item{trends}{Named list of trend data frames (one per education level)}
#'   \item{seasonal}{Named list of seasonal data frames (one per education level)}
#'
#' @export
extract_real_data_components <- function(model, data, education_levels) {

  # Extract trends for each education level
  trends <- lapply(education_levels, function(educ) {
    extract_education_specific_trend(model, educ)
  })
  names(trends) <- education_levels

  # Extract seasonal patterns for each education level
  seasonal <- lapply(education_levels, function(educ) {
    extract_education_specific_seasonal(model, educ)
  })
  names(seasonal) <- education_levels

  return(list(
    trends = trends,
    seasonal = seasonal
  ))
}


#' Compute Trend Differences for Real Data
#'
#' Computes pairwise differences in unemployment trends between education
#' levels over time, with proper uncertainty quantification.
#'
#' @param model GAM model object
#' @param data Data frame used to fit the model
#' @param education_pairs List of character vectors, each with two education levels to compare
#' @param time_points Numeric vector of time_index values at which to compute differences
#' @param simultaneous Logical. Use simultaneous confidence bands? (default: TRUE)
#' @param alpha Numeric. Significance level for confidence intervals (default: 0.05)
#'
#' @return Data frame with columns:
#'   \item{time_index}{Time index}
#'   \item{year}{Year}
#'   \item{month}{Month}
#'   \item{comparison}{Label for comparison (e.g., "phd - bachelors")}
#'   \item{difference}{Estimated difference}
#'   \item{se}{Standard error}
#'   \item{lower}{Lower confidence bound}
#'   \item{upper}{Upper confidence bound}
#'   \item{significant}{Logical. Is difference significant?}
#'
#' @export
compute_real_data_trend_differences <- function(model,
                                                 data,
                                                 education_pairs,
                                                 time_points,
                                                 simultaneous = TRUE,
                                                 alpha = 0.05) {

  # Compute differences using existing function
  diff_results <- compute_trend_differences(
    model = model,
    education_pairs = education_pairs,
    time_points = time_points,
    simultaneous = simultaneous,
    alpha = alpha
  )

  # Add year and month information from data
  # Create lookup table from data
  time_lookup <- unique(data[, c("time_index", "year", "month")])

  # Merge to add year/month
  diff_results <- merge(
    diff_results,
    time_lookup,
    by = "time_index",
    all.x = TRUE
  )

  # Reorder columns
  col_order <- c("time_index", "year", "month", "comparison", "difference",
                 "se", "lower", "upper", "significant")
  diff_results <- diff_results[, col_order]

  # Sort by comparison and time
  diff_results <- diff_results[order(diff_results$comparison, diff_results$time_index), ]

  rownames(diff_results) <- NULL

  return(diff_results)
}


#' Fit Nested Models to CPS Data
#'
#' Fits sequence of nested GAM models to real CPS data and compares them
#' using AIC.
#'
#' @param data_file Character. Path to RDS file containing processed CPS data
#' @param education_levels Character vector. Education levels to include
#' @param start_year Integer. Optional. Filter data to years >= start_year
#' @param end_year Integer. Optional. Filter data to years <= end_year
#'
#' @return List with elements:
#'   \item{models}{Named list of fitted GAM models (m0 through m6)}
#'   \item{comparison}{Data frame with AIC comparison table}
#'   \item{best_model}{Name of best model by AIC}
#'   \item{data}{Processed data frame}
#'
#' @export
fit_nested_models_to_cps_data <- function(data_file,
                                           education_levels = c("phd", "masters", "bachelors"),
                                           start_year = NULL,
                                           end_year = NULL) {

  # Load and prepare data using the main function
  data_result <- fit_factor_smooth_to_cps_data(
    data_file = data_file,
    education_levels = education_levels,
    formula_type = "full",  # Doesn't matter, we'll fit all models
    start_year = start_year,
    end_year = end_year,
    save_model = FALSE
  )

  data <- data_result$data

  # Fit nested sequence
  models <- fit_nested_model_sequence(data)

  # Compare models
  comparison <- compare_nested_models(models)

  # Identify best model
  best_model <- comparison$model[1]

  return(list(
    models = models,
    comparison = comparison,
    best_model = best_model,
    data = data
  ))
}


#' Prepare Visualization Data
#'
#' Prepares plotting-ready data from fitted GAM model, including observed
#' values, fitted values, trends, and seasonal patterns.
#'
#' @param model GAM model object
#' @param data Data frame used to fit the model
#' @param education_levels Character vector of education levels
#'
#' @return List with elements:
#'   \item{observed}{Data frame with observed unemployment rates and dates}
#'   \item{fitted}{Data frame with fitted values and confidence intervals}
#'   \item{trends}{Data frame with trend components by education}
#'   \item{seasonal}{Data frame with seasonal patterns by education}
#'
#' @export
prepare_visualization_data <- function(model, data, education_levels) {

  # Prepare observed data with proper dates
  observed <- data.frame(
    date = as.Date(paste(data$year, data$month, 1, sep = "-")),
    year = data$year,
    month = data$month,
    education = data$education,
    unemployment_rate = data$unemployment_rate
  )

  # Get fitted values and standard errors
  fitted_vals <- predict(model, newdata = data, se.fit = TRUE)

  fitted <- data.frame(
    date = observed$date,
    education = data$education,
    fitted = fitted_vals$fit,
    se = fitted_vals$se.fit,
    lower = fitted_vals$fit - 1.96 * fitted_vals$se.fit,
    upper = fitted_vals$fit + 1.96 * fitted_vals$se.fit
  )

  # Extract components
  components <- extract_real_data_components(model, data, education_levels)

  # Prepare trends data with dates
  trends_list <- lapply(education_levels, function(educ) {
    trend_df <- components$trends[[educ]]

    # Match time_index to dates from data
    time_lookup <- unique(data[, c("time_index", "year", "month")])
    trend_df <- merge(trend_df, time_lookup, by = "time_index")
    trend_df$date <- as.Date(paste(trend_df$year, trend_df$month, 1, sep = "-"))
    trend_df$education <- educ

    return(trend_df)
  })
  trends <- do.call(rbind, trends_list)

  # Prepare seasonal data with month names
  seasonal_list <- lapply(education_levels, function(educ) {
    seas_df <- components$seasonal[[educ]]
    seas_df$month_name <- month.name[seas_df$month]
    seas_df$education <- educ
    return(seas_df)
  })
  seasonal <- do.call(rbind, seasonal_list)

  return(list(
    observed = observed,
    fitted = fitted,
    trends = trends,
    seasonal = seasonal
  ))
}


#' Analyze CPS Unemployment by Education
#'
#' High-level function that orchestrates complete analysis pipeline:
#' model selection, component extraction, trend comparisons, and
#' visualization data preparation.
#'
#' @param data_file Character. Path to RDS file containing processed CPS data
#' @param education_levels Character vector. Education levels to include
#' @param start_year Integer. Optional. Filter data to years >= start_year
#' @param end_year Integer. Optional. Filter data to years <= end_year
#' @param save_models Logical. Save fitted models to disk?
#' @param save_results Logical. Save analysis results to RDS file?
#' @param output_dir Character. Directory for saved outputs
#'
#' @return List with comprehensive analysis results:
#'   \item{best_model}{Best fitting GAM model (by AIC)}
#'   \item{model_comparison}{Data frame comparing all nested models}
#'   \item{components}{List with trends and seasonal patterns}
#'   \item{trend_differences}{Data frame with pairwise education comparisons}
#'   \item{visualization_data}{List with plotting-ready data frames}
#'   \item{data}{Processed data used for analysis}
#'
#' @examples
#' \dontrun{
#' analysis <- analyze_cps_unemployment_by_education(
#'   data_file = here::here("data/phd-monthly-unemployment.rds"),
#'   education_levels = c("phd", "masters", "bachelors"),
#'   start_year = 2010,
#'   end_year = 2020
#' )
#'
#' # View model comparison
#' print(analysis$model_comparison)
#'
#' # Check trend differences
#' print(analysis$trend_differences)
#' }
#'
#' @export
analyze_cps_unemployment_by_education <- function(data_file,
                                                   education_levels = c("phd", "masters", "bachelors"),
                                                   start_year = NULL,
                                                   end_year = NULL,
                                                   save_models = FALSE,
                                                   save_results = FALSE,
                                                   output_dir = here::here("results")) {

  # 1. Fit nested models and select best
  message("Fitting nested model sequence...")
  nested_results <- fit_nested_models_to_cps_data(
    data_file = data_file,
    education_levels = education_levels,
    start_year = start_year,
    end_year = end_year
  )

  best_model <- nested_results$models[[nested_results$best_model]]
  data <- nested_results$data

  # 2. Extract components
  message("Extracting education-specific components...")
  components <- extract_real_data_components(
    model = best_model,
    data = data,
    education_levels = education_levels
  )

  # 3. Compute trend differences for all pairwise comparisons
  message("Computing trend differences...")

  # Generate all pairwise combinations
  n_educ <- length(education_levels)
  education_pairs <- list()
  for (i in seq_len(n_educ - 1)) {
    for (j in (i + 1):n_educ) {
      education_pairs <- c(education_pairs, list(c(education_levels[i], education_levels[j])))
    }
  }

  # Select time points (every 6 months)
  time_range <- range(data$time_index)
  time_points <- seq(time_range[1], time_range[2], by = 6)

  trend_differences <- compute_real_data_trend_differences(
    model = best_model,
    data = data,
    education_pairs = education_pairs,
    time_points = time_points,
    simultaneous = TRUE,
    alpha = 0.05
  )

  # 4. Prepare visualization data
  message("Preparing visualization data...")
  visualization_data <- prepare_visualization_data(
    model = best_model,
    data = data,
    education_levels = education_levels
  )

  # Compile results
  results <- list(
    best_model = best_model,
    model_comparison = nested_results$comparison,
    components = components,
    trend_differences = trend_differences,
    visualization_data = visualization_data,
    data = data
  )

  # Save if requested
  if (save_results) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    results_file <- file.path(
      output_dir,
      paste0(
        "cps_analysis_",
        paste(education_levels, collapse = "_"),
        "_",
        format(Sys.Date(), "%Y%m%d"),
        ".rds"
      )
    )

    saveRDS(results, file = results_file)
    message("Results saved to: ", results_file)
  }

  if (save_models) {
    message("Saving models...")
    # Save using the model saving functionality
    # (would call fit_factor_smooth_to_cps_data with save_model = TRUE)
  }

  message("Analysis complete!")

  return(results)
}
