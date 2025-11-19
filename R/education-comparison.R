#' Compare Unemployment Rates Across Educational Attainment Levels
#'
#' Applies GAM seasonal models to multiple education levels and compares
#' unemployment patterns over time.
#'
#' @param data Data frame. Raw CPS data with EDUC, EMPSTAT, YEAR, MONTH, WTFINL columns
#' @param education_levels Named list. Education codes with names (e.g., list(phd = 125, masters = 123))
#'   If unnamed vector, uses default names based on common codes.
#'
#' @return List with one element per education level, each containing:
#'   \item{monthly_data}{Processed monthly unemployment data}
#'   \item{model}{Fitted GAM model}
#'   \item{seasonal}{Seasonal component}
#'   \item{trend}{Trend component}
#'   \item{education_code}{IPUMS education code}
#'   \item{education_label}{Human-readable label}
#'
#' @details
#' Education codes (CPS):
#' - 125: Doctorate degree
#' - 123: Master's degree
#' - 111: Bachelor's degree
#' - 91-110: Some college or Associate's
#' - 73: High school diploma
#'
#' The function:
#' 1. Processes monthly unemployment rates for each education level
#' 2. Fits seasonal GAM model to each level
#' 3. Extracts seasonal and trend components
#' 4. Returns structured comparison data
#'
#' @examples
#' \dontrun{
#' cps_data <- readRDS("data-raw/ipums_data.rds")
#' result <- compare_unemployment_by_education(
#'   data = cps_data,
#'   education_levels = c(phd = 125, masters = 123, bachelors = 111)
#' )
#' }
#'
#' @export
compare_unemployment_by_education <- function(data, education_levels) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  required_cols <- c("EDUC", "EMPSTAT", "YEAR", "MONTH", "WTFINL")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("data missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Convert to named list if necessary
  if (is.null(names(education_levels))) {
    # Auto-name based on common codes
    educ_names <- sapply(education_levels, function(code) {
      switch(as.character(code),
             "125" = "phd",
             "123" = "masters",
             "111" = "bachelors",
             "73" = "high_school",
             paste0("educ_", code))
    })
    names(education_levels) <- educ_names
  }

  # Load required functions
  if (!exists("process_cps_data_by_education")) {
    source(here::here("R", "data-processing.R"))
  }
  if (!exists("fit_seasonal_gam")) {
    source(here::here("R", "seasonal-gam.R"))
  }

  # Initialize results list
  results <- list()

  # Process each education level
  for (educ_name in names(education_levels)) {
    educ_code <- education_levels[[educ_name]]

    message("Processing ", educ_name, " (code: ", educ_code, ")...")

    # Filter to this education level
    educ_data <- tryCatch({
      filter_education_level(data, educ_code = educ_code)
    }, error = function(e) {
      warning("Error filtering data for ", educ_name, ": ", e$message)
      return(NULL)
    })

    # Skip if no data or error occurred
    if (is.null(educ_data) || nrow(educ_data) == 0) {
      warning("No data found for education code ", educ_code, ", skipping")
      next
    }

    # Process monthly unemployment rates (no longer filters - already filtered)
    monthly_data <- tryCatch({
      aggregate_monthly_unemployment(educ_data, weight_var = "auto")
    }, error = function(e) {
      warning("Error processing data for ", educ_name, ": ", e$message)
      return(NULL)
    })

    if (is.null(monthly_data) || nrow(monthly_data) == 0) {
      warning("No monthly data for ", educ_name, ", skipping")
      next
    }

    # month column already exists in new format

    # Fit GAM model
    model <- tryCatch({
      fit_seasonal_gam(monthly_data, k_month = 10, k_trend = 20)
    }, error = function(e) {
      warning("Error fitting model for ", educ_name, ": ", e$message)
      return(NULL)
    })

    if (is.null(model)) {
      next
    }

    # Extract components
    seasonal <- extract_seasonal_component(model, monthly_data)
    trend <- extract_trend_component(model, monthly_data)

    # Create readable label using constant function
    educ_label <- get_education_description(educ_code)
    if (is.na(educ_label)) {
      educ_label <- paste("Education Code", educ_code)
    }

    # Store results
    results[[educ_name]] <- list(
      monthly_data = monthly_data,
      model = model,
      seasonal = seasonal,
      trend = trend,
      education_code = educ_code,
      education_label = educ_label
    )

    message("  ✓ Completed ", educ_name,
            " (", nrow(monthly_data), " months, ",
            "R² = ", sprintf("%.3f", summary(model)$r.sq), ")")
  }

  if (length(results) == 0) {
    stop("No valid results for any education level")
  }

  # Add metadata
  attr(results, "n_levels") <- length(results)
  attr(results, "date_created") <- Sys.time()

  return(results)
}

#' Plot Unemployment Comparison Across Education Levels
#'
#' Creates publication-quality comparison plots for unemployment rates
#' across educational attainment strata.
#'
#' @param comparison_data List. Output from compare_unemployment_by_education()
#' @param palette Character vector. Colors for each education level (optional)
#'
#' @return List of ggplot objects:
#'   \item{timeseries}{Time series of observed unemployment rates}
#'   \item{seasonal}{Seasonal patterns comparison}
#'   \item{trend}{Long-term trend comparison}
#'   \item{combined}{Combined multi-panel figure}
#'
#' @examples
#' \dontrun{
#' cps_data <- readRDS("data-raw/ipums_data.rds")
#' comparison <- compare_unemployment_by_education(
#'   cps_data,
#'   c(phd = 125, masters = 123, bachelors = 111)
#' )
#' plots <- plot_education_comparison(comparison)
#'
#' # Display combined figure
#' print(plots$combined)
#' }
#'
#' @export
plot_education_comparison <- function(comparison_data, palette = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required")
  }

  # Default color palette if not provided
  if (is.null(palette)) {
    palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")
  }

  n_levels <- length(comparison_data)
  if (n_levels > length(palette)) {
    warning("Not enough colors in palette, recycling")
    palette <- rep(palette, ceiling(n_levels / length(palette)))
  }

  # Extract data for plotting
  educ_names <- names(comparison_data)
  # Map colors to education LABELS, not internal names
  educ_labels <- sapply(comparison_data, function(x) x$education_label)
  colors <- setNames(palette[1:n_levels], educ_labels)

  # ===== PLOT 1: Time Series =====
  ts_data_list <- lapply(educ_names, function(name) {
    data <- comparison_data[[name]]$monthly_data
    data.frame(
      date = data$date,
      unemployment_rate = data$unemployment_rate,
      education = comparison_data[[name]]$education_label,
      stringsAsFactors = FALSE
    )
  })
  ts_data <- do.call(rbind, ts_data_list)

  plot_ts <- ggplot2::ggplot(ts_data, ggplot2::aes(x = date, y = unemployment_rate,
                                                   color = education)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(
      title = "Unemployment Rates by Educational Attainment (2000-2025)",
      x = "Date",
      y = "Unemployment Rate",
      color = "Education Level"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )

  # ===== PLOT 2: Seasonal Patterns =====
  seasonal_data_list <- lapply(educ_names, function(name) {
    seasonal <- comparison_data[[name]]$seasonal
    data.frame(
      month = seasonal$month,
      month_name = factor(month.abb[seasonal$month], levels = month.abb),
      seasonal_effect = seasonal$seasonal_effect,
      se = seasonal$se,
      education = comparison_data[[name]]$education_label,
      stringsAsFactors = FALSE
    )
  })
  seasonal_data <- do.call(rbind, seasonal_data_list)

  plot_seasonal <- ggplot2::ggplot(seasonal_data,
                                   ggplot2::aes(x = month_name, y = seasonal_effect,
                                               color = education, group = education)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = seasonal_effect - 2 * se,
                                       ymax = seasonal_effect + 2 * se),
                          width = 0.2, alpha = 0.6) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(
      title = "Seasonal Unemployment Patterns by Education",
      x = "Month",
      y = "Seasonal Effect",
      color = "Education Level"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )

  # ===== PLOT 3: Trends =====
  trend_data_list <- lapply(educ_names, function(name) {
    trend <- comparison_data[[name]]$trend
    monthly <- comparison_data[[name]]$monthly_data
    dates <- monthly$date[match(trend$time_index, monthly$time_index)]

    data.frame(
      date = dates,
      time_index = trend$time_index,
      trend_effect = trend$trend_effect,
      se = trend$se,
      education = comparison_data[[name]]$education_label,
      stringsAsFactors = FALSE
    )
  })
  trend_data <- do.call(rbind, trend_data_list)

  plot_trend <- ggplot2::ggplot(trend_data,
                                ggplot2::aes(x = date, y = trend_effect,
                                            color = education, fill = education)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = trend_effect - 2 * se,
                                     ymax = trend_effect + 2 * se),
                        alpha = 0.2, color = NA) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(
      title = "Long-Term Unemployment Trends by Education",
      x = "Date",
      y = "Unemployment Rate (Deseasonalized)",
      color = "Education Level",
      fill = "Education Level"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )

  # ===== COMBINED PLOT =====
  if (requireNamespace("patchwork", quietly = TRUE)) {
    plot_combined <- plot_ts / plot_seasonal / plot_trend +
      patchwork::plot_annotation(
        title = "Unemployment Analysis by Educational Attainment",
        subtitle = paste("CPS Monthly Data, 2000-2025 |",
                        n_levels, "Education Levels Compared"),
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(size = 16, face = "bold"),
          plot.subtitle = ggplot2::element_text(size = 12)
        )
      ) +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "top")
  } else {
    plot_combined <- NULL
    message("Install patchwork for combined plot: install.packages('patchwork')")
  }

  # Return list of plots
  list(
    timeseries = plot_ts,
    seasonal = plot_seasonal,
    trend = plot_trend,
    combined = plot_combined
  )
}
