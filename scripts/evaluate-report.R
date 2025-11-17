#!/usr/bin/env Rscript
#' Evaluate Statistical Reports (Simulation and Exploratory)
#'
#' Comprehensive evaluation of statistical reports for quality, completeness,
#' and adherence to best practices. Automatically detects report type:
#'   - SIMULATION: Parameter recovery, coverage validation, method testing
#'   - EXPLORATORY: Real data analysis, applied modeling
#'
#' Usage:
#'   Rscript scripts/evaluate-report.R <report-file> [--verbose] [--type=TYPE]
#'
#' Example:
#'   Rscript scripts/evaluate-report.R reports/factor-smooth-parameter-recovery.qmd
#'   Rscript scripts/evaluate-report.R reports/factor-smooth-unemployment-analysis.html
#'   Rscript scripts/evaluate-report.R report.qmd --verbose --type=exploratory

suppressPackageStartupMessages({
  library(xml2)
  library(rvest)
})

#' Extract Report Content
#'
#' @param file_path Path to report (HTML or QMD)
#' @return List with report structure and content
extract_report_content <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("Report file not found: ", file_path)
  }

  ext <- tools::file_ext(file_path)

  if (ext == "html") {
    # Parse HTML
    html <- read_html(file_path)

    # Extract sections
    sections <- html %>%
      html_nodes("h2, h3") %>%
      html_text()

    # Extract all text content
    text_content <- html %>%
      html_nodes("body") %>%
      html_text()

    # Extract code chunks (if visible)
    code_chunks <- html %>%
      html_nodes("pre code, div.sourceCode") %>%
      html_text()

    # Check for figures
    figures <- html %>%
      html_nodes("img") %>%
      html_attr("src")

  } else if (ext == "qmd" || ext == "Rmd") {
    # Read raw markdown
    content <- readLines(file_path, warn = FALSE)
    text_content <- paste(content, collapse = "\n")

    # Extract section headers
    sections <- grep("^##+ ", content, value = TRUE)

    # Extract code chunks
    code_start <- grep("^```\\{r", content)
    code_end <- grep("^```$", content)
    code_chunks <- character(0)
    if (length(code_start) > 0 && length(code_end) > 0) {
      code_chunks <- sapply(seq_along(code_start), function(i) {
        if (i <= length(code_end)) {
          paste(content[(code_start[i]+1):(code_end[i]-1)], collapse = "\n")
        } else {
          ""
        }
      })
    }

    # Check for figure generation
    figures <- grep("ggplot|plot\\(", content, value = TRUE)

  } else {
    stop("Unsupported file type: ", ext, ". Use .html, .qmd, or .Rmd")
  }

  list(
    file_path = file_path,
    sections = sections,
    text_content = text_content,
    code_chunks = code_chunks,
    figures = figures
  )
}

#' Detect Report Type
#'
#' Automatically detects whether a report is a simulation/validation study
#' or an exploratory analysis of real data.
#'
#' @param content Report content list from extract_report_content()
#' @return Character: "simulation" or "exploratory"
detect_report_type <- function(content) {
  text <- tolower(content$text_content)
  code <- tolower(paste(content$code_chunks, collapse = "\n"))

  # Strong indicators of simulation report
  simulation_indicators <- c(
    grepl("parameter recovery|coverage validation|simulation study", text),
    grepl("validate.*coverage|coverage.*validation", text),
    grepl("n_sims?\\s*=|simulate.*data|data.*generat", code),
    grepl("dgp|data generating process", text),
    grepl("bias.*precision|monte carlo", text),
    any(grepl("parameter recovery|coverage|simulation", content$sections, ignore.case = TRUE))
  )

  # Strong indicators of exploratory/real data report
  exploratory_indicators <- c(
    grepl("validation_type\\s*=\\s*['\"]exploratory['\"]", code),
    grepl("cps.*data|ipums|real.*data.*analysis", text),
    grepl("data source|current population survey", text),
    grepl("applied.*analysis|exploratory.*analysis", text),
    any(grepl("results|findings|discussion|conclusion", content$sections, ignore.case = TRUE))
  )

  simulation_score <- sum(simulation_indicators)
  exploratory_score <- sum(exploratory_indicators)

  # Default to exploratory if ambiguous (safer default)
  if (simulation_score > exploratory_score) {
    return("simulation")
  } else {
    return("exploratory")
  }
}

#' Check Coverage Validation Quality
#'
#' @param content Report content list
#' @param report_type Character: "simulation" or "exploratory"
#' @return List with check results and score
check_coverage_validation <- function(content, report_type = "simulation") {
  text <- tolower(content$text_content)
  code <- tolower(paste(content$code_chunks, collapse = "\n"))

  if (report_type == "simulation") {
    # Strict checks for simulation/validation reports
    checks <- list(
      has_coverage_section = any(grepl("coverage", content$sections, ignore.case = TRUE)),
      mentions_95_percent = grepl("95%", text) || grepl("0.95", text),
      proper_methodology = grepl("validate_difference_coverage|validate.*coverage", code),
      # Check for anti-pattern: testing on training data WITHOUT generating new data
      anti_pattern_avoided = !grepl("test.*on.*training|reuse.*training", text) ||
                            grepl("new.*dataset|generate.*new.*data|held.*out", text),
      tests_differences = grepl("difference.*coverage|pairwise.*comparison", text),
      adequate_n_sims = any(grepl("n_sims?\\s*=\\s*[2-9][0-9]{2,}", code))  # n >= 200
    )

    issues <- character(0)
    if (!checks$has_coverage_section) {
      issues <- c(issues, "Missing coverage validation section")
    }
    if (!checks$proper_methodology) {
      issues <- c(issues, "No evidence of proper coverage validation methodology")
    }
    if (!checks$anti_pattern_avoided) {
      issues <- c(issues, "CRITICAL: Testing on training data without generating new datasets (anti-pattern)")
    }
    if (!checks$adequate_n_sims) {
      issues <- c(issues, "Insufficient simulation runs (recommend n >= 200)")
    }

  } else {
    # Adapted checks for exploratory/real data reports
    checks <- list(
      has_validation = grepl("validate_gam_model|validation|diagnostic", code),
      has_confidence_intervals = grepl("95%|confidence.*interval|uncertainty", text),
      model_diagnostics = grepl("residual|convergence|gam\\.check", code),
      no_overfitting = grepl("cross.*validation|held.*out|test.*data", text) ||
                       grepl("edf|effective.*degrees", text),
      proper_inference = grepl("simultaneous|bonferroni|multiple.*comparison", text) ||
                        grepl("confidence.*band", text),
      adequate_sample_size = grepl("sample.*size|n\\s*=|observations", text)
    )

    issues <- character(0)
    if (!checks$has_validation) {
      issues <- c(issues, "No model validation diagnostics")
    }
    if (!checks$model_diagnostics) {
      issues <- c(issues, "Missing diagnostic checks (residuals, convergence)")
    }
    if (!checks$has_confidence_intervals) {
      issues <- c(issues, "No uncertainty quantification (confidence intervals)")
    }
  }

  score <- sum(unlist(checks)) / length(checks)

  list(
    score = score,
    checks = checks,
    issues = issues,
    grade = if (score >= 0.8) "Pass" else if (score >= 0.6) "Conditional" else "Fail",
    report_type = report_type
  )
}

#' Check DGP Consistency (Simulation) or Data Documentation (Exploratory)
#'
#' @param content Report content list
#' @param report_type Character: "simulation" or "exploratory"
#' @return List with check results
check_dgp_consistency <- function(content, report_type = "simulation") {
  text <- tolower(content$text_content)
  code <- paste(content$code_chunks, collapse = "\n")

  if (report_type == "simulation") {
    # For simulations: check DGP consistency
    checks <- list(
      documents_parameters = grepl("baseline_rate|seasonal_amplitude|trend_slope", code),
      explicit_values = length(gregexpr("=\\s*c?\\(", code)[[1]]) >= 3,
      mentions_consistency = grepl("same.*parameters?|consistent.*dgp|matching.*parameters?", text, ignore.case = TRUE),
      documents_defaults = grepl("default|omitting", text, ignore.case = TRUE),
      has_seed = grepl("seed\\s*=", code, ignore.case = TRUE)
    )

    issues <- character(0)
    if (!checks$documents_parameters) {
      issues <- c(issues, "DGP parameters not clearly documented")
    }
    if (!checks$mentions_consistency) {
      issues <- c(issues, "No discussion of DGP consistency across simulations")
    }

  } else {
    # For exploratory: check data source documentation
    checks <- list(
      documents_data_source = grepl("data source|source:|ipums|cps|survey", text, ignore.case = TRUE),
      time_period = grepl("\\d{4}.*\\d{4}|time period|years?:", text),
      sample_size = grepl("sample.*size|observations|n\\s*=|respondents", text),
      data_availability = grepl("available|repository|reproducib", text),
      preprocessing = grepl("preprocess|clean|transform|filter", text, ignore.case = TRUE)
    )

    issues <- character(0)
    if (!checks$documents_data_source) {
      issues <- c(issues, "Data source not clearly documented")
    }
    if (!checks$time_period) {
      issues <- c(issues, "Time period not specified")
    }
    if (!checks$sample_size) {
      issues <- c(issues, "Sample size not reported")
    }
  }

  score <- sum(unlist(checks)) / length(checks)

  list(
    score = score,
    checks = checks,
    issues = issues,
    grade = if (score >= 0.8) "Pass" else if (score >= 0.6) "Conditional" else "Fail",
    report_type = report_type
  )
}

#' Check Bias Quantification (Simulation) or Uncertainty (Exploratory)
#'
#' @param content Report content list
#' @param report_type Character: "simulation" or "exploratory"
#' @return List with check results
check_bias_quantification <- function(content, report_type = "simulation") {
  text <- tolower(content$text_content)

  if (report_type == "simulation") {
    # For simulations: check bias quantification
    checks <- list(
      reports_bias = grepl("bias|systematic.*error", text),
      reports_precision = grepl("precision|standard deviation|sd\\(", text),
      has_bias_plots = grepl("bias.*distribution|distribution.*bias", text),
      symmetric_check = grepl("symmetric|centered|zero", text),
      relative_bias = grepl("percent|relative|ratio", text)
    )

    issues <- character(0)
    if (!checks$reports_bias) {
      issues <- c(issues, "No bias quantification reported")
    }
    if (!checks$has_bias_plots) {
      issues <- c(issues, "Missing bias distribution visualizations")
    }

  } else {
    # For exploratory: check uncertainty quantification
    checks <- list(
      has_confidence_intervals = grepl("confidence.*interval|95%.*ci|uncertainty", text),
      has_standard_errors = grepl("standard.*error|se\\s*=|stderr", text),
      shows_variability = grepl("variance|variability|uncertainty", text),
      plots_uncertainty = grepl("ribbon|errorbar|confidence.*band", text),
      discusses_precision = grepl("precision|accuracy|reliable", text)
    )

    issues <- character(0)
    if (!checks$has_confidence_intervals) {
      issues <- c(issues, "No confidence intervals reported")
    }
    if (!checks$plots_uncertainty) {
      issues <- c(issues, "Missing uncertainty visualization (ribbons, error bars)")
    }
  }

  score <- sum(unlist(checks)) / length(checks)

  list(
    score = score,
    checks = checks,
    issues = issues,
    grade = if (score >= 0.7) "Pass" else if (score >= 0.5) "Conditional" else "Fail",
    report_type = report_type
  )
}

#' Check False Positive Control
#'
#' @param content Report content list
#' @return List with check results
check_false_positive_control <- function(content) {
  text <- tolower(content$text_content)
  code <- tolower(paste(content$code_chunks, collapse = "\n"))

  checks <- list(
    has_false_positive_test = grepl("false positive|type i error|null.*simulation", text),
    proper_null_data = grepl("no.*difference|identical.*parameter|common_baseline", code),
    reports_rate = grepl("false.*positive.*rate|type.*i.*rate", text),
    compares_to_5_percent = grepl("5%|0\\.05|≤\\s*5", text),
    tests_multiple_types = grepl("baseline.*trend|trend.*baseline", text)
  )

  score <- sum(unlist(checks)) / length(checks)

  issues <- character(0)
  if (!checks$has_false_positive_test) {
    issues <- c(issues, "No false positive rate testing")
  }
  if (!checks$proper_null_data) {
    issues <- c(issues, "Unclear if null simulations use proper setup")
  }

  list(
    score = score,
    checks = checks,
    issues = issues,
    grade = if (score >= 0.6) "Pass" else if (score >= 0.4) "Conditional" else "Fail"
  )
}

#' Check Visualization Quality
#'
#' @param content Report content list
#' @return List with check results
check_visualization_quality <- function(content) {
  code <- paste(content$code_chunks, collapse = "\n")

  checks <- list(
    has_ggplot = grepl("ggplot", code),
    multiple_plots = length(content$figures) >= 3,
    component_plots = grepl("facet|component|trend.*seasonal", code, ignore.case = TRUE),
    ci_plots = grepl("geom_ribbon|geom_errorbar|confidence", code, ignore.case = TRUE),
    clear_labels = grepl("labs\\(|xlab\\(|ylab\\(|title", code)
  )

  score <- sum(unlist(checks)) / length(checks)

  issues <- character(0)
  if (!checks$multiple_plots) {
    issues <- c(issues, "Limited visualizations (recommend 3+ diagnostic plots)")
  }
  if (!checks$ci_plots) {
    issues <- c(issues, "No uncertainty visualization (error bars, ribbons)")
  }

  list(
    score = score,
    checks = checks,
    issues = issues,
    grade = if (score >= 0.7) "Pass" else if (score >= 0.5) "Conditional" else "Fail"
  )
}

#' Check Report Structure
#'
#' @param content Report content list
#' @param report_type Character: "simulation" or "exploratory"
#' @return List with check results
check_report_structure <- function(content, report_type = "simulation") {
  sections_lower <- tolower(content$sections)

  if (report_type == "simulation") {
    # Simulation report structure
    required_sections <- c(
      overview = "overview|introduction|executive summary",
      why_matters = "why.*matter|motivation|importance",
      model_spec = "model.*specification|formula|dgp",
      coverage = "coverage|validation",
      bias = "bias|accuracy|precision",
      interpretation = "interpretation|conclusion|discussion"
    )
  } else {
    # Exploratory report structure
    required_sections <- c(
      overview = "overview|introduction|executive summary|abstract",
      methods = "method|data|model|approach",
      results = "result|finding|analysis",
      model_validation = "validation|diagnostic|model.*check",
      interpretation = "discussion|conclusion|interpretation",
      reproducibility = "reproducib|session.*info|code.*availab"
    )
  }

  checks <- lapply(required_sections, function(pattern) {
    any(grepl(pattern, sections_lower))
  })

  score <- sum(unlist(checks)) / length(checks)

  issues <- character(0)
  missing <- names(checks)[!unlist(checks)]
  if (length(missing) > 0) {
    issues <- c(issues, paste("Missing sections:", paste(missing, collapse = ", ")))
  }

  list(
    score = score,
    checks = checks,
    issues = issues,
    grade = if (score >= 0.8) "Pass" else if (score >= 0.6) "Conditional" else "Fail",
    report_type = report_type
  )
}

#' Check Statistical Rigor
#'
#' @param content Report content list
#' @param report_type Character: "simulation" or "exploratory"
#' @return List with check results
check_statistical_rigor <- function(content, report_type = "simulation") {
  text <- tolower(content$text_content)
  code <- paste(content$code_chunks, collapse = "\n")

  if (report_type == "simulation") {
    # For simulations: check simulation rigor
    checks <- list(
      adequate_n = any(grepl("n_sims?\\s*=\\s*[1-9][0-9]{2,}", code)),  # n >= 100
      proper_ci_construction = grepl("vcov|covariance|standard.*error", text),
      multiple_comparisons = grepl("multiple.*comparison|bonferroni|false.*discovery", text),
      realistic_params = grepl("cps|real.*data|observed.*rate", text, ignore.case = TRUE),
      parallel_support = grepl("parallel\\s*=|future|furrr", code)
    )

    issues <- character(0)
    if (!checks$adequate_n) {
      issues <- c(issues, "Insufficient simulations (recommend n >= 100)")
    }
    if (!checks$proper_ci_construction) {
      issues <- c(issues, "No discussion of proper SE/CI construction")
    }

  } else {
    # For exploratory: check analytical rigor
    checks <- list(
      model_selection = grepl("aic|bic|model.*selection|nested.*model", text),
      proper_inference = grepl("p.*value|significance|hypothesis.*test", text) ||
                        grepl("confidence.*interval|credible.*interval", text),
      multiple_comparisons = grepl("multiple.*comparison|bonferroni|simultaneous", text) ||
                            grepl("family.*wise|false.*discovery", text),
      sample_size_discussion = grepl("sample.*size|power|precision", text),
      robustness_checks = grepl("robust|sensitivity|diagnostic", text)
    )

    issues <- character(0)
    if (!checks$model_selection) {
      issues <- c(issues, "No model selection justification (AIC, BIC, nested comparison)")
    }
    if (!checks$proper_inference) {
      issues <- c(issues, "Limited statistical inference discussion")
    }
  }

  score <- sum(unlist(checks)) / length(checks)

  list(
    score = score,
    checks = checks,
    issues = issues,
    grade = if (score >= 0.6) "Pass" else if (score >= 0.4) "Conditional" else "Fail",
    report_type = report_type
  )
}

#' Check Documentation Quality
#'
#' @param content Report content list
#' @return List with check results
check_documentation_quality <- function(content) {
  text <- tolower(content$text_content)
  code <- paste(content$code_chunks, collapse = "\n")

  checks <- list(
    parameters_documented = grepl("baseline.*=.*0\\.|seasonal.*=.*0\\.|trend.*=", code),
    comparison_section = grepl("what we.*compar|test procedure|validation.*approach", text),
    reproducible_code = grepl("set\\.seed|library\\(|devtools::load_all", code),
    version_info = grepl("version|package|sessioninfo", text),
    runtime_notes = grepl("runtime|computational|parallel|minutes?|hours?", text)
  )

  score <- sum(unlist(checks)) / length(checks)

  issues <- character(0)
  if (!checks$parameters_documented) {
    issues <- c(issues, "Parameters not explicitly documented in code")
  }
  if (!checks$comparison_section) {
    issues <- c(issues, "Missing 'What We're Comparing' explanatory section")
  }
  if (!checks$reproducible_code) {
    issues <- c(issues, "Reproducibility concerns (seed, library loading)")
  }

  list(
    score = score,
    checks = checks,
    issues = issues,
    grade = if (score >= 0.7) "Pass" else if (score >= 0.5) "Conditional" else "Fail"
  )
}

#' Generate Evaluation Report
#'
#' @param file_path Path to report file
#' @param verbose Print detailed diagnostics
#' @param report_type Character: "auto" (default), "simulation", or "exploratory"
#' @return Invisible list with results
evaluate_report <- function(file_path, verbose = FALSE, report_type = "auto") {

  cat("\n")
  cat("========================================\n")
  cat("  Statistical Report Evaluation\n")
  cat("========================================\n\n")

  cat("Report:", basename(file_path), "\n")

  # Extract content
  if (verbose) cat("Extracting report content...\n")
  content <- extract_report_content(file_path)

  # Detect report type if not specified
  if (report_type == "auto") {
    report_type <- detect_report_type(content)
    if (verbose) cat(sprintf("Auto-detected report type: %s\n", toupper(report_type)))
  }

  cat("Type:", toupper(report_type), "\n\n")

  if (verbose) {
    cat(sprintf("  Found %d sections\n", length(content$sections)))
    cat(sprintf("  Found %d code chunks\n", length(content$code_chunks)))
    cat(sprintf("  Found %d figures\n", length(content$figures)))
    cat("\n")
  }

  # Run all checks with report type
  if (verbose) cat("Running evaluation checks...\n\n")

  results <- list(
    coverage = check_coverage_validation(content, report_type),
    dgp_consistency = check_dgp_consistency(content, report_type),
    bias = check_bias_quantification(content, report_type),
    false_positive = check_false_positive_control(content),
    visualization = check_visualization_quality(content),
    structure = check_report_structure(content, report_type),
    rigor = check_statistical_rigor(content, report_type),
    documentation = check_documentation_quality(content)
  )

  # Calculate overall score
  overall_score <- mean(sapply(results, function(x) x$score))
  overall_grade <- if (overall_score >= 0.8) {
    "PASS"
  } else if (overall_score >= 0.65) {
    "CONDITIONAL PASS"
  } else {
    "FAIL"
  }

  # Print results
  cat("===============================\n")
  cat("  EVALUATION RESULTS\n")
  cat("===============================\n\n")

  cat(sprintf("Overall Grade: %s (%.0f%%)\n\n", overall_grade, overall_score * 100))

  # Category scores with context-aware labels
  cat("Category Scores:\n")
  cat("----------------\n")

  # Define friendly category names based on report type
  category_labels <- if (report_type == "simulation") {
    list(
      coverage = "Coverage Validation",
      dgp_consistency = "DGP Consistency",
      bias = "Bias Quantification",
      false_positive = "False Positive Control",
      visualization = "Visualization Quality",
      structure = "Report Structure",
      rigor = "Statistical Rigor",
      documentation = "Documentation Quality"
    )
  } else {
    list(
      coverage = "Model Validation",
      dgp_consistency = "Data Documentation",
      bias = "Uncertainty Quantification",
      false_positive = "Statistical Inference",
      visualization = "Visualization Quality",
      structure = "Report Structure",
      rigor = "Statistical Rigor",
      documentation = "Documentation Quality"
    )
  }

  for (name in names(results)) {
    result <- results[[name]]
    label <- category_labels[[name]]
    cat(sprintf("  %-28s %5.0f%%  [%s]\n",
                paste0(label, ":"),
                result$score * 100,
                result$grade))
  }
  cat("\n")

  # Critical issues
  critical_issues <- unlist(lapply(results, function(x) x$issues))
  critical_issues <- critical_issues[grepl("CRITICAL|Missing coverage|No.*coverage validation|training data", critical_issues)]

  if (length(critical_issues) > 0) {
    cat("CRITICAL ISSUES (Must Fix):\n")
    cat("---------------------------\n")
    for (issue in critical_issues) {
      cat("  ❌", issue, "\n")
    }
    cat("\n")
  }

  # Warnings
  all_issues <- unlist(lapply(results, function(x) x$issues))
  warnings <- setdiff(all_issues, critical_issues)

  if (length(warnings) > 0) {
    cat("Warnings (Should Address):\n")
    cat("--------------------------\n")
    for (warning in warnings) {
      cat("  ⚠️ ", warning, "\n")
    }
    cat("\n")
  }

  # Strengths
  strengths <- character(0)
  if (results$coverage$score >= 0.8) {
    strengths <- c(strengths, "Strong coverage validation methodology")
  }
  if (results$visualization$score >= 0.8) {
    strengths <- c(strengths, "Excellent visualization and diagnostics")
  }
  if (results$rigor$score >= 0.8) {
    strengths <- c(strengths, "Statistically rigorous approach")
  }
  if (results$documentation$score >= 0.8) {
    strengths <- c(strengths, "Well-documented and reproducible")
  }

  if (length(strengths) > 0) {
    cat("Strengths:\n")
    cat("----------\n")
    for (strength in strengths) {
      cat("  ✅", strength, "\n")
    }
    cat("\n")
  }

  # Recommendations
  cat("Recommendations:\n")
  cat("----------------\n")
  if (results$coverage$score < 0.8) {
    cat("  • Enhance coverage validation: fit MANY models, test on NEW data\n")
  }
  if (results$dgp_consistency$score < 0.8) {
    cat("  • Document DGP parameters explicitly and ensure consistency\n")
  }
  if (results$bias$score < 0.7) {
    cat("  • Add bias quantification with distribution plots\n")
  }
  if (results$false_positive$score < 0.6) {
    cat("  • Include false positive rate testing with null simulations\n")
  }
  if (results$visualization$score < 0.7) {
    cat("  • Add more diagnostic visualizations (CI width, bias plots, etc.)\n")
  }
  if (results$documentation$score < 0.7) {
    cat("  • Add 'What We're Comparing' section explaining test procedure\n")
  }

  cat("\n")
  cat("========================================\n\n")

  invisible(list(
    overall_score = overall_score,
    overall_grade = overall_grade,
    category_results = results,
    critical_issues = critical_issues,
    warnings = warnings,
    strengths = strengths
  ))
}

# Main execution
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0) {
    cat("Usage: Rscript scripts/evaluate-report.R <report-file> [OPTIONS]\n")
    cat("\nOptions:\n")
    cat("  --verbose         Show detailed diagnostics\n")
    cat("  --type=TYPE       Force report type: 'simulation' or 'exploratory' (default: auto-detect)\n")
    cat("\nExamples:\n")
    cat("  Rscript scripts/evaluate-report.R reports/factor-smooth-parameter-recovery.html\n")
    cat("  Rscript scripts/evaluate-report.R reports/factor-smooth-unemployment-analysis.qmd --verbose\n")
    cat("  Rscript scripts/evaluate-report.R report.qmd --type=exploratory\n")
    quit(status = 1)
  }

  report_file <- args[1]
  verbose <- "--verbose" %in% args

  # Extract report type if specified
  type_args <- grep("^--type=", args, value = TRUE)
  report_type <- if (length(type_args) > 0) {
    gsub("^--type=", "", type_args[1])
  } else {
    "auto"
  }

  # Validate report type
  if (!report_type %in% c("auto", "simulation", "exploratory")) {
    cat("Error: Invalid report type. Must be 'auto', 'simulation', or 'exploratory'\n")
    quit(status = 1)
  }

  tryCatch({
    result <- evaluate_report(report_file, verbose = verbose, report_type = report_type)

    # Exit with status based on grade
    if (result$overall_grade == "FAIL") {
      quit(status = 1)
    } else {
      quit(status = 0)
    }
  }, error = function(e) {
    cat("\nError evaluating report:\n")
    cat("  ", conditionMessage(e), "\n")
    quit(status = 1)
  })
}
