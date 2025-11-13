#!/usr/bin/env Rscript
#' Evaluate Parameter Recovery and Validation Reports
#'
#' Comprehensive evaluation of statistical reports for quality, completeness,
#' and adherence to validation best practices based on project learnings.
#'
#' Usage:
#'   Rscript scripts/evaluate-report.R <report-file> [--verbose]
#'
#' Example:
#'   Rscript scripts/evaluate-report.R reports/factor-smooth-parameter-recovery.qmd
#'   Rscript scripts/evaluate-report.R reports/factor-smooth-parameter-recovery.html --verbose

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

#' Check Coverage Validation Quality
#'
#' @param content Report content list
#' @return List with check results and score
check_coverage_validation <- function(content) {
  text <- tolower(content$text_content)
  code <- tolower(paste(content$code_chunks, collapse = "\n"))

  checks <- list(
    has_coverage_section = any(grepl("coverage", content$sections, ignore.case = TRUE)),
    mentions_95_percent = grepl("95%", text) || grepl("0.95", text),
    proper_methodology = grepl("validate_difference_coverage|validate.*coverage", code),
    anti_pattern_avoided = !grepl("testing.*training.*data|reusing.*training", text),
    tests_differences = grepl("difference.*coverage|pairwise.*comparison", text),
    adequate_n_sims = any(grepl("n_sims?\\s*=\\s*[2-9][0-9]{2,}", code))  # n >= 200
  )

  # Calculate score
  score <- sum(unlist(checks)) / length(checks)

  # Identify issues
  issues <- character(0)
  if (!checks$has_coverage_section) {
    issues <- c(issues, "Missing coverage validation section")
  }
  if (!checks$proper_methodology) {
    issues <- c(issues, "No evidence of proper coverage validation methodology")
  }
  if (!checks$anti_pattern_avoided) {
    issues <- c(issues, "CRITICAL: May be testing on training data (anti-pattern)")
  }
  if (!checks$adequate_n_sims) {
    issues <- c(issues, "Insufficient simulation runs (recommend n >= 200)")
  }

  list(
    score = score,
    checks = checks,
    issues = issues,
    grade = if (score >= 0.8) "Pass" else if (score >= 0.6) "Conditional" else "Fail"
  )
}

#' Check DGP Consistency
#'
#' @param content Report content list
#' @return List with check results
check_dgp_consistency <- function(content) {
  text <- tolower(content$text_content)
  code <- paste(content$code_chunks, collapse = "\n")

  checks <- list(
    documents_parameters = grepl("baseline_rate|seasonal_amplitude|trend_slope", code),
    explicit_values = length(gregexpr("=\\s*c?\\(", code)[[1]]) >= 3,  # Multiple param assignments
    mentions_consistency = grepl("same.*parameters?|consistent.*dgp|matching.*parameters?", text, ignore.case = TRUE),
    documents_defaults = grepl("default|omitting", text, ignore.case = TRUE),
    has_seed = grepl("seed\\s*=", code, ignore.case = TRUE)
  )

  score <- sum(unlist(checks)) / length(checks)

  issues <- character(0)
  if (!checks$documents_parameters) {
    issues <- c(issues, "DGP parameters not clearly documented")
  }
  if (!checks$mentions_consistency) {
    issues <- c(issues, "No discussion of DGP consistency across simulations")
  }

  list(
    score = score,
    checks = checks,
    issues = issues,
    grade = if (score >= 0.8) "Pass" else if (score >= 0.6) "Conditional" else "Fail"
  )
}

#' Check Bias Quantification
#'
#' @param content Report content list
#' @return List with check results
check_bias_quantification <- function(content) {
  text <- tolower(content$text_content)

  checks <- list(
    reports_bias = grepl("bias|systematic.*error", text),
    reports_precision = grepl("precision|standard deviation|sd\\(", text),
    has_bias_plots = grepl("bias.*distribution|distribution.*bias", text),
    symmetric_check = grepl("symmetric|centered|zero", text),
    relative_bias = grepl("percent|relative|ratio", text)
  )

  score <- sum(unlist(checks)) / length(checks)

  issues <- character(0)
  if (!checks$reports_bias) {
    issues <- c(issues, "No bias quantification reported")
  }
  if (!checks$has_bias_plots) {
    issues <- c(issues, "Missing bias distribution visualizations")
  }

  list(
    score = score,
    checks = checks,
    issues = issues,
    grade = if (score >= 0.7) "Pass" else if (score >= 0.5) "Conditional" else "Fail"
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
#' @return List with check results
check_report_structure <- function(content) {
  sections_lower <- tolower(content$sections)

  required_sections <- c(
    overview = "overview|introduction",
    why_matters = "why.*matter|motivation|importance",
    model_spec = "model.*specification|formula",
    coverage = "coverage|validation",
    bias = "bias|accuracy",
    interpretation = "interpretation|conclusion|discussion"
  )

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
    grade = if (score >= 0.8) "Pass" else if (score >= 0.6) "Conditional" else "Fail"
  )
}

#' Check Statistical Rigor
#'
#' @param content Report content list
#' @return List with check results
check_statistical_rigor <- function(content) {
  text <- tolower(content$text_content)
  code <- paste(content$code_chunks, collapse = "\n")

  checks <- list(
    adequate_n = any(grepl("n_sims?\\s*=\\s*[1-9][0-9]{2,}", code)),  # n >= 100
    proper_ci_construction = grepl("vcov|covariance|standard.*error", text),
    multiple_comparisons = grepl("multiple.*comparison|bonferroni|false.*discovery", text),
    realistic_params = grepl("cps|real.*data|observed.*rate", text, ignore.case = TRUE),
    parallel_support = grepl("parallel\\s*=|future|furrr", code)
  )

  score <- sum(unlist(checks)) / length(checks)

  issues <- character(0)
  if (!checks$adequate_n) {
    issues <- c(issues, "Insufficient simulations (recommend n >= 100)")
  }
  if (!checks$proper_ci_construction) {
    issues <- c(issues, "No discussion of proper SE/CI construction")
  }

  list(
    score = score,
    checks = checks,
    issues = issues,
    grade = if (score >= 0.6) "Pass" else if (score >= 0.4) "Conditional" else "Fail"
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
#' @return Invisible list with results
evaluate_report <- function(file_path, verbose = FALSE) {

  cat("\n")
  cat("========================================\n")
  cat("  Parameter Recovery Report Evaluation\n")
  cat("========================================\n\n")

  cat("Report:", basename(file_path), "\n\n")

  # Extract content
  if (verbose) cat("Extracting report content...\n")
  content <- extract_report_content(file_path)

  if (verbose) {
    cat(sprintf("  Found %d sections\n", length(content$sections)))
    cat(sprintf("  Found %d code chunks\n", length(content$code_chunks)))
    cat(sprintf("  Found %d figures\n", length(content$figures)))
    cat("\n")
  }

  # Run all checks
  if (verbose) cat("Running evaluation checks...\n\n")

  results <- list(
    coverage = check_coverage_validation(content),
    dgp_consistency = check_dgp_consistency(content),
    bias = check_bias_quantification(content),
    false_positive = check_false_positive_control(content),
    visualization = check_visualization_quality(content),
    structure = check_report_structure(content),
    rigor = check_statistical_rigor(content),
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

  # Category scores
  cat("Category Scores:\n")
  cat("----------------\n")
  for (name in names(results)) {
    result <- results[[name]]
    cat(sprintf("  %-20s %5.0f%%  [%s]\n",
                paste0(toupper(substring(name, 1, 1)), substring(name, 2), ":"),
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
    cat("Usage: Rscript scripts/evaluate-report.R <report-file> [--verbose]\n")
    cat("\nExample:\n")
    cat("  Rscript scripts/evaluate-report.R reports/factor-smooth-parameter-recovery.html\n")
    cat("  Rscript scripts/evaluate-report.R reports/parameter-recovery-validation.qmd --verbose\n")
    quit(status = 1)
  }

  report_file <- args[1]
  verbose <- "--verbose" %in% args

  tryCatch({
    result <- evaluate_report(report_file, verbose = verbose)

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
