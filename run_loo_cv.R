#!/usr/bin/env Rscript
# Run LOO-CV comparison with reduced iterations for speed
library(cmdstanr)
library(data.table)
library(here)
library(loo)
library(targets)
library(tarchetypes)

# Source the fitting functions (or load the package)
if (!requireNamespace("phdunemployment", quietly = TRUE)) {
  devtools::load_all()
} else {
  library(phdunemployment)
}

cat("=============================================\n")
cat("LOO-CV Comparison with Reduced Iterations\n")
cat("=============================================\n")

# Load data
data_file <- here::here("data", "education-spectrum-counts.rds")
if (!file.exists(data_file)) {
  cat("Data file not found, loading from targets store...\n")
  counts <- tar_read(education_counts)
} else {
  cat("Loading data from", data_file, "\n")
  counts <- readRDS(data_file)
}
cat("Data dimensions:", nrow(counts), "rows\n")
cat("Education levels:", paste(unique(counts$education), collapse = ", "), "\n")

# Reduced iteration settings
iter_warmup <- 200
iter_sampling <- 200
chains <- 4
parallel_chains <- 2
threads_per_chain <- 7
grainsize <- 1L
K_spline <- 25L

# ===========================================================================
# Fit edu-parallel model
# ===========================================================================
cat("\n", strrep("=", 80), "\n")
cat("Fitting edu-parallel model with reduced iterations\n")
cat("Chains:", chains, "Iterations:", iter_warmup, "warmup +", iter_sampling, "sampling\n")
cat(strrep("=", 80), "\n\n")

start_time <- Sys.time()
edu_parallel_result <- fit_ode_state_space_edu_parallel(
  counts,
  chains = chains,
  iter_sampling = iter_sampling,
  iter_warmup = iter_warmup,
  adapt_delta = 0.99,
  max_treedepth = 15,
  parallel_chains = parallel_chains,
  threads_per_chain = threads_per_chain,
  grainsize = grainsize,
  refresh = 100
)
end_time <- Sys.time()
edu_parallel_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
cat("Edu-parallel fitting completed in", round(edu_parallel_time, 1), "minutes\n")

# ===========================================================================
# Fit education-trend model
# ===========================================================================
cat("\n", strrep("=", 80), "\n")
cat("Fitting education-trend model with reduced iterations\n")
cat("Chains:", chains, "Iterations:", iter_warmup, "warmup +", iter_sampling, "sampling\n")
cat(strrep("=", 80), "\n\n")

start_time <- Sys.time()
education_trend_result <- fit_ode_state_space_education_trend(
  counts,
  education_order = NULL,
  chains = chains,
  iter_sampling = iter_sampling,
  iter_warmup = iter_warmup,
  adapt_delta = 0.99,
  max_treedepth = 15,
  parallel_chains = parallel_chains,
  threads_per_chain = threads_per_chain,
  grainsize = grainsize,
  K_spline = K_spline,
  refresh = 100
)
end_time <- Sys.time()
education_trend_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
cat("Education-trend fitting completed in", round(education_trend_time, 1), "minutes\n")

# ===========================================================================
# Compute LOO-CV
# ===========================================================================
cat("\n", strrep("=", 80), "\n")
cat("Computing LOO-CV metrics\n")
cat(strrep("=", 80), "\n\n")

# Check log_lik availability
if (is.null(edu_parallel_result$log_lik)) {
  cat("WARNING: edu_parallel_result missing log_lik, extracting from fit object...\n")
  edu_parallel_log_lik <- edu_parallel_result$fit$draws(variables = "log_lik", format = "matrix")
} else {
  edu_parallel_log_lik <- edu_parallel_result$log_lik
}

if (is.null(education_trend_result$log_lik)) {
  cat("WARNING: education_trend_result missing log_lik, extracting from fit object...\n")
  education_trend_log_lik <- education_trend_result$fit$draws(variables = "log_lik", format = "matrix")
} else {
  education_trend_log_lik <- education_trend_result$log_lik
}

cat("Edu-parallel log_lik dimensions:", paste(dim(edu_parallel_log_lik), collapse = " x "), "\n")
cat("Education-trend log_lik dimensions:", paste(dim(education_trend_log_lik), collapse = " x "), "\n")

# Compute LOO
cat("\nComputing LOO for edu-parallel model...\n")
loo_edu_parallel <- loo::loo(edu_parallel_log_lik, cores = 4)
cat("Computing LOO for education-trend model...\n")
loo_education_trend <- loo::loo(education_trend_log_lik, cores = 4)

# Compare models
cat("\nComparing models with loo_compare...\n")
comparison <- loo::loo_compare(loo_edu_parallel, loo_education_trend)
print(comparison)

# Create summary data frame
summary_df <- data.frame(
  model = c("edu_parallel", "education_trend"),
  elpd_loo = c(loo_edu_parallel$estimates["elpd_loo", "Estimate"],
               loo_education_trend$estimates["elpd_loo", "Estimate"]),
  se_elpd_loo = c(loo_edu_parallel$estimates["elpd_loo", "SE"],
                  loo_education_trend$estimates["elpd_loo", "SE"]),
  p_loo = c(loo_edu_parallel$estimates["p_loo", "Estimate"],
            loo_education_trend$estimates["p_loo", "Estimate"]),
  se_p_loo = c(loo_edu_parallel$estimates["p_loo", "SE"],
               loo_education_trend$estimates["p_loo", "SE"]),
  looic = c(loo_edu_parallel$estimates["looic", "Estimate"],
            loo_education_trend$estimates["looic", "Estimate"]),
  se_looic = c(loo_edu_parallel$estimates["looic", "SE"],
               loo_education_trend$estimates["looic", "SE"]),
  fitting_time_mins = c(edu_parallel_time, education_trend_time)
)

cat("\nLOO-CV Summary:\n")
print(summary_df)

# Save results
output_dir <- here::here("models", "loo-cv-results")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
output_file <- file.path(output_dir, paste0("loo-cv-comparison-", format(Sys.time(), "%Y%m%d-%H%M"), ".rds"))
saveRDS(
  list(
    edu_parallel_result = edu_parallel_result,
    education_trend_result = education_trend_result,
    loo_edu_parallel = loo_edu_parallel,
    loo_education_trend = loo_education_trend,
    comparison = comparison,
    summary = summary_df,
    settings = list(
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      chains = chains,
      parallel_chains = parallel_chains,
      threads_per_chain = threads_per_chain,
      grainsize = grainsize,
      K_spline = K_spline
    )
  ),
  file = output_file
)
cat("\nResults saved to", output_file, "\n")

# Print conclusion
cat("\n", strrep("=", 80), "\n")
cat("LOO-CV Comparison Results\n")
cat(strrep("=", 80), "\n")
cat("ELPD difference (education_trend - edu_parallel):",
    comparison[2, "elpd_diff"], "±", comparison[2, "se_diff"], "\n")
if (comparison[2, "elpd_diff"] > 0) {
  cat("CONCLUSION: Education-trend model has higher predictive performance.\n")
} else {
  cat("CONCLUSION: Edu-parallel model has higher predictive performance.\n")
}
cat("(Positive ELPD difference favors education-trend model)\n")