#!/usr/bin/env Rscript
# Overnight refitting script for LOO-CV analysis
# Runs both models sequentially with full iterations

library(targets)
library(phdunemployment)

cat("===========================================\n")
cat("OVERNIGHT MODEL REFITTING FOR LOO-CV ANALYSIS\n")
cat("===========================================\n\n")

cat("Current time: ", as.character(Sys.time()), "\n\n")
cat("Education order: less_than_hs, high_school, some_college, bachelors, masters, professional, phd\n\n")

# -----------------------------------------------------------------
# 1. REFIT EDU-PARALLEL MODEL (Baseline)
# -----------------------------------------------------------------
cat("1. REFITTING EDU-PARALLEL MODEL\n")
cat("   -----------------------------\n")
cat("   Chains: 4\n")
cat("   Warmup: 1500 iterations\n")
cat("   Sampling: 1500 iterations\n")
cat("   Threads: 7 per chain\n")
cat("   Expected runtime: ~99 minutes (1.65 hours)\n\n")

edu_parallel_start <- Sys.time()
cat("   Start time: ", as.character(edu_parallel_start), "\n")

tryCatch({
  # Run via targets pipeline
  tar_make('model_ode_state_space_edu_parallel')

  edu_parallel_end <- Sys.time()
  edu_parallel_elapsed <- as.numeric(difftime(edu_parallel_end, edu_parallel_start, units = "mins"))

  cat("   ✓ Edu-parallel refitting complete\n")
  cat("   End time: ", as.character(edu_parallel_end), "\n")
  cat("   Elapsed time: ", round(edu_parallel_elapsed, 1), " minutes\n\n")

}, error = function(e) {
  cat("   ✗ ERROR in edu-parallel refitting: ", e$message, "\n")
  cat("   Continuing with education-trend model anyway...\n\n")
})

# -----------------------------------------------------------------
# 2. REFIT EDUCATION-TREND MODEL
# -----------------------------------------------------------------
cat("2. REFITTING EDUCATION-TREND MODEL\n")
cat("   --------------------------------\n")
cat("   Chains: 4\n")
cat("   Warmup: 1500 iterations\n")
cat("   Sampling: 1500 iterations\n")
cat("   Threads: 7 per chain\n")
cat("   Expected runtime: ~355 minutes (5.92 hours)\n\n")

education_trend_start <- Sys.time()
cat("   Start time: ", as.character(education_trend_start), "\n")

tryCatch({
  # Run via targets pipeline
  tar_make('model_ode_state_space_education_trend')

  education_trend_end <- Sys.time()
  education_trend_elapsed <- as.numeric(difftime(education_trend_end, education_trend_start, units = "mins"))

  cat("   ✓ Education-trend refitting complete\n")
  cat("   End time: ", as.character(education_trend_end), "\n")
  cat("   Elapsed time: ", round(education_trend_elapsed, 1), " minutes\n\n")

}, error = function(e) {
  cat("   ✗ ERROR in education-trend refitting: ", e$message, "\n")
})

# -----------------------------------------------------------------
# 3. SUMMARY
# -----------------------------------------------------------------
cat("3. REFITTING SUMMARY\n")
cat("   -----------------\n")
cat("   Overall start: ", as.character(edu_parallel_start), "\n")
cat("   Overall end: ", as.character(Sys.time()), "\n")

overall_elapsed <- as.numeric(difftime(Sys.time(), edu_parallel_start, units = "mins"))
cat("   Total elapsed: ", round(overall_elapsed, 1), " minutes\n")
cat("   Total elapsed: ", round(overall_elapsed/60, 2), " hours\n\n")

# -----------------------------------------------------------------
# 4. NEXT STEPS
# -----------------------------------------------------------------
cat("4. NEXT STEPS FOR LOO-CV ANALYSIS\n")
cat("   -------------------------------\n")
cat("   To complete LOO-CV analysis:\n")
cat("   1. Compute LOO-CV for both models:\n")
cat("      tar_make(c('loo_edu_parallel', 'loo_education_trend'))\n")
cat("   2. Compare models:\n")
cat("      tar_make('loo_comparison')\n")
cat("      comparison <- tar_read('loo_comparison')\n")
cat("      print(comparison$loo_compare)\n")
cat("   3. Extract education trends:\n")
cat("      trends <- tar_read('education_trend_parameters')\n")
cat("      print(trends)\n\n")

cat("===========================================\n")
cat("REFITTING COMPLETE\n")
cat("===========================================\n")