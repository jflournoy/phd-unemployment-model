#!/usr/bin/env Rscript
# Quick compilation test for ordered categorical Stan model

library(cmdstanr)

cat("Testing compilation of ordered categorical Stan model...\n")

stan_file <- "stan/unemployment-ode-state-space-ordered-categorical.stan"
if (!file.exists(stan_file)) {
  stop("Stan file not found: ", stan_file)
}

cat("Found Stan file:", stan_file, "\n")
cat("Compiling with threading support...\n")

tryCatch({
  model <- cmdstanr::cmdstan_model(
    stan_file,
    cpp_options = list(stan_threads = TRUE),
    force_recompile = TRUE
  )
  cat("✅ Compilation successful!\n")
  cat("Model name:", model$name(), "\n")
  cat("Variables:", model$variables(), "\n")
}, error = function(e) {
  cat("❌ Compilation failed:\n")
  cat(e$message, "\n")
  quit(status = 1)
})

cat("Done.\n")