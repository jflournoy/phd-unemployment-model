#!/bin/bash
# Run edu-parallel model with nohup to survive shell termination
nohup Rscript -e '
library(phdunemployment)
library(data.table)

cat("Loading education counts...\n")
counts <- readRDS("data/education-spectrum-counts.rds")

cat("Starting edu-parallel model fit...\n")
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

result <- fit_ode_state_space_edu_parallel(
  counts,
  chains = 4,
  iter_sampling = 1500,
  iter_warmup = 1500,
  adapt_delta = 0.99,
  max_treedepth = 15,
  parallel_chains = 4,
  threads_per_chain = 6,
  grainsize = 1L,
  refresh = 500
)

cat("\nFitting completed at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Divergent transitions:", result$diagnostics$num_divergent, "\n")
cat("Max treedepth exceeded:", result$diagnostics$max_treedepth_exceeded, "\n")
cat("E-BFMI:", paste(round(result$diagnostics$ebfmi, 3), collapse = ", "), "\n\n")

cat("Saving model...\n")
save_cmdstan_fit(result, "models/ode-state-space-edu-parallel-fit.rds", format = "rds")
cat("Done!\n")
' > models/edu-parallel-fit.log 2>&1 &
echo "PID: $!"
