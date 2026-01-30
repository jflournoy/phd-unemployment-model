#!/usr/bin/env Rscript
# Wrapper script to run devtools::test() with proper library paths
# Used by npm run test:r and npm run r:test

# Set user library path first
user_lib <- "/home/rstudio/R/x86_64-pc-linux-gnu-library/4.3"
.libPaths(c(user_lib, .libPaths()))

# Load devtools
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools", lib = user_lib, repos = "https://cloud.r-project.org")
}

# Run tests
devtools::test()