#!/usr/bin/env Rscript
# General wrapper for Rscript commands with proper library paths
# Usage: Rscript scripts/r-wrapper.R "R code to execute"

# Set user library path first
user_lib <- "/home/rstudio/R/x86_64-pc-linux-gnu-library/4.3"
.libPaths(c(user_lib, .libPaths()))

# Get R code from command line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("No R code provided. Usage: Rscript scripts/r-wrapper.R \"R code to execute\"")
}

# Combine all arguments into a single string (handles quoting from shell)
r_code <- paste(args, collapse = " ")

# Evaluate the R code
tryCatch({
  eval(parse(text = r_code))
}, error = function(e) {
  message("Error executing R code: ", conditionMessage(e))
  quit(status = 1)
})