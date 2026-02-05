#!/usr/bin/env Rscript
library(targets)

cat("Extracting beta parameters from education-trend model...\n")
fit_obj <- tar_read('model_ode_state_space_education_trend')

if (inherits(fit_obj$fit, "CmdStanMCMC")) {
  cat("Successfully loaded CmdStanMCMC fit\n")

  beta_params <- c("beta_logit_u_eq", "beta_log_adj_speed", "beta_log_shock_2008",
                   "beta_log_shock_2020", "beta_decay_2008", "beta_decay_2020",
                   "beta_log_sigma_spline")

  # Get summary
  beta_summary <- fit_obj$fit$summary(variables = beta_params)

  # Add readable names
  beta_summary$parameter_clean <- gsub("^beta_", "", beta_summary$variable)
  beta_summary$parameter_clean <- gsub("_", " ", beta_summary$parameter_clean)
  beta_summary$parameter_clean <- paste0(toupper(substring(beta_summary$parameter_clean, 1, 1)),
                                         substring(beta_summary$parameter_clean, 2))

  cat("\nBeta parameter estimates:\n")
  print(beta_summary[, c("parameter_clean", "mean", "sd", "q5", "q95", "rhat", "ess_bulk")])

  # Determine if credible intervals include zero
  cat("\nCredible interval check (90% CI):\n")
  for (i in 1:nrow(beta_summary)) {
    includes_zero <- beta_summary$q5[i] < 0 && beta_summary$q95[i] > 0
    cat(sprintf("%-25s: CI [%6.3f, %6.3f] %s\n",
                beta_summary$parameter_clean[i],
                beta_summary$q5[i], beta_summary$q95[i],
                ifelse(includes_zero, "(includes zero)", "(excludes zero)")))
  }

  # Which parameters have CI excluding zero?
  sig <- !(beta_summary$q5 < 0 & beta_summary$q95 > 0)
  if (any(sig)) {
    cat("\nParameters with credible intervals excluding zero:\n")
    print(beta_summary[sig, c("parameter_clean", "mean", "q5", "q95")])
  } else {
    cat("\nNo parameters have credible intervals excluding zero.\n")
  }

} else {
  cat("Fit object is not CmdStanMCMC\n")
}