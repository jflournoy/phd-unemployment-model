#!/usr/bin/env Rscript
# Debug quasi-binomial SE calculation issue

library(mgcv)
set.seed(42)

# Generate simple overdispersed data
n_obs <- 60
n_total <- 1000
true_p_mean <- 0.05
phi <- 20

# Beta-binomial generation
alpha_param <- true_p_mean * phi
beta_param <- (1 - true_p_mean) * phi
true_probs <- rbeta(n_obs, alpha_param, beta_param)
n_unemployed <- rbinom(n_obs, size = n_total, prob = true_probs)
n_employed <- n_total - n_unemployed

test_data <- data.frame(
  time_index = 1:n_obs,
  month = rep(1:12, 5),
  n_unemployed = n_unemployed,
  n_employed = n_employed,
  true_prob = true_probs
)

# Fit both models
model_binom <- gam(
  cbind(n_unemployed, n_employed) ~ s(time_index, k = 5) + s(month, bs = "cc", k = 6),
  data = test_data,
  family = binomial(),
  method = "REML"
)

model_quasi <- gam(
  cbind(n_unemployed, n_employed) ~ s(time_index, k = 5) + s(month, bs = "cc", k = 6),
  data = test_data,
  family = quasibinomial(),
  method = "REML"
)

# Get predictions on RESPONSE scale
pred_binom <- predict(model_binom, type = "response", se.fit = TRUE)
pred_quasi <- predict(model_quasi, type = "response", se.fit = TRUE)

# Get predictions on LINK scale
pred_binom_link <- predict(model_binom, type = "link", se.fit = TRUE)
pred_quasi_link <- predict(model_quasi, type = "link", se.fit = TRUE)

cat("\n=== DISPERSION ===\n")
cat("Binomial dispersion:", summary(model_binom)$dispersion, "\n")
cat("Quasi-binomial dispersion:", summary(model_quasi)$dispersion, "\n")
cat("sqrt(dispersion):", sqrt(summary(model_quasi)$dispersion), "\n")

cat("\n=== LINK SCALE SEs ===\n")
cat("Binomial SE (link) - first 10:", head(pred_binom_link$se.fit, 10), "\n")
cat("Quasi SE (link) - first 10:", head(pred_quasi_link$se.fit, 10), "\n")
cat("Ratio (link scale):", mean(pred_quasi_link$se.fit) / mean(pred_binom_link$se.fit), "\n")

cat("\n=== RESPONSE SCALE SEs ===\n")
cat("Binomial SE (response) - first 10:", head(pred_binom$se.fit, 10), "\n")
cat("Quasi SE (response) - first 10:", head(pred_quasi$se.fit, 10), "\n")
cat("Ratio (response scale):", mean(pred_quasi$se.fit) / mean(pred_binom$se.fit), "\n")

cat("\n=== SE PATTERNS ===\n")
cat("Binomial SE range (response):", range(pred_binom$se.fit), "\n")
cat("Quasi SE range (response):", range(pred_quasi$se.fit), "\n")
cat("Binomial SE CV:", sd(pred_binom$se.fit) / mean(pred_binom$se.fit), "\n")
cat("Quasi SE CV:", sd(pred_quasi$se.fit) / mean(pred_quasi$se.fit), "\n")

cat("\n=== POINT PREDICTIONS ===\n")
cat("Binomial fits - first 10:", head(pred_binom$fit, 10), "\n")
cat("Quasi fits - first 10:", head(pred_quasi$fit, 10), "\n")
cat("Max difference in fits:", max(abs(pred_binom$fit - pred_quasi$fit)), "\n")

# Check if the jaggedness is from delta method transformation
cat("\n=== INVESTIGATING JAGGEDNESS ===\n")
# On link scale, quasi SEs should be exactly binom SEs * sqrt(phi)
link_ratio <- pred_quasi_link$se.fit / pred_binom_link$se.fit
cat("Link scale SE ratio range:", range(link_ratio), "\n")
cat("Link scale SE ratio SD:", sd(link_ratio), "\n")
cat("All link ratios equal?", all(abs(link_ratio - mean(link_ratio)) < 1e-6), "\n")

# On response scale, delta method: SE_p = |dp/deta| * SE_eta
# where p = plogis(eta), so dp/deta = p*(1-p)
derivative_binom <- pred_binom$fit * (1 - pred_binom$fit)
derivative_quasi <- pred_quasi$fit * (1 - pred_quasi$fit)

cat("\nDelta method derivatives (binomial) - first 10:", head(derivative_binom, 10), "\n")
cat("Delta method derivatives (quasi) - first 10:", head(derivative_quasi, 10), "\n")
cat("Max difference in derivatives:", max(abs(derivative_binom - derivative_quasi)), "\n")
