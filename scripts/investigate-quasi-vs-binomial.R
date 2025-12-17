#!/usr/bin/env Rscript
# Investigate whether binomial and quasi-binomial should have different fits
# Reference: Wood (2017) section 3.1.7

library(mgcv)
library(MASS)  # For glm.nb if needed

cat("=== TESTING: Do binomial and quasi-binomial give same fits? ===\n\n")

# Test 1: Simple GLM (no smooths) with overdispersed data
set.seed(123)
n <- 100
x <- rnorm(n)
true_p <- plogis(-0.5 + 0.8*x)
# Add overdispersion
phi <- 20
alpha <- true_p * phi
beta <- (1 - true_p) * phi
p_overdispersed <- rbeta(n, alpha, beta)
y <- rbinom(n, size = 50, prob = p_overdispersed)

dat <- data.frame(x = x, y = y, n_total = 50, n_fail = 50 - y)

# Fit both
glm_binom <- glm(cbind(y, n_fail) ~ x, family = binomial(), data = dat)
glm_quasi <- glm(cbind(y, n_fail) ~ x, family = quasibinomial(), data = dat)

cat("GLM (no smooths):\n")
cat("Binomial coefficients:", coef(glm_binom), "\n")
cat("Quasi-binomial coefficients:", coef(glm_quasi), "\n")
cat("Max difference:", max(abs(coef(glm_binom) - coef(glm_quasi))), "\n")
cat("Are they equal? ", all.equal(coef(glm_binom), coef(glm_quasi)), "\n\n")

# Test 2: GAM with smooths
gam_binom <- gam(cbind(y, n_fail) ~ s(x, k = 5), family = binomial(), data = dat)
gam_quasi <- gam(cbind(y, n_fail) ~ s(x, k = 5), family = quasibinomial(), data = dat)

pred_binom <- predict(gam_binom, type = "response")
pred_quasi <- predict(gam_quasi, type = "response")

cat("GAM (with smooths):\n")
cat("Max prediction difference:", max(abs(pred_binom - pred_quasi)), "\n")
cat("Mean prediction difference:", mean(abs(pred_binom - pred_quasi)), "\n")
cat("Correlation:", cor(pred_binom, pred_quasi), "\n\n")

# Test 3: What about with REML vs GCV?
gam_binom_reml <- gam(cbind(y, n_fail) ~ s(x, k = 5), family = binomial(),
                      data = dat, method = "REML")
gam_quasi_reml <- gam(cbind(y, n_fail) ~ s(x, k = 5), family = quasibinomial(),
                      data = dat, method = "REML")

pred_binom_reml <- predict(gam_binom_reml, type = "response")
pred_quasi_reml <- predict(gam_quasi_reml, type = "response")

cat("GAM with REML:\n")
cat("Max prediction difference:", max(abs(pred_binom_reml - pred_quasi_reml)), "\n")
cat("Binomial lambda:", gam_binom_reml$sp, "\n")
cat("Quasi-binomial lambda:", gam_quasi_reml$sp, "\n\n")

# Test 4: Check if it's the smoothing parameter selection that differs
cat("=== Smoothing parameter comparison ===\n")
cat("Binomial (REML) lambda:", gam_binom_reml$sp, "\n")
cat("Quasi-binomial (REML) lambda:", gam_quasi_reml$sp, "\n")
cat("Ratio:", gam_quasi_reml$sp / gam_binom_reml$sp, "\n\n")

# Test 5: What if we FORCE the same smoothing parameter?
cat("=== Forcing same smoothing parameter ===\n")
lambda_fixed <- gam_binom_reml$sp
gam_quasi_fixed <- gam(cbind(y, n_fail) ~ s(x, k = 5), family = quasibinomial(),
                       data = dat, sp = lambda_fixed)

pred_quasi_fixed <- predict(gam_quasi_fixed, type = "response")
cat("With same lambda, max diff:", max(abs(pred_binom_reml - pred_quasi_fixed)), "\n")
cat("Should be near zero if lambda is the only difference\n\n")

# Test 6: Examine effective degrees of freedom
cat("=== Effective degrees of freedom ===\n")
cat("Binomial edf:", sum(gam_binom_reml$edf), "\n")
cat("Quasi-binomial edf:", sum(gam_quasi_reml$edf), "\n")
cat("Binomial smooth edf:", gam_binom_reml$edf, "\n")
cat("Quasi smooth edf:", gam_quasi_reml$edf, "\n\n")

cat("=== CONCLUSION ===\n")
cat("If GLM coefficients match but GAM predictions differ, the issue is:\n")
cat("1. Different smoothing parameter selection (REML uses different scale)\n")
cat("2. Quasi-binomial uses scale = dispersion, binomial uses scale = 1\n")
cat("3. This affects smoothing parameter optimization, leading to different wiggliness\n")
