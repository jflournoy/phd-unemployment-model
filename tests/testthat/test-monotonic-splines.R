# Test suite for monotonic I-spline implementation
# Following TDD: Write tests first, then implement

library(testthat)
library(phdunemployment)

test_that("generate_ispline_basis creates valid I-spline basis", {
  # Test basic I-spline generation for education levels
  education_levels <- 7
  n_basis <- 4  # Number of I-spline basis functions

  # Generate I-spline basis
  basis <- generate_ispline_basis(education_levels, n_basis = n_basis)

  # Should return a matrix
  expect_true(is.matrix(basis))

  # Should have correct number of rows
  expect_equal(nrow(basis), education_levels)

  # Should have at least n_basis columns (intercept=TRUE adds one)
  expect_true(ncol(basis) >= n_basis)

  # All values should be non-negative (I-splines are positive)
  expect_true(all(basis >= 0))
})

test_that("generate_ispline_basis produces monotonically increasing basis functions", {
  # I-spline basis functions should be monotonically increasing
  education_levels <- 7
  n_basis <- 4

  basis <- generate_ispline_basis(education_levels, n_basis = n_basis)

  # Each column (basis function) should be monotonically increasing
  for (j in 1:ncol(basis)) {
    basis_func <- basis[, j]
    # Check each successive value is >= previous
    diffs <- diff(basis_func)
    expect_true(all(diffs >= -1e-10),  # Allow small numerical error
                info = sprintf("Basis function %d not monotonic", j))
  }
})

test_that("fit_ode_state_space_monotonic_spline function exists and has correct signature", {
  # Function should exist
  expect_true(exists("fit_ode_state_space_monotonic_spline"))

  # Should be a function
  expect_true(is.function(fit_ode_state_space_monotonic_spline))

  # Check function arguments
  args <- names(formals(fit_ode_state_space_monotonic_spline))

  # Should have key arguments
  expect_true("data" %in% args)
  expect_true("education_order" %in% args)
  expect_true("n_ispline_basis" %in% args)
  expect_true("chains" %in% args)
})

test_that("monotonic spline Stan model file exists", {
  stan_file <- "../../stan/unemployment-ode-state-space-monotonic-spline.stan"
  expect_true(file.exists(stan_file))
})
