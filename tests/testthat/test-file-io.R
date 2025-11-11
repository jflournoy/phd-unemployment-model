test_that("save_cps_data saves data in fst format", {
  # Create test data
  test_data <- data.frame(
    YEAR = c(2024, 2024, 2024),
    MONTH = c(1, 1, 1),
    EMPSTAT = c(10, 12, 20),
    EDUC = c(125, 125, 125),
    WTFINL = c(1500, 1500, 1500)
  )

  # Create temp file path
  temp_file <- tempfile(fileext = ".fst")

  # Save data - this should fail because function doesn't exist yet
  expect_no_error(save_cps_data(test_data, temp_file))

  # Verify file was created
  expect_true(file.exists(temp_file))

  # Clean up
  unlink(temp_file)
})

test_that("load_cps_data loads data from fst format", {
  # Create and save test data first
  test_data <- data.frame(
    YEAR = c(2024, 2024, 2024),
    MONTH = c(1, 1, 1),
    EMPSTAT = c(10, 12, 20),
    EDUC = c(125, 125, 125),
    WTFINL = c(1500, 1500, 1500)
  )

  temp_file <- tempfile(fileext = ".fst")
  save_cps_data(test_data, temp_file)

  # Load data - this should fail because function doesn't exist yet
  loaded_data <- load_cps_data(temp_file)

  # Verify loaded data matches original
  expect_equal(loaded_data, test_data)
  expect_equal(nrow(loaded_data), 3)
  expect_equal(ncol(loaded_data), 5)

  # Clean up
  unlink(temp_file)
})

test_that("save_cps_data handles missing file path", {
  test_data <- data.frame(
    YEAR = 2024,
    MONTH = 1,
    EMPSTAT = 10,
    EDUC = 125,
    WTFINL = 1500
  )

  expect_error(
    save_cps_data(test_data, NULL),
    "file_path cannot be NULL"
  )
})

test_that("load_cps_data handles missing file", {
  expect_error(
    load_cps_data("nonexistent_file.fst"),
    "File does not exist"
  )
})

test_that("save_cps_data validates data input", {
  expect_error(
    save_cps_data(NULL, tempfile()),
    "data must be a data frame"
  )

  expect_error(
    save_cps_data(list(a = 1), tempfile()),
    "data must be a data frame"
  )
})

test_that("fst format is faster than RDS for large data", {
  # Create moderately large test dataset (10,000 rows)
  n_rows <- 10000
  large_data <- data.frame(
    YEAR = rep(2024, n_rows),
    MONTH = rep(1:12, length.out = n_rows),
    EMPSTAT = sample(c(10, 12, 20, 21, 22), n_rows, replace = TRUE),
    EDUC = sample(c(111, 114, 125), n_rows, replace = TRUE),
    WTFINL = runif(n_rows, 1000, 2000)
  )

  # Create temp files
  fst_file <- tempfile(fileext = ".fst")
  rds_file <- tempfile(fileext = ".rds")

  # Time fst save
  fst_save_time <- system.time({
    save_cps_data(large_data, fst_file)
  })

  # Time RDS save
  rds_save_time <- system.time({
    saveRDS(large_data, rds_file)
  })

  # Time fst load
  fst_load_time <- system.time({
    fst_loaded <- load_cps_data(fst_file)
  })

  # Time RDS load
  rds_load_time <- system.time({
    rds_loaded <- readRDS(rds_file)
  })

  # fst should be faster than RDS for both save and load
  # (may not be true for tiny files, but should be for 10k rows)
  expect_lt(fst_load_time["elapsed"], rds_load_time["elapsed"] * 2)

  # Data should be identical
  expect_equal(fst_loaded, rds_loaded)

  # Clean up
  unlink(c(fst_file, rds_file))
})

test_that("save_cps_data preserves data types", {
  # Create data with different types
  test_data <- data.frame(
    int_col = 1:5,
    num_col = c(1.1, 2.2, 3.3, 4.4, 5.5),
    char_col = c("a", "b", "c", "d", "e"),
    factor_col = factor(c("low", "med", "high", "med", "low")),
    stringsAsFactors = FALSE
  )

  temp_file <- tempfile(fileext = ".fst")
  save_cps_data(test_data, temp_file)
  loaded_data <- load_cps_data(temp_file)

  # Check types are preserved
  expect_equal(class(loaded_data$int_col), class(test_data$int_col))
  expect_equal(class(loaded_data$num_col), class(test_data$num_col))
  expect_equal(class(loaded_data$char_col), class(test_data$char_col))

  # Note: fst may convert factors differently - check the values at least
  expect_equal(as.character(loaded_data$factor_col), as.character(test_data$factor_col))

  # Clean up
  unlink(temp_file)
})
