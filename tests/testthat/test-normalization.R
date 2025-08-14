library(testthat)
library(metaSVR)

# 1. Testing Normalize and Denormalize Expected Result
test_that("Mormalize and denormalize preserve values", {
  x <- c(15, 35, 45, 65, 5, 75)
  norm_x <- normalize(x)
  denorm_x <- denormalize(norm_x, min(x), max(x))

  expect_equal(round(denorm_x, 6), x)
  expect_true(all(norm_x >= 0 & norm_x <= 1))
})

# 2. Testing Normalize Input Validation Test
test_that("Normalize throws error if input is not numeric", {
  expect_error(normalize("a"), "'x' must be a numeric vector.")
})
test_that("Normalize throws error if input has less than 2 elements", {
  expect_error(normalize(5), "'x' must contain at least two values to be normalized.")
})
test_that("Normalize throws error if all elements are the same", {
  expect_error(normalize(c(3, 3, 3)), "Cannot normalize a constant vector")
})

# 3. Testing Denormalize Input Validation Test
test_that("Denormalize throws error if x is not numeric", {
  expect_error(denormalize("a", min = 0, max = 1), "'x' must be a numeric vector.")
})
test_that("Denormalize throws error if min is not numeric scalar", {
  expect_error(denormalize(c(0.1, 0.5), min = "low", max = 10), "'min' must be a single numeric value.")
  expect_error(denormalize(c(0.1, 0.5), min = c(1, 2), max = 10), "'min' must be a single numeric value.")
})
test_that("Denormalize throws error if max is not numeric scalar", {
  expect_error(denormalize(c(0.1, 0.5), min = 0, max = "high"), "'max' must be a single numeric value.")
  expect_error(denormalize(c(0.1, 0.5), min = 0, max = c(10, 20)), "'max' must be a single numeric value.")
})
test_that("Denormalize throws error if min >= max", {
  expect_error(denormalize(c(0.1, 0.5), min = 10, max = 5), "'min' must be strictly less than 'max'.")
})
