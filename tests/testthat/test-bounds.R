library(testthat)
library(metaSVR)

# 1. Test Expected Result of Bounds
test_that("get_default_bounds returns correct structure and values", {
  bounds <- get_default_bounds()
  expect_type(bounds, "list")
  expect_named(bounds, c("lb", "ub", "dim"))
  expect_equal(bounds$dim, 3)
  expected_lb <- c(2^0, 2^(-8), 2^(-8))
  expected_ub <- c(2^10, 2^0, 2^0)
  expect_equal(bounds$lb, expected_lb)
  expect_equal(bounds$ub, expected_ub)
  expect_length(bounds$lb, bounds$dim)
  expect_length(bounds$ub, bounds$dim)
})
