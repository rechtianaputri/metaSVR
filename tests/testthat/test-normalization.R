library(testthat)
library(metaSVR)

test_that("normalize and denormalize preserve values", {
  x <- c(15, 35, 45, 65, 5, 75)
  norm_x <- normalize(x)
  denorm_x <- denormalize(norm_x, min(x), max(x))

  expect_equal(round(denorm_x, 6), x)
  expect_true(all(norm_x >= 0 & norm_x <= 1))
})
