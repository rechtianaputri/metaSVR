library(testthat)
library(metaSVR)

test_that("Loss functions return expected types", {
  y_true <- c(120, 210, 488)
  y_pred <- c(131, 187, 512)

  expect_type(smape(y_true, y_pred), "double")
  expect_type(mape(y_true, y_pred), "double")
  expect_type(mae(y_true, y_pred), "double")
  expect_type(rmse(y_true, y_pred), "double")
})
