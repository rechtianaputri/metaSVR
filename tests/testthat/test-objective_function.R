library(testthat)
library(metaSVR)

y_true <- c(120, 210, 488)
y_pred <- c(131, 187, 512)

# Objective function specific call
test_that("Loss functions return expected types", {
  expect_type(smape(y_pred, y_true), "double")
  expect_type(mape(y_pred, y_true), "double")
  expect_type(mae(y_pred, y_true), "double")
  expect_type(rmse(y_pred, y_true), "double")
})

# Objective function general
test_that("loss_calculate works correctly", {
  expect_type(loss_calculate(y_pred, y_true, "RMSE"), "double")
  expect_error(loss_calculate(y_pred, y_true, "unknown_objective"))
})
