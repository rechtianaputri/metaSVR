library(testthat)
library(metaSVR)

# Initialize The Data
y_true <- c(120, 210, 488)
y_pred <- c(131, 187, 512)

# 1. Testing Objective Function for Specific Call
test_that("Loss functions return expected types", {
  expect_type(smape(y_pred, y_true), "double")
  expect_type(mape(y_pred, y_true), "double")
  expect_type(mae(y_pred, y_true), "double")
  expect_type(rmse(y_pred, y_true), "double")
})

# 2. Testing Objective Function Used in General
test_that("loss_calculate works correctly", {
  expect_type(loss_calculate(y_pred, y_true, "RMSE"), "double")
})

# 3. Loss Calculation Validation Testing
test_that("loss_calculate throws error for non-numeric preds", {
  expect_error(loss_calculate(preds = "a", actuals = c(1, 2, 3), objective = "MAE"),
               "'preds' and 'actuals' must be numeric vectors.")
})
test_that("loss_calculate throws error for non-numeric actuals", {
  expect_error(loss_calculate(preds = c(1, 2, 3), actuals = "b", objective = "MAE"),
               "'preds' and 'actuals' must be numeric vectors.")
})
test_that("loss_calculate throws error for different length preds and actuals", {
  expect_error(loss_calculate(preds = c(1, 2), actuals = c(1, 2, 3), objective = "MAE"),
               "'preds' and 'actuals' must have the same length.")
})
test_that("loss_calculate throws error for non-character objective", {
  expect_error(loss_calculate(preds = c(1, 2, 3), actuals = c(1, 2, 3), objective = 123),
               "'objective' must be a single character string.")
})
test_that("loss_calculate throws error for invalid objective name", {
  expect_error(loss_calculate(y_pred, y_true, "unknown_objective"),
               "Objective must be one of: SMAPE, MAPE, RMSE, or MAE.")
})

# 4. SMAPE Validation Testing
test_that("SMAPE throws error for invalid input", {
  expect_error(smape(preds = c("a", "b"), actuals = c(1, 2)),
               "'preds' and 'actuals' must be numeric vectors.")
  expect_error(smape(preds = c(1, 2, 3), actuals = c(1, 2)),
               "'preds' and 'actuals' must have the same length.")
})

# 5. MAPE Validation Testing
test_that("MAPE throws error for invalid input", {
  expect_error(mape(preds = c("a", "b"), actuals = c(1, 2)),
               "'preds' and 'actuals' must be numeric vectors.")
  expect_error(mape(preds = c(1, 2, 3), actuals = c(1, 2)),
               "'preds' and 'actuals' must have the same length.")
})
test_that("MAPE throws error when actuals contain zero", {
  expect_error(mape(preds = c(10, 20, 30), actuals = c(10, 0, 30)),
               "MAPE is undefined when actual values contain zero.")
})

# 6. RMSE Validation Testing
test_that("RMSE throws error for invalid input", {
  expect_error(rmse(preds = "x", actuals = c(1, 2)),
               "'preds' and 'actuals' must be numeric vectors.")
  expect_error(rmse(preds = c(1, 2), actuals = c(1, 2, 3)),
               "'preds' and 'actuals' must have the same length.")
})

# 7. MAE Validation Testing
test_that("MAE throws error for invalid input", {
  expect_error(mae(preds = c(NA, NA), actuals = c(1, 2)),
               "'preds' and 'actuals' must be numeric vectors.")
  expect_error(mae(preds = c(1, 2, 3), actuals = c(1, 2)),
               "'preds' and 'actuals' must have the same length.")
})
