library(testthat)
library(metaSVR)

# Dummy predicted and actual values for objective function
actuals <- runif(40, min = 10, max = 1000)
preds_gen <- function(params) {
  # Prediction simulation based on parameter
  actuals + sin(seq_along(actuals)) * params[1] - log(1 + abs(params[2])) + params[3] * 0.1
}

test_that("ALO optimizer minimizes MAE loss", {
  set.seed(123)

  objective_function <- function(params) {
    preds <- preds_gen(params)
    loss_calculate(preds, actuals, "MAE")
  }

  bounds <- get_default_bounds()

  result <- ALO(N = 10, Max_iter = 30,
                lb = bounds$lb, ub = bounds$ub, dim = bounds$dim,
                fobj = objective_function)

  expect_type(result, "list")
  expect_true(all(c("best_fitness", "best_position", "jml_iter", "param", "param_list") %in% names(result)))
  expect_length(result$best_position, bounds$dim)
  expect_true(is.numeric(result$best_fitness))
})

test_that("AO optimizer minimizes RMSE loss", {
  set.seed(123)

  # Fungsi objektif yang akan diminimasi
  objective_function <- function(params) {
    preds <- preds_gen(params)
    loss_calculate(preds, actuals, "RMSE")
  }

  bounds <- get_default_bounds()

  result <- AO(N = 10, Max_iter = 30,
               lb = bounds$lb, ub = bounds$ub, dim = bounds$dim,
               fobj = objective_function)

  expect_type(result, "list")
  expect_true(all(c("best_fitness", "best_position", "jml_iter", "param", "param_list") %in% names(result)))
  expect_length(result$best_position, bounds$dim)
  expect_true(is.numeric(result$best_fitness))
})

test_that("AOCBO optimizer minimizes SMAPE loss", {
  set.seed(123)

  # Fungsi objektif yang akan diminimasi
  objective_function <- function(params) {
    preds <- preds_gen(params)
    loss_calculate(preds, actuals, "SMAPE")
  }

  bounds <- get_default_bounds()

  result <- AOCBO(N = 10, Max_iter = 30,
               lb = bounds$lb, ub = bounds$ub, dim = bounds$dim,
               fobj = objective_function)

  expect_type(result, "list")
  expect_true(all(c("best_fitness", "best_position", "jml_iter", "param", "param_list") %in% names(result)))
  expect_length(result$best_position, bounds$dim)
  expect_true(is.numeric(result$best_fitness))
})

test_that("CBO optimizer minimizes MAPE loss", {
  set.seed(123)

  # Fungsi objektif yang akan diminimasi
  objective_function <- function(params) {
    preds <- preds_gen(params)
    loss_calculate(preds, actuals, "MAPE")
  }

  bounds <- get_default_bounds()

  result <- CBO(N = 10, Max_iter = 30,
                  lb = bounds$lb, ub = bounds$ub, dim = bounds$dim,
                  fobj = objective_function)

  expect_type(result, "list")
  expect_true(all(c("best_fitness", "best_position", "jml_iter", "param", "param_list") %in% names(result)))
  expect_length(result$best_position, bounds$dim)
  expect_true(is.numeric(result$best_fitness))
})

test_that("EHHOCBO optimizer minimizes MAE loss", {
  set.seed(123)

  objective_function <- function(params) {
    preds <- preds_gen(params)
    loss_calculate(preds, actuals, "MAE")
  }

  bounds <- get_default_bounds()

  result <- EHHOCBO(N = 10, Max_iter = 30,
                    lb = bounds$lb, ub = bounds$ub, dim = bounds$dim,
                    fobj = objective_function)

  expect_type(result, "list")
  expect_true(all(c("best_fitness", "best_position", "jml_iter", "param", "param_list") %in% names(result)))
  expect_length(result$best_position, bounds$dim)
  expect_true(is.numeric(result$best_fitness))
})

test_that("GWO optimizer minimizes RMSE loss", {
  set.seed(123)

  # Fungsi objektif yang akan diminimasi
  objective_function <- function(params) {
    preds <- preds_gen(params)
    loss_calculate(preds, actuals, "RMSE")
  }

  bounds <- get_default_bounds()

  result <- GWO(N = 10, Max_iter = 30,
                lb = bounds$lb, ub = bounds$ub, dim = bounds$dim,
                fobj = objective_function)

  expect_type(result, "list")
  expect_true(all(c("best_fitness", "best_position", "jml_iter", "param", "param_list") %in% names(result)))
  expect_length(result$best_position, bounds$dim)
  expect_true(is.numeric(result$best_fitness))
})

test_that("HHO optimizer minimizes SMAPE loss", {
  set.seed(123)

  # Fungsi objektif yang akan diminimasi
  objective_function <- function(params) {
    preds <- preds_gen(params)
    loss_calculate(preds, actuals, "SMAPE")
  }

  bounds <- get_default_bounds()

  result <- HHO(N = 10, Max_iter = 30,
                lb = bounds$lb, ub = bounds$ub, dim = bounds$dim,
                fobj = objective_function)

  expect_type(result, "list")
  expect_true(all(c("best_fitness", "best_position", "jml_iter", "param", "param_list") %in% names(result)))
  expect_length(result$best_position, bounds$dim)
  expect_true(is.numeric(result$best_fitness))
})
