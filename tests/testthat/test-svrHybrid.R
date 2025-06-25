library(testthat)
library(metaSVR)

test_that("svrHybrid returns expected structure without normalization", {
  x <- matrix(runif(50*8), ncol = 8)
  y <- runif(50)
  x_train <- x[1:35, ]
  y_train <- y[1:35]
  x_test <- x[36:50, ]
  y_test <- y[36:50]

  result <- svrHybrid(x_train, y_train, x_test, y_test,
                      optimizer = "CBO", kernel = "radial",
                      objective = "SMAPE",
                      is.y.normalize = FALSE,
                      max_iter = 100, N = 10)

  expect_type(result, "list")
  expect_true(all(c("best_params", "total_iter", "model", "time") %in% names(result)))
  expect_s3_class(result$model, "svm")
  expect_equal(ncol(result$model$SV), ncol(x_train))  # memastikan model SVR terbentuk
})

test_that("svrHybrid returns expected structure with normalization", {
  x <- matrix(runif(50*8), ncol = 8)
  y <- runif(50)
  x_norm <- normalize(x)
  y_norm <- normalize(y)
  min_y <- min(y)
  max_y <- max(y)

  x_train <- x_norm[1:35, ]
  y_train <- y_norm[1:35]
  x_test <- x_norm[36:50, ]
  y_test <- y_norm[36:50]

  result <- svrHybrid(x_train, y_train, x_test, y_test,
                      optimizer = "CBO", kernel = "radial",
                      objective = "SMAPE",
                      is.y.normalize = TRUE,
                      min_y, max_y,
                      max_iter = 100, N = 10)

  expect_type(result, "list")
  expect_true(all(c("best_params", "total_iter", "model", "time") %in% names(result)))
  expect_s3_class(result$model, "svm")
  expect_equal(ncol(result$model$SV), ncol(x_train))  # memastikan model SVR terbentuk
})
