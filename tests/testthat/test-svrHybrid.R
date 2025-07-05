library(testthat)
library(metaSVR)

# 1. svrHybrid Function Testing without Normalization
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

# 2. svrHybrid Function Testing with Normalization
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

# 3. x_train Input in svrHybrid Validation
test_that("Input x_train as a factor in svrHybrid is invalid", {
  set.seed(42)
  n <- 20
  x_train_bad <- data.frame(
    cat_var = factor(sample(c("A","B","C"), n, replace = TRUE)),
    num_var = runif(n)
  )
  y_train_bad <- rnorm(n)
  x_test_bad  <- x_train_bad
  y_test_bad  <- y_train_bad
  expect_error(
    svrHybrid(
      x_train = x_train_bad,
      y_train = y_train_bad,
      x_test  = x_test_bad,
      y_test  = y_test_bad,
      optimizer = "AO",
      kernel    = "radial",
      objective = "RMSE",
      is.y.normalize = FALSE
    )
  )
})

# 4. x_test Input in svrHybrid Validation
test_that("Input x_test as a factor in svrHybrid is invalid", {
  set.seed(42)
  n <- 20
  x_train_good <- data.frame(
    var1 = runif(n),
    var2 = rnorm(n)
  )
  y_train_good <- rnorm(n)
  x_test_bad <- data.frame(
    cat_var = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    num_var = runif(n)
  )
  y_test_bad <- rnorm(n)
  expect_error(
    svrHybrid(
      x_train = x_train_good,
      y_train = y_train_good,
      x_test  = x_test_bad,
      y_test  = y_test_bad,
      optimizer = "AO",
      kernel    = "radial",
      objective = "RMSE",
      is.y.normalize = FALSE
    )
  )
})

# 5. y_train Input in svrHybrid Validation
test_that("Input y_train in svrHybrid must be numeric", {
  set.seed(123)
  n <- 20
  x_train_good <- data.frame(
    var1 = runif(n),
    var2 = rnorm(n)
  )
  x_test_good <- data.frame(
    var1 = runif(n),
    var2 = rnorm(n)
  )
  y_train_bad <- factor(sample(c("low", "medium", "high"), n, replace = TRUE))
  y_test_good <- rnorm(n)
  expect_error(
    svrHybrid(
      x_train = x_train_good,
      y_train = y_train_bad,
      x_test  = x_test_good,
      y_test  = y_test_good,
      optimizer = "AO",
      kernel    = "radial",
      objective = "RMSE",
      is.y.normalize = FALSE
    )
  )
})

# 6. y_test Input in svrHybrid Validation
test_that("Input y_test in svrHybrid must be numeric", {
  set.seed(123)
  n <- 20
  x_train_good <- data.frame(
    var1 = runif(n),
    var2 = rnorm(n)
  )
  x_test_good <- data.frame(
    var1 = runif(n),
    var2 = rnorm(n)
  )
  y_test_bad <- factor(sample(c("low", "medium", "high"), n, replace = TRUE))
  y_train_good <- rnorm(n)
  expect_error(
    svrHybrid(
      x_train = x_train_good,
      y_train = y_train_good,
      x_test  = x_test_good,
      y_test  = y_test_bad,
      optimizer = "AO",
      kernel    = "radial",
      objective = "RMSE",
      is.y.normalize = FALSE
    )
  )
})

# 7. Input Data Training Length Validation
test_that("The lenght on x_train and y_train input in svrHybrid has to be SAME", {
  x_train_bad <- data.frame(
    var1 = runif(20),
    var2 = rnorm(20)
  )
  y_train_bad <- rnorm(15)
  x_test_good <- data.frame(
    var1 = runif(10),
    var2 = rnorm(10)
  )
  y_test_good <- rnorm(10)
  expect_error(
    svrHybrid(
      x_train = x_train_bad,
      y_train = y_train_bad,
      x_test  = x_test_good,
      y_test  = y_test_good,
      optimizer = "AO",
      kernel    = "radial",
      objective = "RMSE",
      is.y.normalize = FALSE
    )
  )
})

# 8. Input Data Testing Length Validation
test_that("The lenght on x_test and y_test input in svrHybrid has to be SAME", {
  x_train_good <- data.frame(
    var1 = runif(20),
    var2 = rnorm(20)
  )
  y_train_good <- rnorm(20)
  x_test_bad <- data.frame(
    var1 = runif(8),
    var2 = rnorm(8)
  )
  y_test_bad <- rnorm(5)
  expect_error(
    svrHybrid(
      x_train = x_train_good,
      y_train = y_train_good,
      x_test  = x_test_bad,
      y_test  = y_test_bad,
      optimizer = "AO",
      kernel    = "radial",
      objective = "RMSE",
      is.y.normalize = FALSE
    )
  )
})

# 9. Input x_train in Dataframe Validation
test_that("x_train input in svrHybrid must be matrix or dataframe", {
  y_train <- rnorm(10)
  y_test  <- rnorm(10)
  x_train_bad <- list(a = 1:10, b = 11:20)
  x_test_good <- matrix(rnorm(20), ncol = 2)
  expect_error(
    svrHybrid(
      x_train = x_train_bad,
      y_train = y_train,
      x_test  = x_test_good,
      y_test  = y_test,
      optimizer = "AO",
      kernel    = "radial",
      objective = "RMSE",
      is.y.normalize = FALSE
    )
  )
})

# 10. Input x_test in Dataframe Validation
test_that("x_test input in svrHybrid must be matrix or dataframe", {
  y_train <- rnorm(10)
  y_test  <- rnorm(10)
  x_test_bad <- list(a = 1:10, b = 11:20)
  x_train_good <- matrix(rnorm(20), ncol = 2)
  expect_error(
    svrHybrid(
      x_train = x_train_good,
      y_train = y_train,
      x_test  = x_test_bad,
      y_test  = y_test,
      optimizer = "AO",
      kernel    = "radial",
      objective = "RMSE",
      is.y.normalize = FALSE
    )
  )
})

# 11. Input Kernel in svrHybrid Testing
test_that("Incorrect kernel selection in svrHybrid", {
  set.seed(123)
  n_train <- 40
  n_test  <- 10
  n_feat  <- 3
  x_train <- data.frame(
    x1 = runif(n_train),
    x2 = rnorm(n_train),
    x3 = rnorm(n_train, mean = 5)
  )
  x_test <- data.frame(
    x1 = runif(n_test),
    x2 = rnorm(n_test),
    x3 = rnorm(n_test, mean = 5)
  )
  y_train <- x_train$x1 * 2 + x_train$x2 - x_train$x3 + rnorm(n_train)
  y_test  <- x_test$x1 * 2 + x_test$x2 - x_test$x3 + rnorm(n_test)
  expect_error(
    svrHybrid(
      x_train = x_train,
      y_train = y_train,
      x_test  = x_test,
      y_test  = y_test,
      optimizer = "AO",
      kernel    = "radialo",
      objective = "RMSE",
      is.y.normalize = FALSE
    )
  )
})

# 12. Input Optimizer in svrHybrid Testing
test_that("Incorrect optimizer selection in svrHybrid", {
  set.seed(123)
  n_train <- 40
  n_test  <- 10
  n_feat  <- 3
  x_train <- data.frame(
    x1 = runif(n_train),
    x2 = rnorm(n_train),
    x3 = rnorm(n_train, mean = 5)
  )
  x_test <- data.frame(
    x1 = runif(n_test),
    x2 = rnorm(n_test),
    x3 = rnorm(n_test, mean = 5)
  )
  y_train <- x_train$x1 * 2 + x_train$x2 - x_train$x3 + rnorm(n_train)
  y_test  <- x_test$x1 * 2 + x_test$x2 - x_test$x3 + rnorm(n_test)
  expect_error(
    svrHybrid(
      x_train = x_train,
      y_train = y_train,
      x_test  = x_test,
      y_test  = y_test,
      optimizer = "AOB",
      kernel    = "radial",
      objective = "RMSE",
      is.y.normalize = FALSE
    )
  )
})

# 13. Input Objective in svrHybrid Testing
test_that("Incorrect objective selection in svrHybrid", {
  set.seed(123)
  n_train <- 40
  n_test  <- 10
  n_feat  <- 3
  x_train <- data.frame(
    x1 = runif(n_train),
    x2 = rnorm(n_train),
    x3 = rnorm(n_train, mean = 5)
  )
  x_test <- data.frame(
    x1 = runif(n_test),
    x2 = rnorm(n_test),
    x3 = rnorm(n_test, mean = 5)
  )
  y_train <- x_train$x1 * 2 + x_train$x2 - x_train$x3 + rnorm(n_train)
  y_test  <- x_test$x1 * 2 + x_test$x2 - x_test$x3 + rnorm(n_test)
  expect_error(
    svrHybrid(
      x_train = x_train,
      y_train = y_train,
      x_test  = x_test,
      y_test  = y_test,
      optimizer = "AO",
      kernel    = "radial",
      objective = "RMSEP",
      is.y.normalize = FALSE
    )
  )
})

# 14. Validation for Input Maximum Iteration in svrHybrid Testing
test_that("Incorrect maximum iteration input on svrHybrid, must be positive", {
  set.seed(123)
  n_train <- 40
  n_test  <- 10
  n_feat  <- 3
  x_train <- data.frame(
    x1 = runif(n_train),
    x2 = rnorm(n_train),
    x3 = rnorm(n_train, mean = 5)
  )
  x_test <- data.frame(
    x1 = runif(n_test),
    x2 = rnorm(n_test),
    x3 = rnorm(n_test, mean = 5)
  )
  y_train <- x_train$x1 * 2 + x_train$x2 - x_train$x3 + rnorm(n_train)
  y_test  <- x_test$x1 * 2 + x_test$x2 - x_test$x3 + rnorm(n_test)
  expect_error(
    svrHybrid(
      x_train = x_train,
      y_train = y_train,
      x_test  = x_test,
      y_test  = y_test,
      optimizer = "AO",
      kernel    = "radial",
      objective = "RMSE",
      is.y.normalize = FALSE,
      max_iter = 0,
      N = 10
    )
  )
})

# 15. Validation for Input Population (N) in svrHybrid Testing
test_that("Incorrect population (N) input on svrHybrid, must be positive", {
  set.seed(123)
  n_train <- 40
  n_test  <- 10
  n_feat  <- 3
  x_train <- data.frame(
    x1 = runif(n_train),
    x2 = rnorm(n_train),
    x3 = rnorm(n_train, mean = 5)
  )
  x_test <- data.frame(
    x1 = runif(n_test),
    x2 = rnorm(n_test),
    x3 = rnorm(n_test, mean = 5)
  )
  y_train <- x_train$x1 * 2 + x_train$x2 - x_train$x3 + rnorm(n_train)
  y_test  <- x_test$x1 * 2 + x_test$x2 - x_test$x3 + rnorm(n_test)
  expect_error(
    svrHybrid(
      x_train = x_train,
      y_train = y_train,
      x_test  = x_test,
      y_test  = y_test,
      optimizer = "AO",
      kernel    = "radial",
      objective = "RMSE",
      is.y.normalize = FALSE,
      max_iter = 100,
      N = -2
    )
  )
})
