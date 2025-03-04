#' @importFrom stats na.omit
#' @importFrom e1071 svm
#' @importFrom stats predict

#' @export
svrHybrid <- function(x_train, y_train,
                      x_test, y_test,
                      kernel = "radial", optimizer = "AO",
                      objective = "RMSE", is.y.normalize = TRUE,
                      min.y = min.y, max.y = max.y,
                      max_iter = 100, N = 30,
                      seed = 123, degree = 3, coef0 = 0,
                      nu = 0.5, class.weights = NULL,
                      cachesize = 40, tolerance = 0.001,
                      scale = TRUE, shrinking = TRUE,
                      cross = 0, probability = FALSE,
                      fitted = TRUE, ..., subset,
                      na.action = na.omit) {

  set.seed(seed)

  bounds <- get_default_bounds()
  batas_bawah <- matrix(bounds$lb, nrow = 1)
  batas_atas <- matrix(bounds$ub, nrow = 1)
  dimensi <- bounds$dim

  fun <- function(params) {
    cost <- params[1]
    gamma <- params[2]
    epsilon <- params[3]

    svr_model <- svm(x = x_train, y = y_train, type = "eps-regression",
                     kernel = kernel, cost = cost, gamma = gamma,
                     epsilon = epsilon)

    preds <- predict(svr_model, x_test)

    if (is.y.normalize == TRUE) {
      preds <- denormalize(preds, min.y, max.y)
      actuals <- denormalize(y_test, min.y, max.y)
    } else {
      actuals <- y_test
    }

    loss_calculate(actuals, preds, objective)
  }

  optimizer_func <- switch(optimizer,
                           "AO" = AO,
                           "CBO" = CBO,
                           "AOCBO" = AOCBO)

  result <- optimizer_func(N = N, Max_iter = max_iter, lb=batas_bawah, ub = batas_atas,
                           dim = dimensi, fobj=fun)

  best_params <- list(cost = result$best_position[1],
                      gamma = result$best_position[2],
                      epsilon = result$best_position[3])

  # Setelah best_params didapat
  # Bangun SVR final
  svr_final <- svm(
    x = x_train,
    y = y_train,
    type = "eps-regression",
    kernel = kernel,
    cost = best_params$cost,
    gamma = best_params$gamma,
    epsilon = best_params$epsilon,
    degree = degree, coef0 = coef0,
    nu = nu, class.weights = class.weights,
    cachesize = cachesize, tolerance = tolerance,
    shrinking = shrinking, cross = cross,
    probability = probability, fitted = fitted,
    subset = subset, na.action = na.action
  )

  # Return hasil
  list(
    best_params = best_params,
    total_iter = result$jml_iter,
    iter_history = result$iter_history,
    model = svr_final
  )
}
