#' @importFrom stats na.omit
#' @importFrom e1071 svm
#' @importFrom stats predict
#' @importFrom hms as_hms

#' @export
svrHybrid <- function(x_train, y_train,
                      x_test, y_test,
                      kernel = "radial", optimizer = "AO",
                      objective = "RMSE", is.y.normalize = FALSE,
                      min.y = min.y, max.y = max.y,
                      max_iter = 100, N = 30,
                      seed = 123, degree = 3, coef0 = 0,
                      nu = 0.5, class.weights = NULL,
                      cachesize = 40, tolerance = 0.001,
                      scale = TRUE, shrinking = TRUE,
                      cross = 0, probability = FALSE,
                      fitted = TRUE, ..., subset,
                      na.action = na.omit) {

  start <- Sys.time()

  set.seed(seed)

  bounds <- get_default_bounds()
  lower_bound <- matrix(bounds$lb, nrow = 1)
  upper_bound <- matrix(bounds$ub, nrow = 1)
  dimension <- bounds$dim

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
                           "AOCBO" = AOCBO,
                           "HHO" = HHO,
                           "GWO" = GWO,
                           "ALO" = ALO,
                           "EHHOCBO" = EHHOCBO)

  result <- optimizer_func(N = N, Max_iter = max_iter, lb=lower_bound, ub = upper_bound,
                           dim = dimension, fobj=fun)

  best_params <- list(cost = result$best_position[1],
                      gamma = result$best_position[2],
                      epsilon = result$best_position[3])

  # after best_params earned
  # build the final SVR
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

  finish <- Sys.time()

  # Return result
  list(
    best_params = best_params,
    total_iter = result$jml_iter,
    iter_history = result$iter_history,
    model = svr_final,
    time = as_hms(difftime(finish,start))
  )
}
