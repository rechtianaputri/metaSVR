#' Hybrid SVR Optimization with Metaheuristic Algorithms
#'
#' Trains a Support vector Regression Model by optimizing its parameter (Cost, Gamma, and Epsilon)
#' using Metaheuristic Algorithms such as: Archimedes Optimization (AO), Coot Bird Optimization (CBO),
#' Combined Archimedes Optimization with Coot Bird Optimization (AOCBO),
#' Harris Hawks Optimization (HHO), Grey Wolf Optimizer (GWO), Ant Lion Optimization (ALO),
#' and Enhanced Harris Hawks Optimization with Coot Bird Optimization (EHHOCBO).
#'
#' @importFrom stats na.omit
#' @importFrom e1071 svm
#' @importFrom stats predict
#' @importFrom hms as_hms

#' @param x_train A matrix or data frame contain predictors variable for training the model.
#' @param y_train A numeric vector of target values for training model.
#' @param x_test A matrix or data frame contain predictors variable for testing the model. It can be replaced by
#' data validation to get the parameter if you separated the data as three categories.
#' @param y_test A numeric vector of target values for training model. It can be replaced by
#' data validation to get the parameter if you separated the data as three categories.
#' @param kernel SVR kernel type used for modelling. Options: "radial", "polynomial", and "sigmoid". Default is radial.
#' @param optimizer Metaheuristic Algorithms selection, such as: "AO", "CBO", "AOCBO", "HHO", "GWO", "ALO", and "EHHOCBO". Default is AO.
#' @param objective Objective function used for optimization as prediction quality measures. Options: "SMAPE", "MAPE", "RMSE", and "MAE". Default is RMSE.
#' @param is.y.normalize Logical; use when prediction of target variable 'y' is on min-max scalling normalization. Default is FALSE.
#' @param min.y Minimum value of target (used for denormalization).
#' @param max.y Maximum value of target (used for denormalization).
#' @param max_iter Maximum number of iterations for the optimizer. Default is 100.
#' @param N Population size for the optimizer. Default is 30.
#' @param seed Random seed for reproducibility. Default is 123.
#' @param degree Degree parameter for polynomial kernel.
#' @param coef0 Coefficient parameter used in polynomial/sigmoid kernels.
#' @param nu Parameter for 'nu-regression' to controlling max proportion of error training and minimum proportion of support vectors. Default is 0.5, range: 0.1-0.9.
#' Only use if the type of regression choosen is 'nu-regression'.
#' @param class.weights A named list of class weights.
#' @param cachesize Size of kernel cache (in MB). Default is 40.
#' @param tolerance Tolerance of termination criterion.
#' @param scale Logical; whether to scale inputs. Default is TRUE.
#' @param shrinking Logical; whether to use shrinking heuristics. Default is TRUE.
#' @param cross Number of folds for cross-validation. Default is 0, no cross validation.
#' @param probability Logical; whether to enable probability model. Default is FALSE.
#' @param fitted Logical; whether to keep fitted values. Default is TRUE.
#' @param ... Additional arguments passed to `svm()`.
#' @param subset Optional vector specifying subset of observations to be used in the training fit.
#' @param na.action Function which indicates what should happen when the data contain NAs.
#'
#' @return A list containing:
#' \describe{
#'   \item{best_params}{A list with the best values for `cost`, `gamma`, and `epsilon`.}
#'   \item{total_iter}{Total number of iterations run by the optimizer.}
#'   \item{model}{The final trained SVR model (using `e1071::svm`).}
#'   \item{time}{Total training time in HMS format.}
#' }
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' x <- matrix(rnorm(100), ncol = 2)
#' y <- x[,1] * 3 + rnorm(50)
#' model <- svrHybrid(x_train = x[1:40,], y_train = y[1:40],
#'                    x_test = x[41:50,], y_test = y[41:50],
#'                    kernel = "radial", optimizer = "AO",
#'                    objective = "RMSE", is.y.normalize = FALSE)
#' model$best_params
#' }
#'
#' @export
#'
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

  if (length(result$best_position) != 3) {
    stop("Optimasi gagal: panjang best_position tidak sama dengan 3. Saat ini: ",
         length(result$best_position))
  }

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
    model = svr_final,
    time = as_hms(difftime(finish,start))
  )
}
