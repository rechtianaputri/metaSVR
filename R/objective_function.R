#' Calculate Loss Based on Selected Objective Function
#'
#' Compute the loss between predictive and actual values using a selected objective function.
#' Supported objecive functions used in this functions are: "SMAPE', "MAPE", "RMSE", and "MAE".
#'
#' @param preds A numeric vector of predicted values.
#' @param actuals A numeric vector of actual (true) values.
#' @param objective A string character that indicates the loss function type: "SMAPE", "MAPE", "RMSE", or "MAE".
#'
#' @return A numeric value that represent the computed loss.
#'
#' @examples
#' preds <- c(80, 120, 180)
#' actuals <- c(95, 115, 177)
#' loss_calculate(preds, actuals, "RMSE")
#'
#' @export
loss_calculate <- function(preds, actuals, objective) {
  # Input validation
  if (!is.numeric(preds) || !is.numeric(actuals)) {
    stop("'preds' and 'actuals' must be numeric vectors.")
  }
  if (length(preds) != length(actuals)) {
    stop("'preds' and 'actuals' must have the same length.")
  }
  if (!is.character(objective) || length(objective) != 1) {
    stop("'objective' must be a single character string.")
  }

  switch(objective,
         "SMAPE" = smape(preds, actuals),
         "MAPE" = mape(preds, actuals),
         "RMSE" = rmse(preds, actuals),
         "MAE" = mae(preds, actuals),
         stop("Invalid objective function! Objective must be one of: SMAPE, MAPE, RMSE, or MAE.")
  )
}

#' Symmetric Mean Absolute Percentage Error
#'
#' Calculate the SMAPE value between predicted and actual values.
#'
#' @param preds A numeric vector of predicted values.
#' @param actuals A numeric vector of actual (true) values.
#'
#' @return SMAPE value (percentage).
#'
#' @examples
#' preds <- c(80, 120, 180)
#' actuals <- c(95, 115, 177)
#' smape(preds, actuals)
#'
#' @export
smape <- function(preds, actuals) {
  # Input Validation
  if (!is.numeric(preds) || !is.numeric(actuals)) {
    stop("'preds' and 'actuals' must be numeric vectors.")
  }
  if (length(preds) != length(actuals)) {
    stop("'preds' and 'actuals' must have the same length.")
  }

  mean(2 * abs(preds - actuals) / (abs(preds) + abs(actuals))) * 100
}


#' Mean Absolute Percentage Error
#'
#' Calculate the MAPE value between predicted and actual values.
#' Can't be used if the actual values contain 0 value.
#'
#' @param preds A numeric vector of predicted values.
#' @param actuals A numeric vector of actual (true) values.
#'
#' @return MAPE value (percentage).
#'
#' @examples
#' preds <- c(80, 120, 180)
#' actuals <- c(95, 115, 177)
#' mape(preds, actuals)
#'
#' @export
mape <- function(preds, actuals) {
  # Input Validation
  if (!is.numeric(preds) || !is.numeric(actuals)) {
    stop("'preds' and 'actuals' must be numeric vectors.")
  }
  if (length(preds) != length(actuals)) {
    stop("'preds' and 'actuals' must have the same length.")
  }
  if (any(actuals == 0)) {
    stop("MAPE is undefined when actual values contain zero.")
  }

  mean(abs((actuals - preds) / actuals)) * 100
}

#' Root Mean Squared Error
#'
#' Calculate the RMSE value between predicted and actual values.
#'
#' @param preds A numeric vector of predicted values.
#' @param actuals A numeric vector of actual (true) values.
#'
#' @return RMSE value.
#'
#' @examples
#' preds <- c(80, 120, 180)
#' actuals <- c(95, 115, 177)
#' rmse(preds, actuals)
#'
#' @export
rmse <- function(preds, actuals) {
  # Input Validation
  if (!is.numeric(preds) || !is.numeric(actuals)) {
    stop("'preds' and 'actuals' must be numeric vectors.")
  }
  if (length(preds) != length(actuals)) {
    stop("'preds' and 'actuals' must have the same length.")
  }

  sqrt(mean((preds - actuals)^2))
}

#' Mean Absolute Error
#'
#' Calculate the RMSE value between predicted and actual values.
#'
#' @param preds A numeric vector of predicted values.
#' @param actuals A numeric vector of actual (true) values.
#'
#' @return MAE value.
#'
#' @examples
#' preds <- c(80, 120, 180)
#' actuals <- c(95, 115, 177)
#' mae(preds, actuals)
#'
#' @export
mae <- function(preds, actuals) {
  # Input Validation
  if (!is.numeric(preds) || !is.numeric(actuals)) {
    stop("'preds' and 'actuals' must be numeric vectors.")
  }
  if (length(preds) != length(actuals)) {
    stop("'preds' and 'actuals' must have the same length.")
  }

  mean(abs(preds - actuals))
}

