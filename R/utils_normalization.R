#' Normalize
#'
#' Normalize data using min-max scale.
#'
#' @param x is a predictor variable that is a numeric vector to be normalized.
#'
#' @return A numeric vector scaled between 0 and 1.
#'
#' @examples
#' # Normalize example use:
#' data <- c(10, 20, 30, 40, 50)
#' normalize(data)
#'
#'@export
normalize <- function(x) {
  # Input Validation
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector.")
  }
  if (length(x) < 2) {
    stop("'x' must contain at least two values to be normalized.")
  }
  if (all(x == x[1])) {
    stop("Cannot normalize a constant vector; all values are equal.")
  }

  return((x - min(x)) / (max(x) - min(x)))
}

#' Denormalize
#'
#' Convert normalized data back to original scale using given min and max.
#'
#' @param x Numeric vector that has been normalized (values is between 0 and 1).
#' @param min The minimum value of the original data.
#' @param max The maximum value of the original data.
#'
#' @return A numeric vector already converted to original scale.
#'
#' @examples
#' # Example of the original data
#' original_data <- c(10, 20, 30, 40, 50)
#'
#' # Data being normalized
#' normalized_data <- (original_data - min(original_data)) /
#'                    (max(original_data) - min(original_data))
#'
#' # Denormalization function use to change value to the original
#' denormalize(normalized_data, min(original_data), max(original_data))
#'
#' @export
denormalize <- function(x,min,max){
  # Input Validation
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector.")
  }
  if (!is.numeric(min) || length(min) != 1) {
    stop("'min' must be a single numeric value.")
  }
  if (!is.numeric(max) || length(max) != 1) {
    stop("'max' must be a single numeric value.")
  }
  if (min >= max) {
    stop("'min' must be strictly less than 'max'.")
  }
  tolerance <- 1e-8
  if (any(x < 0 - tolerance | x > 1 + tolerance)) {
    warning("Some values in 'x' are outside [0, 1]; results may be invalid denormalization.")
  }

  return((x * (max-min)) + min)
}
