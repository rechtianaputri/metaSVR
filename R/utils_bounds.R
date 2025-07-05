#' Default Bounds Initialization for SVR Optimization
#'
#' This function return the default value of lower and upper bounds also the dimension for SVR optimization.
#' The three dimensions represent as the parameter that need to be optimized in SVR with exact range of bound.
#' Three dimension and the range represent as: Cost (C): 2^0 to 2^10; Gamma: 2^(-8) to 2^0; Epsilon: 2^(-8) to 2^0.
#'
#' @return A list containing:
#' \describe{
#'   \item{lb}{A numeric vector of lower bounds.}
#'   \item{ub}{A numeric vector of upper bounds.}
#'   \item{dim}{An integer representing the number of dimensions, 3.}
#' }
#'
#' @note The bounds for parameters search space is based on previous research with range:  Cost=[2^0,2^10], Gamma=[2^(-8),2^0], dan Epsilon=[2^(-8),2^0]
#'
#' @references
#' Liu, H.-H., Chang, L.-C., Li, C.-W., & Yang, C.-H. (2018). Particle Swarm Optimization-Based Support Vector Regression for Tourist Arrivals Forecasting. Computational Intelligence and Neuroscience, 2018, 1–13. https://doi.org/10.1155/2018/6076475.
#'
#' @export
#'
#' @examples
#' bounds <- get_default_bounds()
#' bounds$lb  # Lower bounds
#' bounds$ub  # Upper bounds
#' bounds$dim # Number of parameters
#'
get_default_bounds <- function() {
  lb <- c(2^0, 2^(-8), 2^(-8))
  ub <- c(2^10, 2^0, 2^0)
  dim <- 3
  list(lb = lb, ub = ub, dim=dim)
}
