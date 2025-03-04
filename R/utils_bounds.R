#' @export
get_default_bounds <- function() {
  lb <- c(2^0, 2^(-8), 2^(-8))
  ub <- c(2^10, 2^0, 2^0)
  dim <- 3
  list(lb = lb, ub = ub, dim=dim)
}
