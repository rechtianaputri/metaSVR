get_default_bounds <- function() {
  lb <- c(2^0, 2^(-8), 2^(-8))
  ub <- c(2^10, 2^0, 2^0)
  list(lb = lb, ub = ub)
}
