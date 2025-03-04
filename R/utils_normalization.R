#' @export
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#' @export
denormalize <- function(x,min,max){
  return((x * (max-min)) + min)
}
