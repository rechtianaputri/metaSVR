normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

denormalize <- function(x,min,max){
  return((x * (max-min)) + min)
}
