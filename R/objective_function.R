loss_calculate <- function(preds, actuals, objective) {
  switch(objective,
         "SMAPE" = smape(preds, actuals),
         "MAPE" = mape(preds, actuals),
         "RMSE" = rmse(preds, actuals),
         "MAE" = mae(preds, actuals),
         stop("Fungsi objektif tidak valid")
  )
}

smape <- function(preds, actuals) {
  mean(2 * abs(preds - actuals) / (abs(preds) + abs(actuals))) * 100
}

mape <- function(preds, actuals) {
  mean(abs((actuals - preds) / actuals)) * 100
}

rmse <- function(preds, actuals) {
  sqrt(mean((preds - actuals)^2))
}

mae <- function(preds, actuals) {
  mean(abs(preds - actuals))
}

