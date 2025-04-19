library(readxl)

test_that("svrHybrid runs correctly with external data", {
  # dataset path that related to the package
  data_path <- system.file("extdata", "Data_TH_solar_valid.xlsx",
                           package = "metaSVR")
  expect_true(file.exists(data_path))  # make sure file exist

  # Load data
  data <- as.data.frame(read_excel(data_path))

  data$lag1 <- c(NA,data$Solar[-length(data$Solar)])

  x_data <- dplyr::select(data, -Country, -Year, -Solar)
  y_data <- data$Solar

  # Delete first row of x_data and y_data
  x_data <- x_data[-1, ]
  y_data <- y_data[-1]

  # List of column that aren't normalize
  columns_to_exclude <- c("Share_Electricity", "RE_Growth", "RE_Consumption",
                          "Elec_Access", "ElecShare_Primary", "GDP_Growth",
                          "Inflation", "Forest_Area", "Pop_Growth",
                          "Urbanization", "Employment")

  # seperate column to normalize
  columns_to_normalize <- setdiff(colnames(x_data), columns_to_exclude)

  # normalize only in column that are meant to be normalize
  x_data_norm <- x_data
  x_data_norm[columns_to_normalize] <- lapply(x_data[columns_to_normalize], normalize)

  # normalize y_data target
  y_data_norm <- normalize(y_data)

  train_size <- 1:round(nrow(x_data_norm) * 0.7)  # 70% data untuk training
  x_train <- x_data_norm[train_size,]
  y_train <- y_data_norm[train_size]
  x_test <- x_data_norm[-train_size, ]
  y_test <- y_data_norm[-train_size]

  # Min Max on X and Y for normalization
  min_y <- min(y_data)
  max_y <- max(y_data)

  test <- svrHybrid(
                    x_train, y_train,
                    x_test, y_test,
                    kernel = "sigmoid", optimizer = "EHHOCBO",
                    objective = "SMAPE", is.y.normalize = TRUE,
                    min_y, max_y, max_iter = 100, N = 11)

  # optimation test result
  best_params <- test$best_params
  total_iter <- test$total_iter
  model_final <- test$model
  time_use <- test$time

  # predict use final model
  preds <- predict(model_final, x_test)

  # if on Y applied a normalization, do the denormalization on prediction result
  preds_final <- denormalize(preds, min_y, max_y)
  actuals_final <- denormalize(y_test, min_y, max_y)

  # print test
  print(paste("Best Cost:", best_params$cost))
  print(paste("Best Gamma:", best_params$gamma))
  print(paste("Best Epsilon:", best_params$epsilon))
  print(paste("Total Iterations:", total_iter))
  print(paste("Time Use:", time_use))
  print("Prediction:")
  print(head(preds_final))
  print("Actual:")
  print(head(actuals_final))

  # Checking loss for actual and predicted result
  final_loss <- loss_calculate(actuals_final, preds_final, "SMAPE")
  print(paste("Final Loss:", final_loss))
})

