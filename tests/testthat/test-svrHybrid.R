library(readxl)

test_that("svrHybrid runs correctly with external data", {
  # Cari path dataset relatif terhadap package
  data_path <- system.file("extdata", "Data_TH_solar_valid.xlsx",
                           package = "metaSVR")
  expect_true(file.exists(data_path))  # Pastikan file ada

  # Load data
  data <- as.data.frame(read_excel(data_path))

  data$lag1 <- c(NA,data$Solar[-length(data$Solar)])

  # Preprocessing: Menghapus Hydropower dan Bioenergy dari X
  x_data <- dplyr::select(data, -Country, -Year, -Solar)
  y_data <- data$Solar

  # Menghapus barisan pertama dari x_data dan y_data
  x_data <- x_data[-1, ]
  y_data <- y_data[-1]

  # List nama kolom yang tidak akan dinormalisasi
  columns_to_exclude <- c("Share_Electricity", "RE_Growth", "RE_Consumption",
                          "Elec_Access", "ElecShare_Primary", "GDP_Growth",
                          "Inflation", "Forest_Area", "Pop_Growth",
                          "Urbanization", "Employment")

  # Pisahkan kolom yang akan dinormalisasi
  columns_to_normalize <- setdiff(colnames(x_data), columns_to_exclude)

  # Normalisasi hanya pada kolom yang ingin Anda ubah
  x_data_norm <- x_data
  x_data_norm[columns_to_normalize] <- lapply(x_data[columns_to_normalize], normalize)

  # Normalisasi target y_data tetap seperti sebelumnya
  y_data_norm <- normalize(y_data)

  # Sampling yang diperbaiki
  train_size <- 1:round(nrow(x_data_norm) * 0.7)  # 70% data untuk training
  x_train <- x_data_norm[train_size,]
  y_train <- y_data_norm[train_size]
  x_test <- x_data_norm[-train_size, ]
  y_test <- y_data_norm[-train_size]

  # Deklarasi Min Max karena dia normalisasi
  min_y <- min(y_data)
  max_y <- max(y_data)

  test <- svrHybrid(
                    x_train, y_train,
                    x_test, y_test,
                    kernel = "sigmoid", optimizer = "CBO",
                    objective = "SMAPE", is.y.normalize = TRUE,
                    min_y, max_y,
                    max_iter = 100, N = 11)

  # Ambil test optimasi
  best_params <- test$best_params
  total_iter <- test$total_iter
  model_final <- test$model

  # Prediksi menggunakan model final
  preds <- predict(model_final, x_test)

  # Jika pakai normalisasi Y, lakukan denormalisasi test prediksi
  preds_final <- denormalize(preds, min_y, max_y)
  actuals_final <- denormalize(y_test, min_y, max_y)

  # Cetak test
  print(paste("Best Cost:", best_params$cost))
  print(paste("Best Gamma:", best_params$gamma))
  print(paste("Best Epsilon:", best_params$epsilon))
  print(paste("Total Iterations:", total_iter))
  print("Prediksi:")
  print(head(preds_final))
  print("Aktual:")
  print(head(actuals_final))

  # Pengecekan kesalahan untuk hasil aktual maupun prediksi
  final_smape <- loss_calculate(actuals_final, preds_final, "SMAPE")
  print(paste("Final SMAPE:", final_smape))
})

