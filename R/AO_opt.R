#' @importFrom stats runif

AO <- function(N, Max_iter, lb, ub, dim, fobj) {
  # Initialization
  C1 <- 2
  C2 <- 6
  C3 <- 1
  C4 <- 2
  u <- 0.9
  l <- 0.1

  # Initialize tracking variables
  #smape <- matrix(0, nrow = Max_iter, ncol = 1)
  objective_history <- numeric(Max_iter) # penghitungan fobj
  #param_list <- matrix(0, nrow = Max_iter, ncol = 1)  # Fix: Make sure it's initialized correctly
  param <- matrix(0, nrow = Max_iter, ncol = dim)  # Fix: Ensure param is properly initialized
  param_list <- numeric(Max_iter)  # Inisialisasi sebagai vektor #NOTED
  Xbest <- matrix(0, nrow = 1, ncol = dim)
  Scorebest <- Inf

  # Initial positions (Eq. 4)
  # Debug: Cek perhitungan N * dim
  print("Cek perhitungan N * dim:")
  print(N * dim)

  # Initial positions (Eq. 4)
  # Buat X dengan benar sesuai dengan N dan dim
  X <- matrix(0, nrow = N, ncol = dim)
  # Inisialisasi nilai X secara acak berdasarkan lb dan ub
  for (i in 1:N) {
    for (j in 1:dim) {
      X[i, j] <- lb[j] + runif(1) * (ub[j] - lb[j])
      #X[i, j] <- max(lb[j] + runif(1) * (ub[j] - lb[j]), 1e-5)  # Batas bawah minimum
    }
  }

  # X <- matrix(lb + runif(N * dim) * (ub - lb), nrow = N, ncol = dim)
  den <- matrix(runif(N * dim), nrow = N, ncol = dim) # Eq. 5
  vol <- matrix(runif(N * dim), nrow = N, ncol = dim)
  #acc <- matrix(lb + runif(N * dim) * (ub - lb), nrow = N, ncol = dim) # Eq. 6

  # Inisialisasi acc dengan benar sesuai dimensi lb dan ub
  acc <- matrix(0, nrow = N, ncol = dim) # Initialize acc with correct size
  for (i in 1:N) {
    for (j in 1:dim) {
      acc[i, j] <- lb[j] + runif(1) * (ub[j] - lb[j])  # Setiap elemen sesuai dengan lb dan ub
      #acc[i, j] <- max(lb[j] + runif(1) * (ub[j] - lb[j]), 1e-5)  # Batas bawah minimum
    }
  }

  # Initialize objective function values
  Y <- numeric(N) # Vector to store objective function values
  for (i in 1:N) {
    Y[i] <- fobj(X[i, ]) # Evaluate objective function for each row of X
  }

  # Assign best
  Scorebest <- min(Y)
  Score_index <- which.min(Y)
  Xbest <- X[Score_index, ]
  #Xbest <- pmax(X[Score_index, ], 1e-5) #NOTED
  den_best <- den[Score_index, ]
  vol_best <- vol[Score_index, ]
  acc_best <- acc[Score_index, ]
  acc_norm <- acc

  #NOTED
  objective_history[1] <- Scorebest
  param_list[1] <- Scorebest
  param[1] <- Xbest
  cat("At iteration 1 the best fitness is", Scorebest,"\n")
  t <- 2
  bound <- 0 # Initialize bound

  # Looping for every population_size
  while (t < Max_iter) {
    # Eq. 8
    TF <- exp((t - Max_iter) / Max_iter)
    if (TF > 1){
      TF <- 1
    }

    # Eq. 9
    d <- exp((Max_iter - t) / Max_iter) - (t / Max_iter)


    # Fase Update Acceleration and Nornalize Acceleration
    acc <- acc_norm
    acc_temp <- matrix(0, nrow = N, ncol = dim)
    for (i in 1:N) {
      # Eq. 7
      den[i, ] <- den[i, ] + runif(1) * (den_best - den[i, ])
      vol[i, ] <- vol[i, ] + runif(1) * (vol_best - vol[i, ])

      if (TF <= 0.5) { # Collision (Eq. 10), fase eksplorasi AO
        mr <- sample(1:N, 1)
        acc_temp[i, ] <- (den[mr, ] + (vol[mr, ] * acc[mr, ])) / (runif(1) * den[i, ] * vol[i, ])
      } else { # Eq. 11, fase eksploitasi AO
        acc_temp[i, ] <- (den_best + (vol_best * acc_best)) / (runif(1) * den[i, ] * vol[i, ])
      }
    }

    # Normalize acceleration
    if (max(acc_temp) != min(acc_temp)) {
      acc_norm <- ((u * (acc_temp - min(acc_temp))) / (max(acc_temp) - min(acc_temp))) + l
    } else {
      acc_norm <- matrix(l, nrow = N, ncol = dim) # Default value jika rentang nol
    }

    # Fase update position
    Xnew <- matrix(0, nrow = N, ncol = dim)
    for (i in 1:N) {
      if (TF <= 0.5) { # Update position for exploration phase
        for (j in 1:dim) {
          mrand <- sample(1:N, 1)
          Xnew[i, j] <- X[i, j] + C1 * runif(1) * acc_norm[i, j] * (X[mrand, j] - X[i, j]) * d # Eq. 13
          # Pastikan nilai Xnew berada dalam rentang [lb[j], ub[j]] -> NOTED
          Xnew[i, j] <- max(min(Xnew[i, j], ub[j]), lb[j])
        }
      } else {
        for (j in 1:dim) {
          p <- 2 * runif(1) - C4
          T <- C3 * TF
          if (T > 1){
            T <- 1
          }

          if (p <= 0.5) { # Update position if not collision (exploitation phase)
            # Jika p <0.5 maka F=1
            Xnew[i, j] <- Xbest[j] + C2 * runif(1) * acc_norm[i, j] * (T * Xbest[j] - X[i, j]) * d # Eq. 14
          } else {
            # Jika p >= 0.5 maka F = -1
            Xnew[i, j] <- Xbest[j] - C2 * runif(1) * acc_norm[i, j] * (T * Xbest[j] - X[i, j]) * d # Eq. 14
          }
          # check boundary
          Xnew[i, j] <- max(min(Xnew[i, j], ub[j]), lb[j]) # Tidak melebihi batas atas, dan tidak kurang batas bawah
        }
      }
    }
    for (i in 1:N) {
      v <- fobj(Xnew[i, ])
      if (v < Y[i]) {
        X[i, ] <- Xnew[i, ]
        Y[i] <- v
      }
    }

    var_Ybest <- min(Y)
    var_index <- which.min(Y)

    if (var_Ybest < Scorebest) {
      Scorebest <- var_Ybest
      Score_index <- var_index
      Xbest <- X[var_index, ]
      #Xbest <- pmax(X[var_index, ], 1e-5) #NOTED
      den_best <- den[Score_index, ]
      vol_best <- vol[Score_index, ]
      acc_best <- acc_norm[Score_index, ]
      # Pastikan nilai parameter selalu lebih besar dari 0
      #param[t, ] <- pmax(Xbest, 1e-5) ## NOTED
    }

    # Update tracking variables
    objective_history[t] <- Scorebest
    param_list[t] <- Scorebest
    param[t,] <- Xbest
    cat("At iteration", t, "the best fitness is", Scorebest,"\n")
    if (t > 1 && objective_history[t-1]-Scorebest <= 0.00001 && objective_history[t-1]-Scorebest >= 0) {
      bound <- bound + 1
    } else {
      bound <- 0
    }
    if (bound == 10) {
      break;
    }
    t=t+ 1
  }

  # Pastikan variabel valid sebelum dikembalikan
  if (is.null(Scorebest) || is.null(Xbest)) {
    stop("Scorebest atau Xbest tidak valid.")
  }

  # Final results
  param <- t(param)
  result <- list(
    best_fitness = Scorebest,
    best_position = Xbest,
    jml_iter = t,
    param = param,
    param_list = param_list
  )
  return(result)
}
