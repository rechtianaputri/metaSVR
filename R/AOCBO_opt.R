

#' @param N
#'
#' @param Max_iter
#' @param lb
#' @param ub
#' @param dim
#' @param fobj
#'
#' @importFrom stats runif

AOCBO <- function(N, Max_iter, lb, ub, dim, fobj) {
  # Initialization
  C1 <- 2
  C2 <- 6
  C3 <- 1
  C4 <- 2
  u <- 0.9
  l <- 0.1

  # Initialize tracking variables
  #smape <- matrix(0, nrow = Max_iter, ncol = 1)
  objective_history <- numeric(Max_iter)
  #param_list <- matrix(0, nrow = Max_iter, ncol = 1)  # Fix: Make sure it's initialized correctly
  param <- matrix(0, nrow = Max_iter, ncol = dim)  # Fix: Ensure param is properly initialized
  param_list <- numeric(Max_iter)  # param initialized as vector
  Xbest <- matrix(0, nrow = 1, ncol = dim)
  Scorebest <- Inf

  # Initial positions (Eq. 4)
  # Make X as it N and dim
  X <- matrix(0, nrow = N, ncol = dim)
  # Random initialized X based on lb and ub
  for (i in 1:N) {
    for (j in 1:dim) {
      X[i, j] <- lb[j] + runif(1) * (ub[j] - lb[j])
      #X[i, j] <- max(lb[j] + runif(1) * (ub[j] - lb[j]), 1e-5)  # lb minimum
    }
  }

  # X <- matrix(lb + runif(N * dim) * (ub - lb), nrow = N, ncol = dim)
  den <- matrix(runif(N * dim), nrow = N, ncol = dim) # Eq. 5
  vol <- matrix(runif(N * dim), nrow = N, ncol = dim)
  #acc <- matrix(lb + runif(N * dim) * (ub - lb), nrow = N, ncol = dim) # Eq. 6

  # Initialized acc match with the dimention of lb and ub

  acc <- matrix(0, nrow = N, ncol = dim) # Initialize acc with correct size
  for (i in 1:N) {
    for (j in 1:dim) {
      acc[i, j] <- lb[j] + runif(1) * (ub[j] - lb[j])  # generate every element based on lb and ub
      #acc[i, j] <- max(lb[j] + runif(1) * (ub[j] - lb[j]), 1e-5)  # lb minimum
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
    # Early exploration on CBO with Leader Selection using Xbest that has been initialized before
    B<- 2-t/Max_iter
    for (i in 1:N) {
      if (runif(1)<0.5){
        R<- (-1)+2*runif(1)
        R3 <- runif(1)
      } else {
        R<- (-1)+2*runif(dim)
        R3 <- runif(dim)
      }

      if (runif(1)<0.5){
        temp <- B*R3*cos(2*pi*R)*(Xbest-X[i,]) + Xbest
      } else {
        temp <- B*R3*cos(2*pi*R)*(Xbest-X[i,]) - Xbest
      }

      Tp <- temp > ub
      Tm <- temp <lb
      temp <- (temp*(!(Tp+Tm)))+ub*Tp+lb*Tm
      tempFit <- fobj(temp)

      if (tempFit < fobj(X[i,])) {
        X[i,] <- temp
        if (fobj(X[i,])< Scorebest) {
          Xbest <- X[i,]
          Scorebest <- fobj(X[i,])
          den_best <- den[i, ]
          vol_best <- vol[i, ]
          acc_best <- acc[i, ]
          acc_norm <- acc
        }
      }
    }

    # Proses Archimedes Optimization
    Xbest_CBO <- Xbest # use Xbest ftom early exploration
    den_best_CBO <- den_best
    vol_best_CBO <- vol_best
    acc_best_CBO <- acc_best
    acc_norm_CBO <- acc_norm

    # Eq. 8
    TF <- exp((t - Max_iter) / Max_iter)
    if (TF > 1){
      TF <- 1
    }

    # Eq. 9
    d <- exp((Max_iter - t) / Max_iter) - (t / Max_iter)


    # Fase Update Acceleration and Nornalize Acceleration
    acc <- acc_norm_CBO
    acc_temp <- matrix(0, nrow = N, ncol = dim)
    for (i in 1:N) {
      # Eq. 7
      den[i, ] <- den[i, ] + runif(1) * (den_best_CBO - den[i, ])
      vol[i, ] <- vol[i, ] + runif(1) * (vol_best_CBO - vol[i, ])

      if (TF <= 0.5) { # Collision (Eq. 10), AO exploration phase
        mr <- sample(1:N, 1)
        acc_temp[i, ] <- (den[mr, ] + (vol[mr, ] * acc[mr, ])) / (runif(1) * den[i, ] * vol[i, ])
      } else { # Eq. 11, AO exploitation phase
        acc_temp[i, ] <- (den_best_CBO + (vol_best_CBO * acc_best_CBO)) / (runif(1) * den[i, ] * vol[i, ])
      }
    }

    # Normalize acceleration
    if (max(acc_temp) != min(acc_temp)) {
      acc_norm <- ((u * (acc_temp - min(acc_temp))) / (max(acc_temp) - min(acc_temp))) + l
    } else {
      acc_norm <- matrix(l, nrow = N, ncol = dim) # default value if the range is 0
    }

    # Fase update position
    Xnew <- matrix(0, nrow = N, ncol = dim)
    for (i in 1:N) {
      if (TF <= 0.5) { # Update position for exploration phase
        for (j in 1:dim) {
          mrand <- sample(1:N, 1)
          Xnew[i, j] <- X[i, j] + C1 * runif(1) * acc_norm[i, j] * (X[mrand, j] - X[i, j]) * d # Eq. 13
          # Xnew is on [lb[j], ub[j]] range
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
            # if p <0.5 so F=1
            Xnew[i, j] <- Xbest_CBO[j] + C2 * runif(1) * acc_norm[i, j] * (T * Xbest_CBO[j] - X[i, j]) * d # Eq. 14
          } else {
            # if p >= 0.5 so F = -1
            Xnew[i, j] <- Xbest_CBO[j] - C2 * runif(1) * acc_norm[i, j] * (T * Xbest_CBO[j] - X[i, j]) * d # Eq. 14
          }
          # check boundary
          Xnew[i, j] <- max(min(Xnew[i, j], ub[j]), lb[j]) # make sure in range of lb and ub
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
      den_best <- den[Score_index, ]
      vol_best <- vol[Score_index, ]
      acc_best <- acc_norm[Score_index, ]
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

  xBest <- Xbest
  xBestScore <- Scorebest

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
