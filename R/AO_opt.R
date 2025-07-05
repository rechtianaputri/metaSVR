#' Archimedes Optimization
#'
#' An algorithm built by Hashim et al. (2021) use buoyancy law and fluid dynamics behavior
#' in Archimedes principle to optimized real-valued objective function in continuous search space
#' in a population-based manner.
#'
#' @param N An integer indicate population size.
#' @param Max_iter An integer indicate maximum number of iterations.
#' @param lb A numeric vector that show lower bounds of the search space. One value per dimension.
#' @param ub A numeric vector that show upper bounds of the search space. One value per dimension.
#' @param dim An integer show the number of dimension (parameters) of the problem to optimize. It indicate the number of parameters to be optimized.
#' @param fobj An objective function used to be minimized. It is return single numeric value that show evaluation matrix result in every iteration.
#' It used to calculate the best fitness in every iteration.
#'
#' @return A list containing:
#' \describe{
#'   \item{best_fitness}{The best (minimum) fitness value found.}
#'   \item{best_position}{The parameter vector (position) corresponding to the best fitness.}
#'   \item{jml_iter}{The number of iterations executed.}
#'   \item{param}{Matrix of best parameters found across every iterations (dim × iter).}
#'   \item{param_list}{Vector of best fitness values at each iteration.}
#' }
#'
#' @details
#' This algorithm uses population-based search to conduct physical law such as volume, density difference, and acceleration in every iteration.
#' It balancing the exploration and exploitation phase by using Transfer Function (TF) as a shifting indicates.
#'
#' The algorithm performs until maximum iteration reached or convergence condition when the difference
#' in objective values for ten consecutive times is less than 10^-5.
#'
#' @note
#' The input vectors 'lb' and 'ub' must have the same length as the number of dimensions 'dim'.
#'
#' This optimization function used inside svrHybrid function.
#'
#' Constant of C3 = 1 and C4 = 2 used in basic standard optimization function.
#'
#' @examples
#' \dontrun{
#' sphere_fn <- function(x) sum(x^2) # simple function for objective function
#'
#' # AO optimization
#' set.seed(123)
#' result <- AO(N = 20, Max_iter = 50, lb = [-5,-5,-5], ub = [5,5,5], dim = 3, fobj = sphere_fn)
#'
#' # View best fitness and position found
#' result$best_fitness
#' result$best_position
#' }
#'
#' @references
#' Hashim, F. A., Hussain, K., Houssein, E. H., Mabrouk, M. S., & Al-Atabany, W. (2021).
#' Archimedes Optimization Algorithm: A New Metaheuristic Algorithm for Solving Optimization Problems.
#' Applied Intelligence, 51(3), 1531–1551. https://doi.org/10.1007/s10489-020-01893-z
#'
#' @importFrom stats runif

AO <- function(N, Max_iter, lb, ub, dim, fobj) {
  # Input Validation Checking
  if (!is.numeric(N) || length(N) != 1 || N <= 0 || N != as.integer(N)) {
    stop("'N' must be a positive integer.")
  }
  if (!is.numeric(Max_iter) || length(Max_iter) != 1 || Max_iter <= 0 || Max_iter != as.integer(Max_iter)) {
    stop("'Max_iter' must be a positive integer.")
  }
  if (!is.numeric(dim) || length(dim) != 1 || dim <= 0 || dim != as.integer(dim)) {
    stop("'dim' must be a positive integer.")
  }
  if (!is.numeric(lb) || length(lb) != dim) {
    stop("'lb' must be a numeric vector with length equal to 'dim'.")
  }
  if (!is.numeric(ub) || length(ub) != dim) {
    stop("'ub' must be a numeric vector with length equal to 'dim'.")
  }
  if (any(lb >= ub)) {
    stop("Each element of 'lb' must be strictly less than the corresponding element in 'ub'.")
  }
  if (!is.function(fobj)) {
    stop("'fobj' must be a valid function that returns a single numeric value.")
  }
  test_eval <- try(fobj(rep((lb + ub) / 2, dim)), silent = TRUE)
  if (inherits(test_eval, "try-error") || !is.numeric(test_eval) || length(test_eval) != 1) {
    stop("The objective function 'fobj' must return a single numeric value when passed a numeric vector of length 'dim'.")
  }

  # Initialization
  C1 <- 2
  C2 <- 6
  C3 <- 1
  C4 <- 2
  u <- 0.9
  l <- 0.1

  # Initialize tracking variables
  objective_history <- numeric(Max_iter) # calculate fobj
  param <- matrix(0, nrow = Max_iter, ncol = dim)  # Fix: Ensure param is properly initialized
  param_list <- numeric(Max_iter)  # initialize as vector
  Xbest <- rep(0, dim)
  Scorebest <- Inf

  # Initial positions (Eq. 4)
  # matrix X generated based on N and dim
  X <- matrix(0, nrow = N, ncol = dim)
  # Randomly initialize X based on lb and ub
  for (i in 1:N) {
    for (j in 1:dim) {
      X[i, j] <- lb[j] + runif(1) * (ub[j] - lb[j])
    }
  }

  # X <- matrix(lb + runif(N * dim) * (ub - lb), nrow = N, ncol = dim)
  den <- matrix(runif(N * dim), nrow = N, ncol = dim) # Eq. 5
  vol <- matrix(runif(N * dim), nrow = N, ncol = dim)

  # Initialize acc based on lb and ub dimention
  acc <- matrix(0, nrow = N, ncol = dim) # Initialize acc with correct size
  for (i in 1:N) {
    for (j in 1:dim) {
      acc[i, j] <- lb[j] + runif(1) * (ub[j] - lb[j])  # every element based on lb and ub
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
  if (length(Xbest) == dim) {
    param[1, ] <- Xbest
  }
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

      # Update position phase
    Xnew <- matrix(0, nrow = N, ncol = dim)
    for (i in 1:N) {
      if (TF <= 0.5) { # Update position for exploration phase
        for (j in 1:dim) {
          mrand <- sample(1:N, 1)
          Xnew[i, j] <- X[i, j] + C1 * runif(1) * acc_norm[i, j] * (X[mrand, j] - X[i, j]) * d # Eq. 13
          # Make sure that Xnew on range [lb[j], ub[j]]
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
            Xnew[i, j] <- Xbest[j] + C2 * runif(1) * acc_norm[i, j] * (T * Xbest[j] - X[i, j]) * d # Eq. 14
          } else {
            # if p >= 0.5 so F = -1
            Xnew[i, j] <- Xbest[j] - C2 * runif(1) * acc_norm[i, j] * (T * Xbest[j] - X[i, j]) * d # Eq. 14
          }
          # check boundary
          Xnew[i, j] <- max(min(Xnew[i, j], ub[j]), lb[j]) # on the range of ub and lb
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
      Xbest <- X[Score_index, ]
      den_best <- den[Score_index, ]
      vol_best <- vol[Score_index, ]
      acc_best <- acc_norm[Score_index, ]
    }

    # Update tracking variables
    objective_history[t] <- Scorebest
    param_list[t] <- Scorebest
    if (length(Xbest) == dim) {
      param[t, ] <- Xbest
    }
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
