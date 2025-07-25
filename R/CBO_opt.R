#' Coot Bird Optimization
#'
#' An algorithm built by Naruei & Keynia (2021) that mimics the regular-irregular movement behaviour of
#' Coot birds. Its population divided by two groups as leaders to guide the process and coots to follow leaders and randomly explore search space.
#' This movement use to  optimized real-valued objective function in continuous search space.
#'
#' @importFrom stats runif
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
#' This algorithms used movement such as: random movement, chain movement, adjusting the position based on
#' the group leaders, and leader movement to emphasize the exploration and exploitation phase to get the best fitness.
#'
#' The algorithm performs until maximum iteration reached or convergence condition when the difference
#' in objective values for ten consecutive times is less than 10^-5.
#'
#' @note
#' The input vectors 'lb' and 'ub' must have the same length as the number of dimensions 'dim'.
#'
#' This optimization function used inside svrHybrid function.
#'
#' @examples
#' \dontrun{
#' sphere_fn <- function(x) sum(x^2) # simple function for objective function
#'
#' # CBO optimization
#' set.seed(123)
#' result <- CBO(N = 20, Max_iter = 50, lb = [-5,-5,-5], ub = [5,5,5], dim = 3, fobj = sphere_fn)
#'
#' # View best fitness and position found
#' result$best_fitness
#' result$best_position
#' }
#'
#' @references
#' Naruei, I., & Keynia, F. (2021). A New Optimization Method Based on COOT Bird Natural Life Model.
#' Expert Systems with Applications, 183. https://doi.org/10.1016/j.eswa.2021.115352
#'
#' @export
#'
CBO <- function(N,Max_iter,lb,ub,dim,fobj) {
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
  test_eval <- try(fobj(rep((lb + ub)/2, dim)), silent = TRUE)
  if (inherits(test_eval, "try-error") || !is.numeric(test_eval) || length(test_eval) != 1) {
    stop("The objective function 'fobj' must return a single numeric value when passed a numeric vector of length 'dim'.")
  }

  if (length(ub) == 1 && length(lb) == 1) {
    ub <- rep(ub, dim)
    lb <- rep(lb, dim)
  }

  # Constant initialization
  nLeader <- ceiling(0.1*N)
  nCoot <- N-nLeader
  objective_history <- matrix(0L, nrow= Max_iter, ncol= 1)
  param_list <<- matrix(0L, nrow = 1, ncol=Max_iter)
  param <- NULL
  gBest <- matrix(0L, nrow = 1, ncol= dim)
  gBestScore <- Inf

  # initialize Coot and Leader position
  if (length(ub) == 1 && length(lb) == 1) {
    cootPos <- matrix(runif(nCoot * dim), nCoot, dim) * (ub - lb) + lb
    leaderPos <- matrix(runif(nLeader * dim), nLeader, dim) * (ub - lb) + lb
  } else if (length(ub) == dim && length(lb) == dim) {
    cootPos <- matrix(NA, nCoot, dim)
    leaderPos <- matrix(NA, nLeader, dim)
    for (i in 1:dim) {
      cootPos[, i] <- runif(nCoot) * (ub[i] - lb[i]) + lb[i]
      leaderPos[, i] <- runif(nLeader) * (ub[i] - lb[i]) + lb[i]
    }
  }

  cootFitness <- matrix(0L, nrow = 1, ncol=nCoot)
  leaderFitness <- matrix(0L, nrow = 1, ncol=nLeader)

  # Calculate Fitness of Coots and Leaders
  for (i in 1:nCoot) {
    cootFitness[i] <- fobj(cootPos[i,])
    if (gBestScore > cootFitness[i]) {
      gBestScore <- cootFitness[i]
      gBest <- cootPos[i,]
    }
  }

  for (i in 1:nLeader) {
    leaderFitness[i] <- fobj(leaderPos[i,])
    if (gBestScore > leaderFitness[i]) {
      gBestScore <- leaderFitness[i]
      gBest <- leaderPos[i,]
    }
  }

  objective_history[1] <- gBestScore
  param_list[1] <- gBestScore
  param <- c(param, gBest)
  l=2
  bound <- 0
  while (l < Max_iter+1) { # MaX Iteration
    B <- 2-l/Max_iter
    A <- 1-l/Max_iter

    # Coots Movement
    for (i in 1:nCoot) {
      # level of randomness for each coot
      if (runif(1)<0.5) {
        R <- -1+2*runif(1)
        R1 <- runif(1)
      } else {
        R<- (-1)+2*runif(dim)
        R1 <- runif(dim)
      }

      # assign leader for this coot
      k <- 1+i %% nLeader

      # select movement
      if (runif(1)<0.5) {
        # Follow the Leaders
        cootPos[i,] <- 2*R1*cos(2*pi*R)*(leaderPos[k,]-cootPos[i,])+leaderPos[k,]
        Tp <- cootPos[i,]>ub
        Tm <- cootPos[i,]<lb
        cootPos[i,] <-(cootPos[i,]*(!(Tp+Tm)))+ub*Tp+lb*Tm
      } else {
        if (runif(1)<0.5 && i!=1) {
          # Chain Movement
          cootPos[i,] <- (cootPos[i,]+cootPos[i-1,])/2
        } else {
          # Random Movement
          Q <- runif(dim)*(ub-lb)+lb
          R1 <- 0.2+0.6*runif(1)
          cootPos[i,] <- cootPos[i,]+A*R1*(Q-cootPos[i,])
        }
        Tp <-cootPos[i,]>ub
        Tm <-cootPos[i,]<lb
        cootPos[i,] <- (cootPos[i,]*(!(Tp+Tm)))+ub*Tp+lb*Tm
      }
    }

    # Fitness of Coots Location
    for (i in 1:nCoot) {
      cootFitness[i] <- fobj(cootPos[i,])
      k<- 1+i %% nLeader

      # Update location of Coot
      if (cootFitness[i] < leaderFitness[k]) {
        temp <- leaderPos[k,]
        tempFit <- leaderFitness[k]
        leaderFitness[k] <- cootFitness[i]
        leaderPos[k,] <- cootPos[i,]
        cootFitness[i] <- tempFit
        cootPos[i,] <- temp
      }
    }

    # Leaders Movement and Fitness
    for (i in 1:nLeader) {
      # level of randomness for each leader
      if (runif(1)<0.5) {
        R <- -1+2*runif(1)
        R3 <- runif(1)
      } else {
        R<- (-1)+2*runif(dim)
        R3 <- runif(dim)
      }

      if (runif(1)<0.5){
        temp <- B*R3*cos(2*pi*R)*(gBest-leaderPos[i,]) + gBest
      } else {
        temp <- B*R3*cos(2*pi*R)*(gBest-leaderPos[i,]) - gBest
      }

      # check boundary
      Tp <- temp > ub
      Tm <- temp < lb
      temp <-(temp*(!(Tp+Tm)))+ub*Tp+lb*Tm

      tempFit <- fobj(temp)

      #Update Location of Leader
      if (gBestScore > tempFit) {
        leaderFitness[i] <- gBestScore
        leaderPos[i,] <- gBest
        gBestScore <- tempFit
        gBest <- temp
      }
    }

    objective_history[l] <- gBestScore
    param_list[l] <- gBestScore
    param <- c(param, gBest)
    if (objective_history[l-1,]-gBestScore <= 0.00001 & objective_history[l-1,]-gBestScore >= 0) {
      bound <- bound + 1
    } else {
      bound <- 0
    }
    if (bound == 10) {
      break;
    }
    l=l+ 1;
  }

  param <- matrix(param, nrow = dim)
  result <- list(
    best_fitness = gBestScore,
    best_position = gBest,
    jml_iter = l,
    param = param,
    param_list = param_list)

  return(result)
}
