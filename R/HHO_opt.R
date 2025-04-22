#' Initialize Position on Harris Hawks Optimization
#'
#' This function generates the initial position of Harris Hawk agents within the defined upper and lower bound in every dimension.
#'
#' @param N An integer indicate population size.
#' @param dim An integer show the number of dimension (parameters) of the problem to optimize. It indicate the number of parameters to be optimized.
#' @param ub A numeric vector that show upper bounds of the search space. One value per dimension
#' @param lb A numeric vector that show lower bounds of the search space. One value per dimension.
#'
#' @return A numeric matrix of shape \code{(N, dim)} representing initialized positions.
#'
#' @note
#' This function used inside HHO function for initialization process.
#'
#' @importFrom stats runif

initHHO <- function(N,dim,ub,lb){
  if (ncol(ub) == 1) {
    X<- matrix(runif(N*dim),N,dim)*(ub-lb)+lb
  }
  if (ncol(ub) > 1) {
    X<- matrix(NA, nrow=N, ncol=dim)
    for (i in 1:dim) {
      high <- ub[i]
      low <- lb[i]
      X[,i] <- matrix(runif(1*N),1,N)*(high-low)+low
    }
  }
  return(X)
}

#' Levy Flight Generator
#'
#' Generates a random step vector based on Lévy flight distribution, used in the exploitation phase of the HHO algorithm.
#'
#' @param dim An integer that indicate the dimensionality of search space.
#'
#' @return A numeric vector of length \code{dim} representing the Lévy flight step.
#'
#' @note
#' This function used inside HHO function to generate random Levy Flight vector.
#'
#' @importFrom stats rnorm

levyHHO <- function(dim) {
  beta <- 1.5
  sigma <- ((gamma(1+beta)*sin(pi*beta/2))/(gamma((1+beta)/2)*beta*
                                              2^((beta-1)/2)))^(1/beta)
  u <- rnorm(dim)*sigma
  v <- rnorm(dim)
  step <- u/(abs(v)^(1/beta))
  o <- step
  return(o)
}

#' Harris Hawks Optimization
#'
#' An algorithm built by Heidari et al. (2019) that inspired by the movement of Harris Hawks on cooperative hunting behaviour
#' to optimized real-valued objective function in continous search space in a population-based manner.
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
#' There are two phase of Harris Hawks hunting, namely exploration and exploitation that will be modelized to find optimization result.
#' The movement used in this algorithms such as: exploration phase; transition between exploitation and exploration phase; and exploitation phase
#' that has 4 different strategies based on E and r (soft besiege, hard besiege, soft besiege with progressive rapid, and hard besiege with progressive rapid)
#'
#' The algorithm performs until maximum iteration reached or convergence condition when the difference
#' in objective values for ten consecutive times is less than 10^-5.
#'
#' @note The input vectors `lb` and `ub` must have the same length as the number of dimensions `dim`.
#' @note This optimization function used inside svrHybrid function.
#'
#' @references
#' Heidari, A. A., Mirjalili, S., Faris, H., Aljarah, I., Mafarja, M., & Chen, H. (2019).
#' Harris hawks optimization: Algorithm and applications. Future generation computer systems, 97, 849-872.
#' https://doi.org/10.1016/j.future.2019.02.028
#'

HHO <- function(N,Max_iter,lb,ub,dim,fobj) {
  rab_loc <- matrix(0L, nrow = 1, ncol= dim)
  rab_en <- Inf
  X <- initHHO(N,dim,ub,lb)
  param_list <- matrix(0L, nrow = 1, ncol=Max_iter)
  objective_history <- matrix(0L, nrow= Max_iter, ncol= 1)
  #smape <- matrix(0L, nrow = Max_iter, ncol= 1)
  param <- NULL
  t<-0
  bound <- 0
  while (t<Max_iter) {
    for (i in 1:nrow(X)){
      FU <- X[i,] > ub
      FL <- X[i,] < lb
      X[i,] <- (X[i,]*as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb*as.numeric(FL)
      fitness <- fobj(X[i,])
      if (fitness < rab_en){
        rab_en <- fitness
        rab_loc <- X[i,]
      }
    }
    E1 <- 2%*% (1-(t/Max_iter))
    for (i in 1:nrow(X)){
      E0 <- 2%*%runif(1)-1
      esc_en <- E1%*%(E0)
      if (abs(esc_en) >= 1) {
        q <- runif(1)
        rand_hawk <- floor(N%*%runif(1)+1)
        X_rand <- X[rand_hawk,]
        if (q<0.5) {
          X[i,] <- X_rand-(runif(1)%*%abs(X_rand-2%*%runif(1)%*%X[i,]))
        } else {
          X[i,] <- (rab_loc-colMeans(X))-(runif(1)%*%(lb+(runif(1)%*%(ub-lb))))
        }
        ##
        FU <- X[i,] > ub
        FL <- X[i,] < lb
        X[i,] <- (X[i,]*as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb*as.numeric(FL)
      } else {
        r<- runif(1)
        if (r>= 0.5 & abs(esc_en) <0.5) {
          X[i,] <- (rab_loc)-esc_en%*%abs(rab_loc-X[i,])
          ##
          FU <- X[i,]>ub
          FL <- X[i,]<lb
          X[i,] <- (X[i,]*as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb*as.numeric(FL)
        } else if (r >= 0.5 & abs(esc_en) >= 0.5) {
          jump_str <- 2%*%(1-runif(1))
          X[i,] <- (rab_loc-X[i,])-esc_en%*%abs(jump_str%*%rab_loc-X[i,])
          ##
          FU <- X[i,]>ub
          FL <- X[i,]<lb
          X[i,] <- (X[i,]*as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb*as.numeric(FL)
        } else if (r <0.5 & abs(esc_en)>=0.5) {
          jump_str <- 2%*%(1-runif(1))
          X1 <- rab_loc-esc_en%*%abs(jump_str%*%rab_loc-X[i,])
          ##
          FU <- X1 > ub
          FL <- X1 <lb
          X1 <- (X1*as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb*as.numeric(FL)
          if (fobj(X1)<fobj(X[i,])) {
            X[i,] <-X1
          } else {
            X2 <- rab_loc-esc_en%*%abs(jump_str%*%rab_loc-X[i,])+
              runif(dim)*levyHHO(dim)
            ##
            FU <- X2 > ub
            FL <- X2 <lb
            X2 <- (X2*as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb*as.numeric(FL)

            if(fobj(X2) < fobj(X[i,])){
              X[i,] <- X2
            }
          }
        } else {
          jump_str <- 2%*% (1-runif(1))
          X1 <- rab_loc-esc_en%*%abs(jump_str%*%rab_loc-colMeans(X))
          ##
          FU <- X1 > ub
          FL <- X1 < lb
          X1 <- (X1*as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb*as.numeric(FL)
          if (fobj(X1) < fobj(X[i,])) {
            X[i,] <- X1
          } else {
            X2 <- rab_loc-esc_en%*%abs(jump_str%*%rab_loc-colMeans(X))+
              runif(dim)*levyHHO(dim)
            ##
            FU <- X2 > ub
            FL <- X2 <lb
            X2 <- (X2*as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb*as.numeric(FL)

            if (fobj(X2) < fobj(X[i,])) {
              X[i,] <- X2
            }
          }
        }
      }
    }

    t<- t+1
    objective_history[t,] <- rab_en
    param <- c(param, rab_loc)
    param_list[t] <- rab_en
    cat("At iteration", t, "the best fitness is", rab_en,"\n")

    if (t> 1) {
      if (t > 1 && objective_history[t-1]-rab_en <= 0.00001 && objective_history[t-1]-rab_en >= 0) {
        bound <-bound + 1
      } else {
        bound <- 0
      }
    }

    if (bound == 10) {
      break;
    }
  }
  param <- matrix(param, nrow= dim)
  result <- list(best_fitness = rab_en, best_position=rab_loc, jml_iter = t,
                 param = param, param_list = param_list)
  return(result)
}
