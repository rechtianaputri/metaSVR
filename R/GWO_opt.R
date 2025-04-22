#' Initialize Position on Grey Wolf Optimizer
#'
#' This function generates the initial position of gray wolf within the defined upper and lower bound in every dimension.
#'
#' @param N An integer indicate population size.
#' @param dim An integer show the number of dimension (parameters) of the problem to optimize. It indicate the number of parameters to be optimized.
#' @param ub A numeric vector that show upper bounds of the search space. One value per dimension
#' @param lb A numeric vector that show lower bounds of the search space. One value per dimension.
#'
#' @return A numeric matrix of shape \code{(N, dim)} representing initialized positions.
#'
#' @note
#' This function used inside GWO function for initialization process.
#'
#' @importFrom stats runif

initGWO <- function(N,dim,ub,lb) {
  bound_no <- ncol(ub)
  if (bound_no == 1) {
    X<- matrix(runif(N*dim),N,dim) * (ub-lb) + lb
  }
  if (bound_no > 1) {
    X<- matrix(NA, nrow = N,ncol = dim)
    for (i in 1:dim) {
      ub_i <- ub[i]
      lb_i <- lb[i]
      X[,i] <- matrix(runif(1*N),1,N) * (ub_i - lb_i) + lb_i
    }
  }
  return(X)
}

#' Grey Wolf Optimizer
#'
#' An algorithm built by Mirjalili et al. (2014) inspired by leadership hierarchy and hunting mechanism of
#' grey wolves in nature to optimized real-valued objective function in continous search space in a population-based manner.
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
#' This algorithm proposed social hierarchy on GWO to obtain the best fitness and get the best proposed hunting method to locate
#' probable position of the pray. Adaptive values on alpha and A make it possible smooth transition between exploration and exploitation phase.
#'
#' The algorithm performs until maximum iteration reached or convergence condition when the difference
#' in objective values for ten consecutive times is less than 10^-5.
#'
#' @note
#' The input vectors 'lb' and 'ub' must have the same length as the number of dimensions 'dim'.
#'
#' This optimization function used inside svrHybrid function.
#'
#' @references
#' Mirjalili, S., Mirjalili, S. M., & Lewis, A. (2014). Grey wolf optimizer.
#' Advances in engineering software, 69, 46-61. https://doi.org/10.1016/j.advengsoft.2013.12.007
#'

GWO <- function(N,Max_iter,lb,ub,dim,fobj) {
  alpha_pos <- matrix(0, nrow= 1, ncol= dim)
  alpha_score <- Inf
  beta_pos <- matrix(0, nrow = 1, ncol = dim)
  beta_score <- Inf
  delta_pos <- matrix(0, nrow = 1, ncol = dim)
  delta_score <- Inf

  pos <- initGWO(N,dim,ub,lb)
  con_curve <- matrix(0L, nrow = 1, ncol=Max_iter)
  param <- c()
  objective_history <- matrix(0L, nrow= Max_iter, ncol= 1)
  bound <- 0
  l<-0
  while (l<Max_iter) {
    for (i in 1:nrow(pos)) {
      Flag4ub <- pos[i,]>ub
      Flag4lb <- pos[i,]<lb
      pos[i,] <- (pos[i,]*(as.numeric(!(Flag4ub+Flag4lb)))) + ub*as.numeric(Flag4ub) +
        lb*as.numeric(Flag4lb)
      fitness <- fobj(abs(pos[i,]))

      if (fitness<alpha_score) {
        alpha_score <- fitness
        alpha_pos <- pos[i,]
      }

      if ((fitness>alpha_score)&(fitness<beta_score)) {
        beta_score <- fitness
        beta_pos <- pos[i,]
      }

      if ((fitness>alpha_score)&(fitness>beta_score)&(fitness<delta_score)) {
        delta_score <- fitness
        delta_pos <-pos[i,]
      }
    }

    a <- 2-(1*((2)/Max_iter))
    for (i in 1:nrow(pos)){
      for (j in 1:ncol(pos)){
        r1 <- runif(1)
        r2 <- runif(1)

        A1 <- 2*a*r1-a
        C1<- 2*r2

        D_alpha <- abs(C1*alpha_pos[j]-pos[i,j])
        X1 <- alpha_pos[j]-A1*D_alpha

        r1 <- runif(1)
        r2 <- runif(1)

        A2 <- 2*a*r1-a
        C2 <- 2*r2

        D_beta <- abs(C2* beta_pos[j] - pos[i,j])
        X2 <- beta_pos[j]-A2*D_beta

        r1 <- runif(1)
        r2 <- runif(1)

        A3 <- 2*a*r1-a
        C3 <- 2*r2

        D_delta <- abs(C3*delta_pos[j]-pos[i,j])
        X3 <- delta_pos[j]-A3*D_delta

        pos[i,j] <- (X1+X2+X3)/3
      }
    }

    con_curve[l] <- alpha_score
    param <- c(param, alpha_pos)

    cat("At iteration", l+1, "the best fitness is", alpha_score,"\n")

    objective_history[l,] <- alpha_score
    if (l> 1) {
      if (objective_history[l-1,]-alpha_score <= 0.00001 & objective_history[l-1,]-alpha_score >= 0) {
        bound <- bound + 1
      } else {
        bound <- 0
      }
    }

    if (bound == 10) {
      break;
    }

    l <- l+1
  }
  param <- matrix(param, nrow = dim)
  res <- list(best_fitness = alpha_score, best_position= alpha_pos,
              jml_iter = l+1, param = param, param_list = con_curve)
  return(res)
}
