#' Initialize Position on Grey Wolf Optimizer
#'
#' This function generates the initial position of gray wolf within the defined upper and lower bound in every dimension.
#'
#' @keywords internal
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
  if (length(ub) == 1) {
    X<- matrix(runif(N*dim),N,dim) * (ub-lb) + lb
  } else {
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
#' grey wolves in nature to optimized real-valued objective function in continuous search space in a population-based manner.
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
#' @examples
#' \dontrun{
#' sphere_fn <- function(x) sum(x^2) # simple function for objective function
#'
#' # GWO optimization
#' set.seed(123)
#' result <- GWO(N = 20, Max_iter = 50, lb = [-5,-5,-5], ub = [5,5,5], dim = 3, fobj = sphere_fn)
#'
#' # View best fitness and position found
#' result$best_fitness
#' result$best_position
#' }
#' @references
#' Mirjalili, S., Mirjalili, S. M., & Lewis, A. (2014). Grey wolf optimizer.
#' Advances in engineering software, 69, 46-61. https://doi.org/10.1016/j.advengsoft.2013.12.007
#'
#' @export
#'
GWO <- function(N,Max_iter,lb,ub,dim,fobj) {
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
