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

levyHHO <- function(d) {
  beta <- 1.5
  sigma <- ((gamma(1+beta)*sin(pi*beta/2))/(gamma((1+beta)/2)*beta*
                                              2^((beta-1)/2)))^(1/beta)
  u <- rnorm(d)*sigma
  v <- rnorm(d)
  step <- u/(abs(v)^(1/beta))
  o <- step
  return(o)
}

HHO <- function(N,Max_iter,lb,ub,dim,fobj) {
  rab_loc <- matrix(0L, nrow = 1, ncol= dim)
  rab_en <- Inf
  X <- initHHO(N,dim,ub,lb)
  param_list <- matrix(0L, nrow = 1, ncol=Max_iter)
  smape <- matrix(0L, nrow = Max_iter, ncol= 1)
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
    smape[t,] <- rab_en
    param <- c(param, rab_loc)
    param_list[t] <- rab_en

    if (mod(t,1) == 0) {
      cat("At iteration", t, "the best fitness is", rab_en,"\n")
    }
    if (t> 1) {
      if (smape[t-1,]-rab_en <= 0.00001 & smape[t-1,]-rab_en >= 0) {
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
