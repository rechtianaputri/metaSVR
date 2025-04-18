#' @importFrom stats runif

initEHHOCBO <- function(N, dim, ub,lb){
  if (ncol(ub) == 1) {
    X<- matrix(runif(N*dim),N,dim) * (ub - lb) + lb
  }
  if (ncol(ub)> 1) {
    X<- matrix(NA, nrow= N, ncol= dim)
    for (i in 1:dim){
      high <- ub[i]
      low <- lb[i]
      X[,i] <- matrix(runif(1*N),1,N) * (high - low) + low
    }
  }
  return(X)
}


levyEHHOCBO <- function(d) {
  beta <- 1.5
  sigma <- ((gamma(1 + beta) * sin(pi*beta/2))
            /(gamma((1+beta)/2)*beta*2^((beta-1)/2)))^(1/beta)
  u <- runif(d)*sigma
  v <- runif(d)
  step <-u/(abs(v)^(1/beta))
  o <- step
  return(o)
}

EHHOCBO <- function(N,Max_iter,lb,ub,dim,fobj) {
  rab_loc <- matrix(0L, nrow = 1, ncol= dim)
  rab_en <- Inf
  X<- initEHHOCBO(N,dim,ub,lb)
  param_list <- matrix(0L, nrow = 1, ncol = Max_iter)
  objective_history <- matrix(0L, nrow = Max_iter, ncol= 1)
  param <- NULL
  t<-0
  bound <- 0
  while (t<Max_iter) {
    for (i in 1:nrow(X)) {
      FU <- X[i,]>ub
      FL <- X[i,]<lb
      X[i,] <- (X[i,]*as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb*as.numeric(FL)
      fitness <- fobj(X[i,])
      if (fitness < rab_en){
        rab_en <- fitness
        rab_loc <- X[i,]
      }
    }

    B<- 2-t/Max_iter
    for (i in 1:nrow(X)) {
      if (runif(1)<0.5){
        R<- (-1)+2*runif(1)
        R3 <- runif(1)
      } else {
        R<- (-1)+2*runif(dim)
        R3 <- runif(dim)
      }

      if (runif(1)<0.5){
        temp <- B*R3*cos(2*pi*R)*(rab_loc-X[i,]) + rab_loc
      } else {
        temp <- B*R3*cos(2*pi*R)*(rab_loc-X[i,]) - rab_loc
      }

      Tp <- temp > ub
      Tm <- temp <lb
      temp <- (temp*(!(Tp+Tm)))+ub*Tp+lb*Tm
      tempFit <- fobj(temp)

      if (tempFit < fobj(X[i,])) {
        X[i,] <- temp
        if (fobj(X[i,])<rab_en) {
          rab_loc <- X[i,]
          rab_en <- fobj(X[i,])
        }
      }
    }

    E1 <-2%*% (1-(t/Max_iter))
    for (i in 1:nrow(X)) {
      E0<- 2%*% runif(1) - 1
      esc_en <- E1 %*% (E0)
      if (abs(esc_en) >= 1) {
        q<- runif(1)
        rand_hawk <- floor(N %*% runif(1) +1)
        X_rand <- X[rand_hawk,]
        if (q<0.5) {
          X[i,] <- X_rand - (runif(1) %*% abs(X_rand-2%*%runif(1)%*%X[i,]))
        } else {
          X[i,] <- (rab_loc - colMeans(X)) - (runif(1) %*%
                                                (lb + (runif(1) %*% (ub -lb))))
        }

        ##
        FU <- X[i,]>ub
        FL <- X[i,]<lb
        X[i,] <- (X[i,]*as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb *as.numeric(FL)
      } else {
        r<- runif(1)
        if (r>= 0.5 & abs(esc_en) <0.5) {
          X[i,] <- (rab_loc) - esc_en %*% abs(rab_loc - X[i,])
          ##
          FU <- X[i,]> ub
          FL <- X[i,]<lb
          X[i,] <-(X[i,]*as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb*as.numeric(FL)
        } else if (r >= 0.5 & abs(esc_en) >= 0.5) {
          jump_str <- 2%*%(1 - runif(1))
          X[i,] <- (rab_loc - X[i,])-esc_en%*%abs(jump_str%*%rab_loc-X[i,])
          ##
          FU <- X[i,]>ub
          FL <- X[i,]<lb
          X[i,] <- (X[i,]*as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb*as.numeric(FL)
        } else if (r <0.5 & abs(esc_en) >= 0.5) {
          jump_str <- 2%*%(1 - runif(1))
          X1 <- rab_loc-esc_en%*%abs(jump_str%*%rab_loc-X[i,])
          ##
          FU <- X1 > ub
          FL <- X1 <lb
          X1 <- (X1* as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb*as.numeric(FL)
          if (fobj(X1)<fobj(X[i,])) {
            X[i,] <-X1
          } else {
            X2 <- rab_loc-esc_en%*%abs(jump_str%*%rab_loc-X[i,])
            +runif(dim)*levyEHHOCBO(dim)
            ##
            FU <- X2 > ub
            FL <- X2 <lb
            X2 <- (X2*as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb*as.numeric(FL)
            if (fobj(X2)<fobj(X[i,])){
              X[i,] <- X2
            }
          }
        } else{
          jump_str <- 2%*% (1-runif(1))
          X1 <- rab_loc-esc_en %*% abs(jump_str%*%rab_loc-colMeans(X))
          ##
          FU <- X1 > ub
          FL <- X1 < lb
          X1 <- (X1*as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb*as.numeric(FL)
          if (fobj(X1)<fobj(X[i,])) {
            X[i,] <- X1
          } else {
            X2 <- rab_loc-esc_en %*%
              abs(jump_str%*%rab_loc-colMeans(X))+runif(dim)*levyEHHOCBO(dim)
            ##
            FU <- X2 > ub
            FL <- X2 < lb
            X2 <- (X2*as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb*as.numeric(FL)
            if (fobj(X2)<fobj(X[i,])) {
              X[i,] <- X2
            }
          }
        }
      }

      V <- matrix(NA, nrow = 3, ncol= dim)
      if (runif(1) < 0.1) {
        V[1,] <- X[sample(1:nrow(X),1),]+(X[sample(1:nrow(X),1),]
                                          -X[sample(1:nrow(X),1),])
      } else {
        V[1,] <- X[i,]
      }

      if (runif(1) < 0.2) {
        V[2,] <- X[sample(1:nrow(X),1),]+
          0.8*(X[sample(1:nrow(X),1),]-X[sample(1:nrow(X),1),])+
          0.8*(X[sample(1:nrow(X),1),]-X[sample(1:nrow(X),1),])
      } else {
        V[2,] <- X[i,]
      }

      if (runif(1)<0.9) {
        V[3,] <- X[sample(1:nrow(X),1),] +
          (X[sample(1:nrow(X),1),] - X[sample(1:nrow(X),1),]) +
          (X[sample(1:nrow(X),1),] - X[sample(1:nrow(X),1),])
      } else {
        V[3,] <- X[i,]
      }

      V_en <- Inf
      for (j in 1:3) {
        FU <- V[j,] > ub
        FL <- V[j,]< lb
        V[j,] <- (V[j,]*as.numeric(!(FU+FL))) + ub*as.numeric(FU) + lb*as.numeric(FL)
        fitness <- fobj(V[j,])
        if (fitness < V_en){
          tempFit <- fitness
          temp <- V[j,]
        }
      }
      if (tempFit<fobj(X[i,])) {
        X[i,] <- temp
      }
    }

    temp <- matrix(NA, nrow = 1, ncol= dim)
    for (i in 1:dim) {
      temp[1,i] <- ub[i] + lb[i] - rab_loc[i]
    }
    FU <- temp > ub
    FL <- temp < lb
    temp <- (temp*as.numeric(!(FU+FL)))+ub*as.numeric(FU)+lb*as.numeric(FL)
    tempFit <- fobj(temp)
    if (tempFit < rab_en) {
      rab_loc <- temp
      rab_en <- tempFit
    }

    t <- t+1
    objective_history[t,] <- rab_en
    param <- c(param, rab_loc)
    param_list[t] <- rab_en
    if (t %% 1 == 0) {
      cat("At iteration", t, "the best fitness is", rab_en,"\n")
    }
    if (t > 1) {
      if (objective_history[t-1,]-rab_en <= 0.00001 & objective_history[t-1,]-rab_en >= 0) {
        bound <- bound +1
      } else {
        bound <- 0
      }
    }
    if (bound == 10) {
      break;
    }
  }
  param <- matrix(param, nrow = dim)
  result <- list(best_fitness = rab_en, best_position = rab_loc,
                 jml_iter = t, param = param, param_list = param_list)
  return(result)
}
