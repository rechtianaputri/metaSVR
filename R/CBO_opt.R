#' @importFrom stats runif

initCBO <- function(N,dim,ub,lb){
  if (ncol(ub)==1) {
    X<- matrix(runif(N*dim),N,dim) * (ub - lb) + lb
  }
  if (ncol(ub)> 1) {
    X<- matrix(NA, nrow= N, ncol= dim)
    for (i in 1:dim) {
      high <- ub[i]
      low <- lb[i]
      X[,i] <- matrix(runif(1*N),1,N) * (high-low) + low
    }
  }
  return(X)
}

CBO <- function(N,Max_iter,lb,ub,dim,fobj) {
  if (ncol(ub) == 1) {
    ub <- matrix(ub, nrow= 1, ncol= dim)
    lb <- matrix(lb, nrow = 1, ncol= dim)
  }
  # Constant initCBOialization
  nLeader <- ceiling(0.1*N)
  nCoot <- N-nLeader
  objective_history <- matrix(0L, nrow= Max_iter, ncol= 1)
  param_list <<- matrix(0L, nrow = 1, ncol=Max_iter)
  param <- NULL
  gBest <- matrix(0L, nrow = 1, ncol= dim)
  gBestScore <- Inf

  # initCBOialize Coot and Leader position
  if (ncol(ub)>1) {
    cootPos <- matrix(0,nCoot,dim)
    leaderPos <- matrix(0,nLeader,dim)
    for (i in 1:dim) {
      high <- ub[i]
      low <- lb[i]
      cootPos[,i] <- runif(nCoot)*(high-low)+low
      leaderPos[,i] <- runif(nLeader)*(high-low)+low
    }
  } else {
    cootPos <- matrix(runif(nCoot*dim),nCoot,dim)*(ub-lb)+lb
    leaderPos <- matrix(runif(nLeader*dim),nLeader,dim)*(ub-lb)+lb
  }

  # Check for NA values in initial positions
  if (any(is.na(cootPos)) || any(is.na(leaderPos))) {
    print("Initialization value of cootPos or leaderPos contain NA!")
  }

  #cootPos <- initCBO(nCoot,dim,ub,lb)
  #leaderPos <- initCBO(nLeader,dim.ub.lb)
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
  cat("At iteration 1 the best fitness is", gBestScore,"\n")
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
    cat("At iteration", l, "the best fitness is", gBestScore,"\n")
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
