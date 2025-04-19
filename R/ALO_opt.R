#' @importFrom stats runif

initALO <- function(N,dim,ub,lb){
  bound_no <- ncol(ub)
  if (bound_no == 1){
    X <- matrix(runif(N*dim),N,dim)* (ub-lb) + lb
  }
  if (bound_no > 1){
    X <- matrix(NA, nrow = N, ncol = dim)
    for (i in 1:dim){
      ub_i <- ub[i]
      lb_i <- lb[i]
      X[,i] <- matrix(runif(1*N), 1, N) * (ub_i - lb_i) + lb_i
    }
  }
  return(X)
}

Random_walk_around_antlion <- function(dim, Max_iter, lb, ub, antlion, current_iter){
  if (nrow(lb) == 1 & ncol(lb) == 1) {
    lb <- matrix(1, 1, dim) %*% lb
    ub <- matrix(1, 1, dim) %*% ub
  }
  if (nrow(lb)>ncol(lb)){
    lb <- t(lb)
    ub <- t(ub)
  }

  I<-1
  if (current_iter > Max_iter/10){
    I <- 1+100*(current_iter/Max_iter)
  }
  if (current_iter > Max_iter/2){
    I<- 1+1000*(current_iter/Max_iter)
  }
  if (current_iter > Max_iter*(3/4)){
    I <- 1+10000*(current_iter/Max_iter)
  }
  if (current_iter > Max_iter*(0.9)){
    I <- 1+100000*(current_iter/Max_iter)
  }
  if (current_iter > Max_iter*(0.95)){
    I <- 1+1000000*(current_iter/Max_iter)
  }

  lb <- lb/I
  ub <- ub/I

  rand <- runif(1)

  if (rand <0.5){
    lb <- lb + antlion
  } else {
    lb <- -lb + antlion
  }

  if (rand >= 0.5){
    ub <- ub + antlion
  } else {
    ub <- -ub + antlion
  }

  RWs <- matrix(NA, nrow= Max_iter, ncol= dim)
  for (i in 1:dim){
    X<- cbind(t(colSums(2%*%(as.numeric(runif(Max_iter) > 0.5)) - 1)))
    a <- min(X)
    b <- max(X)
    c <- lb[i]
    d <- ub[i]
    X_norm <- ((X-a) * (d-c)) / (b-a) + c
    RWs[,i] <-X_norm
  }

  return(RWs)
}

RouletteWheelSelection <- function(weights){
  accumulation <- sum(weights)
  p<- runif(1) %*% accumulation[length(accumulation)]
  chosen_index <- -1
  for (index in 1:length(accumulation)){
    if (accumulation[index] > p){
      chosen_index <- index
      break;
    }
  }
  choice <- chosen_index
  return(choice)
}

ALO <- function(N, Max_iter, lb, ub, dim, fobj){
  antlion_position <- initALO(N,dim,ub,lb)
  ant_position <- initALO(N,dim,ub,lb)
  Sorted_antlions <- matrix(0, nrow = N, ncol = dim)
  Elite_antlion_position <- matrix(0, nrow = 1, ncol= dim)
  Elite_antlion_fitness <- Inf
  Convergence_curve <- matrix(0, nrow = 1, ncol=Max_iter)
  antlions_fitness <- matrix(0, nrow = 1, ncol=N)
  ants_fitness <- matrix(0, nrow = 1, ncol=N)

  for (i in 1:nrow(antlion_position)){
    antlions_fitness[1,i] <- fobj(abs(antlion_position[i,]))
  }

  sorted_antlion_fitness <- sort(antlions_fitness)
  sorted_indexes <- order(antlions_fitness)

  for (newindex in 1:N) {
    Sorted_antlions[newindex,] <- antlion_position[sorted_indexes[newindex],]
  }

  Elite_antlion_position <- Sorted_antlions[1,]
  Elite_antlion_fitness <- sorted_antlion_fitness[1]
  Convergence_curve[1] <- Elite_antlion_fitness
  param <- c(Elite_antlion_position)
  cat("At iteration 1 the best fitness is", Elite_antlion_fitness,"\n")

  Current_iter <- 2
  objective_history <- matrix(0L, nrow = Max_iter, ncol = 1)
  bound <- 0
  while (Current_iter < Max_iter+1) {
    for (i in 1:nrow(ant_position)){
      Rolette_index <- RouletteWheelSelection(1/sorted_antlion_fitness)
      if (Rolette_index == -1){
        Rolette_index = 1
      }

      RA <- Random_walk_around_antlion(dim,Max_iter,lb,ub,
                                       Sorted_antlions[Rolette_index,],Current_iter)
      RE <- Random_walk_around_antlion(dim,Max_iter,lb,ub,Elite_antlion_position,
                                       Current_iter)
      ant_position[i,] <- (RA[Current_iter,]+RE[Current_iter,])/2
    }

    for (i in 1:nrow(ant_position)){
      Flag4ub <- ant_position[i,]>ub
      Flag4lb <- ant_position[i,]<lb
      ant_position[i,] <- (ant_position[i,] * (as.numeric(!(Flag4ub+Flag4lb))))+
        ub*as.numeric(Flag4ub) + lb*as.numeric(Flag4lb)
      ants_fitness[1,i] <- fobj(abs(ant_position[i,]))
    }

    double_population <- rbind(Sorted_antlions, ant_position)
    double_fitness <- c(sorted_antlion_fitness, ants_fitness)

    double_fitness_sorted <- sort(double_fitness)
    I <- order(double_fitness)

    double_sorted_population <- double_population[I,]

    antlions_fitness <- double_fitness_sorted[1:N]
    Sorted_antlions <- double_sorted_population[1:N,]

    if (antlions_fitness[1] < Elite_antlion_fitness){
      Elite_antlion_position <- Sorted_antlions[1,]
      Elite_antlion_fitness <- antlions_fitness[1]
    }

    Sorted_antlions[1,] <- Elite_antlion_position
    antlions_fitness[1] <- Elite_antlion_fitness

    Convergence_curve[Current_iter] <- Elite_antlion_fitness
    param <- c(param, Elite_antlion_position)

    cat("At iteration", Current_iter,"the best fitness is",Elite_antlion_fitness,"\n")

    objective_history[Current_iter,] <- Elite_antlion_fitness
    if (Current_iter >2){
      if (objective_history[Current_iter-1,]-Elite_antlion_fitness <= 0.00001 &
          objective_history[Current_iter-1,]-Elite_antlion_fitness >= 0) {
        bound <- bound + 1
      } else {
        bound <- 0
      }
    }

    if (bound == 10) {
      break;
    }

    Current_iter <- Current_iter+1;
  }

  param <- matrix(param, nrow = dim)
  res <-list(best_fitness = Elite_antlion_fitness, best_position = Elite_antlion_position,
             jml_iter = Current_iter, param=param, param_list = Convergence_curve)

  return(res)
}
