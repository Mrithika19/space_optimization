
# Returns packing height as decoded by the specified heuristic routine
get_fitness <- function(data, orders, FUN = NA, strip_width){
  # data = data.frame of problem instance; id, height, width
  # orders = matrix of population of permutations of item orders
  # FUN = fitness function (heuristic)
  # strip_width = strip width
  
  FUN <- match.fun(FUN)
  
  fitness <- c()
  for(i in 1:ncol(orders)){
    fitness[i] <- heuristic(data[orders[, i], ], strip_width, FUN)$height
  }
  
  return(fitness)
}

temperature <- function(t0, current_temp, alpha, niter, schedule = c("geometric", "linear", "logarithmic", "lundymees")){
  # t0 = initial temperature of the optimization routine
  # current_temp = current temperature
  # alpha = cooling rate parameter
  # niter = iteration number of the temperature cycle
  # schedule = the function that governs the cooling behaviour
  if(schedule == "geometric" | schedule == 1){
    temp <- t0*alpha^niter
  } else if(schedule == "linear" | schedule == 2){
    temp <- t0 - alpha*niter
  } else if(schedule == "logarithmic" | schedule == 3){
    temp <- t0/(1+alpha*log(1+niter))
  } else if(schedule == "lundymees" | schedule == 4){
    temp <- current_temp/(1+alpha*current_temp)
  }
  return(temp)
}
