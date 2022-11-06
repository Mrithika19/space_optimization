# Hybrid Genetic Algorithm


# Function to sort items by desired criteria
sort_instance <- function(items, method = c("perimeter", "area", "width", "height"), decreasing = TRUE){
  if(decreasing){
    if(method == "perimeter" | method == 1){
      return(items[order(items$width + items$height, decreasing = TRUE), ])
    } else if(method == "area" | method == 2){
      return(items[order(items$width*items$height, decreasing = TRUE), ])
    } else if(method == "width" | method == 3){
      return(items[order(items$width, decreasing = TRUE), ])
    } else if(method == "height" | method == 4){
      return(items[order(items$height, decreasing = TRUE), ])
    }
  } else {
    if(method == "perimeter" | method == 1){
      return(items[order(items$width + items$height, decreasing = FALSE), ])
    } else if(method == "area" | method == 2){
      return(items[order(items$width*items$height, decreasing = FALSE), ])
    } else if(method == "width" | method == 3){
      return(items[order(items$width, decreasing = FALSE), ])
    } else if(method == "height" | method == 4){
      return(items[order(items$height, decreasing = FALSE), ])
    }
  }
}

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

get_population <- function(data, population_size, seed = TRUE){
  # data = data.frame with columns id, height and width of instance items
  # population_size = number of solutions in each generation
  # seed = whether or not the initial population is seeded with ordered items
  
  population <- matrix(data$id, nrow(data), population_size)
  population <- apply(population, 2, sample)
  
  # Seed initial population with good quality solutions
  n <- ifelse(population_size < 8, population_size, 8)
  if(seed){
    inc_dec <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
    ord <- rep(c("perimeter", "area", "width", "height"), 2)
    for(i in 1:n){
      population[, i] <- sort_instance(data, ord[i], inc_dec[i])$id
    }
  }
  
  return(population)
}

roulette_selection <- function(fitness){
  # fitness = data.frame with item id and fitness
  
  # Size of population
  n <- length(fitness)
  
  # Vector of subintervals based on proportional fitness
  wheel <- cumsum(prop.table(1/fitness))
  
  # Index of parent 1 and 2
  index <- sample(1:n, size = 2, replace = FALSE, prob = wheel)
  
  return(index)
}

tournament_selection <- function(fitness, tournament_size){
  
  ind <- 1:length(fitness)
  
  parent1 <- 0
  parent2 <- 0
  while(parent1 == parent2){
    # create a random subsamples of individuals to compete
    candidatesParent1 <- sample(ind, tournament_size, replace = TRUE)
    candidatesParent2 <- sample(ind, tournament_size, replace = TRUE)
    parent1 <- candidatesParent1[which.min(fitness[candidatesParent1])]
    parent2 <- candidatesParent2[which.min(fitness[candidatesParent2])]
  }
  
  return(c(parent1, parent2))
}

pmx <- function(ch1, ch2){
  
  n <- length(ch1)
  k <- sample(1:n, size = 2)
  k <- seq(min(k), max(k))
  
  offspring1 <- ch1
  offspring2 <- ch2
  
  offspring1[k] <- ch2[k]
  offspring2[k] <- ch1[k]
  
  while(sum(duplicated(c(offspring1[-k], ch2[k]))) > 0){
    for(i in setdiff(1:n, k)){
      if(offspring1[i] %in% ch2[k]){
        ind <- which(ch2[k] == offspring1[i])
        offspring1[i] <- ch1[k][ind]
      }
    }
  }
  
  while(sum(duplicated(c(offspring2[-k], ch1[k]))) > 0){
    for(i in setdiff(1:n, k)){
      if(offspring2[i] %in% ch1[k]){
        ind <- which(ch1[k] == offspring2[i])
        offspring2[i] <- ch2[k][ind]
      }
    }
  }
  
  return(data.frame("ch1" = offspring1, "ch2" = offspring2))
}

ox <- function(ch1, ch2){
  
  n <- length(ch1)
  k <- sample(1:n, 2)
  k <- seq(min(k), max(k))
  
  if(min(k) == 1 & max(k) == n){
    return(data.frame("ch1" = ch2, "ch2" = ch1))
  } else if(min(k) == 1 & max(k) != n){
    ordering1 <- ch1
    ordering2 <- ch2
  } else if(max(k) == n & min(k) != 1){
    ordering1 <- c(ch1[k], ch1[-k])
    ordering2 <- c(ch2[k], ch2[-k])
  } else if(min(k) != 1 & max(k) != n){
    ordering1 <- c(ch1[k], ch1[(max(k)+1):n], ch1[1:(min(k)-1)])
    ordering2 <- c(ch2[k], ch2[(max(k)+1):n], ch2[1:(min(k)-1)])
  }
  
  offspring1 <- rep(NA, n)
  offspring2 <- rep(NA, n)
  
  offspring1[k] <- ch2[k]
  offspring2[k] <- ch1[k]
  
  offspring1[is.na(offspring1)] <- setdiff(ordering1, offspring1)
  offspring2[is.na(offspring2)] <- setdiff(ordering2, offspring2)
  
  return(data.frame("ch1" = offspring1, "ch2" = offspring2))
}

swap_mutation <- function(data, orders, fitness, FUN, strip_width){
  
  chromosome <- sample(1:ncol(orders), 1)
  swap_index <- sample(1:nrow(orders), 2, replace = F)
  
  FUN <- match.fun(FUN)
  
  # Swap index's of selected chromosome
  orders[swap_index, chromosome] <- orders[rev(swap_index), chromosome]
  
  # Calculate fitness of new chromosome
  fitness[chromosome] <- heuristic(data[orders[, chromosome], ], strip_width, FUN)$height
  
  return(list("orders" = orders, "fitness" = fitness))
}


