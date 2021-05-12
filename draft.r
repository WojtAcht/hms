library("GA")
library("purrr")
library("msm")
library("magrittr")

lbound <- c(0, 0, 0)
ubound <- c(5, 5, 5)
deme_population_size <- 50
sigma <- c(1,1,1,1,1)
accuracy <- c(2,2,2,2,2)
# We need to handle vector of objects representing GA params like: maxiter, mutationType etc

setClass("Deme", slots = c(
  population = "matrix",
  level = "numeric",
  search_accuracy = "numeric",
  best_fitness = "numeric",
  best_solution = "numeric",
  metaepochs_since_last_improvement = "numeric"
))

rnorm_population <- function(mean, sd, lower, upper, population_size){
  random_coordinate <- function(i) {
    rtnorm(mean = mean[[i]],
           sd = sd[[i]],
           lower = lower[[i]],
           upper = upper[[i]],
           n = population_size)
  }
  mapply(random_coordinate, seq_along(lower))
}

runif_population <- function(lower, upper, population_size){
  random_coordinate <- function(i){
    runif(population_size, 
          min = lower[[i]], 
          max = upper[[i]])
  }
  mapply(random_coordinate, seq_along(lower))
}


create_deme <- function(lower, upper, population_size, level, initial_points=c()){
  population <- c()
  if(length(initial_points) == 0) {
    population <- runif_population(lower = lower,
                                   upper = upper,
                                   population_size = population_size)
  } 
  else {
    population <- rnorm_population(mean = initial_points[[1]],
                                   sd = sigma[[level]],
                                   lower = lower,
                                   upper = upper,
                                   population_size = population_size)
  }
  d <- new("Deme", 
           population = population, 
           level = level + 1, 
           search_accuracy = accuracy[[level]])
  return(d)
}

hms <- function(max_tree_height, f, lower, upper, sigma, accuracy, population_size){
  root <- create_deme(lower, upper, population_size, 1)
  active_demes <- c(root)
  parents <- mapply(function(x) c(), 0:max_tree_height)
  best_solution <- -Inf
  best_fitness <- -Inf
  for(i in 1:1000){
    new_demes <- c()
    print(length(active_demes))
    for(deme in active_demes){
      GA <- ga(type = "real-valued", 
               fitness = f, 
               suggestions = deme@population, 
               popSize = length(deme@population),
               lower = lower,
               upper = upper,
               maxiter = 10,
               keepBest = TRUE,
               monitor = FALSE)
      deme@population <- GA@population
      potential_parent <- tail(GA@bestSol, n=1)[[1]][[1]]
      current_best <- f(potential_parent)
      deme_best <- (if(length(deme@best_fitness)==0) -Inf else deme@best_fitness)
      if(current_best > deme_best) {
        deme@best_fitness <- current_best
        deme@best_solution <- potential_parent
        deme@metaepochs_since_last_improvement <- 0
      }
      else{
        deme@metaepochs_since_last_improvement <- deme@metaepochs_since_last_improvement + 1
      }
      if(deme@metaepochs_since_last_improvement > 5) {
        if(deme@best_fitness > best_fitness) {
          best_fitness <- deme@best_fitness
          best_solution <- deme@best_solution
        }
        next
      }
      new_demes <- c(new_demes, deme)
      if(deme@level > max_tree_height) next
      level_parents <- parents[[deme@level]]
      if(!any(abs(level_parents-potential_parent) < 1e5)){
        new_deme <- create_deme(lower, upper, population_size, deme@level, initial_points=potential_parent)
        new_demes <- c(new_demes, new_deme)
        parents[[deme@level]] <- c(parents[[deme@level]], potential_parent)
      }
    }   
    active_demes <- new_demes
  }
  for(deme in active_demes){
    if(length(deme@best_fitness) != 0 && deme@best_fitness > best_fitness) {
          best_fitness <- deme@best_fitness
          best_solution <- deme@best_solution
    }
  }
  best_solution
}