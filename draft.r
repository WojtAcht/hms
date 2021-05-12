library("GA")
library("purrr")
library("msm")
library("magrittr")

lbound <- c(0, 0, 0)
ubound <- c(5, 5, 5)
deme_population_size <- 50
sigma <- c(1,1,1,1,1)
accuracy <- c(2,2,2,2,2)
global_stopping_condition <- TRUE
# We need to handle vector of objects representing GA params like: maxiter, mutationType etc

setClass("Deme", slots = c(
  population = "numeric",
  level = "numeric",
  search_accuracy = "numeric"
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

hms <- function(max_tree_height, f, lower, upper, sigma, accuracy){
  root <- create_deme(1)
  active_demes <- c(root)
  while(global_stopping_condition){
    for(deme in active_demes){
      GA <- ga(type = "real-valued", 
               fitness = f, 
               suggestions = deme, 
               popSize = length(deme@population),
               lower = lower,
               upper = upper,
               maxiter = 2)
      deme@population <- GA@population
    }
  }
}