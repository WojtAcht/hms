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

setClassUnion("DemeOrNULL", members = c("Deme", "NULL"))

setClass("Deme", slots = c(
  population = "matrix",
  level = "numeric",
  best_fitness = "numeric",
  best_solution = "numeric",
  parent = "DemeOrNULL",
  best_fitnesses_per_metaepoch = "numeric"
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


create_deme <- function(lower, upper, parent, population_size) {
  population <- c()
  new_deme_level <- ifelse(is.null(parent), 1, parent@level + 1)
  if(is.null(parent)) {
    population <- runif_population(lower = lower,
                                   upper = upper,
                                   population_size = population_size)
  } 
  else {
    population <- rnorm_population(mean = parent@best_solution,
                                   sd = sigma[[new_deme_level]],
                                   lower = lower,
                                   upper = upper,
                                   population_size = population_size)
  }
  d <- new("Deme", 
           population = population, 
           level = new_deme_level,
           parent = NULL)
  return(d)
}

do_ga_metaeoch <- function(fitness, population, lower, upper) {
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
}