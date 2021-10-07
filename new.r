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
  best_solutions_per_metaepoch = "numeric",
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
  new("Deme", 
      population = population, 
      level = new_deme_level,
      parent = NULL)
}

ga_metaepoch <- function(config_ga) {
  function(fitness, suggestions, lower, upper, tree_level) {
    config <- config_ga[[tree_level]]
    legal_passed_param_names <- Filter(function(name) { name %in% formalArgs(ga) }, config)
    params <- list()
    for(param_name in legal_passed_param_names) { 
      params[param_name] <- passed_params[param_name]
    } 
    GA <- ga(type = "real-valued", 
             fitness = fitness, 
             lower = lower,
             upper = upper,
             # population = gaControl(type)$population,
             # selection = gaControl(type)$selection,
             # crossover = gaControl(type)$crossover,
             # mutation = gaControl(type)$mutation,
             # pcrossover = 0.8,
             # pmutation = 0.1,
             suggestions = suggestions, 
             popSize = length(suggestions),
             maxiter = 10, # To powinno być parametryzowane
             # run = 10, # To też powinno być parametryzowane
             keepBest = TRUE,
             monitor = FALSE)
    list("parent" = GA@solution, "population" = GA@population, "parent_value" = GA@fitnessValue)
  }
}

local_stopping_condition <- function(deme) {
  max_metaepochs_without_improvement <- 5
  best_fitness_metaepoch <- match(deme@best_fintess, deme@best_fitnesses_per_metaepoch)
  metaepoch_count <- length(deme@best_fitnesses_per_metaepoch)
  best_fitness_metaepoch < metaepoch_count - max_metaepochs_without_improvement
}

local_stopping_condition2 <- function(deme) {
  max_metaepochs_without_improvement <- 5
  last_five_best_fitness <- tail(deme@best_fitnesses_per_metaepoch, n=max_metaepochs_without_improvement)
  all(sort(last_five_best_fitness) == last_five_best_fitness)
}

global_stopping_condition <- function(execution_time) {

}

hms <- function(max_tree_height, f, lower, upper, sigma, accuracy, population_size){
  root <- create_deme(lower, upper, NULL, population_size)
  active_demes <- c(root)
  parents <- mapply(function(x) c(), 0:max_tree_height)
  best_solution <- -Inf
  best_fitness <- -Inf
  for(i in 1:1000){
    new_demes <- c()
    for(deme in active_demes){
      metaepoch_result <- do_ga_metaepoch(fitness, deme@population, lower, upper)
      ### UPDATE DEME:
      potential_parent <- metaepoch_result$parent
      current_best <- metaepoch_result$parent_value
      deme@population <- metaepoch_result$population
      deme@best_fitnesses_per_metaepoch <- c(deme@best_fitnesses_per_metaepoch, current_best)
      deme@best_solutions_per_metaepoch <- c(deme@best_solutions_per_metaepoch, parent)
      deme_best <- ifelse(length(deme@best_fitness)==0), -Inf, deme@best_fitness)
      if(current_best > deme_best) {
        deme@best_fitness <- current_best
        deme@best_solution <- potential_parent
      }
      new_demes <- c(new_demes, deme)
      if(local_stopping_condition(deme)){
        if(deme@best_fitness > best_fitness) {
          best_fitness <- deme@best_fitness
          best_solution <- deme@best_solution
        }
        next
      }
      if(deme@level > max_tree_height) next
      level_parents <- parents[[deme@level]]
      sprouting_condition <- function (level_parent) {euclidean_distance(level_parent, potential_parent) < 0.5}
      if(is.null(level_parents) || length(Filter(sprouting_condition, level_parents)) == 0){
        new_deme <- create_deme(lower, upper, population_size, deme@level, initial_points=potential_parent)
        new_demes <- c(new_demes, new_deme)
        parents[[deme@level]] <- c(parents[[deme@level]], list(potential_parent))
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
