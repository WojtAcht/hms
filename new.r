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
setClassUnion("numericOrNULL", members = c("numeric", "NULL"))

setClass("Deme", slots = c(
  population = "matrix",
  level = "numeric",
  best_fitness = "numeric",
  best_solution = "numeric",
  parent = "DemeOrNULL",
  best_solutions_per_metaepoch = "numeric",
  best_fitnesses_per_metaepoch = "numeric",
  sprout = "numeric"
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
        sprout = ifelse(is.null(parent), NULL, parent@best_solution)
        parent = parent)
}


max_euclidean_distance_sprouting_condition <- function(max_distance) {
    function(potential_sprout, level_demes) {
        single_deme_condition <- function (deme) {
            if(is.null(deme@sprout)) FALSE
            else euclidean_distance(deme@sprout, potential_parent) < max_distance
        }
        length(Filter(single_deme_condition, level_demes)) == 0
    }
}

ga_metaepoch <- function(config_ga) {
  function(fitness, suggestions, lower, upper, tree_level) {
    config <- config_ga[tree_level]
    legal_passed_param_names <- Filter(function(name) { name %in% formalArgs(ga) }, config)
    params <- list("maxiter" = 10)
    for(param_name in legal_passed_param_names) { 
      params[param_name] <- passed_params[param_name]
    } 
    params$fitness = fitness
    params$lower = lower
    params$upper = upper
    params$suggestions = suggestions
    params$monitor = FALSE

    # TODO Always better to have more data :)
    params$keepBest = TRUE

    GA <- do.call(GA::ga, params)
    list("solution" = GA@solution, "population" = GA@population, "value" = GA@fitnessValue)
  }
}

local_stopping_condition <- function(deme) {
  max_metaepochs_without_improvement <- 5
  best_fitness_metaepoch <- match(deme@best_fintess, deme@best_fitnesses_per_metaepoch)
  metaepoch_count <- length(deme@best_fitnesses_per_metaepoch)
  best_fitness_metaepoch < metaepoch_count - max_metaepochs_without_improvement
}

global_stopping_condition <- function(metaepochs,  execution_time) {

}

hms <- function(
    max_tree_height = 5,
    fitness,
    lower,
    upper,
    sigma,
    population_size,
    run_metaepoch = ga_metaepoch(list(list(), list(), list(), list(), list())), # TODO :)
    global_stopping_condition = global_stopping_condition,
    local_stopping_condition = local_stopping_condition,
    sprouting_condition = max_euclidean_distance_sprouting_condition(0.5),
){
  root <- create_deme(lower, upper, NULL, population_size)
  active_demes <- c(root)
  best_solution <- -Inf
  best_fitness <- -Inf
  for(i in 1:1000) {
    new_demes <- c()
    for(deme in active_demes) {
      metaepoch_result <- run_metaepoch(fitness, deme@population, lower, upper)
      ### UPDATE DEME:
      potential_sprout <- metaepoch_result$solution
      metaepoch_best <- metaepoch_result$value
      deme@population <- metaepoch_result$population
      deme@best_fitnesses_per_metaepoch <- c(deme@best_fitnesses_per_metaepoch, metaepoch_best)
      deme@best_solutions_per_metaepoch <- c(deme@best_solutions_per_metaepoch, parent)
      deme_best <- ifelse(length(deme@best_fitness)==0), -Inf, deme@best_fitness)
      if(metaepoch_best > deme_best) {
        deme@best_fitness <- metaepoch_best
        deme@best_solution <- potential_sprout
      }
      
      if(local_stopping_condition(deme)){
        if(deme@best_fitness > best_fitness) {
          best_fitness <- deme@best_fitness
          best_solution <- deme@best_solution
        }
        next
      }
      new_demes <- c(new_demes, deme)

      if(deme@level > max_tree_height) next
      
      level_demes <- Filter(function(d) { d@level == deme@level }, active_demes)
      if(sprouting_condition(potential_sprout, level_demes))
        new_deme <- create_deme(lower, upper, population_size, deme@level, initial_points=potential_parent)
        new_demes <- c(new_demes, new_deme)
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
