setClassUnion("numericOrNULL", members = c("numeric", "NULL"))
setClassUnion("characterOrNULL", members = c("character", "NULL"))

setClass("Deme", slots = c(
  id = "character",
  population = "matrix",
  level = "numeric",
  best_fitness = "numeric",
  best_solution = "numeric",
  best_solutions_per_metaepoch = "numeric",
  best_fitnesses_per_metaepoch = "numeric",
  sprout = "numericOrNULL",
  parent_id = "characterOrNULL"
))

rnorm_population <- function(mean, sigma, lower, upper, population_size, tree_level) {
  sd <- sigma[[tree_level]]
  random_coordinate <- function(i) {
    msm::rtnorm(
      mean = mean[[i]],
      sd = sd[[i]],
      lower = lower[[i]],
      upper = upper[[i]],
      n = population_size
    )
  }
  mapply(random_coordinate, seq_along(lower))
}

runif_population <- function(mean, sigma, lower, upper, population_size, tree_level) {
  random_coordinate <- function(i) {
    stats::runif(population_size,
      min = lower[[i]],
      max = upper[[i]]
    )
  }
  mapply(random_coordinate, seq_along(lower))
}

default_create_population <- function(mean, sigma, lower, upper, population_size, tree_level) {
  if(tree_level == 1){
    runif_population(mean, sigma, lower, upper, population_size, tree_level)
  }
  else{
    rnorm_population(mean, sigma, lower, upper, population_size, tree_level)
  }
}

create_deme <- function(lower, upper, parent, population_size, sigma, create_population) {
  new_deme_level <- ifelse(is.null(parent), 1, parent@level + 1)
  new_population <- create_population(
    mean = parent@best_solution,
    sigma = sigma,
    lower = lower,
    upper = upper,
    population_size = population_size,
    tree_level = new_deme_level
  )
  if (dim(new_population)[[1]] != population_size |
      dim(new_population)[[2]] !=  length(lower)){
    stop("Created population is invalid - wrong dimensions.")
  }
  new_sprout <- if (is.null(parent)) {
    NULL
  } else {
    utils::tail(parent@best_solutions_per_metaepoch, n = 1)
  }
  methods::new("Deme",
    population = new_population,
    level = new_deme_level,
    sprout = new_sprout,
    id = uuid::UUIDgenerate(),
    parent_id = if (is.null(parent)) NULL else parent@id
  )
}

update_deme <- function(metaepoch_result, deme) {
  if(is.null(metaepoch_result$solution) |
     is.null(metaepoch_result$value) |
     is.null(metaepoch_result$population) |
     !is.numeric(metaepoch_result$solution) |
     !is.numeric(metaepoch_result$value) |
     !is.numeric(metaepoch_result$population)) {
    stop("The run_metaepoch function must return a list with following named parameters of type numeric: solution, value, population")
  }

  potential_sprout <- metaepoch_result$solution
  metaepoch_best <- metaepoch_result$value
  deme@population <- metaepoch_result$population
  deme@best_fitnesses_per_metaepoch <- c(deme@best_fitnesses_per_metaepoch, metaepoch_best)
  deme@best_solutions_per_metaepoch <- c(deme@best_solutions_per_metaepoch, potential_sprout)
  deme_best <- ifelse(length(deme@best_fitness) == 0, -Inf, deme@best_fitness)
  if (metaepoch_best > deme_best) {
    deme@best_fitness <- metaepoch_best
    deme@best_solution <- potential_sprout
  }
  deme
}
