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

rnorm_population <- function(mean, sd, lower, upper, population_size) {
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

runif_population <- function(lower, upper, population_size) {
  random_coordinate <- function(i) {
    stats::runif(population_size,
      min = lower[[i]],
      max = upper[[i]]
    )
  }
  mapply(random_coordinate, seq_along(lower))
}

create_deme <- function(lower, upper, parent, population_size, sigma) {
  new_population <- c()
  new_deme_level <- ifelse(is.null(parent), 1, parent@level + 1)
  if (is.null(parent)) {
    new_population <- runif_population(
      lower = lower,
      upper = upper,
      population_size = population_size
    )
  } else {
    new_population <- rnorm_population(
      mean = parent@best_solution,
      sd = sigma[[new_deme_level]],
      lower = lower,
      upper = upper,
      population_size = population_size
    )
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
