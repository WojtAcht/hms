setClassUnion("numericOrNULL", members = c("numeric", "NULL"))
setClassUnion("characterOrNULL", members = c("character", "NULL"))

setClass("Deme", slots = c(
  id = "character",
  population = "matrix",
  level = "numeric",
  best_fitness = "numeric",
  best_solution = "numeric",
  best_solutions_per_metaepoch = "list",
  best_fitnesses_per_metaepoch = "list",
  sprout = "numericOrNULL",
  parent_id = "characterOrNULL",
  evaluations_count = "numeric",
  is_active = "logical"
))

rnorm_population <- function(mean, lower, upper, population_size, tree_level, sigma) {
  sd <- sigma[[tree_level]]
  random_coordinate <- function(i) {
    msm::rtnorm(
      mean = mean[[i]],
      sd = sd[[i]],
      lower = lower[[i]],
      upper = upper[[i]],
      n = population_size - 1
    )
  }
  population <- mapply(random_coordinate, seq_along(lower))
  rbind(population, mean)
}

runif_population <- function(mean, lower, upper, population_size, tree_level, sigma) {
  random_coordinate <- function(i) {
    stats::runif(population_size,
      min = lower[[i]],
      max = upper[[i]]
    )
  }
  mapply(random_coordinate, seq_along(lower))
}

default_create_population <- function(sigma) {
  function(mean, lower, upper, population_size, tree_level) {
    if (tree_level == 1) {
      runif_population(mean, lower, upper, population_size, tree_level, sigma)
    } else {
      rnorm_population(mean, lower, upper, population_size, tree_level, sigma)
    }
  }
}

create_deme <- function(lower, upper, parent, population_size, create_population) {
  new_deme_level <- ifelse(is.null(parent), 1, parent@level + 1)
  new_sprout <- if (is.null(parent)) {
    NULL
  } else {
    unlist(utils::tail(parent@best_solutions_per_metaepoch, n = 1))
  }
  new_population <- create_population(
    mean = new_sprout,
    lower = lower,
    upper = upper,
    population_size = population_size,
    tree_level = new_deme_level
  )
  if (any(dim(new_population) != c(population_size, length(lower)))) {
    stop("Created population is invalid - wrong dimensions.")
  }
  methods::new("Deme",
    population = new_population,
    level = new_deme_level,
    sprout = new_sprout,
    id = uuid::UUIDgenerate(),
    parent_id = if (is.null(parent)) NULL else parent@id,
    evaluations_count = 0,
    is_active = TRUE
  )
}

update_deme <- function(metaepoch_result, deme, minimize = FALSE) {
  if (is.null(metaepoch_result$solution) |
    is.null(metaepoch_result$value) |
    is.null(metaepoch_result$population) |
    !is.numeric(metaepoch_result$solution) |
    !is.numeric(metaepoch_result$value) |
    !is.numeric(metaepoch_result$population)) {
    stop("The run_metaepoch function must return a list with following named parameters of type numeric: solution, value, population")
  }
  if (any(dim(metaepoch_result$population) != dim(deme@population))) {
    stop("The run_metaepoch function must return population with matching dimensions.")
  }
  if (length(metaepoch_result$solution) != dim(deme@population)[[2]]) {
    stop("The run_metaepoch function must return solution with matching dimensions.")
  }
  if (length(metaepoch_result$value) != 1) {
    stop("The run_metaepoch function must return 1D value.")
  }

  potential_sprout <- metaepoch_result$solution
  metaepoch_best <- metaepoch_result$value
  deme@population <- metaepoch_result$population
  deme@best_fitnesses_per_metaepoch <- c(deme@best_fitnesses_per_metaepoch, list(metaepoch_best))
  deme@best_solutions_per_metaepoch <- c(deme@best_solutions_per_metaepoch, list(potential_sprout))
  min_value <- ifelse(minimize, Inf, -Inf)
  deme_best <- ifelse(length(deme@best_fitness) == 0, min_value, deme@best_fitness)
  operator <- ifelse(minimize, `<`, `>`)
  if (operator(metaepoch_best, deme_best)) {
    deme@best_fitness <- metaepoch_best
    deme@best_solution <- potential_sprout
  }
  deme
}

is_leaf <- function(deme, tree_height) {
  length(deme@best_fitness) != 0 & deme@level == tree_height
}

is_root <- function(deme) {
  deme@level == 1
}
