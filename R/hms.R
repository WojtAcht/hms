#' Title
#'
#' @param max_tree_height - numeric - default value: 5
#' @param fitness - fitness function
#' @param lower - numeric - lower bound
#' @param upper - numeric - upper bound
#' @param sigma - numeric - vector of standard deviations for each tree level
#' @param population_size - numeric
#' @param run_metaepoch - function - returns list with named fields: solution, population, value
#' @param global_stopping_condition - function
#' @param local_stopping_condition - function
#' @param sprouting_condition - function
#'
#' @return numeric best solution
#' @export
#'
#' @examples
hms <- function(max_tree_height = 5,
                fitness,
                lower,
                upper,
                sigma,
                population_size,
                run_metaepoch = ga_metaepoch(list(list(), list(), list(), list(), list())), # TODO :)
                global_stopping_condition = default_global_stopping_condition,
                local_stopping_condition = default_local_stopping_condition,
                sprouting_condition = max_euclidean_distance_sprouting_condition(0.5)) {
  root <- create_deme(lower, upper, NULL, population_size, sigma)
  active_demes <- c(root)
  best_solution <- -Inf
  best_fitness <- -Inf
  metaepochs_count <- 0
  while (!global_stopping_condition(metaepochs_count, 0, 0) && length(active_demes) > 0) {
    new_demes <- c()
    for (deme in active_demes) {
      metaepoch_result <- run_metaepoch(fitness, deme@population, lower, upper)
      deme <- update_deme(metaepoch_result, deme)
      if (local_stopping_condition(deme)) {
        if (deme@best_fitness > best_fitness) {
          best_fitness <- deme@best_fitness
          best_solution <- deme@best_solution
        }
        next
      }
      new_demes <- c(new_demes, deme)
      if (deme@level > max_tree_height) next
      level_demes <- Filter(function(d) {
        d@level == deme@level
      }, active_demes)
      if (sprouting_condition(metaepoch_result$solution, level_demes)) {
        new_deme <- create_deme(lower, upper, deme, population_size, sigma)
        new_demes <- c(new_demes, new_deme)
      }
    }
    active_demes <- new_demes
    metaepochs_count <- metaepochs_count + 1
  }
  for (deme in active_demes) {
    if (length(deme@best_fitness) != 0 && deme@best_fitness > best_fitness) {
      best_fitness <- deme@best_fitness
      best_solution <- deme@best_solution
    }
  }
  best_solution
}
