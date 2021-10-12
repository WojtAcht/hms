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
                population_size = 20,
                run_metaepoch = ga_metaepoch(list(list(), list(), list(), list(), list())), # TODO :)
                global_stopping_condition = default_global_stopping_condition,
                local_stopping_condition = default_local_stopping_condition,
                sprouting_condition = max_euclidean_distance_sprouting_condition(0.5)) {
  if (max_tree_height < 2) {
    stop("Max tree height has to be greater or equal 2.")
  }
  if (!is.function(fitness)) {
    stop("Fitness function must be provided.")
  }
  if (missing(lower) | missing(upper)) {
    stop("A lower and upper range of values must be provided.")
  }
  if (!is.vector(sigma) & !is.list(sigma)) {
    stop("A list of standard deviations (sigma) must be provided.")
  }
  if (!length(sigma) >= max_tree_height) {
    stop("The list of standard deviations (sigma) must have max_tree_height elements.")
  }
  total_metaepoch_time <- 0
  start_time <- Sys.time()
  root <- create_deme(lower, upper, NULL, population_size, sigma)
  active_demes <- c(root)
  inactive_demes <- c()
  best_solution <- -Inf
  best_fitness <- -Inf
  metaepochs_count <- 0
  while (!global_stopping_condition(metaepochs_count, 0, 0) && length(active_demes) > 0) {
    new_demes <- c()
    for (deme in active_demes) {
      start_metaepoch_time <- Sys.time()
      metaepoch_result <- run_metaepoch(fitness, deme@population, lower, upper, deme@level)
      end_metaepoch_time <- Sys.time()
      total_metaepoch_time <- total_metaepoch_time + (end_metaepoch_time - start_metaepoch_time)
      deme <- update_deme(metaepoch_result, deme)
      if (local_stopping_condition(deme)) {
        if (deme@best_fitness > best_fitness) {
          best_fitness <- deme@best_fitness
          best_solution <- deme@best_solution
        }
        inactive_demes <- c(inactive_demes, deme)
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
  total_time <- Sys.time() - start_time
  new("hms",
    root_id = root@id,
    demes = c(active_demes, inactive_demes),
    best_fitness = best_fitness,
    best_solution = best_solution,
    total_time = total_time,
    total_metaepoch_time = total_metaepoch_time
  )
}

setClass("hms", slots = c(
  root_id = "character",
  demes = "list",
  best_fitness = "numeric",
  best_solution = "numeric",
  total_time = "difftime",
  total_metaepoch_time = "difftime"
))

setGeneric("printTree", function(object) standardGeneric("printTree"))

setMethod("printTree", "hms", function(object) {
  cat("A tree :)")
})

plot.hms <- function(x, y, ylim, cex.points = 0.7,
                     col = c("green3", "dodgerblue3", adjustcolor("green3", alpha.f = 0.1)),
                     pch = c(16, 1), lty = c(1, 2), legend = TRUE,
                     grid = graphics:::grid, ...) {
  cat("TODO")
}

setMethod("plot", "hms", plot.hms)
