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
  root <- create_deme(lower, upper, NULL, population_size, sigma)
  active_demes <- c(root)
  inactive_demes <- c()
  best_solution <- -Inf
  best_fitness <- -Inf
  metaepochs_count <- 0
  while (!global_stopping_condition(metaepochs_count, 0, 0) && length(active_demes) > 0) {
    new_demes <- c()
    for (deme in active_demes) {
      metaepoch_result <- run_metaepoch(fitness, deme@population, lower, upper, deme@level)
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
  methods::new("hms",
    root_id = root@id,
    demes = c(active_demes, inactive_demes),
    best_fitness = best_fitness,
    best_solution = best_solution
  )
}

setClass("hms", slots = c(
  root_id = "character",
  demes = "list",
  best_fitness = "numeric",
  best_solution = "numeric"
))

setGeneric("printTree", function(object) standardGeneric("printTree"))

setMethod("printTree", "hms", function(object) {
  get_deme_by_id <- function(id) {
    Filter(function(deme) { deme@id == id }, object@demes)[[1]]

  }
  get_children <- function(deme) {
    Filter(function(d) { identical(d@parent_id, deme@id) }, object@demes)
  }
  print_deme <- function(deme) {
    color <- if (deme@best_solution == object@best_solution) crayon::red else identity
    cat(color("f("))
    for(x in deme@best_solution) {
      if (x != deme@best_solution[[1]]) {
        cat(", ")
      }
      cat(color(sprintf(x, fmt = '%#.2f')))
    }
    cat(color(paste(") = ", sprintf(deme@best_fitness, fmt = '%#.2f'), "\n", sep = "")))
  }

  print_tree_from_deme <- function(deme, prefix = "") {
    children <- get_children(deme)
    for(child in children) {
      if (length(child@best_solution) == 0) {
        # This deme did not participate in any metaepoch
        next
      }
      is_last <- child@id == children[[length(children)]]@id
      cat(prefix)
      if (is_last) {
        cat("\u2514")
      } else {
        cat("\u251C")
      }
      cat("-- ")
      print_deme(child)
      print_tree_from_deme(child, prefix = paste(prefix, if(is_last) " " else "|", "   ", sep = ""))
    }
  }
  root <- get_deme_by_id(object@root_id)
  print_deme(root)
  print_tree_from_deme(root)
})

plot.hms <- function(x) {
  cat("TODO")
}

setMethod("plot", "hms", plot.hms)
