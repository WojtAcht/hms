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
  metaepoch_snapshots <- list()
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


    for (deme in active_demes) {
      if (length(deme@best_fitness) != 0 && deme@best_fitness > best_fitness) {
        best_fitness <- deme@best_fitness
        best_solution <- deme@best_solution
      }
    }

    previous_metaepochs_time = 0
    for (metaepoch_snapshot in metaepoch_snapshots) {
      previous_metaepochs_time <- previous_metaepochs_time + metaepoch_snapshot@time_in_seconds
    }

    snapshot <- methods::new("MetaepochSnapshot",
                             demes = c(active_demes, inactive_demes),
                             best_fitness = best_fitness,
                             best_solution = best_solution,
                             time_in_seconds = seconds_since(start_time) - previous_metaepochs_time)
    metaepoch_snapshots <- c(metaepoch_snapshots, snapshot)
    metaepochs_count <- metaepochs_count + 1
  }

  methods::new("hms",
    root_id = root@id,
    metaepoch_snapshots = metaepoch_snapshots,
    best_fitness = best_fitness,
    best_solution = best_solution,
    total_time_in_seconds = seconds_since(start_time),
    total_metaepoch_time_in_seconds = as.numeric(total_metaepoch_time, units = "secs"),
    metaepochs_count = metaepochs_count,
    deme_population_size = population_size,
    lower = lower,
    upper = upper,
    call = match.call()
  )
}

setClass("MetaepochSnapshot", slots = c(
  demes = "list",
  best_fitness = "numeric",
  best_solution = "numeric",
  time_in_seconds = "numeric"
))

setClass("hms", slots = c(
  root_id = "character",
  metaepoch_snapshots = "list",
  best_fitness = "numeric",
  best_solution = "numeric",
  total_time_in_seconds = "numeric",
  total_metaepoch_time_in_seconds = "numeric",
  metaepochs_count = "numeric",
  deme_population_size = "numeric",
  lower = "numeric",
  upper = "numeric",
  call = "language"
))

setMethod("print", "hms", function(x, ...) utils::str(x))

setMethod("show", "hms", function(object) {
  cat("An object of class \"hms\"\n")
  cat("\nCall:\n", deparse(object@call), "\n\n", sep="")
  cat("Available slots:\n")
  print(methods::slotNames(object))
})

setGeneric("printTree", function(object) standardGeneric("printTree"))

setMethod("printTree", "hms", function(object) {
  last_metaepoch_snapshot <- tail(object@metaepoch_snapshots, n = 1)
  if (length(last_metaepoch_snapshot) == 0) {
    return()
  }
  demes <- last_metaepoch_snapshot[[1]]@demes

  get_deme_by_id <- function(id) {
    Filter(function(deme) { deme@id == id }, demes)[[1]]
  }
  get_children <- function(deme) {
    Filter(function(d) { identical(d@parent_id, deme@id) }, demes)
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


summary.hms <- function(object, ...) {
  domain_element_to_string <- function(x) {
    rounded_params <- mapply(function(x) { sprintf(x, fmt = '%#.2f') }, x)
    comma_separated_params <- do.call(paste, c(as.list(rounded_params), sep = ", "))
    if(length(x) > 1) {
      paste("(", comma_separated_params, ")", sep = "")
    } else {
      comma_separated_params
    }
  }
  out <- list(fitness = object@best_fitness,
              solution = domain_element_to_string(object@best_solution),
              metaepochs = object@metaepochs_count,
              deme_population_size = object@deme_population_size,
              lower_bound = domain_element_to_string(object@lower),
              upper_bound = domain_element_to_string(object@upper),
              computation_time = paste(as.numeric(object@total_time_in_seconds), " seconds", sep = ""))
  class(out) <- "summary.hms"
  out
}

setMethod("summary", "hms", summary.hms)

plot.hms <- function(x) {
  cat("TODO")
}

setMethod("plot", "hms", plot.hms)
