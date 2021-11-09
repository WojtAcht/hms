#' Title
#'
#' @param tree_height - numeric - default value: 5
#' @param fitness - fitness function
#' @param lower - numeric - lower bound
#' @param upper - numeric - upper bound
#' @param sigma - numeric - vector of standard deviations for each tree level
#' @param population_size - numeric
#' @param run_metaepoch - function - returns list with named fields: solution, population, value
#' @param global_stopping_condition - function
#' @param local_stopping_condition - function
#' @param sprouting_condition - function
#' @param create_population - function
#' @param suggestions - matrix
#' @param with_gradient_method - logical
#' @param gradient_method_args - list of parameters that are passed to the gradient method
#' @param run_gradient_method - function - returns list with named fields: solution, population, value
#'
#' @return numeric best solution
#' @export
#'
#' @examples
hms <- function(tree_height = 5,
                fitness,
                lower,
                upper,
                sigma,
                population_size_per_tree_level = rep(10, tree_level),
                run_metaepoch = ga_metaepoch(list(list(), list(), list(), list(), list())), # TODO :)
                global_stopping_condition = default_global_stopping_condition,
                local_stopping_condition = default_local_stopping_condition,
                sprouting_condition = max_metric_sprouting_condition(euclidean_distance, 0.5),
                create_population,
                suggestions = NULL,
                with_gradient_method = FALSE,
                gradient_method_args = list(
                  method = "L-BFGS-B",
                  poptim = 0.05,
                  pressel = 0.5,
                  control = list(fnscale = -1, maxit = 100)
                ),
                run_gradient_method,
                monitor = FALSE) {
  if (tree_height < 1) {
    stop("Max tree height has to be greater or equal 1.")
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
  if (!length(sigma) >= tree_height) {
    stop("The list of standard deviations (sigma) must have tree_height elements.")
  }
  if (!is.vector(population_size_per_tree_level) & !is.list(population_size_per_tree_level)) {
    stop("population_size_per_tree_level must be a list or a vector.")
  }
  if (!length(population_size_per_tree_level) >= tree_height) {
    stop("population_size_per_tree_level must have at least tree_height elements.")
  }
  if (!missing(suggestions) & !is.matrix(suggestions)) {
    stop("Invalid object for argument \"suggestions\": should be or extend class matrix.")
  }
  if (!missing(suggestions) & any(dim(suggestions) != c(population_size_per_tree_level[[1]], length(lower)))) {
    stop("Provided suggestions have wrong dimensions.")
  }
  if (missing(create_population) & missing(sigma)) {
    stop("A list of standard deviations (sigma) or a function to create population must be provided.")
  }
  if (missing(create_population)) {
    create_population <- default_create_population(sigma)
  }
  if (with_gradient_method & missing(run_gradient_method)) {
    gradient_method_args$lower <- lower
    gradient_method_args$upper <- upper
    if (gradient_method_args$poptim & (gradient_method_args$poptim < 0 | gradient_method_args$poptim > 1)) {
      stop("gradient_method_args: poptim value has to be within [0,1].")
    }
    gradient_method_args$control$maxit <- as.integer(gradient_method_args$control$maxit)
    if (is.null(gradient_method_args$control$fnscale)) {
      gradient_method_args$control$fnscale <- -1
    }
    if (gradient_method_args$control$fnscale > 0) {
      warning("gradient_method_args: fnscale should not be positive.")
      gradient_method_args$control$fnscale <- -1 * gradient_method_args$control$fnscale
    }
    run_gradient_method <- default_run_gradient_method
  }
  root <- if (is.null(suggestions)) {
    create_deme(lower, upper, NULL, population_size_per_tree_level[[1]], create_population)
  } else {
    methods::new("Deme",
      population = suggestions,
      level = 1,
      sprout = NULL,
      id = uuid::UUIDgenerate(),
      parent_id = NULL
    )
  }
  total_metaepoch_time <- 0
  start_time <- Sys.time()
  active_demes <- c(root)
  inactive_demes <- c()
  best_solution <- -Inf
  best_fitness <- -Inf
  metaepochs_count <- 0
  metaepoch_snapshots <- list()
  fitness_eavaluations_count <- 0
  f <- function(x) {
    fitness_eavaluations_count <<- fitness_eavaluations_count + 1
    fitness(x)
  }
  while (!global_stopping_condition(metaepoch_snapshots) && length(active_demes) > 0) {
    new_demes <- c()
    for (deme in active_demes) {
      start_metaepoch_time <- Sys.time()
      deme_evaluations_count <- 0
      deme_f <- function(x) {
        deme_evaluations_count <<- deme_evaluations_count + 1
        f(x)
      }
      metaepoch_result <- run_metaepoch(deme_f, deme@population, lower, upper, deme@level)
      end_metaepoch_time <- Sys.time()
      total_metaepoch_time <- total_metaepoch_time + (end_metaepoch_time - start_metaepoch_time)
      deme <- update_deme(metaepoch_result, deme)
      deme@evaluations_count <- deme@evaluations_count + deme_evaluations_count
      if (local_stopping_condition(deme, metaepoch_snapshots)) {
        if (deme@best_fitness > best_fitness) {
          best_fitness <- deme@best_fitness
          best_solution <- deme@best_solution
        }
        inactive_demes <- c(inactive_demes, deme)
        next
      }
      new_demes <- c(new_demes, deme)
      if (deme@level >= tree_height) next
      if (sprouting_condition(metaepoch_result$solution, deme@level + 1, c(active_demes, inactive_demes))) {
        new_deme <- create_deme(lower, upper, deme, population_size_per_tree_level[[deme@level + 1]], create_population)
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

    previous_metaepochs_time <- 0
    for (metaepoch_snapshot in metaepoch_snapshots) {
      previous_metaepochs_time <- previous_metaepochs_time + metaepoch_snapshot@time_in_seconds
    }

    snapshot <- methods::new("MetaepochSnapshot",
      demes = c(active_demes, inactive_demes),
      best_fitness = best_fitness,
      best_solution = best_solution,
      time_in_seconds = seconds_since(start_time) - previous_metaepochs_time,
      fitness_evaluations = fitness_eavaluations_count,
      is_evolutionary = TRUE
    )
    if (monitor) {
      cat("Metaepoch: ", metaepochs_count, "\n")
      prind(snapshot@demes, root@id, best_solution)
      cat("\n\n")
    }
    metaepoch_snapshots <- c(metaepoch_snapshots, snapshot)
    metaepochs_count <- metaepochs_count + 1
  }

  # Gradient methods:
  if (with_gradient_method) {
    last_metaepoch_snapshot <- utils::tail(metaepoch_snapshots, n = 1)
    leaves <- last_metaepoch_snapshot[[1]]@demes
    leaves_after_gradient_method <- c()
    for (deme in leaves) {
      if (length(deme@best_fitness) != 0) {
        result <- run_gradient_method(deme, fitness, gradient_method_args)
        deme_after_gradient_method <- update_deme(result, deme)
        leaves_after_gradient_method <- c(leaves_after_gradient_method, deme_after_gradient_method)
        if (deme_after_gradient_method@best_fitness > best_fitness) {
          best_fitness <- deme_after_gradient_method@best_fitness
          best_solution <- deme_after_gradient_method@best_solution
        }
      } else {
        leaves_after_gradient_method <- c(leaves_after_gradient_method, deme)
      }
    }
    # TODO: dodać jakąś funkcję na to
    previous_metaepochs_time <- 0
    for (metaepoch_snapshot in metaepoch_snapshots) {
      previous_metaepochs_time <- previous_metaepochs_time + metaepoch_snapshot@time_in_seconds
    }
    snapshot <- methods::new("MetaepochSnapshot",
      demes = leaves_after_gradient_method,
      best_fitness = best_fitness,
      best_solution = best_solution,
      time_in_seconds = seconds_since(start_time) - previous_metaepochs_time,
      fitness_evaluations = 0, # TODO
      is_evolutionary = FALSE
    )
    metaepoch_snapshots <- c(metaepoch_snapshots, snapshot)
    metaepochs_count <- metaepochs_count + 1

    print(length(active_demes))
  }
  methods::new("hms",
    root_id = root@id,
    metaepoch_snapshots = metaepoch_snapshots,
    best_fitness = best_fitness,
    best_solution = best_solution,
    total_time_in_seconds = seconds_since(start_time),
    total_metaepoch_time_in_seconds = as.numeric(total_metaepoch_time, units = "secs"),
    metaepochs_count = metaepochs_count,
    deme_population_size = population_size_per_tree_level,
    lower = lower,
    upper = upper,
    call = match.call()
  )
}

setClass("MetaepochSnapshot", slots = c(
  demes = "list",
  best_fitness = "numeric",
  best_solution = "numeric",
  time_in_seconds = "numeric",
  fitness_evaluations = "numeric",
  is_evolutionary = "logical"
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
  cat("\nCall:\n", deparse(object@call), "\n\n", sep = "")
  cat("Available slots:\n")
  print(methods::slotNames(object))
})

setGeneric("printTree", function(object) standardGeneric("printTree"))

prind <- function(demes, root_id, best_solution) {
  get_deme_by_id <- function(id) {
    Filter(function(deme) {
      deme@id == id
    }, demes)[[1]]
  }
  get_children <- function(deme) {
    Filter(function(d) {
      identical(d@parent_id, deme@id)
    }, demes)
  }
  print_deme <- function(deme) {
    deme_distinguisher <- if (deme@best_solution == best_solution) "***" else ""
    if (!is.null(deme@sprout)) {
      cat("spr: (")
      for (x in deme@sprout) {
        if (x != deme@sprout[[1]]) {
          cat(", ")
        }
        cat(sprintf(x, fmt = "%#.2f"))
      }
      cat("); ")
    }
    cat(paste(deme_distinguisher, "f(", sep = ""))
    for (x in deme@best_solution) {
      if (x != deme@best_solution[[1]]) {
        cat(", ")
      }
      cat(sprintf(x, fmt = "%#.2f"))
    }
    cat(paste(") = ", sprintf(deme@best_fitness, fmt = "%#.2f"), deme_distinguisher, " evaluations: ", deme@evaluations_count, "\n", sep = ""))
  }

  print_tree_from_deme <- function(deme, prefix = "") {
    children <- get_children(deme)
    for (child in children) {
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
      print_tree_from_deme(child, prefix = paste(prefix, if (is_last) " " else "|", "   ", sep = ""))
    }
  }
  root <- get_deme_by_id(root_id)
  print_deme(root)
  print_tree_from_deme(root)
}

setMethod("printTree", "hms", function(object) {
  last_metaepoch_snapshot <- utils::tail(object@metaepoch_snapshots, n = 1)
  if (length(last_metaepoch_snapshot) == 0) {
    return()
  }
  demes <- last_metaepoch_snapshot[[1]]@demes
  prind(demes, object@root_id, object@best_solution)
})


summary.hms <- function(object, ...) {
  domain_element_to_string <- function(x) {
    rounded_params <- mapply(function(x) {
      sprintf(x, fmt = "%#.2f")
    }, x)
    comma_separated_params <- do.call(paste, c(as.list(rounded_params), sep = ", "))
    if (length(x) > 1) {
      paste("(", comma_separated_params, ")", sep = "")
    } else {
      comma_separated_params
    }
  }
  out <- list(
    fitness = object@best_fitness,
    solution = domain_element_to_string(object@best_solution),
    metaepochs = object@metaepochs_count,
    deme_population_size = object@deme_population_size,
    lower_bound = domain_element_to_string(object@lower),
    upper_bound = domain_element_to_string(object@upper),
    computation_time = paste(as.numeric(object@total_time_in_seconds), " seconds", sep = "")
  )
  class(out) <- "summary.hms"
  out
}

setMethod("summary", "hms", summary.hms)

plot.hms <- function(x) {
  object <- x
  metaepochs <- 1:object@metaepochs_count
  metaepoch_fitnesses <- mapply(function(snapshot) {
    snapshot@best_fitness
  }, object@metaepoch_snapshots)
  plot(metaepochs,
    ylim = c(min(metaepoch_fitnesses), max(metaepoch_fitnesses)),
    xlab = "metaepoch",
    ylab = "fitness",
    type = "n"
  )
  graphics::lines(metaepochs,
    metaepoch_fitnesses,
    pch = 16,
    type = "b",
    col = "green3"
  )
  graphics::legend("bottomright",
    inset = 0.02,
    legend = "Best fitness",
    fill = "green"
  )
}

setMethod("plot", "hms", plot.hms)
