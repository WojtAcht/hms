#' Title
#'
#' @param tree_height - numeric - default value: 5
#' @param fitness - fitness function
#' @param lower - numeric - lower bound
#' @param upper - numeric - upper bound
#' @param sigma - numeric - vector of standard deviations for each tree level
#' @param population_size_per_tree_level - numeric
#' @param run_metaepoch - function - returns list with named fields: solution, population, value
#' @param global_stopping_condition - function
#' @param local_stopping_condition - function
#' @param sprouting_condition - function
#' @param create_population - function
#' @param suggestions - matrix
#' @param with_gradient_method - logical
#' @param gradient_method_args - list of parameters that are passed to the gradient method
#' @param run_gradient_method - function - returns list with named fields: solution, population, value
#' @param monitor_level - string - one of {'none', 'basic', 'basic_tree', 'verbose_tree'}
#'
#' @return numeric best solution
#' @export
#'
#' @examples
hms <- function(tree_height = 3,
                fitness,
                lower,
                upper,
                sigma = default_sigma(lower, upper, tree_height),
                population_size_per_tree_level = default_population_size_per_tree_level(tree_height),
                run_metaepoch = default_ga_metaepoch(tree_height),
                global_stopping_condition = default_global_stopping_condition,
                local_stopping_condition = default_local_stopping_condition,
                sprouting_condition = max_metric_sprouting_condition(euclidean_distance,
                                                                     sprouting_default_euclidean_distances(sigma)),
                create_population,
                suggestions = NULL,
                with_gradient_method = FALSE,
                gradient_method_args = default_gradient_method_args,
                run_gradient_method,
                monitor_level = "basic") {
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
  if (!missing(sigma) & !length(sigma) >= tree_height) {
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
    message("A list of standard deviations (sigma) or a function to create population should be provided.")
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
  monitor_level <- getMonitorLevel(monitor_level)

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
  fitness_evaluations_count <- 0
  f <- function(x) {
    fitness_evaluations_count <<- fitness_evaluations_count + 1
    fitness(x)
  }
  while (!global_stopping_condition(metaepoch_snapshots) && length(active_demes) > 0) {
    new_demes <- c()
    blocked_sprouts <- list()
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
        deme@isActive <- FALSE
        inactive_demes <- c(inactive_demes, deme)
        next
      }
      new_demes <- c(new_demes, deme)
      if (deme@level >= tree_height) next
      if (sprouting_condition(metaepoch_result$solution, deme@level + 1, c(active_demes, inactive_demes))) {
        new_deme <- create_deme(lower, upper, deme, population_size_per_tree_level[[deme@level + 1]], create_population)
        new_demes <- c(new_demes, new_deme)
      } else {
        blocked_sprouts <- c(blocked_sprouts, list(metaepoch_result$solution))
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
      fitness_evaluations = fitness_evaluations_count,
      blocked_sprouts = blocked_sprouts,
      is_evolutionary = TRUE
    )
    if (monitor_level > MONITOR_LEVEL_NONE) {
      cat("Metaepoch:", metaepochs_count, "Best fitness:", best_fitness, "\n")

      if (monitor_level > MONITOR_LEVEL_BASIC) {
        cat("Best solution:", get_solution_string(best_solution), "\n")
        population_size <- Reduce(`+`, mapply(function(deme) base::nrow(deme@population), active_demes))
        cat("Whole population size:", population_size, "\n")
        cat("Demes count:", length(Filter(function(deme) length(deme@best_solution > 0), snapshot@demes)), "\n")
        cat("Calculation time:", snapshot@time_in_seconds, "sec\n")
        cat("Fitness evaluations count:", snapshot@fitness_evaluations, "\n")
        if (monitor_level >= MONITOR_LEVEL_BASIC_TREE) {
          print_tree(snapshot@demes, root@id, best_solution, show_details = (monitor_level > MONITOR_LEVEL_BASIC_TREE))
        }
        cat("\n\n")
      }
    }
    metaepoch_snapshots <- c(metaepoch_snapshots, snapshot)
    metaepochs_count <- metaepochs_count + 1
  }

  if (length(active_demes) == 0) {
    message("HMS stopped due to a lack of active demes!")
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
      blocked_sprouts = list(),
      is_evolutionary = FALSE
    )
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
    deme_population_size = population_size_per_tree_level,
    lower = lower,
    upper = upper,
    call = match.call()
  )
}
