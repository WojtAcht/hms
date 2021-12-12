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
                sprouting_condition = max_metric_sprouting_condition(
                  euclidean_distance,
                  sprouting_default_euclidean_distances(sigma)
                ),
                create_population = default_create_population(sigma),
                suggestions = NULL,
                with_gradient_method = FALSE,
                gradient_method_args = default_gradient_method_args,
                run_gradient_method,
                monitor_level = "basic",
                parallel = FALSE) {
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
    stop("Invalid object for argument \"sigma\": should be a vector or a list.")
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
  if (with_gradient_method & missing(run_gradient_method)) {
    gradient_method_args <- validate_gradient_method_args(gradient_method_args, lower, upper)
    run_gradient_method <- default_run_gradient_method
  }
  monitor_level <- getMonitorLevel(monitor_level) # TODO :((

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
  demes <- c(root)
  metaepochs_count <- 0
  metaepoch_snapshots <- list()
  fitness_evaluations_count <- 0
  f <- function(x) {
    lock <- `if`(parallel, filelock::lock("/general.lck"), NULL)
    fitness_evaluations_count <<- fitness_evaluations_count + 1
    if (parallel) {
      filelock::unlock(lock)
    }
    fitness(x)
  }
  while (!global_stopping_condition(metaepoch_snapshots)) {
    if (length(Filter(function(deme) { deme@is_active }, demes)) == 0) {
      message("HMS stopped due to a lack of active demes!")
      break
    }

    next_metaepoch_demes <- Filter(function(deme) { !deme@is_active }, demes)
    blocked_sprouts <- list()
    for (deme in Filter(function(deme) { deme@is_active }, demes)) {
      start_metaepoch_time <- Sys.time()
      deme_evaluations_count <- 0
      deme_f <- function(x) {
        lock <- `if`(parallel, filelock::lock("/deme.lck"), NULL)
        deme_evaluations_count <<- deme_evaluations_count + 1
        if (parallel) {
          filelock::unlock(lock)
        }
        f(x)
      }
      metaepoch_result <- run_metaepoch(deme_f, deme@population, lower, upper, deme@level)

      end_metaepoch_time <- Sys.time()
      total_metaepoch_time <- total_metaepoch_time + (end_metaepoch_time - start_metaepoch_time)

      deme <- update_deme(metaepoch_result, deme)
      deme@evaluations_count <- deme@evaluations_count + deme_evaluations_count

      if (local_stopping_condition(deme, metaepoch_snapshots)) {
        deme@is_active <- FALSE
        next_metaepoch_demes <- c(next_metaepoch_demes, deme)
        next
      } else {
        next_metaepoch_demes <- c(next_metaepoch_demes, deme)
      }

      # Leaves cannot sprout
      if (deme@level >= tree_height) next


      demes_including_this_metaepoch_sprouts <- c(
        next_metaepoch_demes,
        get_not_yet_processed_demes(demes, next_metaepoch_demes)
      )
      if (sprouting_condition(metaepoch_result$solution, deme@level + 1, demes_including_this_metaepoch_sprouts)) {
        new_deme <- create_deme(lower, upper, deme, population_size_per_tree_level[[deme@level + 1]], create_population)
        next_metaepoch_demes <- c(next_metaepoch_demes, new_deme)
      } else {
        blocked_sprouts <- c(blocked_sprouts, list(metaepoch_result$solution))
      }
    }
    demes <- next_metaepoch_demes

    best <- find_best_solution(demes)

    snapshot <- methods::new("MetaepochSnapshot",
      demes = demes,
      best_fitness = best$fitness,
      best_solution = best$solution,
      time_in_seconds = seconds_since(start_time) - evaluation_times_sum(metaepoch_snapshots),
      fitness_evaluations = fitness_evaluations_count,
      blocked_sprouts = blocked_sprouts,
      is_evolutionary = TRUE
    )

    log_metaepoch_snapshot(snapshot, root@id, metaepochs_count, monitor_level)

    metaepoch_snapshots <- c(metaepoch_snapshots, snapshot)
    metaepochs_count <- metaepochs_count + 1
  }


  # Gradient metaepoch:
  if (with_gradient_method) {
    demes_after_gradient_metaepoch <- run_gradient_metaepoch_for_leaves(
      f,
      run_gradient_method,
      gradient_method_args,
      metaepoch_snapshots,
      tree_height
    )
    best <- find_best_solution(demes_after_gradient_metaepoch)
    gradient_snapshot <- methods::new("MetaepochSnapshot",
      demes = demes_after_gradient_metaepoch,
      best_fitness = best$fitness,
      best_solution = best$solution,
      time_in_seconds = seconds_since(start_time) - evaluation_times_sum(metaepoch_snapshots),
      fitness_evaluations = fitness_evaluations_count, # TODO
      blocked_sprouts = list(),
      is_evolutionary = FALSE
    )
    metaepoch_snapshots <- c(metaepoch_snapshots, gradient_snapshot)
    metaepochs_count <- metaepochs_count + 1
  }
  methods::new("hms",
    root_id = root@id,
    metaepoch_snapshots = metaepoch_snapshots,
    best_fitness = metaepoch_snapshots[[metaepochs_count]]@best_fitness,
    best_solution = metaepoch_snapshots[[metaepochs_count]]@best_solution,
    total_time_in_seconds = seconds_since(start_time),
    total_metaepoch_time_in_seconds = as.numeric(total_metaepoch_time, units = "secs"),
    metaepochs_count = metaepochs_count,
    deme_population_size_per_tree_level = population_size_per_tree_level,
    lower = lower,
    upper = upper,
    call = match.call()
  )
}

find_best_solution <- function(demes) {
  best_solution <- NULL
  best_fitness <- -Inf
  for (deme in demes) {
    if (length(deme@best_fitness) == 0) {
      next
    }

    if (deme@best_fitness > best_fitness) {
      best_fitness <- deme@best_fitness
      best_solution <- deme@best_solution
    }
  }

  list("fitness" = best_fitness, "solution" = best_solution)
}

evaluation_times_sum <- function(metaepoch_snapshots) {
  time_sum <- 0
  for (metaepoch_snapshot in metaepoch_snapshots) {
    time_sum <- time_sum + metaepoch_snapshot@time_in_seconds
  }
  time_sum
}

get_not_yet_processed_demes <- function(all_demes, already_processed_demes) {
  is_processed <- function(deme) {
    for (processed_deme in already_processed_demes) {
      if (deme@id == processed_deme@id) {
        return(TRUE)
      }
    }
    FALSE
  }
  Filter(function(deme) { !is_processed(deme) }, all_demes)
}
