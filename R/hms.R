#' Maximization (or minimization) of a fitness function using Hierarchic Memetic Strategy.
#'
#' @param tree_height numeric - default value: 5. It determines the maximum tree height
#' which will usually be reached unless a very strict local stopping condition, global
#' stopping condition or sprouting condition is used.
#' @param fitness fitness function, that returns a numerical value, to be optimized by the strategy.
#' @param lower numeric - lower bound of the domain, a vector of length equal
#' to the decision variables.
#' @param upper numeric - upper bound of the domain, a vector of length equal
#' to the decision variables.
#' @param sigma numeric - Vector of standard deviations for each tree level used to create
#' a population of a sprouted deme.
#' @param population_sizes numeric - Sizes of deme populations on each tree level.
#' @param run_metaepoch A function that takes 5 parameters: fitness, suggestions, lower,
#' upper, tree_level, runs a metaepoch on the given deme population and returns list with
#' 3 named fields: solution, population, value.
#' @param gsc global stopping condition function taking a list of MetaepochSnapshot
#' objects and returning a logical value; it is evaluated after every metaepoch and
#' determines whether whole computation should be stopped. See \code{\link{gsc_metaepochs_count}} for more details.
#' @param lsc local stopping condition - function taking a deme and a list of MetaepochSmapshot
#' objects representing previous metaepochs; it is run on every deme after it has run a metaepoch
#' and determines whether that deme will remain active. See \code{\link{lsc_max_fitness_evaluations}} for more details.
#' @param sc sprouting condition - function taking 3 arguments: an individual, a tree level
#' and a list of Deme objects; it determines whether the given individual can sprout a new deme
#' on the given level. See \code{\link{sc_max_metric}} for more details.
#' @param create_population function taking 6 parameters: mean, lower, upper, population_size,
#' tree_level, sigma that returns a population for a Deme object to be created on the given
#' tree level.
#' @param suggestions matrix of individuals for the initial population of the root
#' @param with_gradient_method logical determining whether a gradient method should be run
#' for all leaves at the end of the computation to refine their best solutions.
#' @param gradient_method_args list of parameters that are passed to the gradient method
#' @param run_gradient_method function - returns list with named fields: solution, population, value
#' @param monitor_level string - one of: 'none', 'basic', 'basic_tree', 'verbose_tree'.
#' @param parallel logical - \code{TRUE} when run_metaepoch runs in parallel.
#' @param minimize logical - \code{TRUE} when fitness shall be minimized.
#'
#' @return Returns an object of class hms.
#' @export
#'
#' @examples
#' f <- function(x) x
#' result <- hms(fitness = f, lower = -5, upper = 5)
hms <- function(tree_height = 3,
                minimize = FALSE,
                fitness,
                lower,
                upper,
                sigma = default_sigma(lower, upper, tree_height),
                population_sizes = default_population_sizes(tree_height),
                run_metaepoch = default_ga_metaepoch(tree_height),
                gsc = gsc_default,
                lsc = lsc_default,
                sc = sc_max_metric(
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
  if (!is.vector(population_sizes) & !is.list(population_sizes)) {
    stop("population_sizes must be a list or a vector.")
  }
  if (!length(population_sizes) >= tree_height) {
    stop("population_sizes must have at least tree_height elements.")
  }
  if (!missing(suggestions) & !is.matrix(suggestions)) {
    stop("Invalid object for argument \"suggestions\": should be or extend class matrix.")
  }
  if (!missing(suggestions) & any(dim(suggestions) != c(population_sizes[[1]], length(lower)))) {
    stop("Provided suggestions have wrong dimensions.")
  }
  if (missing(create_population) & missing(sigma)) {
    message("A list of standard deviations (sigma) or a function to create population should be provided.")
  }
  if (with_gradient_method & missing(run_gradient_method)) {
    gradient_method_args <- validate_gradient_method_args(gradient_method_args, lower, upper)
    run_gradient_method <- default_run_gradient_method
  }
  monitor_level <- get_monitor_level(monitor_level)

  operator <- ifelse(minimize, `<`, `>`)

  root <- if (is.null(suggestions)) {
    create_deme(lower, upper, NULL, population_sizes[[1]], create_population)
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
    lock <- if (parallel) filelock::lock("/general.lck") else NULL
    fitness_evaluations_count <<- fitness_evaluations_count + 1
    if (parallel) {
      filelock::unlock(lock)
    }
    fitness(x)
  }
  while (!gsc(metaepoch_snapshots)) {
    if (length(Filter(function(deme) {
      deme@is_active
    }, demes)) == 0) {
      message("HMS stopped due to a lack of active demes!")
      break
    }

    next_metaepoch_demes <- Filter(function(deme) {
      !deme@is_active
    }, demes)
    blocked_sprouts <- list()
    for (deme in Filter(function(deme) {
      deme@is_active
    }, demes)) {
      start_metaepoch_time <- Sys.time()
      deme_evaluations_count <- 0
      deme_f <- function(x) {
        lock <- if (parallel) filelock::lock("/deme.lck") else NULL
        deme_evaluations_count <<- deme_evaluations_count + 1
        if (parallel) {
          filelock::unlock(lock)
        }
        f(x)
      }
      metaepoch_result <- run_metaepoch(deme_f, deme@population, lower, upper, deme@level, minimize)

      end_metaepoch_time <- Sys.time()
      total_metaepoch_time <- total_metaepoch_time + (end_metaepoch_time - start_metaepoch_time)

      deme <- update_deme(metaepoch_result, deme, minimize)
      deme@evaluations_count <- deme@evaluations_count + deme_evaluations_count

      if (lsc(deme, metaepoch_snapshots)) {
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
      if (sc(metaepoch_result$solution, deme@level + 1, demes_including_this_metaepoch_sprouts)) {
        new_deme <- create_deme(lower, upper, deme, population_sizes[[deme@level + 1]], create_population)
        next_metaepoch_demes <- c(next_metaepoch_demes, new_deme)
      } else {
        blocked_sprouts <- c(blocked_sprouts, list(metaepoch_result$solution))
      }
    }
    demes <- next_metaepoch_demes

    best <- find_best_solution(demes, minimize)

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
    best <- find_best_solution(demes_after_gradient_metaepoch, minimize)
    gradient_snapshot <- methods::new("MetaepochSnapshot",
      demes = demes_after_gradient_metaepoch,
      best_fitness = best$fitness,
      best_solution = best$solution,
      time_in_seconds = seconds_since(start_time) - evaluation_times_sum(metaepoch_snapshots),
      fitness_evaluations = fitness_evaluations_count,
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
    deme_population_sizes = population_sizes,
    lower = lower,
    upper = upper,
    call = match.call()
  )
}

find_best_solution <- function(demes, minimize) {
  operator <- ifelse(minimize, `<`, `>`)
  best_solution <- NULL
  best_fitness <- ifelse(minimize, Inf, -Inf)
  for (deme in demes) {
    if (length(deme@best_fitness) == 0) {
      next
    }

    if (operator(deme@best_fitness, best_fitness)) {
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
  Filter(function(deme) {
    !is_processed(deme)
  }, all_demes)
}
