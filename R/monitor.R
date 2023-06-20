MONITOR_LEVEL_NONE <- 0
MONITOR_LEVEL_BASIC <- 1
MONITOR_LEVEL_BASIC_TREE <- 2
MONITOR_LEVEL_VERBOSE_TREE <- 3

get_monitor_level <- function(level_name) {
  if (level_name == "verbose_tree") {
    return(MONITOR_LEVEL_VERBOSE_TREE)
  }
  if (level_name == "basic_tree") {
    return(MONITOR_LEVEL_BASIC_TREE)
  }
  if (level_name == "basic") {
    return(MONITOR_LEVEL_BASIC)
  }
  if (level_name == "none") {
    return(MONITOR_LEVEL_NONE)
  }
  warning("Monitor level should be one of {'none', 'basic', 'basic_tree', 'verbose_tree'}")
  MONITOR_LEVEL_NONE
}

log_metaepoch_snapshot <- function(snapshot, root_id, metaepochs_count, monitor_level) {
  if (monitor_level > MONITOR_LEVEL_NONE) {
    cat("Metaepoch:", metaepochs_count, "Best fitness:", snapshot@best_fitness, "\n")

    if (monitor_level > MONITOR_LEVEL_BASIC) {
      cat("Best solution:", get_solution_string(snapshot@best_solution), "\n")
      population_size <- Reduce(`+`, mapply(function(deme) base::nrow(deme@population), snapshot@demes))
      cat("Whole population size:", population_size, "\n")
      cat("Demes count:", length(Filter(function(deme) length(deme@best_solution > 0), snapshot@demes)), "\n")
      cat("Calculation time:", snapshot@time_in_seconds, "sec\n")
      cat("Fitness evaluations count:", snapshot@fitness_evaluations, "\n")
      if (monitor_level >= MONITOR_LEVEL_BASIC_TREE) {
        print_tree(snapshot@demes, root_id, snapshot@best_solution, show_details = (monitor_level > MONITOR_LEVEL_BASIC_TREE))
      }
      cat("\n\n")
    }
  }
}
