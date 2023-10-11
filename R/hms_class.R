#' A S4 class representing a snapshot of one metaepoch.
#'
#' @slot demes list of objects of class Deme.
#' @slot best_fitness numeric - best fitness value of a metaepoch.
#' @slot best_solution numeric - best solution of a metaepoch.
#' @slot time_in_seconds numeric - time of metaepoch in seconds.
#' @slot fitness_evaluations numeric - number of fitness evaluations.
#' @slot blocked_sprouts list - list of sprouts that were blocked by sprouting
#' condition. A sprout is a potential origin of a new Deme, it can be blocked
#' by sc -- sprouting condition. See \code{\link{sc_max_metric}} for more details.
#' @slot is_evolutionary logical - \code{TRUE} for all metaepochs except the
#' gradient one.
#'
#' @export
setClass("MetaepochSnapshot", slots = c(
  demes = "list",
  best_fitness = "numeric",
  best_solution = "numeric",
  time_in_seconds = "numeric",
  fitness_evaluations = "numeric",
  blocked_sprouts = "list",
  is_evolutionary = "logical"
))

#' A S4 class representing a result of hms.
#'
#' @slot root_id character - UUID of a root Deme.
#' @slot metaepoch_snapshots list of objects of class MetaepochSnapshot.
#' @slot best_fitness numeric - best fitness value of all metaepochs.
#' @slot best_solution numeric -  best solution of all metaepochs.
#' @slot total_time_in_seconds numeric - time of a hms execution in seconds.
#' @slot total_metaepoch_time_in_seconds numeric - time of all metaepochs in seconds.
#' @slot metaepochs_count numeric - total number of all metaepochs.
#' @slot deme_population_sizes numeric - sizes of deme populations on each tree level.
#' Same as \code{population_sizes} parameter of hms function.
#' @slot lower numeric - lower bound of the domain, a vector of length equal
#' to the decision variables.
#' @slot upper numeric - upper bound of the domain, a vector of length equal
#' to the decision variables.
#' @slot call language - an object of class "call" representing the matched call.
#'
#' @export
setClass("hms", slots = c(
  root_id = "character",
  metaepoch_snapshots = "list",
  best_fitness = "numeric",
  best_solution = "numeric",
  total_time_in_seconds = "numeric",
  total_metaepoch_time_in_seconds = "numeric",
  metaepochs_count = "numeric",
  deme_population_sizes = "numeric",
  lower = "numeric",
  upper = "numeric",
  call = "language"
))

#' Print method for class "hms".
#'
#' @param x - hms s4 object
#' @param ... - other print arguments
#'
#' @return It does not return anything. The obvious side effect is output to the terminal.
#'
#' @export
#'
#' @examples
#' f <- function(x) x
#' result <- hms(fitness = f, lower = -5, upper = 5)
#' print(result)
setMethod("print", "hms", function(x, ...) utils::str(x))

#' Show method for class "hms".
#'
#' @param object - hms s4 object
#'
#' @return It returns the names of the slots and the classes associated with the
#' slots in the "hms" class. It prints call details.
#'
#' @export
#'
#' @examples
#' f <- function(x) x
#' result <- hms(fitness = f, lower = -5, upper = 5)
#' show(result)
setMethod("show", "hms", function(object) {
  cat("An object of class \"hms\"\n")
  cat("\nCall:\n", deparse(object@call), "\n\n", sep = "")
  cat("Available slots:\n")
  print(methods::slotNames(object))
})

#' printTree method for class "hms".
#'
#' @param object - hms s4 object
#'
#' @return It does not return anything. It prints the hms tree.
#'
#' @export
#'
#' @examples
#' f <- function(x) x
#' result <- hms(fitness = f, lower = -5, upper = 5)
#' printTree(result)
setGeneric("printTree", function(object) standardGeneric("printTree"))

get_solution_string <- function(solution, format = "%#.2f") {
  solution_values <- mapply(function(sol) sprintf(sol, fmt = format), solution)
  paste("(", paste(solution_values, collapse = ", "), ")", sep = "")
}

print_tree <- function(demes, root_id, best_solution, show_details = TRUE) {
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
    deme_distinguisher <- if (all(deme@best_solution == best_solution)) "***" else ""
    is_root <- is.null(deme@sprout)
    if (!is_root & show_details) {
      cat("spr: ")
      cat(get_solution_string(deme@sprout))
      cat("; ")
    }
    cat(paste(deme_distinguisher, "f", sep = ""))
    cat(get_solution_string(deme@best_solution))
    active <- ifelse(deme@is_active, " A", "")
    new_deme <- ifelse(length(deme@best_fitnesses_per_metaepoch) <= 1 & show_details & !is_root, " (new_deme)", "")
    cat(paste(" = ", sprintf(deme@best_fitness, fmt = "%#.2f"), deme_distinguisher, sep = ""))
    if (show_details) {
      cat(paste(" evaluations: ", deme@evaluations_count, sep = ""))
    }
    cat(paste(active, new_deme, "\n", sep = ""))
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


#' printTree method for class "hms".
#'
#' @param object - hms s4 object
#'
#' @return It does not return anything. It prints the hms tree.
#'
#' @export
#'
#' @examples
#' f <- function(x) x
#' result <- hms(fitness = f, lower = -5, upper = 5)
#' printTree(result)
setMethod("printTree", "hms", function(object) {
  last_metaepoch_snapshot <- utils::tail(object@metaepoch_snapshots, n = 1)
  if (length(last_metaepoch_snapshot) == 0) {
    return()
  }
  demes <- last_metaepoch_snapshot[[1]]@demes
  print_tree(demes, object@root_id, object@best_solution)
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
    deme_population_sizes = object@deme_population_sizes,
    lower_bound = domain_element_to_string(object@lower),
    upper_bound = domain_element_to_string(object@upper),
    computation_time = paste(as.numeric(object@total_time_in_seconds), " seconds", sep = "")
  )
  class(out) <- "summary.hms"
  out
}

#' Summary method for class "hms".
#'
#' @param object - hms s4 object
#' @param ... - other summary arguments
#'
#' @return Returns a list with fields: fitness, solution, metaepochs, deme_population_sizes,
#' lower_bound, upper_bound, computation_time. These fields should match fields
#' of class "hms".
#'
#' @export
#'
#' @examples
#' f <- function(x) x
#' result <- hms(fitness = f, lower = -5, upper = 5)
#' summary(result)
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

#' Plot method for "hms" class.
#'
#' @param x - hms s4 object
#'
#' @return It doesn't return anything meaningful. It plots the fitness by metaepoch count.
#'
#' @export
#'
#' @examples
#' f <- function(x) x
#' result <- hms(fitness = f, lower = -5, upper = 5)
#' plot(result)
setMethod("plot", "hms", plot.hms)

#' plotActiveDemes method for "hms" class.
#'
#' @param object - hms s4 object
#'
#' @return It doesn't return anything meaningful. It plots the number of active
#' demes per metaepoch.
#'
#' @export
#'
#' @examples
#' f <- function(x) x
#' result <- hms(fitness = f, lower = -5, upper = 5)
#' plotActiveDemes(result)
setGeneric("plotActiveDemes", function(object) standardGeneric("plotActiveDemes"))

#' plotActiveDemes method for "hms" class.
#'
#' @param object - hms s4 object
#'
#' @return It doesn't return anything meaningful. It plots the number of active
#' demes per metaepoch.
#'
#' @export
#'
#' @examples
#' f <- function(x) x
#' result <- hms(fitness = f, lower = -5, upper = 5)
#' plotActiveDemes(result)
setMethod("plotActiveDemes", "hms", function(object) {
  metaepochs <- 1:object@metaepochs_count
  active_demes_per_metaepoch <- mapply(function(snapshot) {
    Filter(function(deme) {
      deme@is_active
    }, snapshot@demes)
  }, object@metaepoch_snapshots)
  active_demes_count_per_metaepoch <- mapply(length, active_demes_per_metaepoch)
  graphics::barplot(active_demes_count_per_metaepoch,
    names.arg = metaepochs,
    main = "Active demes per metaepoch.",
    xlab = "Metaepoch",
    ylab = "Active demes count"
  )
})

#' printBlockedSprouts method for "hms" class.
#'
#' @param object - hms s4 object
#'
#' @return It doesn't return anything. It prints blocked sprouts per metaepoch.
#'
#' @export
#'
#' @examples
#' f <- function(x) x
#' result <- hms(fitness = f, lower = -5, upper = 5)
#' printBlockedSprouts(result)
setGeneric("printBlockedSprouts", function(object) standardGeneric("printBlockedSprouts"))

#' printBlockedSprouts method for "hms" class.
#'
#' @param object - hms s4 object
#'
#' @return It doesn't return anything. It prints blocked sprouts per metaepoch.
#'
#' @export
#'
#' @examples
#' f <- function(x) x
#' result <- hms(fitness = f, lower = -5, upper = 5)
#' printBlockedSprouts(result)
setMethod("printBlockedSprouts", "hms", function(object) {
  metaepochs <- 1:object@metaepochs_count
  blocked_sprouts_per_metaepoch <- mapply(function(snapshot) {
    snapshot@blocked_sprouts
  }, object@metaepoch_snapshots)
  for (metaepoch in metaepochs) {
    blocked_sprouts <- blocked_sprouts_per_metaepoch[[metaepoch]]
    cat(sprintf("Metaepoch %d - blocked sprouts count: %d\n", metaepoch, length(blocked_sprouts)))
    for (blocked_sprout in blocked_sprouts) {
      print(blocked_sprout)
    }
    cat("\n")
  }
})


#' plotPopulation method for "hms" class.
#'
#' @param object - hms s4 object
#' @param dimensions - two selected dimensions
#'
#' @return It doesn't return anything meaningful. It plots the selected two
#' dimensions of a population.
#'
#' @export
#'
#' @examples
#' f <- function(x) x
#' result <- hms(fitness = f, lower = -5, upper = 5)
#' plotPopulation(result, c(1, 1))
setGeneric("plotPopulation", function(object, dimensions) standardGeneric("plotPopulation"))

#' plotPopulation method for "hms" class.
#'
#' @param object - hms s4 object
#' @param dimensions - two selected dimensions
#'
#' @return It doesn't return anything meaningful. It plots the selected two
#' dimensions of a population.
#'
#' @export
#'
#' @examples
#' f <- function(x) x
#' result <- hms(fitness = f, lower = -5, upper = 5)
#' plotPopulation(result, c(1, 1))
setMethod("plotPopulation", "hms", function(object, dimensions) {
  if (!length(dimensions) == 2) {
    stop("The vector of dimensions must have two elements.")
  }
  dimensions_names <- mapply(function(n) {
    paste("x", toString(n), sep = "")
  }, dimensions)
  last_metaepoch_demes <- utils::tail(object@metaepoch_snapshots, n = 1)[[1]]@demes
  demes_ids <- mapply(function(deme) deme@id, last_metaepoch_demes)
  populations_with_indices <- lapply(
    last_metaepoch_demes,
    function(deme) {
      deme_population <- deme@population[, dimensions]
      colnames(deme_population) <- dimensions_names
      deme_df <- as.data.frame(deme_population)
      deme_df["color"] <- match(deme@id, demes_ids)
      deme_df
    }
  )
  population <- unique(Reduce(rbind, populations_with_indices))
  plot(
    x = population[, dimensions_names[[1]]],
    y = population[, dimensions_names[[2]]],
    col = population$color,
    pch = 16,
    xlab = dimensions_names[[1]],
    ylab = dimensions_names[[2]],
    main = "Last snapshot population"
  )
})

#' saveMetaepochsPopulations method for "hms" class.
#'
#' @param object hms s4 object
#' @param path path
#' @param dimensions vector of two selected dimensions e.g. c(1,2)
#'
#' @return It doesn't return anything. It creates plots and saves them to a specified
#' directory.
#'
#' @export
#' @examples
#' fitness <- function(x) x[1] + x[2]
#' lower <- c(-5, -5)
#' upper <- c(5, 5)
#' result <- hms(fitness = fitness, lower = lower, upper = upper)
#' selected_dimensions <- c(1, 2)
#' saveMetaepochsPopulations(result, tempdir(), selected_dimensions)
setGeneric("saveMetaepochsPopulations", function(object, path, dimensions) standardGeneric("saveMetaepochsPopulations"))

#' saveMetaepochsPopulations
#'
#' @param object hms s4 object
#' @param path path
#' @param dimensions vector of two selected dimensions e.g. c(1,2)
#'
#' @return It doesn't return anything. It creates plots and saves them to a specified
#' directory.
#'
#' @export
#' @examples
#' fitness <- function(x) x[1] + x[2]
#' lower <- c(-5, -5)
#' upper <- c(5, 5)
#' result <- hms(fitness = fitness, lower = lower, upper = upper)
#' selected_dimensions <- c(1, 2)
#' saveMetaepochsPopulations(result, tempdir(), selected_dimensions)
setMethod("saveMetaepochsPopulations", "hms", function(object, path, dimensions) {
  if (!length(dimensions) == 2) {
    stop("The vector of dimensions must have two elements.")
  }
  dimensions_names <- mapply(function(n) {
    paste("x", toString(n), sep = "")
  }, dimensions)
  xlim <- c(object@lower[[dimensions[[1]]]], object@upper[[dimensions[[1]]]])
  ylim <- c(object@lower[[dimensions[[2]]]], object@upper[[dimensions[[2]]]])
  demes_ids_per_snapshot <- mapply(
    function(s) mapply(function(d) d@id, s@demes),
    object@metaepoch_snapshots
  )
  demes_ids <- Reduce(c, demes_ids_per_snapshot)
  for (i in seq_along(object@metaepoch_snapshots)) {
    snapshot <- object@metaepoch_snapshots[[i]]
    snapshot_demes <- snapshot@demes
    populations_with_indices <- lapply(
      snapshot_demes,
      function(deme) {
        deme_population <- deme@population[, dimensions]
        colnames(deme_population) <- dimensions_names
        deme_df <- as.data.frame(deme_population)
        deme_df["color"] <- match(deme@id, demes_ids)
        deme_df
      }
    )
    snapshot_population <- Reduce(rbind, populations_with_indices)
    grDevices::jpeg(
      file = paste(path, "/snapshot", toString(i), ".png", sep = ""),
      width = 480,
      height = 480
    )
    plot(
      x = snapshot_population[, dimensions_names[[1]]],
      y = snapshot_population[, dimensions_names[[2]]],
      col = snapshot_population$color,
      pch = 16,
      xlab = dimensions_names[[1]],
      ylab = dimensions_names[[2]],
      xlim = xlim,
      ylim = ylim,
      main = paste("Snapshot", toString(i))
    )
    grDevices::dev.off()
  }
})
