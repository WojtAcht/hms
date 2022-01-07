#' Title
#'
#' @slot demes list.
#' @slot best_fitness numeric.
#' @slot best_solution numeric.
#' @slot time_in_seconds numeric.
#' @slot fitness_evaluations numeric.
#' @slot blocked_sprouts list.
#' @slot is_evolutionary logical.
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

#' Title
#'
#' @slot root_id character.
#' @slot metaepoch_snapshots list.
#' @slot best_fitness numeric.
#' @slot best_solution numeric.
#' @slot total_time_in_seconds numeric.
#' @slot total_metaepoch_time_in_seconds numeric.
#' @slot metaepochs_count numeric.
#' @slot deme_population_sizes numeric.
#' @slot lower numeric.
#' @slot upper numeric.
#' @slot call language.
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

#' Title
#'
#' @param x - hms s4 object
#' @param ... - other print arguments
#'
#' @export
setMethod("print", "hms", function(x, ...) utils::str(x))

#' Title
#'
#' @param object - hms s4 object
#'
#' @export
setMethod("show", "hms", function(object) {
  cat("An object of class \"hms\"\n")
  cat("\nCall:\n", deparse(object@call), "\n\n", sep = "")
  cat("Available slots:\n")
  print(methods::slotNames(object))
})

#' Title
#'
#' @param object - hms s4 object
#'
#' @export
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


#' Title
#'
#' @param object - hms s4 object
#'
#' @export
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

#' Title
#'
#' @param object - hms s4 object
#' @param ... - other summary arguments
#'
#' @export
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

#' Title
#'
#' @param x - hms s4 object
#'
#' @export
setMethod("plot", "hms", plot.hms)

#' Title
#'
#' @param object - hms s4 object
#'
#' @export
setGeneric("plotActiveDemes", function(object) standardGeneric("plotActiveDemes"))

#' Title
#'
#' @param object - hms s4 object
#'
#' @export
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

#' Title
#'
#' @param object - hms s4 object
#'
#' @export
setGeneric("printBlockedSprouts", function(object) standardGeneric("printBlockedSprouts"))

#' Title
#'
#' @param object - hms s4 object
#'
#' @export
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
