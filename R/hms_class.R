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
#' @return
#' @export
#'
#' @examples
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
#' @return
#' @export
#'
#' @examples
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
#' @param hms - hms s4 object
#'
#' @return
#' @export
#'
#' @examples
setMethod("print", "hms", function(x, ...) utils::str(x))

#' Title
#'
#' @param hms - hms s4 object
#'
#' @return
#' @export
#'
#' @examples
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
#' @return
#' @export
#'
#' @examples
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
#' @param hms - hms s4 object
#'
#' @return
#' @export
#'
#' @examples
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
#' @param hms - hms s4 object
#'
#' @return
#' @export
#'
#' @examples
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
#' @param hms - hms s4 object
#'
#' @return
#' @export
#'
#' @examples
setMethod("plot", "hms", plot.hms)

#' Title
#'
#' @param object - hms s4 object
#'
#' @return
#' @export
#'
#' @examples
setGeneric("plotActiveDemes", function(object) standardGeneric("plotActiveDemes"))

#' Title
#'
#' @param hms - hms s4 object
#'
#' @return
#' @export
#'
#' @examples
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
#' @return
#' @export
#'
#' @examples
setGeneric("printBlockedSprouts", function(object) standardGeneric("printBlockedSprouts"))

#' Title
#'
#' @param hms - hms s4 object
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param object - hms s4 object
#'
#' @return
#' @export
#'
#' @examples
setGeneric("plotPopulation", function(object, dimensions) standardGeneric("plotPopulation"))

#' Title
#'
#' @param hms - hms s4 object
#'
#' @return
#' @export
#'
#' @examples
setMethod("plotPopulation", "hms", function(object, dimensions) {
  dimensions_names <- mapply(function(n) {paste("x",  toString(n), sep="")}, dimensions)
  last_metaepoch_demes <- tail(object@metaepoch_snapshots, n=1)[[1]]@demes
  demes_ids <- mapply(function(deme) deme@id, last_metaepoch_demes)
  populations_with_indices <- lapply(
    last_metaepoch_demes,
    function(deme) {
      deme_population <- deme@population[,dimensions]
      colnames(deme_population) <- dimensions_names
      deme_df <- as.data.frame(deme_population)
      deme_df["color"] <- match(deme@id, demes_ids)
      deme_df
    })
  population <- Reduce(rbind, populations_with_indices)
  plot(
    x=population[,dimensions_names[[1]]],
    y=population[,dimensions_names[[2]]],
    col=population$color,
    pch=16,
    xlab = dimensions_names[[1]],
    ylab = dimensions_names[[2]]
  )
})

#' Title
#'
#' @param object - hms s4 object
#'
#' @return
#' @export
#'
#' @examples
setGeneric("saveMetaepochsPopulations", function(object, path, dimensions) standardGeneric("saveMetaepochsPopulations"))

#' Title
#'
#' @param hms - hms s4 object
#'
#' @return
#' @export
#'
#' @examples
setMethod("saveMetaepochsPopulations", "hms", function(object, path, dimensions) {
  dimensions_names <- mapply(function(n) {paste("x",  toString(n), sep="")}, dimensions)
  xlim <- c(result@lower[[dimensions[[1]]]], result@upper[[dimensions[[1]]]])
  ylim <- c(result@lower[[dimensions[[2]]]], result@upper[[dimensions[[2]]]])
  demes_ids_per_snapshot <- mapply(
    function(s) mapply(function(d) d@id, s@demes),
    result@metaepoch_snapshots
  )
  demes_ids <- Reduce(c, demes_ids_per_snapshot)
  for(i in seq_along(object@metaepoch_snapshots)) {
    snapshot <- object@metaepoch_snapshots[[i]]
    snapshot_demes <- snapshot@demes
    populations_with_indices <- lapply(
      snapshot_demes,
      function(deme) {
        deme_population <- deme@population[,dimensions]
        colnames(deme_population) <- dimensions_names
        deme_df <- as.data.frame(deme_population)
        deme_df["color"] <- match(deme@id, demes_ids)
        deme_df
    })
    snapshot_population <- Reduce(rbind, populations_with_indices)
    jpeg(
      file = paste(path, "/snapshot", toString(i), ".png", sep=""),
      width = 480,
      height = 480
    )
    plot(
      x = snapshot_population[,dimensions_names[[1]]],
      y = snapshot_population[,dimensions_names[[2]]],
      col = snapshot_population$color,
      pch = 16,
      xlab = dimensions_names[[1]],
      ylab = dimensions_names[[2]],
      xlim = xlim,
      ylim = ylim,
      main = paste("Snapshot", toString(i))
    )
    dev.off()
  }
})
