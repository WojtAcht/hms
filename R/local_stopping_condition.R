#' Factory function for a local stopping condition that stops a deme
#' after given number of consecutive metaeopochs without an improvement
#' of the best solution found in that deme.
#'
#' @param max_metaepochs_without_improvement - numeric
#'
#' @return Function that can be used as a local stopping condition for hms.
#'
#' @export
#'
#' @examples
#' local_stopping_condition <- lsc_metaepochs_without_improvement(5)
lsc_metaepochs_without_improvement <- function(max_metaepochs_without_improvement) {
  function(deme, previous_metaepoch_snapshots) {
    best_fitness_metaepoch <- match(deme@best_fitness, deme@best_fitnesses_per_metaepoch)
    metaepoch_count <- length(deme@best_fitnesses_per_metaepoch)
    !is_root(deme) & best_fitness_metaepoch <= metaepoch_count - max_metaepochs_without_improvement
  }
}

#' Factory function for a local stopping condition that stops a deme
#' after given number of fitness function evaluations has been made
#' in that deme.
#'
#' @param max_evaluations - numeric
#'
#' @return Function that can be used as a local stopping condition for hms.
#'
#' @export
#'
#' @examples
#' local_stopping_condition <- lsc_max_fitness_evaluations(500)
lsc_max_fitness_evaluations <- function(max_evaluations) {
  function(deme, previous_metaepoch_snapshots) {
    !is_root(deme) & deme@evaluations_count > max_evaluations
  }
}

#' Factory function for a local stopping condition that stops a deme
#' after given number of metaepochs have past since last metaepoch during
#' which this deme had an active child.
#'
#' @param metaepochs_limit - number of metaepochs that a deme can be active
#' without any active child
#'
#' @return Function that can be used as a local stopping condition for hms.
#'
#' @export
#'
#' @examples
#' local_stopping_condition <- lsc_metaepochs_without_active_child(3)
lsc_metaepochs_without_active_child <- function(metaepochs_limit) {
  function(deme, previous_metaepoch_snapshots) {
    has_active_child <- function(snapshot) {
      any(mapply(function(d) {
        identical(d@parent_id, deme@id) && d@is_active
      }, snapshot@demes))
    }

    !any(mapply(
      has_active_child,
      utils::tail(previous_metaepoch_snapshots, n = metaepochs_limit)
    ))
  }
}

#' Factory function for a trivial local stopping condition that
#' lets a deme be active forever. It is usually used in the root of
#' a hms tree.
#'
#' @return Function that always returns \code{FALSE}, which can be
#' used as a local stopping condition for hms.
#'
#' @export
#'
#' @examples
#' local_stopping_condition <- lsc_trivial()
lsc_trivial <- function() {
  function(deme, previous_metaepoch_snapshots) {
    FALSE
  }
}

lsc_default <- lsc_metaepochs_without_improvement(6)
