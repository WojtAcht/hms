#' Default sprouting condition based on given metric.
#'
#' @param metric - Metric used for deme distance comparison
#' @param max_distance - numeric
#'
#' @return logical
#' @export
#'
#' @examples
max_metric_sprouting_condition <- function(metric, max_distances) {
  function(potential_sprout, potential_sprout_level, demes) {
    level_demes <- Filter(function(d) {
      identical(d@level, potential_sprout_level)
    }, demes)

    single_deme_condition <- function(deme) {
      if (is.null(deme@sprout)) {
        FALSE
      } else {
        metric(deme@sprout, potential_sprout) < max_distances[[potential_sprout_level-1]]
      }
    }
    length(Filter(single_deme_condition, level_demes)) == 0
  }
}

default_local_stopping_condition <- function(deme, previous_metaepoch_snapshots) {
  max_metaepochs_without_improvement <- 5
  best_fitness_metaepoch <- match(deme@best_fitness, deme@best_fitnesses_per_metaepoch)
  metaepoch_count <- length(deme@best_fitnesses_per_metaepoch)
  best_fitness_metaepoch < metaepoch_count - max_metaepochs_without_improvement
}

default_global_stopping_condition <- function(metaepoch_snapshots) {
  length(metaepoch_snapshots) > 10
}

max_fitness_evaluations_global_stopping_condition <- function(max_evaluations) {
  function(metaepoch_snapshots) {
    length(metaepoch_snapshots) > 0 &&
      tail(metaepoch_snapshots, n = 1)[[1]]@fitness_evaluations > max_evaluations
  }
}
