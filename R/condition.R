#' Default sprouting condition basing on euclidean distance.
#'
#' @param max_distance - numeric
#'
#' @return logical
#' @export
#'
#' @examples
max_euclidean_distance_sprouting_condition <- function(max_distance) {
  function(potential_sprout, level_demes) {
    single_deme_condition <- function(deme) {
      if (is.null(deme@sprout)) {
        FALSE
      } else {
        euclidean_distance(deme@sprout, potential_sprout) < max_distance
      }
    }
    length(Filter(single_deme_condition, level_demes)) == 0
  }
}

default_local_stopping_condition <- function(deme) {
  max_metaepochs_without_improvement <- 5
  best_fitness_metaepoch <- match(deme@best_fitness, deme@best_fitnesses_per_metaepoch)
  metaepoch_count <- length(deme@best_fitnesses_per_metaepoch)
  best_fitness_metaepoch < metaepoch_count - max_metaepochs_without_improvement
}

default_global_stopping_condition <- function(metaepochs, fitness_evaluations, execution_time) {
  metaepochs > 10
}
