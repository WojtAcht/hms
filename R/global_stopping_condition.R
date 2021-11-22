#' global_stopping_condition_metaepochs_count
#'
#' @param metaepochs_count - numeric
#'
#' @return
#' @export
#'
#' @examples
global_stopping_condition_metaepochs_count <- function(metaepochs_count) {
  function(metaepoch_snapshots) {
    length(metaepoch_snapshots) >= metaepochs_count
  }
}

#' global_stopping_condition_max_fitness_evaluations
#'
#' @param max_evaluations - numeric
#'
#' @return
#' @export
#'
#' @examples
global_stopping_condition_max_fitness_evaluations <- function(max_evaluations) {
  function(metaepoch_snapshots) {
    length(metaepoch_snapshots) > 0 &&
      utils::tail(metaepoch_snapshots, n = 1)[[1]]@fitness_evaluations > max_evaluations
  }
}

#' global_stopping_condition_trivial
#'
#' @return
#' @export
#'
#' @examples
global_stopping_condition_trivial <- function() {
  function(metaepoch_snapshots) {
    FALSE
  }
}

default_global_stopping_condition <- global_stopping_condition_metaepochs_count(10)
