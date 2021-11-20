#' local_stopping_condition_metaepochs_without_improvement
#'
#' @param max_metaepochs_without_improvement - numeric
#'
#' @return
#' @export
#'
#' @examples
local_stopping_condition_metaepochs_without_improvement <- function(max_metaepochs_without_improvement) {
  function(deme, previous_metaepoch_snapshots) {
    best_fitness_metaepoch <- match(deme@best_fitness, deme@best_fitnesses_per_metaepoch)
    metaepoch_count <- length(deme@best_fitnesses_per_metaepoch)
    best_fitness_metaepoch < metaepoch_count - max_metaepochs_without_improvement
  }
}

#' local_stopping_condition_max_fitness_evaluations
#'
#' @param max_evaluations_count - numeric
#'
#' @return
#' @export
#'
#' @examples
local_stopping_condition_max_fitness_evaluations <- function(max_evaluations_count) {
  function(deme, previous_metaepoch_snapshots) {
    deme@evaluations_count < max_evaluations_count
  }
}

#' local_stopping_condition_trivial
#'
#' @return
#' @export
#'
#' @examples
local_stopping_condition_trivial <- function() {
  function(deme, previous_metaepoch_snapshots){
    FALSE
  }
}

default_local_stopping_condition <- local_stopping_condition_metaepochs_without_improvement(5)
