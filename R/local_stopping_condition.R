#' lsc_metaepochs_without_improvement
#'
#' @param max_metaepochs_without_improvement - numeric
#'
#' @return
#' @export
#'
#' @examples
lsc_metaepochs_without_improvement <- function(max_metaepochs_without_improvement) {
  function(deme, previous_metaepoch_snapshots) {
    best_fitness_metaepoch <- match(deme@best_fitness, deme@best_fitnesses_per_metaepoch)
    metaepoch_count <- length(deme@best_fitnesses_per_metaepoch)
    !is_root(deme) & best_fitness_metaepoch <= metaepoch_count - max_metaepochs_without_improvement
  }
}

#' lsc_max_fitness_evaluations
#'
#' @param max_evaluations - numeric
#'
#' @return
#' @export
#'
#' @examples
lsc_max_fitness_evaluations <- function(max_evaluations) {
  function(deme, previous_metaepoch_snapshots) {
    !is_root(deme) & deme@evaluations_count > max_evaluations
  }
}

#' lsc_trivial
#'
#' @return
#' @export
#'
#' @examples
lsc_trivial <- function() {
  function(deme, previous_metaepoch_snapshots){
    FALSE
  }
}

lsc_default <- lsc_metaepochs_without_improvement(6)
