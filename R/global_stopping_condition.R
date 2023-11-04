#' Factory function for a global stopping condition that stops
#' the computation after given number of metaepochs.
#'
#' @param metaepochs_count - numeric - maximum number of metaepochs
#'
#' @return Function that receives a list of metaepoch snapshots
#' and returns a Boolean value determining whether the computation
#' should be stopped based on how many metaepochs have passed,
#'  which can be used as a global stopping condition for the hms function.
#'
#' @export
#'
#' @examples
#' global_stopping_condition <- gsc_metaepochs_count(10)
gsc_metaepochs_count <- function(metaepochs_count) {
  function(metaepoch_snapshots) {
    length(metaepoch_snapshots) >= metaepochs_count
  }
}

#' Factory function for a global stopping condition that stops
#' the computation after fitness function has been evaluated
#' given number of times.
#'
#' @param max_evaluations - numeric - maximum number of fitness function evaluations
#'
#' @return Function that receives a list of metaepoch snapshots
#' and returns a Boolean value determining whether the computation
#' should be stopped based on how many fitness function evaluations
#' have been made, which can be used as a global stopping condition
#' for the hms function.
#'
#' @export
#'
#' @examples
#' global_stopping_condition <- gsc_max_fitness_evaluations(10000)
gsc_max_fitness_evaluations <- function(max_evaluations) {
  function(metaepoch_snapshots) {
    length(metaepoch_snapshots) > 0 &&
      utils::tail(metaepoch_snapshots, n = 1)[[1]]@fitness_evaluations > max_evaluations
  }
}

#' Factory function for a global stopping condition that never stops
#' the computation. It results in hms running until there are no more
#' active demes.
#'
#' @return function that always returns \code{FALSE}, which can be used
#' as a global stopping condition for the hms function.
#'
#' @export
#'
#' @examples
#' global_stopping_condition <- gsc_trivial()
gsc_trivial <- function() {
  function(metaepoch_snapshots) {
    FALSE
  }
}

gsc_default <- gsc_metaepochs_count(10)
