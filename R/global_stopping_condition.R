#' gsc_metaepochs_count
#'
#' @param metaepochs_count - numeric
#'
#' @return
#' @export
#'
#' @examples
gsc_metaepochs_count <- function(metaepochs_count) {
  function(metaepoch_snapshots) {
    length(metaepoch_snapshots) >= metaepochs_count
  }
}

#' gsc_max_fitness_evaluations
#'
#' @param max_evaluations - numeric
#'
#' @return
#' @export
#'
#' @examples
gsc_max_fitness_evaluations <- function(max_evaluations) {
  function(metaepoch_snapshots) {
    length(metaepoch_snapshots) > 0 &&
      utils::tail(metaepoch_snapshots, n = 1)[[1]]@fitness_evaluations > max_evaluations
  }
}

#' gsc_trivial
#'
#' @return
#' @export
#'
#' @examples
gsc_trivial <- function() {
  function(metaepoch_snapshots) {
    FALSE
  }
}

gsc_default <- gsc_metaepochs_count(10)
