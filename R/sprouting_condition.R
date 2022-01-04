#' Default sprouting condition based on given metric.
#'
#' @param metric - Metric used for deme distance comparison (e.g.
#' euclidean_distance, manhattan_distance)
#' @param max_distances - numeric
#'
#' @return logical
#' @export
sc_max_metric <- function(metric, max_distances) {
  function(potential_sprout, potential_sprout_level, demes) {
    level_demes <- Filter(function(d) {
      identical(d@level, potential_sprout_level)
    }, demes)

    single_deme_condition <- function(deme) {
      if (is.null(deme@sprout)) {
        FALSE
      } else {
        metric(deme@sprout, potential_sprout) < max_distances[[potential_sprout_level - 1]]
      }
    }
    length(Filter(single_deme_condition, level_demes)) == 0
  }
}

sprouting_default_euclidean_distances <- function(sigma) {
  sprouting_condition_distance_ratio <- 0.6
  lapply(sigma, function(x) {
    sum(x * sprouting_condition_distance_ratio)
  })
}
