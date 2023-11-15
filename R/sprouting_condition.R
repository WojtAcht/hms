#' Default sprouting condition based on given metric.
#'
#' It allows an individual to sprout only if there are no other
#' demes on the target level that have centroid within the given
#' distance.
#'
#' @param metric - Metric used for deme distance comparison (e.g.
#' euclidean_distance, manhattan_distance)
#' @param max_distances - numeric - maximum distance to a centroid of
#' a deme on the target level that would allow the individual to sprout
#'
#' @return Function that can be used as a sprouting condition of hms.
#'
#' @export
#'
#' @examples
#' sprouting_condition <- sc_max_metric(euclidean_distance, c(20, 10))
sc_max_metric <- function(metric, max_distances) {
  function(potential_sprout, potential_sprout_level, demes) {
    level_demes <- Filter(function(d) {
      identical(d@level, potential_sprout_level)
    }, demes)

    single_deme_condition <- function(deme) {
      centroid <- colMeans(deme@population)
      metric(centroid, potential_sprout) < max_distances[[potential_sprout_level - 1]]
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
