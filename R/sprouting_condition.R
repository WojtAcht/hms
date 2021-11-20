#' Default sprouting condition based on given metric.
#'
#' @param metric - Metric used for deme distance comparison
#' @param max_distances_per_tree_level - numeric
#'
#' @return logical
#' @export
#'
#' @examples
max_metric_sprouting_condition <- function(metric, max_distances_per_tree_level) {
  function(potential_sprout, potential_sprout_level, demes) {
    level_demes <- Filter(function(d) {
      identical(d@level, potential_sprout_level)
    }, demes)

    single_deme_condition <- function(deme) {
      if (is.null(deme@sprout)) {
        FALSE
      } else {
        metric(deme@sprout, potential_sprout) < max_distances_per_tree_level[[potential_sprout_level - 1]]
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
