#' rtnorm mutation - truncated normal distribution
#'
#' @param lower - numeric
#' @param upper - numeric
#' @param sd - numeric
#'
#' @return function that can be used as a GA mutation function
#' @export
rtnorm_mutation <- function(lower, upper, sd) {
  function(object, parent) {
    parent <- as.vector(object@population[parent, ])
    random_coordinate <- function(i) {
      msm::rtnorm(
        mean = parent[[i]],
        sd = sd[[i]],
        lower = lower[[i]],
        upper = upper[[i]],
        n = 1
      )
    }
    as.vector(mapply(random_coordinate, seq_along(lower)))
  }
}
