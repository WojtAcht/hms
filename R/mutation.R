#' Factory function that creates normal mutation function
#'
#' Given the domain bounds and standard deviation returns a function
#' compatible with GA interface that performs a mutation on the given
#' individual using truncated normal distribution.
#'
#' @param lower - Lower bound of the problem's domain
#' @param upper - Upper bound of the problem's domain
#' @param sd - Standard deviation of the truncated normal distribution
#'  used for the mutation
#'
#' @return Function that takes two parameters (the GA object \code{object}
#' and an individual to perform the mutation on \code{parent}) and returns
#' a new individual that is the result of normal mutation applied to the parent.
#'
#' @export
#'
#' @examples
#' mutation <- rtnorm_mutation(
#'   lower = rep(-500, 5),
#'   upper = rep(500, 5),
#'   sd = rep(50, 5)
#' )
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
