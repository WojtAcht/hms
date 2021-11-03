#' Title
#' f(x) = 0 at x = (0, ..., 0)
#' x_i in  [-5.12, 5.12]
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
Rastrigin <- function(x) {
  sum <- 10 * length(x)
  for (x_i in x) {
    sum <- sum + (x_i^2 - 10*cos(2 * pi * x_i))
  }
  sum
}

#' Title
#' f(x) = 0 at x = (0, ..., 0)
#' x_i in [-32.768, 32.768]
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
Ackley <- function(x) {
  d <- length(x)
  a <- 20
  b <- 0.2
  c <- 2 * pi

  squares_sum <- 0
  for (x_i in x) {
    squares_sum <- squares_sum + x_i^2
  }

  cos_sum <- 0
  for (x_i in x) {
    cos_sum <- cos_sum + cos(c * x_i)
  }

  -a * exp(-b * sqrt((1 / d) * squares_sum)) - exp((1 / d) * cos_sum) + a + exp(1)
}

#' Title
#' Min: f(x) = 0 at x = (420.9687, ..., 420.9687)
#' x_i in [-500, 500]
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
Schwefel <- function(x) {
  d <- length(x)

  sum <- 0
  for (x_i in x) {
    sum <- sum + x_i * sin(sqrt(abs(x_i)))
  }

  418.9829 * d - sum
}
