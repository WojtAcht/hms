#' Rastrigin benchmark function
#' f(x) = 0 at x = (0, ..., 0)
#' x_i in  \[-5.12, 5.12\]
#'
#' @param x - numeric
#'
#' @return numeric value of Rastrigin function
#' @export
Rastrigin <- function(x) {
  sum <- 10 * length(x)
  for (x_i in x) {
    sum <- sum + (x_i^2 - 10 * cos(2 * pi * x_i))
  }
  sum
}

#' Ackley benchmark function
#' f(x) = 0 at x = (0, ..., 0)
#' x_i in \[-32.768, 32.768\]
#'
#' @param x - numeric
#'
#' @return numeric value of Ackley function
#' @export
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

#' Schwefel benchmark function
#' Min: f(x) = 0 at x = (420.9687, ..., 420.9687)
#' x_i in \[-500, 500\]
#'
#' @param x - numeric
#'
#' @return numeric value of Schwefel function
#' @export
Schwefel <- function(x) {
  d <- length(x)

  sum <- 0
  for (x_i in x) {
    sum <- sum + x_i * sin(sqrt(abs(x_i)))
  }

  418.9829 * d - sum
}


#' Griewank benchmark function
#' Min: f(x) = 0 at x = (0, ..., 0)
#' x_i in \[-600, 600\]
#'
#' @param x - numeric
#'
#' @return numeric value of Griewank function
#' @export
Griewank <- function(x) {
  i <- c(1:length(x))
  sum <- sum(x^2 / 4000)
  prod <- prod(cos(x / sqrt(i)))

  y <- sum - prod + 1
  return(y)
}


#' Baele benchmark function
#' Min: f(x) = 0 at x = (3, 0.5)
#' x1, x2 in \[-4.5, 4.5\]
#'
#' @param x - numeric
#'
#' @return numeric value of Baele function
#' @export
Baele <- function(x) {
  x1 <- x[[1]]
  x2 <- x[[2]]
  (1.5 - x1 + x1 * x2)^2 + (2.25 - x1 + x1 * x2^2)^2 + (2.625 - x1 + x1 * x2^3)^2
}

#' Eggholder benchmark function
#' Min: f(x) = -959.6407 at x = (512, 404.2319)
#' x1, x2 in \[-512, 512]
#'
#' @param x - numeric
#'
#' @return numeric value of Eggholder function
#' @export
Eggholder <- function(x) {
  x1 <- x[[1]]
  x2 <- x[[2]]
  -1 * (x2 + 47) * sin(sqrt(abs(x1 / 2 + x2 + 47))) - x1 * sin(sqrt(abs(x1 - x2 - 47)))
}
