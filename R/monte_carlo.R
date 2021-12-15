runif_sample <- function(lower, upper, n) {
  random_coordinate <- function(i) {
    stats::runif(length(lower),
                 # all domains are symmetric :)
                 min = lower[[1]],
                 max = upper[[1]]
    )
  }
  lapply(1:n, random_coordinate)
}

naive_monte_carlo <- function(lower, upper, fitness, n) {
  results <- lapply(runif_sample(lower, upper, n) ,fitness)
  max(unlist(results))
}

gradient_monte_carlo <- function(lower, upper, fitness, n, maxit = 100) {
  gradient <- function(x) {
    result <- stats::optim(
      fn = fitness,
      par = x,
      method = "L-BFGS-B",
      lower = lower,
      upper = upper,
      control = list(fnscale = -1, maxit = maxit)
    )
    result$value
  }
  results <- lapply(runif_sample(lower, upper, n), gradient)
  max(unlist(results))
}
