default_population_sizes <- function(tree_height) {
  population_size <- 60
  population_size_ratio <- 0.5
  population_sizes <- c()
  for (height in 1:tree_height) {
    population_sizes <- c(population_sizes, population_size)
    population_size <- round(population_size * population_size_ratio)
  }
  population_sizes
}

default_sigma <- function(lower, upper, tree_height) {
  sigma_ratio <- 0.04
  sigma_exponent <- 0.5
  domain_length <- upper - lower
  sigma <- list()
  for (height in 1:tree_height) {
    sigma <- c(sigma, list(domain_length * sigma_ratio))
    sigma_ratio <- sigma_ratio * sigma_exponent
  }
  sigma
}

default_gradient_method_args <- list(
  method = "L-BFGS-B",
  poptim = 0.05,
  pressel = 0.5,
  control = list(fnscale = -1, maxit = 100)
)
