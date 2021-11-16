default_population_size_per_tree_level <- function(tree_height) {
  population_size <- 60
  population_size_ratio <- 0.5
  population_size_per_tree_level <- c()
  for (height in 1:tree_height) {
    population_size_per_tree_level <- c(population_size_per_tree_level, population_size)
    population_size <- round(population_size * population_size_ratio)
  }
  population_size_per_tree_level
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
