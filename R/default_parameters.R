default_sigma <- function(lower, upper, tree_height) {
  sigma_ratio <- 0.04
  sigma_exponent <- 0.5
  domain_length <- upper - lower
  sigma <- list()
  for(height in 1:tree_height){
    sigma <- c(sigma, list(domain_length * sigma_ratio))
    sigma_ratio <- sigma_ratio * sigma_exponent
  }
  sigma
}

sprouting_condition_default_euclidean_distances <- function(sigma) {
  sprouting_condition_distance_ratio <- 0.6
  lapply(sigma, function(x){sum(x * sprouting_condition_distance_ratio)})
}
