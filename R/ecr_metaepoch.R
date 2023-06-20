#' Function that runs one ecr metaepoch. Wrapper function for ecr::ecr.
#'
#' @param config_ecr - list of ecr::ecr params
#'
#' @return list with named fields: solution, population, value. See
#' \code{\link{ga_metaepoch}} for more details.
#'
#' @export
#'
#' @examples
#' tree_height <- 3
#' empty_config_ecr <- lapply(1:tree_height, function(x) {
#'   list()
#' })
#' ecr_metaepoch(empty_config_ecr)
ecr_metaepoch <- function(config_ecr) { # nocov start
  function(fitness, suggestions, lower, upper, tree_level, minimize) {
    config <- config_ecr[[tree_level]]
    legal_passed_param_names <- Filter(function(name) {
      name %in% methods::formalArgs(ecr::ecr)
    }, names(config))
    population_size <- nrow(suggestions)
    iterations_count <- 5
    params <- list(
      "mu" = population_size,
      "mutator" = ecr::setup(ecr::mutGauss, lower = lower, upper = upper),
      "lambda" = 1L
    )
    for (param_name in legal_passed_param_names) {
      params[param_name] <- config[param_name]
    }
    params$fitness.fun <- fitness
    params$n.objectives <- 1L
    params$minimize <- minimize
    params$lower <- lower
    params$upper <- upper
    params$n.dim <- length(lower)
    params$initial.solutions <- matrix_to_list(suggestions)
    params$representation <- "float"
    params$monitor <- FALSE
    params$terminators <- list(ecr::stopOnIters(max.iter = iterations_count * population_size))
    result <- do.call(ecr::ecr, params)
    population <- list_to_matrix(result$last.population, length(lower))
    list("solution" = result$best.x[[1]], "population" = population, "value" = result$best.y[[1]])
  }
}

default_ecr_metaepoch <- function(tree_height) {
  empty_config_ecr <- lapply(1:tree_height, function(x) {
    list()
  })
  ecr_metaepoch(empty_config_ecr)
} # nocov end
