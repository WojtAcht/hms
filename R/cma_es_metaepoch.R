#' Function that runs one cmaes metaepoch. Wrapper function for cmaes::cma_es.
#'
#' @param config_cmaes - list of cmaes::cma_es params
#'
#' @return list with named fields: solution, population, value. See
#' \code{\link{ga_metaepoch}} for more details.
#'
#' @export
#'
#' @examples
#' tree_height <- 3
#' empty_config_cma_es <- lapply(1:tree_height, function(x) {
#'   list()
#' })
#' cma_es_metaepoch(empty_config_cma_es)
cma_es_metaepoch <- function(config_cmaes) {
  # nocov start
  function(fitness,
           suggestions,
           lower,
           upper,
           tree_level,
           minimize) {
    cma_es_fitness <- ifelse(minimize, fitness, function(x) {
      -1 * fitness(x)
    })
    config <- config_cmaes[[tree_level]]
    population_size <- nrow(suggestions)
    suggestions_centroid <- colMeans(suggestions)
    iterations_count <- 5
    control <- list(
      "maxit" = iterations_count,
      "keep.best" = TRUE,
      "mu" = population_size %/% 2,
      "lambda" = population_size
    )
    params <- list(
      "par" = suggestions_centroid,
      "fn" = cma_es_fitness,
      "lower" = lower,
      "upper" = upper,
      "control" = control
    )
    tryCatch(
      {
        result <- do.call(cmaes::cma_es, params)
      },
      error = function(e) {
        warning("cmaes::cma_es failed with error: ", e)
        return(NULL)
      }
    )
    if(is.null(result$par)){
      # Something went wrong, the result is NULL.
      return(NULL)
    }
    population <- matrix(rep(result$par, population_size), ncol = length(result$par), byrow = TRUE)
    value <- ifelse(minimize, result$value, result$value * -1)
    list(
      "solution" = result$par,
      "population" = population,
      "value" = value
    )
  }
}

default_cma_es_metaepoch <- function(tree_height) {
  empty_config_cmaes <- lapply(1:tree_height, function(x) {
    list()
  })
  cma_es_metaepoch(empty_config_cmaes)
} # nocov end
