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
           deme,
           lower,
           upper,
           minimize) {
    cma_es_fitness <- ifelse(minimize, fitness, function(x) {
      -1 * fitness(x)
    })
    config <- config_cmaes[[deme@level]]
    ignore_errors <- ifelse(is.null(config$ignore_errors), TRUE, config$ignore_errors)
    population_size <- nrow(deme@population)
    par <- if (deme@evaluations_count == 0) {
      deme@sprout
    } else {
      colMeans(deme@population)
    }
    iterations_count <- 5
    control <- list(
      "maxit" = iterations_count,
      "keep.best" = TRUE,
      "mu" = population_size %/% 2,
      "lambda" = population_size,
      "diag.sigma" = TRUE
    )
    if (!is.null(deme@context$sigma)) {
      control$sigma <- deme@context$sigma
    }
    params <- list(
      "par" = par,
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
        if (ignore_errors) {
          warning("cmaes::cma_es failed with error: ", e)
          return(NULL)
        } else {
          stop("cmaes::cma_es failed with error: ", e)
        }
      }
    )
    if (is.null(result$par)) {
      # Something went wrong, the result is NULL.
      warning("cmaes::cma_es returned NULL")
      return(NULL)
    }
    population <- matrix(rep(result$par, population_size), ncol = length(result$par), byrow = TRUE)
    value <- ifelse(minimize, result$value, result$value * -1)
    fitness_values <- rep(value, population_size)
    sigma <- utils::tail(result$diagnostic$sigma, n = 1)
    list(
      "solution" = result$par,
      "population" = population,
      "value" = value,
      "fitness_values" = fitness_values,
      "context" = list("sigma" = sigma)
    )
  }
}

default_cma_es_metaepoch <- function(tree_height) {
  empty_config_cmaes <- lapply(1:tree_height, function(x) {
    list()
  })
  cma_es_metaepoch(empty_config_cmaes)
} # nocov end
