#' Function that runs one differential evolution metaepoch. Wrapper function for DEoptim::DEoptim.
#'
#' @param config_deoptim - list of DEoptim::DEoptim params
#'
#' @return list with named fields: solution, population, value. See
#' \code{\link{ga_metaepoch}} for more details.
#'
#' @export
#'
#' @examples
#' tree_height <- 3
#' empty_config_deoptim <- lapply(1:tree_height, function(x) {
#'   list()
#' })
#' deoptim_metaepoch(empty_config_deoptim)
deoptim_metaepoch <- function(config_deoptim) {
  # nocov start
  function(fitness,
           deme,
           lower,
           upper,
           minimize) {
    deoptim_fitness <- ifelse(minimize, fitness, function(x) {
      -1 * fitness(x)
    })
    config <- config_deoptim[[deme@level]]
    ignore_errors <-
      ifelse(is.null(config$ignore_errors),
        TRUE,
        config$ignore_errors
      )
    population_size <- nrow(deme@population)
    iterations_count <- 5
    control <-
      DEoptim::DEoptim.control(
        strategy = 4,
        NP = nrow(deme@population),
        itermax = iterations_count,
        trace = FALSE
      )
    params <- list(
      "fn" = deoptim_fitness,
      "lower" = lower,
      "upper" = upper,
      "control" = control
    )
    tryCatch(
      {
        result <- do.call(DEoptim::DEoptim, params)
      },
      error = function(e) {
        if (ignore_errors) {
          warning("DEoptim::DEoptim failed with error: ", e)
          return(NULL)
        } else {
          stop("DEoptim::DEoptim failed with error: ", e)
        }
      }
    )
    population <- result$member$pop
    value <-
      ifelse(minimize, result$optim$bestval, result$optim$bestval * -1)
    fitness_values <- rep(NaN, population_size)
    list(
      "solution" = result$optim$bestmem,
      "population" = population,
      "value" = value,
      "fitness_values" = fitness_values,
      "context" = NULL
    )
  }
}

default_cma_es_metaepoch <- function(tree_height) {
  empty_config_cmaes <- lapply(1:tree_height, function(x) {
    list()
  })
  cma_es_metaepoch(empty_config_cmaes)
} # nocov end
