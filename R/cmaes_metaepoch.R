#' Function that runs one cmaes metaepoch. Wrapper function for cmaesr::cmaes.
#'
#' @param config_cmaes - list of cmaesr::cmaes params
#'
#' @return list with named fields: solution, population, value. See
#' \code{\link{ga_metaepoch}} for more details.
#'
#' @export
#'
#' @examples
#' tree_height <- 3
#' empty_config_cmaes <- lapply(1:tree_height, function(x) {
#'   list()
#' })
#' cmaes_metaepoch(empty_config_cmaes)
cmaes_metaepoch <- function(config_cmaes) {
  # nocov start
  function(fitness,
           suggestions,
           lower,
           upper,
           tree_level,
           minimize) {
    config <- config_cmaes[[tree_level]]
    legal_passed_param_names <- Filter(function(name) {
      name %in% methods::formalArgs(cmaesr::cmaes)
    }, names(config))
    population_size <- nrow(suggestions)
    suggestions_centroid <- colMeans(suggestions)
    iterations_count <- 5
    control <- list(
      "mu" = population_size %/% 2,
      "lambda" = population_size,
      "stop.ons" = list(cmaesr::stopOnMaxIters(iterations_count)),
      "log.population" = TRUE
    )
    domain_params <- list()
    for (variable_index in seq_along(lower)) {
      domain_param <- makeNumericParam(paste("x", variable_index, sep = ""),
        lower = lower[variable_index],
        upper = upper[variable_index]
      )
      domain_params <-
        c(domain_params, list(domain_param))
    }
    param_set <- do.call(ParamHelpers::makeParamSet, domain_params)
    smoof_fitness <-
      smoof::makeSingleObjectiveFunction(
        name = "hmsr_fitness_function",
        fn = fitness,
        minimize = minimize,
        has.simple.signature = TRUE,
        vectorized = FALSE,
        par.set = param_set
      )
    params <- list(
      "objective.fun" = smoof_fitness,
      "start.point" = suggestions_centroid,
      "control" = control,
      "monitor" = NULL
    )
    for (param_name in legal_passed_param_names) {
      params[param_name] <- config[param_name]
    }
    result <- do.call(cmaesr::cmaes, params)
    population <- utils::tail(result$population.trace, n = 1)[[1]]
    list(
      "solution" = result$best.param,
      "population" = population,
      "value" = result$best.fitness
    )
  }
}

default_cmaes_metaepoch <- function(tree_height) {
  empty_config_cmaes <- lapply(1:tree_height, function(x) {
    list()
  })
  cmaes_metaepoch(empty_config_cmase)
} # nocov end
