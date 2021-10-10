#' Function that runs one GA metaepoch.
#'
#' @param config_ga - list of GA::ga params
#'
#' @return list with named fields: solution, population, value
#' @export
#'
#' @examples
ga_metaepoch <- function(config_ga) {
  function(fitness, suggestions, lower, upper, tree_level) {
    config <- config_ga[tree_level]
    legal_passed_param_names <- Filter(function(name) {
      name %in% methods::formalArgs(GA::ga)
    }, config)
    params <- list("maxiter" = 10)
    for (param_name in legal_passed_param_names) {
      params[param_name] <- config[param_name]
    }
    params$fitness <- fitness
    params$lower <- lower
    params$upper <- upper
    params$suggestions <- suggestions
    params$type <- "real-valued"
    params$monitor <- FALSE

    # TODO Always better to have more data :)
    params$keepBest <- TRUE

    GA <- do.call(GA::ga, params)
    list("solution" = c(GA@solution), "population" = GA@population, "value" = GA@fitnessValue)
  }
}
