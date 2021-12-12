#' Function that runs one GA metaepoch.
#'
#' @param config_rgenoud - list of rgenoud::genoud params
#'
#' @return list with named fields: solution, population, value
#' @export
#'
#' @examples
rgenoud_metaepoch <- function(config_rgenoud) {
  function(fitness, suggestions, lower, upper, tree_level) {
    config <- config_rgenoud[[tree_level]]
    legal_passed_param_names <- Filter(function(name) {
      name %in% methods::formalArgs(rgenoud::genoud)
    }, names(config))
    params <- list("pop.size" = nrow(suggestions),
                   "max.generations" = 5)
    for (param_name in legal_passed_param_names) {
      params[param_name] <- config[param_name]
    }
    params$boundary.enforcement <- 2
    params$hard.generation.limit <- TRUE
    params$fn <- fitness
    params$Domains <- matrix(c(lower, upper), ncol=2)
    params$starting.values <- suggestions
    params$max <- TRUE
    params$nvars <- length(lower)
    params$print.level <- 0
    result <- suppressWarnings(do.call(rgenoud::genoud, params))
    population <- rnorm_population(
      mean=result$par,
      lower=lower,
      upper=upper,
      population_size=params$pop.size,
      tree_level=tree_level,
      sigma=default_sigma(lower, upper, tree_level)
    )
    list("solution" = result$par, "population" = population, "value" = result$value)
  }
}

default_rgenoud_metaepoch <- function(tree_height) {
  empty_config_rgenoud <- lapply(1:tree_height, function(x) {
    list()
  })
  rgenoud_metaepoch(empty_config_rgenoud)
}
