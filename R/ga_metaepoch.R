#' Function that runs one GA metaepoch. Wrapper function for GA::ga.
#'
#' @param config_ga - list of GA::ga params
#'
#' @return list with named fields: solution, population, value, fitness_values.
#' A solution is a value of the decision variable giving the best fitness.
#' A population is a matrix representing final population.
#' A value is the value of a fitness function for the solution.
#' A fitness_values is a vector of fitness values for the final population.
#'
#' @export
#'
#' @examples
#' tree_height <- 3
#' empty_config_ga <- lapply(1:tree_height, function(x) {
#'   list()
#' })
#' ga_metaepoch(empty_config_ga)
ga_metaepoch <- function(config_ga) {
  function(fitness, suggestions, lower, upper, tree_level, minimize) {
    ga_fitness <- ifelse(minimize, function(x) {
      -1 * fitness(x)
    }, fitness)
    config <- config_ga[[tree_level]]
    ignore_errors <- ifelse(is.null(config$ignore_errors), TRUE, config$ignore_errors)
    legal_passed_param_names <- Filter(function(name) {
      name %in% methods::formalArgs(GA::ga)
    }, names(config))
    params <- list("maxiter" = 5, "popSize" = nrow(suggestions))
    for (param_name in legal_passed_param_names) {
      params[param_name] <- config[param_name]
    }
    # GA is used for maximization of a fitness function
    params$fitness <- ga_fitness
    params$lower <- lower
    params$upper <- upper
    params$suggestions <- suggestions
    params$type <- "real-valued"
    params$monitor <- FALSE
    tryCatch(
      {
        GA <- do.call(GA::ga, params)
      },
      error = function(e) {
        if(ignore_errors){
          warning("GA::ga failed with error: ", e)
          return(NULL)
        }
        else{
          stop("GA::ga failed with error: ", e)
        }
      }
    )
    value <- ifelse(minimize, GA@fitnessValue * -1, GA@fitnessValue)
    fitness_values <- ifelse(minimize, GA@fitness * -1, GA@fitness)
    list(
      "solution" = c(GA@solution[1, ]),
      "population" = GA@population,
      "value" = value,
      "fitness_values" = fitness_values
    )
  }
}

default_ga_metaepoch <- function(tree_height) {
  empty_config_ga <- lapply(1:tree_height, function(x) {
    list()
  })
  ga_metaepoch(empty_config_ga)
}
