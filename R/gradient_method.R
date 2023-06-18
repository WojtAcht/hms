validate_gradient_method_args <- function(gradient_method_args, lower, upper) {
  gradient_method_args$lower <- lower
  gradient_method_args$upper <- upper
  if (gradient_method_args$poptim & (gradient_method_args$poptim < 0 | gradient_method_args$poptim > 1)) {
    stop("gradient_method_args: poptim value has to be within [0,1].")
  }
  gradient_method_args$control$maxit <- as.integer(gradient_method_args$control$maxit)
  if (is.null(gradient_method_args$control$fnscale)) {
    gradient_method_args$control$fnscale <- -1
  }
  if (gradient_method_args$control$fnscale > 0) {
    message("gradient_method_args: fnscale should not be positive.")
    gradient_method_args$control$fnscale <- -1 * gradient_method_args$control$fnscale
  }
  gradient_method_args
}

#' Function that runs gradient method for one deme.
#' Wrapper function for stats::optim.
#'
#' @param deme - Deme
#' @param fitness - fitness function
#' @param optim_args - list of additional parameters (stats::optim parameters)
#'
#' @return list with named fields: solution, population, value. See
#' \code{\link{ga_metaepoch}} for more details.
default_run_gradient_method <- function(deme, fitness, optim_args) {
  result <- suppressWarnings(
    do.call(
      stats::optim,
      c(list(
        fn = fitness,
        par = deme@best_solution,
        method = optim_args$method,
        lower = optim_args$lower,
        upper = optim_args$upper,
        control = optim_args$control
      ))
    )
  )
  list("solution" = result$par, "value" = result$value, "population" = deme@population)
}

run_gradient_metaepoch_for_leaves <- function(fitness,
                                              run_gradient_method,
                                              gradient_method_args,
                                              metaepoch_snapshots,
                                              tree_height) {
  last_metaepoch_snapshot <- utils::tail(metaepoch_snapshots, n = 1)
  last_metaepoch_demes <- last_metaepoch_snapshot[[1]]@demes
  demes_after_gradient_method <- c()
  for (deme in last_metaepoch_demes) {
    if (is_leaf(deme, tree_height)) {
      result <- run_gradient_method(deme, fitness, gradient_method_args)
      leaf_after_gradient_method <- update_deme(result, deme)
      demes_after_gradient_method <- c(demes_after_gradient_method, leaf_after_gradient_method)
    } else {
      demes_after_gradient_method <- c(demes_after_gradient_method, deme)
    }
  }
  demes_after_gradient_method
}
