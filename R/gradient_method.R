#' Function that runs gradient method for one deme.
#'
#' @param deme - Deme
#' @param fitness - function
#' @param optim_args - list of additional parameters
#'
#' @export
#'
#' @examples
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
