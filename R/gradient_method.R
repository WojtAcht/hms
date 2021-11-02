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
