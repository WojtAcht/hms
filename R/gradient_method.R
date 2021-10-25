run_gradient_method <- function(deme,
                                fitness,
                                optim_args = list(method = "L-BFGS-B",
                                                  poptim = 0.05,
                                                  pressel = 0.5,
                                                  control = list(fnscale = -1, maxit = 100))){
  opt <- suppressWarnings(
    do.call(stats::optim,
            c(list(fn = fitness,
                   par = deme@best_solution,
                   method = optim_args$method,
                   lower = optim_args$lower,
                   upper = optim_args$upper,
                   control = optim_args$control)))
    )
  result <- list("solution" = opt$par, "value" = opt$value, population = deme@population)
  update_deme(result, deme)
}
