#' Function that generates run_metaepoch function for two level HMS.
#' First level: DE, second level: CMA-ES.
#'
#' @param deoptim_cma_es_config - list that consists of two lists: DEoptim params and CMA-ES params.
#'
#' @return list with named fields: solution, population, value. See
#' \code{\link{ga_metaepoch}} for more details.
#'
#' @export
#'
#' @examples
#' tree_height <- 2
#' de_config <- list()
#' cma_es_config <- list()
#' config <- list(de_config, cma_es_config)
#' deoptim_cma_es_metaepoch(config)
deoptim_cma_es_metaepoch <- function(deoptim_cma_es_config) {
  run_cma_es_metaepoch <- cma_es_metaepoch(deoptim_cma_es_config)
  run_de_metaepoch <- deoptim_metaepoch(deoptim_cma_es_config)
  run_metaepoch <- function(fitness,
                            deme,
                            lower,
                            upper,
                            minimize) {
    if (deme@level == 1) {
      run_de_metaepoch(fitness, deme, lower, upper, minimize)
    } else {
      run_cma_es_metaepoch(fitness, deme, lower, upper, minimize)
    }
  }
  return(run_metaepoch)
}
