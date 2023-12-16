#' Function that generates run_metaepoch function for two level HMS.
#' First level: GA, second level: CMA-ES.
#'
#' @param ga_cma_es_config - list that consists of two lists: GA params and CMA-ES params.
#'
#' @return list with named fields: solution, population, value. See
#' \code{\link{ga_metaepoch}} for more details.
#'
#' @export
#'
#' @examples
#' tree_height <- 2
#' ga_config <- list()
#' cma_es_config <- list()
#' config <- list(ga_configm cma_es_config)
#' ga_cma_es_metaepoch(config)
ga_cma_es_metaepoch <- function(ga_cma_es_config) {
  run_cma_es_metaepoch <- cma_es_metaepoch(ga_cma_es_config)
  run_ga_metaepoch <- ga_metaepoch(ga_cma_es_config)
  run_metaepoch <- function(fitness,
                            deme,
                            lower,
                            upper,
                            minimize) {
    if (deme@level == 1) {
      run_ga_metaepoch(fitness, deme, lower, upper, minimize)
    } else {
      run_cma_es_metaepoch(fitness, deme, lower, upper, minimize)
    }
  }
  return(run_metaepoch)
}
