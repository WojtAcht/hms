% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cma_es_metaepoch.R
\name{cma_es_metaepoch}
\alias{cma_es_metaepoch}
\title{Function that runs one cmaes metaepoch. Wrapper function for cmaes::cma_es.}
\usage{
cma_es_metaepoch(config_cmaes)
}
\arguments{
\item{config_cmaes}{\itemize{
\item list of cmaes::cma_es params
}}
}
\value{
list with named fields: solution, population, value. See
\code{\link{ga_metaepoch}} for more details.
}
\description{
Function that runs one cmaes metaepoch. Wrapper function for cmaes::cma_es.
}
\examples{
tree_height <- 3
empty_config_cma_es <- lapply(1:tree_height, function(x) {
  list()
})
cma_es_metaepoch(empty_config_cma_es)
}
