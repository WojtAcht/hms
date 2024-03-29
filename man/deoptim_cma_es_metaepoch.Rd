% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deoptim_cma_es_metaepoch.R
\name{deoptim_cma_es_metaepoch}
\alias{deoptim_cma_es_metaepoch}
\title{Function that generates run_metaepoch function for two level HMS.
First level: DE, second level: CMA-ES.}
\usage{
deoptim_cma_es_metaepoch(deoptim_cma_es_config)
}
\arguments{
\item{deoptim_cma_es_config}{\itemize{
\item list that consists of two lists: DEoptim params and CMA-ES params.
}}
}
\value{
list with named fields: solution, population, value. See
\code{\link{ga_metaepoch}} for more details.
}
\description{
Function that generates run_metaepoch function for two level HMS.
First level: DE, second level: CMA-ES.
}
\examples{
tree_height <- 2
de_config <- list()
cma_es_config <- list()
config <- list(de_config, cma_es_config)
deoptim_cma_es_metaepoch(config)
}
