HMSStartupMessage <- function() {
  msg <- "HMS - Hierarchic Memetic Strategy."
  return(msg)
}

.onAttach <- function(lib, pkg) {
  msg <- HMSStartupMessage()
  packageStartupMessage(msg)
  invisible()
}
