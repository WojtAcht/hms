HMSStartupMessage <- function() {
  msg <- c(paste0("HMS - Hierarchic Memetic Strategy version ",
                  packageVersion("hms")))
  return(msg)
}

.onAttach <- function(lib, pkg) {
  # startup message
  msg <- HMSStartupMessage()
  if (!interactive()) {
    msg[1] <- paste("Package 'HMS' version", packageVersion("hms"))
  }
  packageStartupMessage(msg)
  invisible()
}
