euclidean_distance <- function(x, y) {
  sum((x - y)^2)
}

seconds_since <- function(start_time) {
  as.numeric(Sys.time() - start_time, units = "secs")
}
