#' Title
#'
#' @param x - numeric
#' @param y - numeric
#'
#' @return
#' @export
#'
#' @examples
euclidean_distance <- function(x, y) {
  sqrt(sum((x - y)^2))
}

#' Title
#'
#' @param x - numeric
#' @param y - numeric
#'
#' @return
#' @export
#'
#' @examples
manhattan_distance <- function(x, y) {
  sum(mapply(abs, x - y))
}

seconds_since <- function(start_time) {
  as.numeric(Sys.time() - start_time, units = "secs")
}
