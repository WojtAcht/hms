#' Euclidean distance
#'
#' @param x - numeric
#' @param y - numeric
#'
#' @return numeric - euclidean distance between x and y
#' @export
#'
#' @examples
#' euclidean_distance(c(1, 1), c(1, 2))
euclidean_distance <- function(x, y) {
  sqrt(sum((x - y)^2))
}

#' Manhattan distance
#'
#' @param x - numeric
#' @param y - numeric
#'
#' @return numeric - manhattan distance between x and y
#' @export
#'
#' @examples
#' manhattan_distance(c(1, 1), c(1, 2))
manhattan_distance <- function(x, y) {
  sum(mapply(abs, x - y))
}

seconds_since <- function(start_time) {
  as.numeric(Sys.time() - start_time, units = "secs")
}

matrix_to_list <- function(x) {
  lapply(seq_len(nrow(x)), function(i) x[i, ])
}

list_to_matrix <- function(x, ncol) {
  matrix(unlist(x), ncol = ncol, byrow = TRUE)
}
