test_that("Euclidean distance works:", {
  x1 <- c(5, 5, 5, 5, 5)
  y1 <- c(4, 4, 4, 4, 4)
  expect_equal(euclidean_distance(x1, y1), sqrt(5))
  x2 <- c(5, 5)
  y2 <- c(3, 3)
  expect_equal(euclidean_distance(x2, y2), sqrt(8))
})

test_that("Manhattan distance works:", {
  x1 <- c(5, 5, 5, 5, 5)
  y1 <- c(4, 4, 4, 4, 4)
  expect_equal(manhattan_distance(x1, y1), 5)
  x2 <- c(5, 5)
  y2 <- c(3, 3)
  expect_equal(manhattan_distance(x2, y2), 4)
})

test_that("seconds_since works:", {
  start_time <- Sys.time()
  Sys.sleep(1)
  expect_true(seconds_since(start_time) > 1)
})

test_that("matrix_to_list works:", {
  x <- list(c(1, 2, 3), c(1, 2, 3), c(1, 2, 3))
  matrix_from_x <- list_to_matrix(x, 3)
  expect_equal(nrow(matrix_from_x), 3)
  expect_equal(ncol(matrix_from_x), 3)
  matrix_as_list <- matrix_to_list(matrix_from_x)
  expect_equal(x, matrix_as_list)
})
