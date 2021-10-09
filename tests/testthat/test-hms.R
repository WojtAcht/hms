test_that("multiplication works", {
  f <- function(x) { x }
  sigma <- c(1,1,1,1,1,1)
  result <- hms(fitness = f, lower = -5, upper = 5, sigma = sigma, population_size = 10)
  expect_true(abs(5-result) < 1e5)
})
