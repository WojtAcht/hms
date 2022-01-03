test_that("default population size per tree level:", {
  tree_height <- 5
  expected_population_sizes <- c(60, 30, 15, 8, 4)
  expect_equal(default_population_sizes(tree_height), expected_population_sizes)
})

test_that("default sigma - 5d Schwefel:", {
  lower <- rep(-500, 5)
  upper <- rep(500, 5)
  tree_height <- 3
  expected_sigma <- list(rep(40, 5), rep(20, 5), rep(10, 5))
  expect_equal(default_sigma(lower, upper, tree_height), expected_sigma)
})

test_that("default sigma - function with non-cube domain:", {
  lower <- c(-500, -200, -100)
  upper <- c(500, 200, 100)
  tree_height <- 3
  expected_sigma <- list(c(40, 16, 8), c(20, 8, 4), c(10, 4, 2))
  expect_equal(default_sigma(lower, upper, tree_height), expected_sigma)
})

test_that("default sprouting distance - 5d Schwefel with default sigma:", {
  lower <- rep(-500, 5)
  upper <- rep(500, 5)
  tree_height <- 3
  sigma <- default_sigma(lower, upper, tree_height)
  expected_sprouting_distances <- list(120, 60, 30)
  expect_equal(sprouting_default_euclidean_distances(sigma), expected_sprouting_distances)
})

test_that("default sprouting distance - function with non-cube domain:", {
  lower <- c(-500, -200, -100)
  upper <- c(500, 200, 100)
  tree_height <- 3
  sigma <- default_sigma(lower, upper, tree_height)
  expected_sprouting_distances <- list(38.4, 19.2, 9.6)
  expect_equal(sprouting_default_euclidean_distances(sigma), expected_sprouting_distances)
})
