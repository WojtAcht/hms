test_that("create_deme for 1D", {
  lower <- -5
  upper <- 5
  tree_height <- 3
  population_size <- 5
  sigma <- default_sigma(lower, upper, tree_height)
  deme <- create_deme(lower, upper, NULL, population_size, default_create_population(sigma))
  expect_equal(deme@level, 1)
  expect_equal(nrow(deme@population), 5)
  expect_equal(ncol(deme@population), 1)
  expect_true(deme@is_active)
  expect_null(deme@parent_id)
  expect_null(deme@sprout)
})

test_that("create_deme for 2D", {
  lower <- c(-5, -5)
  upper <- c(5, 5)
  tree_height <- 3
  population_size <- 5
  sigma <- default_sigma(lower, upper, tree_height)
  deme <- create_deme(lower, upper, NULL, population_size, default_create_population(sigma))
  expect_equal(deme@level, 1)
  expect_equal(nrow(deme@population), 5)
  expect_equal(ncol(deme@population), 2)
  expect_true(deme@is_active)
  expect_null(deme@parent_id)
  expect_null(deme@sprout)
})

test_that("create_deme with parent for 2D", {
  lower <- c(-5, -5)
  upper <- c(5, 5)
  tree_height <- 2
  population_size <- 5
  sigma <- default_sigma(lower, upper, tree_height)
  parent_deme <- create_deme(lower, upper, NULL, population_size, default_create_population(sigma))
  sprout <- c(1, 1)
  parent_deme@best_solutions_per_metaepoch <- list(sprout)
  deme <- create_deme(lower, upper, parent_deme, population_size, default_create_population(sigma))
  expect_equal(deme@level, 2)
  expect_equal(nrow(deme@population), 5)
  expect_equal(ncol(deme@population), 2)
  expect_true(deme@is_active)
  expect_equal(deme@parent_id, parent_deme@id)
  expect_equal(deme@sprout, sprout)
})

test_that("create_deme error for invalid dimensions of population", {
  invalid_create_population <- function(mean, lower, upper, population_size, tree_level) {
    matrix(lower)
  }
  lower <- c(-5, -5)
  upper <- c(5, 5)
  tree_height <- 3
  population_size <- 5
  sigma <- default_sigma(lower, upper, tree_height)
  expect_error(
    create_deme(lower, upper, NULL, population_size, invalid_create_population),
    "Created population is invalid - wrong dimensions."
  )
})

test_that("update_deme", {
  dimensions <- 2
  lower <- c(-5, -5)
  upper <- c(5, 5)
  tree_height <- 2
  population_size <- 5
  sigma <- default_sigma(lower, upper, tree_height)
  deme <- create_deme(lower, upper, NULL, population_size, default_create_population(sigma))
  solution <- c(0, 0)
  value <- 0
  population <- t(matrix(rep(solution, population_size), nrow = dimensions))
  metaepoch_result <- list(
    "solution" = solution,
    "value" = value,
    "population" = population
  )
  updated_deme <- update_deme(metaepoch_result, deme)
  expect_equal(updated_deme@best_solution, solution)
  expect_equal(updated_deme@best_fitness, value)
  expect_equal(updated_deme@population, population)
  expect_true(updated_deme@is_active)
})

test_that("is_leaf", {
  lower <- c(-5, -5)
  upper <- c(5, 5)
  tree_height <- 2
  population_size <- 5
  sigma <- default_sigma(lower, upper, tree_height)
  parent_deme <- create_deme(lower, upper, NULL, population_size, default_create_population(sigma))
  sprout <- c(1, 1)
  parent_deme@best_solution <- sprout
  parent_deme@best_solutions_per_metaepoch <- list(sprout)
  deme <- create_deme(lower, upper, parent_deme, population_size, default_create_population(sigma))
  expect_false(is_leaf(deme, tree_height))
  deme@best_fitness <- 1
  expect_true(is_leaf(deme, tree_height))
  expect_false(is_leaf(parent_deme, tree_height))
})
