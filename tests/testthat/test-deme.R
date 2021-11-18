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
  expect_true(deme@isActive)
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
  expect_true(deme@isActive)
  expect_null(deme@parent_id)
  expect_null(deme@sprout)
})
