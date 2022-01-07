test_that("HMS is stopped when tree_height is smaller than 1", {
  expected_error <- "Max tree height has to be greater or equal 1."
  expect_error(hms(fitness = function(x) {
    x
  }, tree_height = 0), expected_error)
  expect_error(hms(fitness = function(x) {
    x
  }, tree_height = -1), expected_error)
})

test_that("HMS is stopped when lower or upper is missing", {
  expected_error <- "A lower and upper range of values must be provided."
  expect_error(hms(fitness = function(x) {
    x
  }), expected_error)
  expect_error(hms(fitness = function(x) {
    x
  }, lower = -5), expected_error)
  expect_error(hms(fitness = function(x) {
    x
  }, upper = 5), expected_error)
})

test_that("HMS is stopped when fitness is not a function", {
  expected_error <- "Fitness function must be provided."
  expect_error(hms(fitness = 1), expected_error)
})
