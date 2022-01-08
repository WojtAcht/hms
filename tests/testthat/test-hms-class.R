test_that("HMS result is valid - trivial 1D function:", {
  set.seed(1)
  f <- function(x) {
    x
  }
  lower <- -5
  upper <- 5
  result <- hms(
    fitness = f,
    lower = lower,
    upper = upper,
    monitor_level = "none"
  )
  root <- result@metaepoch_snapshots[[1]]@demes[[1]]
  metaepochs_count <- 10
  expect_equal(root@id, result@root_id)
  expect_equal(lower, result@lower)
  expect_equal(upper, result@upper)
  expect_equal(metaepochs_count, result@metaepochs_count)
  expect_equal(length(result@metaepoch_snapshots), result@metaepochs_count)
  expect_true(result@total_time_in_seconds >= result@total_metaepoch_time_in_seconds)
  expect_equal(result@deme_population_sizes, default_population_sizes(3))
  expect_true(all(mapply(function(snapshot) {
    snapshot@is_evolutionary
  }, result@metaepoch_snapshots)))
})
