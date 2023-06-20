test_that("get_monitor_level works:", {
  expect_equal(get_monitor_level("verbose_tree"), MONITOR_LEVEL_VERBOSE_TREE)
  expect_equal(get_monitor_level("basic_tree"), MONITOR_LEVEL_BASIC_TREE)
  expect_equal(get_monitor_level("basic"), MONITOR_LEVEL_BASIC)
  expect_equal(get_monitor_level("none"), MONITOR_LEVEL_NONE)
})

test_that("log_metaepoch_snapshot works:", {
  metaepoch_snapshot <- methods::new(
    "MetaepochSnapshot",
    demes = list(),
    best_fitness = 0,
    best_solution = 0,
    time_in_seconds = 0,
    fitness_evaluations = 0,
    blocked_sprouts = list(),
    is_evolutionary = TRUE
  )
  root_id <- 0
  metaepochs_count <- 1
  monitor_level <- MONITOR_LEVEL_BASIC
  expect_output(
    log_metaepoch_snapshot(
      metaepoch_snapshot,
      root_id,
      metaepochs_count,
      monitor_level
    )
  )
})
