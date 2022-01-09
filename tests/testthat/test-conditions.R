test_that("Global stopping condition (metaepochs count) works:", {
  max_metaepochs_count <- 2
  global_stopping_condition <- gsc_metaepochs_count(max_metaepochs_count)
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
  metaepoch_snapshots <- list(metaepoch_snapshot)
  expect_false(global_stopping_condition(metaepoch_snapshots))
  new_metaepoch_snapshots <- c(metaepoch_snapshots, list(metaepoch_snapshot))
  expect_true(global_stopping_condition(new_metaepoch_snapshots))
})

test_that("Global stopping condition (fitness evaluations count) works:", {
  max_evaluations_count <- 2
  global_stopping_condition <- gsc_max_fitness_evaluations(max_evaluations_count)
  metaepoch_snapshot_with_evaluations_count_lower_than_max <- methods::new(
    "MetaepochSnapshot",
    demes = list(),
    best_fitness = 0,
    best_solution = 0,
    time_in_seconds = 0,
    fitness_evaluations = 1,
    blocked_sprouts = list(),
    is_evolutionary = TRUE
  )
  metaepoch_snapshots <- list(metaepoch_snapshot_with_evaluations_count_lower_than_max)
  expect_false(global_stopping_condition(metaepoch_snapshots))
  metaepoch_snapshot_with_evaluations_count_greater_than_max <- methods::new(
    "MetaepochSnapshot",
    demes = list(),
    best_fitness = 0,
    best_solution = 0,
    time_in_seconds = 0,
    fitness_evaluations = 3,
    blocked_sprouts = list(),
    is_evolutionary = TRUE
  )
  new_metaepoch_snapshots <- c(metaepoch_snapshots, list(metaepoch_snapshot_with_evaluations_count_greater_than_max))
  expect_true(global_stopping_condition(new_metaepoch_snapshots))
})

test_that("Global stopping condition (trivial) works:", {
  global_stopping_condition <- gsc_trivial()
  expect_false(global_stopping_condition(list()))
  expect_false(global_stopping_condition(NULL))
})

test_that("Local stopping condition (max metaepochs without improvement) works:", {
  local_stopping_condition <- lsc_metaepochs_without_improvement(5)
  deme <- methods::new(
    "Deme",
    id = "id1",
    population = matrix(),
    level = 2,
    best_fitness = 5,
    best_solution = 5,
    best_solutions_per_metaepoch = list(5, 4, 3, 2, 1),
    best_fitnesses_per_metaepoch = list(5, 4, 3, 2, 1),
    sprout = NULL,
    parent_id = NULL,
    evaluations_count = 0,
    is_active = TRUE
  )
  expect_false(local_stopping_condition(deme, list()))
  deme@best_fitnesses_per_metaepoch <- list(5, 4, 3, 2, 1, 0)
  deme@best_solutions_per_metaepoch <- list(5, 4, 3, 2, 1, 0)
  expect_true(local_stopping_condition(deme, list()))
})

test_that("Local stopping condition (max evaluations) works:", {
  local_stopping_condition <- lsc_max_fitness_evaluations(5)
  deme <- methods::new(
    "Deme",
    id = "id1",
    population = matrix(),
    level = 2,
    best_fitness = 0,
    best_solution = 0,
    best_solutions_per_metaepoch = list(),
    best_fitnesses_per_metaepoch = list(),
    sprout = NULL,
    parent_id = NULL,
    evaluations_count = 0,
    is_active = TRUE
  )
  expect_false(local_stopping_condition(deme, list()))
  deme@evaluations_count <- 6
  expect_true(local_stopping_condition(deme, list()))
})

test_that("Local stopping condition (mmetaepochs without active child) works:", {
  deme <- methods::new(
    "Deme",
    id = "id1",
    population = matrix(),
    level = 2,
    best_fitness = 0,
    best_solution = 0,
    best_solutions_per_metaepoch = list(),
    best_fitnesses_per_metaepoch = list(),
    sprout = NULL,
    parent_id = NULL,
    evaluations_count = 0,
    is_active = TRUE
  )
  active_child_deme <- methods::new(
    "Deme",
    id = "id2",
    population = matrix(),
    level = 2,
    best_fitness = 0,
    best_solution = 0,
    best_solutions_per_metaepoch = list(),
    best_fitnesses_per_metaepoch = list(),
    sprout = NULL,
    parent_id = "id1",
    evaluations_count = 0,
    is_active = TRUE
  )
  inactive_child_deme <- methods::new(
    "Deme",
    id = "id3",
    population = matrix(),
    level = 2,
    best_fitness = 0,
    best_solution = 0,
    best_solutions_per_metaepoch = list(),
    best_fitnesses_per_metaepoch = list(),
    sprout = NULL,
    parent_id = "id1",
    evaluations_count = 0,
    is_active = FALSE
  )
  snapshot <- function(demes) {
    methods::new(
      "MetaepochSnapshot",
      demes = demes,
      best_fitness = 0,
      best_solution = 0,
      time_in_seconds = 0,
      fitness_evaluations = 3,
      blocked_sprouts = list(),
      is_evolutionary = FALSE
    )
  }
  expect_false(lsc_metaepochs_without_active_child(1)(deme, list(snapshot(
    list(deme, active_child_deme)
  ))))
  expect_true(lsc_metaepochs_without_active_child(1)(deme, list(snapshot(
    list(deme, active_child_deme)
  ), snapshot(
    list(deme, inactive_child_deme)
  ))))
  expect_false(lsc_metaepochs_without_active_child(2)(deme, list(snapshot(
    list(deme, active_child_deme)
  ), snapshot(
    list(deme, inactive_child_deme)
  ))))
  expect_true(lsc_metaepochs_without_active_child(2)(deme, list(snapshot(
    list(deme, inactive_child_deme)
  ), snapshot(
    list(deme, inactive_child_deme)
  ))))
  expect_true(lsc_metaepochs_without_active_child(2)(deme, list(
    snapshot(list(deme, active_child_deme)), snapshot(list(deme, inactive_child_deme)), snapshot(list(deme, inactive_child_deme))
  )))
})

test_that("Local stopping condition (trivial) works:", {
  local_stopping_condition <- lsc_trivial()
  deme <- methods::new(
    "Deme",
    id = "id1",
    population = matrix(),
    level = 2,
    best_fitness = 0,
    best_solution = 0,
    best_solutions_per_metaepoch = list(),
    best_fitnesses_per_metaepoch = list(),
    sprout = NULL,
    parent_id = NULL,
    evaluations_count = 0,
    is_active = TRUE
  )
  expect_false(local_stopping_condition(deme, list()))
  expect_false(local_stopping_condition(NULL, list()))
})
