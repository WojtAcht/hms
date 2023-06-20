test_that("HMS works - trivial 1D function:", {
  set.seed(1)
  f <- function(x) {
    x
  }
  result <- hms(
    fitness = f,
    lower = -5,
    upper = 5,
    monitor_level = "none"
  )
  expected_result <- 5
  expect_true(euclidean_distance(expected_result, result@best_solution) < 1e-2)
  expect_output(printTree(result))
  expect_output(printBlockedSprouts(result))
  expect_visible(summary(result))
  expect_output(print(result))
})

test_that("HMS works - Rastrigin:", {
  set.seed(1)
  Rastrigin <- smoof::makeRastriginFunction(2L)
  lower <- c(-5.12, -5.12)
  upper <- c(5.12, 5.12)
  result <- hms(
    fitness = Rastrigin,
    minimize = TRUE,
    lower = lower,
    upper = upper,
    monitor_level = "none"
  )
  expected_result <- c(0, 0)
  expect_true(euclidean_distance(expected_result, result@best_solution) < 1e-3)
})

test_that("HMS works - Ackley:", {
  set.seed(1)
  Ackley <- smoof::makeAckleyFunction(2L)
  lower <- c(-32.768, -32.768)
  upper <- c(32.768, 32.768)
  result <- hms(
    fitness = Ackley,
    minimize = TRUE,
    lower = lower,
    upper = upper,
    monitor_level = "none"
  )
  expected_result <- c(0, 0)
  expect_true(euclidean_distance(expected_result, result@best_solution) < 1e-2)
})

test_that("HMS works - Schwefel:", {
  set.seed(1)
  Schwefel <- smoof::makeSchwefelFunction(2L)
  lower <- c(-500, -500)
  upper <- c(500, 500)
  result <- hms(
    fitness = Schwefel,
    minimize = TRUE,
    lower = lower,
    upper = upper,
    monitor_level = "none"
  )
  expected_result <- c(420.9687, 420.9687)
  expect_true(euclidean_distance(expected_result, result@best_solution) < 1e0)
})

test_that("HMS works - Griewank:", {
  set.seed(1)
  Griewank <- smoof::makeGriewankFunction(2L)
  lower <- c(-600, -600)
  upper <- c(600, 600)
  result <- hms(
    fitness = Griewank,
    minimize = TRUE,
    lower = lower,
    upper = upper,
    monitor_level = "none"
  )
  expected_result <- c(0, 0)
  expect_true(euclidean_distance(expected_result, result@best_solution) < 1e1)
})

test_that("HMS works - Baele:", {
  set.seed(1)
  Baele <- smoof::makeBealeFunction()
  lower <- c(-4.5, -4.5)
  upper <- c(4.5, 4.5)
  result <- hms(
    fitness = Baele,
    minimize = TRUE,
    lower = lower,
    upper = upper,
    monitor_level = "none"
  )
  expected_result <- c(3, 0.5)
  expect_true(euclidean_distance(expected_result, result@best_solution) < 1e-2)
})

test_that("HMS works - Baele with gradient metaepoch:", {
  set.seed(1)
  Baele <- smoof::makeBealeFunction()
  lower <- c(-4.5, -4.5)
  upper <- c(4.5, 4.5)
  fitness <- function(x) {
    -1 * Baele(x)
  }
  result <- hms(
    fitness = fitness,
    lower = lower,
    upper = upper,
    with_gradient_method = TRUE,
    monitor_level = "none"
  )
  expected_result <- c(3, 0.5)
  expect_true(euclidean_distance(expected_result, result@best_solution) < 1e-4)
})

test_that("HMS works - Eggholder with gradient method:", {
  set.seed(1)
  Eggholder <- smoof::makeEggholderFunction()
  lower <- c(-512, -512)
  upper <- c(512, 512)
  sigma <- list(c(200, 200), c(100, 100), c(50, 50))
  ga_config <- list(
    list(
      pmutation = 0.4, mutation = rtnorm_mutation(lower, upper, sigma[[1]])
    ),
    list(
      pmutation = 0.2,
      mutation = rtnorm_mutation(lower, upper, sigma[[2]])
    ),
    list(
      pmutation = 0.2,
      mutation = rtnorm_mutation(lower, upper, sigma[[3]])
    )
  )
  result <- hms(
    fitness = Eggholder,
    minimize = TRUE,
    tree_height = 3,
    lower = lower,
    upper = upper,
    run_metaepoch = ga_metaepoch(ga_config),
    population_sizes = c(50, 30, 15),
    sigma = sigma,
    gsc = gsc_max_fitness_evaluations(25000),
    sc = sc_max_metric(euclidean_distance, c(40, 20, 10)),
    lsc = lsc_metaepochs_without_improvement(15),
    monitor_level = "none",
    with_gradient_method = TRUE
  )
  expected_solution <- c(512, 404.2319)
  expected_fitness <- Eggholder(expected_solution)
  expect_true(abs(result@best_fitness - expected_fitness) < 1e1)
})
