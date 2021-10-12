test_that("HMS works - trivial 1D function:", {
  f <- function(x) {
    x
  }
  sigma <- c(1, 1, 1, 1, 1, 1)
  result <- hms(
    fitness = f,
    lower = -5,
    upper = 5,
    sigma = sigma,
    population_size = 100,
    max_tree_height = 3
  )
  expected_result <- 5
  expect_true(euclidean_distance(expected_result, result@best_solution) < 1e-1)
})

test_that("HMS works - Rastrigin:", {
  Rastrigin <- function(x) {
    x1 <- x[[1]]
    x2 <- x[[2]]
    result <- 20 + x1^2 + x2^2 - 10 * (cos(2 * pi * x1) + cos(2 * pi * x2))
    -1 * result
  }
  default_sd <- c(1, 1)
  sigma <- list(default_sd, default_sd, default_sd, default_sd, default_sd, default_sd)
  lower <- c(-5.12, -5.12)
  upper <- c(5.12, 5.12)
  result <- hms(
    fitness = Rastrigin,
    lower = lower,
    upper = upper,
    sigma = sigma,
    population_size = 100,
    max_tree_height = 3
  )
  expected_result <- c(0, 0)
  expect_true(euclidean_distance(expected_result, result@best_solution) < 1e-1)
})

test_that("HMS works - Ackley:", {
  Ackley <- function(x) {
    x1 <- x[[1]]
    x2 <- x[[2]]
    result <- -20 * exp(-0.2 * sqrt(0.5 * (x1^2 + x2^2))) - exp(0.5 * (cos(2 * pi * x1) + cos(2 * pi * x2))) + exp(1) + 20
    -1 * result
  }
  default_sd <- c(1, 1)
  sigma <- list(default_sd, default_sd, default_sd, default_sd, default_sd, default_sd)
  lower <- c(-32.768, -32.768)
  upper <- c(32.768, 32.768)
  result <- hms(
    fitness = Ackley,
    lower = lower,
    upper = upper,
    sigma = sigma,
    population_size = 100,
    max_tree_height = 3
  )
  expected_result <- c(0, 0)
  expect_true(euclidean_distance(expected_result, result@best_solution) < 1e-1)
})

test_that("HMS works - Baele:", {
  Baele <- function(x) {
    x1 <- x[[1]]
    x2 <- x[[2]]
    result <- (1.5 - x1 + x1 * x2)^2 + (2.25 - x1 + x1 * x2^2)^2 + (2.625 - x1 + x1 * x2^3)^2
    -1 * result
  }
  default_sd <- c(1, 1)
  sigma <- list(default_sd, default_sd, default_sd, default_sd, default_sd, default_sd)
  lower <- c(-4.5, -4.5)
  upper <- c(4.5, 4.5)
  result <- hms(
    fitness = Baele,
    lower = lower,
    upper = upper,
    sigma = sigma,
    population_size = 50,
    max_tree_height = 3
  )
  expected_result <- c(3, 0.5)
  expect_true(euclidean_distance(expected_result, result@best_solution) < 1e-1)
})

test_that("HMS works - Eggholder:", {
  Eggholder <- function(x) {
    x1 <- x[[1]]
    x2 <- x[[2]]
    result <- -1 * (x2 + 47) * sin(sqrt(abs(x1 / 2 + x2 + 47))) - x1 * sin(sqrt(abs(x1 - x2 - 47)))
    -1 * result
  }
  default_sd <- c(1, 1)
  sigma <- list(default_sd, default_sd, default_sd, default_sd, default_sd, default_sd)
  lower <- c(-512, -512)
  upper <- c(512, 512)
  result <- hms(
    fitness = Eggholder,
    lower = lower,
    upper = upper,
    sigma = sigma,
    population_size = 100,
    max_tree_height = 3
  )
  expected_fitness <- Eggholder(c(512, 404.2319))
  expect_true(abs(result@best_fitness - expected_fitness) < 100)
})
