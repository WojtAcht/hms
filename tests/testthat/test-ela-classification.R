test_that("Classification works for first separable 2D BBOB problem.", {
  lower <- c(-5, -5)
  upper <- c(5, 5)
  f <- smoof::makeBBOBFunction(2, 1, 1)
  problem_type <- classify_optimization_problem(f, lower, upper)
  expect_equal(as.character(problem_type), "separable")
})

test_that("Classification works for first low or moderate conditioning 2D BBOB problem.", {
  lower <- c(-5, -5)
  upper <- c(5, 5)
  f <- smoof::makeBBOBFunction(2, 6, 1)
  problem_type <- classify_optimization_problem(f, lower, upper)
  expect_equal(as.character(problem_type), "low-conditioning")
})
