evaluations_count <- 0
run_ga <- function(max_evaluations_count) {
  evaluations_count <<- 0
  f <- function(x) {
    if (evaluations_count > max_evaluations_count) {
      -10000
    } else {
      evaluations_count <<- evaluations_count + 1
      fitness(x)
    }
  }
  iter <- round(max_evaluations_count / 100) + 1
  result <- ga(
    type = "real-valued",
    fitness = f,
    lower = lower,
    upper = upper,
    monitor = FALSE,
    maxiter = iter,
    popSize =  100)
  result@fitnessValue
}

run_ga_n_times <- function(max_evaluations_count, n) {
  mapply(function(x) {
    run_ga(max_evaluations_count)
  }, 1:n)
}

plot_mean_ga <- function(fitness, lower, upper, evaluations, n) {
  results <- mapply(function(x){mean(run_ga_n_times(x,n))}, evaluations)
  plot(evaluations,
    results,
    ylim = c(min(results), max(results)),
    xlab = "evaluations",
    ylab = "fitness"
  )
  results
}

run_hms <- function(max_evaluations_count) {
  result <- hms(
    fitness = fitness,
    tree_height = 3,
    lower = lower,
    upper = upper,
    run_metaepoch = ga_metaepoch(ga_config),
    population_size_per_tree_level = c(50, 30, 15),
    sigma = sigma,
    global_stopping_condition = max_fitness_evaluations_global_stopping_condition(max_evaluations_count),
    sprouting_condition = max_metric_sprouting_condition(euclidean_distance, 25)
  )
  result@best_fitness
}
run_hms_n_times <- function(max_evaluations_count, n) {
  mapply(function(x) {
    run_hms(max_evaluations_count)
  }, 1:n)
}

plot_mean_hms <- function(fitness, lower, upper, sigma, evaluations, n) {
  results <- mapply(function(x){run_hms_n_times(x,n)}, evaluations)
  plot(evaluations,
    results,
    ylim = c(min(results), max(results)),
    xlab = "evaluations",
    ylab = "fitness"
  )
  results
}

plot_hms_ga <- function(fitness, lower, upper, sigma, evaluations, n=10) {
  hms_results <- lapply(evaluations, function(x){run_hms_n_times(x,n)})
  ga_results <- lapply(evaluations, function(x){run_ga_n_times(x,n)})
  ga_mean_results <- mapply(mean, ga_results)
  hms_mean_results <- mapply(mean, hms_results)
  ga_median_results <- mapply(median, ga_results)
  hms_median_results <- mapply(median, hms_results)
  min_fitness <- min(c(hms_mean_results, hms_median_results, ga_mean_results, ga_median_results))
  max_fitness <- max(c(hms_mean_results, hms_median_results, ga_mean_results, ga_median_results))
  plot(evaluations,
    xlim = c(min(evaluations), max(evaluations)),
    ylim = c(min_fitness, max_fitness),
    xlab = "evaluations",
    ylab = "fitness",
    type = "n"
  )
  lines(evaluations,
    hms_mean_results,
    pch = 16,
    type = "p",
    col = "green3"
  )
  lines(evaluations,
        hms_median_results,
        pch = 16,
        type = "p",
        col = "lightgreen"
  )
  lines(evaluations,
    ga_median_results,
    pch = 16,
    type = "p",
    col = "cyan3"
  )
  lines(evaluations,
        ga_mean_results,
        pch = 16,
        type = "p",
        col = "blue3"
  )
  legend("bottomright",
         inset = 0.02,
         legend = c("HMS mean", "HMS median", "GA mean", "GA median"),
         fill = c("green", "lightgreen", "blue", "cyan")
  )
}


# Examples:
# fitness = function(x) { -Ackley(x) }
# lower = rep(-32.768, 10)
# upper = rep(32.768, 10)
# sigma = list(rep(5,10), rep(3,10), rep(2,10), rep(2,10))

fitness <- Eggholder
lower <- c(-512, -512)
upper <- c(512, 512)
sigma <- list(c(50, 50), c(10, 10), c(5, 5))


ga_config <- list(
  list(
    pcrossover = 0.8,
    pmutation = 0.3,
    mutation = rnorm_mutation(lower, upper, sigma[[1]])
  ),
  list(
    pcrossover = 0.8,
    pmutation = 0.1,
    mutation = rnorm_mutation(lower, upper, sigma[[2]])
  ),
  list(
    pcrossover = 0.8,
    pmutation = 0.05,
    mutation = rnorm_mutation(lower, upper, sigma[[3]])
  )
)
