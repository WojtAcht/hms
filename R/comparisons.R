hms_mutation_comparison <- function(dimensions, evaluations) {
  seeds <- 1:30

  fitness <- function(x) { -1 * Griewank(x) }
  lower <- rep(-600, dimensions)
  upper <- rep(600, dimensions)
  sigma <- list(rep(200, dimensions), rep(100, dimensions), rep(50, dimensions))

  run_hms <- function(with_ga_mutation, seed) {
    print(match.call())
    ga_config <- list(
      list(
        pmutation = 0.4
      ),
      list(
        pmutation = 0.2
      ),
      list(
        pmutation = 0.2
      )
    )
    norm_ga_config <- list(
      list(
        pmutation = 0.4,
        mutation = rtnorm_mutation(lower, upper, sigma[[1]])
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

    set.seed(seed)

    result <- hms(
      fitness = fitness,
      tree_height = 3,
      lower = lower,
      upper = upper,
      run_metaepoch = ga_metaepoch(if(with_ga_mutation) ga_config else norm_ga_config),
      population_size_per_tree_level = c(50, 30, 15),
      sigma = sigma,
      global_stopping_condition =   global_stopping_condition_max_fitness_evaluations(evaluations),
      sprouting_condition = max_metric_sprouting_condition(euclidean_distance, c(40, 20, 10)),
      local_stopping_condition = local_stopping_condition_metaepochs_without_improvement(15),
      monitor_level = "none"
    )
    printTree(result)
    print(result@best_fitness)
    result@best_fitness
  }

  norm_mutation <- mapply(function(seed) { run_hms(FALSE, seed) }, seeds)
  ga_mutation <- mapply(function(seed) { run_hms(TRUE, seed) }, seeds)


  cat("norm:", mean(norm_mutation), "\n")
  cat("ga:", mean(ga_mutation), "\n")

  list(norm = norm_mutation, ga = ga_mutation)
}

hms_vs_ga <- function(evaluations) {
  seeds <- 31:60

  fitness <- function(x) { -1 * Eggholder(x) }
  lower <- rep(-512, 2)
  upper <- rep(512, 2)
  sigma <- list(rep(200, 2), rep(100, 2), rep(50, 2))

  run_hms <- function(seed) {
    ga_config <- list(
      list(
        pmutation = 0.4,
        mutation = rtnorm_mutation(lower, upper, sigma[[1]])
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

    set.seed(seed)

    result <- hms(
      fitness = fitness,
      tree_height = 3,
      lower = lower,
      upper = upper,
      run_metaepoch = ga_metaepoch(ga_config),
      population_size_per_tree_level = c(50, 30, 15),
      sigma = sigma,
      global_stopping_condition =   global_stopping_condition_max_fitness_evaluations(evaluations),
      sprouting_condition = max_metric_sprouting_condition(euclidean_distance, c(40, 20, 10)),
      local_stopping_condition = local_stopping_condition_metaepochs_without_improvement(15),
      monitor_level = "none"
    )
    printTree(result)
    print(result@best_fitness)
    result@best_fitness
  }

  evals <- 0
  ga_f <- function(x) {
    evals <<- evals + 1

    if (evals > evaluations) {
      -Inf
    } else {
      fitness(x)
    }
  }

  run_ga <- function(seed) {
    set.seed(seed)
    evals <<- 0

    result <- GA::ga(type = "real-valued", fitness = ga_f, lower = lower, upper = upper, monitor = FALSE, maxiter = (evaluations / 150)*5, popSize = 150)
    print(result@fitnessValue)

    result@fitnessValue
  }

  hms <- mapply(function(seed) { run_hms(seed) }, seeds)
  ga <- mapply(function(seed) { run_ga(seed) }, seeds)


  cat("hms:", mean(hms), "\n")
  cat("ga:", mean(ga), "\n")

  list(hms = hms, ga = ga)
}

get_hms_vs_ga_res <- function() {
  a_2k = hms_vs_ga(2000)
  a_4k = hms_vs_ga(4000)
  a_6k = hms_vs_ga(6000)
  a_8k = hms_vs_ga(8000)
  a_10k = hms_vs_ga(10000)
  a_12k = hms_vs_ga(12000)
  a_14k = hms_vs_ga(14000)
  a_16k = hms_vs_ga(16000)

  list(ga_2k = a_2k$ga, hms_2k = a_2k$hms, ga_4k = a_4k$ga, hms_4k = a_4k$hms, ga_6k = a_6k$ga, hms_6k = a_6k$hms, ga_8k = a_8k$ga, hms_8k = a_8k$hms, ga_10k = a_10k$ga, hms_10k = a_10k$hms, ga_12k = a_12k$ga, hms_12k = a_12k$hms, ga_14k = a_14k$ga, hms_14k = a_14k$hms, ga_16k = a_16k$ga, hms_16k = a_16k$hms)
}

plot_hms_vs_ga <- function(res) {
  cols = rainbow(2, s = 0.5)
  boxplot(hms_vs_ga_res2, at=c(1,2,5,6,9,10,13,14,17,18,21,22,25,26,29,30), col=rep(cols, times=8))
}
