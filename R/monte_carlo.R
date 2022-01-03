runif_sample <- function(lower, upper, n) {
  random_coordinate <- function(i) {
    stats::runif(length(lower),
                 # all domains are symmetric :)
                 min = lower[[1]],
                 max = upper[[1]]
    )
  }
  lapply(1:n, random_coordinate)
}

pure_random_search <- function(lower, upper, fitness, n) {
  results <- lapply(runif_sample(lower, upper, n) ,fitness)
  max(unlist(results))
}

multi_start <- function(lower, upper, fitness, n, max_evaluations_count) {
  total_evaluations_count <- 0
  gradient <- function(x) {
    result <- stats::optim(
      fn = fitness,
      par = x,
      method = "L-BFGS-B",
      lower = lower,
      upper = upper,
      control = list(fnscale = -1, maxit = 100)
    )
    total_evaluations_count <<- total_evaluations_count + sum(result$counts)
    result$value
  }
  max_value <- -10000
  for(x in runif_sample(lower, upper, n)){
    result <- gradient(x)
    if(result > max_value){
      max_value <- result
    }
    if(total_evaluations_count > max_evaluations_count) {
      break
    }
  }
  print(total_evaluations_count)
  max_value
}


hms_vs_monte_carlo <- function(evaluations) {
  seeds <- 1241:1271

  fitness <- function(x) { -1 * Ackley(x) }
  lower <- rep(-32.768, 15)
  upper <- rep(32.768, 15)
  sigma <- list(rep(15, 15), rep(10, 15), rep(5, 15))

  run_hms <- function(seed) {
    ga_config <- list(
      list(
        pmutation = 0.4#,
        #mutation = rtnorm_mutation(lower, upper, sigma[[1]])
      ),
      list(
        pmutation = 0.2#,
        #mutation = rtnorm_mutation(lower, upper, sigma[[2]])
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
      sprouting_condition = max_metric_sprouting_condition(euclidean_distance, c(15, 10, 1)),
      local_stopping_condition = local_stopping_condition_metaepochs_without_improvement(8),
      monitor_level = "none"
    )
    printTree(result)
    print(result@best_fitness)
    result@best_fitness
  }

  run_monte_carlo <- function(seed){
    set.seed(seed)
    multi_start(lower, upper, fitness, round(evaluations / 5), evaluations)
  }

  evals <- 0

  hms <- mapply(function(seed) { run_hms(seed) }, seeds)
  monte_carlo <- mapply(function(seed) { run_monte_carlo(seed) }, seeds)


  cat("hms:", mean(hms), "\n")
  cat("monte carlo:", mean(monte_carlo), "\n")

  list(hms = hms, monte_carlo=monte_carlo)
}

get_hms_vs_monte_carlo_res <- function() {
  a_2k = hms_vs_monte_carlo(2000)
  a_4k = hms_vs_monte_carlo(4000)
  a_6k = hms_vs_monte_carlo(6000)
  a_8k = hms_vs_monte_carlo(8000)
  a_10k = hms_vs_monte_carlo(10000)
  a_12k = hms_vs_monte_carlo(12000)
  a_14k = hms_vs_monte_carlo(14000)
  a_16k = hms_vs_monte_carlo(16000)

  list(monte_carlo_2k = a_2k$monte_carlo,
       hms_2k = a_2k$hms,
       monte_carlo_4k = a_4k$monte_carlo,
       hms_4k = a_4k$hms,
       monte_carlo_6k = a_6k$monte_carlo,
       hms_6k = a_6k$hms,
       monte_carlo_8k = a_8k$monte_carlo,
       hms_8k = a_8k$hms,
       monte_carlo_10k = a_10k$monte_carlo,
       hms_10k = a_10k$hms,
       monte_carlo_12k = a_12k$monte_carlo,
       hms_12k = a_12k$hms,
       monte_carlo_14k = a_14k$monte_carlo,
       hms_14k = a_14k$hms,
       monte_carlo_16k = a_16k$monte_carlo,
       hms_16k = a_16k$hms)
}

plot_hms_vs_monte_carlo <- function(res) {
  cols = tail(rainbow(3, s = 0.1), n=2)
  boxplot(res, at=c(1,2,5,6,9,10,13,14,17,18,21,22,25,26,29,30),
          col=rep(cols, times=8),
          ylab="f(x)",
          xlab="Fitness evaluations",
          names = c("2k", "2k", "4k", "4k", "6k", "6k", "8k", "8k", "10k", "10k", "12k", "12k", "14k", "14k", "16k", "16k"))
  points(x = c(1,2,5,6,9,10,13,14,17,18,21,22,25,26,29,30),
         y = mapply(mean, res),
         col = rep(tail(rainbow(3), n=2), times=8),
         pch = 16)
  legend("topleft", fill = tail(rainbow(3), n=2), legend = c("MultiStart", "HMS"), horiz = T)
}
