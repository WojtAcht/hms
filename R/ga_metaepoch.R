ga_optimized <- function(type = c("binary", "real-valued", "permutation"),
                         fitness, ...,
                         lower, upper, nBits,
                         population = gaControl(type)$population,
                         selection = gaControl(type)$selection,
                         crossover = gaControl(type)$crossover,
                         mutation = gaControl(type)$mutation,
                         popSize = 50,
                         pcrossover = 0.8,
                         pmutation = 0.1,
                         elitism = base::max(1, round(popSize * 0.05)),
                         updatePop = FALSE,
                         postFitness = NULL,
                         maxiter = 100,
                         run = maxiter,
                         maxFitness = Inf,
                         names = NULL,
                         suggestions = NULL,
                         fitnessSuggestions = NULL,
                         optim = FALSE,
                         optimArgs = list(
                           method = "L-BFGS-B",
                           poptim = 0.05,
                           pressel = 0.5,
                           control = list(fnscale = -1, maxit = 100)
                         ),
                         keepBest = FALSE,
                         parallel = FALSE,
                         monitor = if (interactive()) gaMonitor else FALSE,
                         seed = NULL) {
  call <- match.call()

  type <- match.arg(type, choices = eval(formals(ga)$type))

  if (!is.function(population)) population <- get(population)
  if (!is.function(selection)) selection <- get(selection)
  if (!is.function(crossover)) crossover <- get(crossover)
  if (!is.function(mutation)) mutation <- get(mutation)

  if (missing(fitness)) {
    stop("A fitness function must be provided")
  }
  if (!is.function(fitness)) {
    stop("A fitness function must be provided")
  }
  if (popSize < 10) {
    warning("The population size is less than 10.")
  }
  if (maxiter < 1) {
    stop("The maximum number of iterations must be at least 1.")
  }
  if (elitism > popSize) {
    stop("The elitism cannot be larger that population size.")
  }
  elitism <- as.integer(elitism)
  if (pcrossover < 0 | pcrossover > 1) {
    stop("Probability of crossover must be between 0 and 1.")
  }
  if (is.numeric(pmutation)) {
    if (pmutation < 0 | pmutation > 1) {
      stop("If numeric probability of mutation must be between 0 and 1.")
    } else if (!is.function(population)) {
      stop("pmutation must be a numeric value in (0,1) or a function.")
    }
  }

  # check for min and max arguments instead of lower and upper
  callArgs <- list(...)
  if (any("min" %in% names(callArgs))) {
    lower <- callArgs$min
    callArgs$min <- NULL
    warning("'min' arg is deprecated. Use 'lower' instead.")
  }
  if (any("max" %in% names(callArgs))) {
    upper <- callArgs$max
    callArgs$max <- NULL
    warning("'max' arg is deprecated. Use 'upper' instead.")
  }

  if (missing(lower) & missing(upper) & missing(nBits)) {
    stop("A lower and upper range of values (for 'real-valued' or 'permutation' GA) or nBits (for 'binary' GA) must be provided!")
  }

  # check GA search type
  switch(type,
    "binary" = {
      nBits <- as.vector(nBits)[1]
      lower <- upper <- NA
      nvars <- nBits
      if (is.null(names)) {
        names <- paste0("x", 1:nvars)
      }
    },
    "real-valued" = {
      lnames <- names(lower)
      unames <- names(upper)
      lower <- as.vector(lower)
      upper <- as.vector(upper)
      nBits <- NA
      if (length(lower) != length(upper)) {
        stop("lower and upper must be vector of the same length!")
      }
      nvars <- length(upper)
      if (is.null(names) & !is.null(lnames)) {
        names <- lnames
      }
      if (is.null(names) & !is.null(unames)) {
        names <- unames
      }
      if (is.null(names)) {
        names <- paste0("x", 1:nvars)
      }
    },
    "permutation" = {
      lower <- as.vector(lower)[1]
      upper <- as.vector(upper)[1]
      nBits <- NA
      nvars <- length(seq.int(lower, upper))
      if (is.null(names)) {
        names <- paste0("x", 1:nvars)
      }
    }
  )

  # check suggestions
  if (is.null(suggestions)) {
    suggestions <- matrix(nrow = 0, ncol = nvars)
  } else {
    if (is.vector(suggestions)) {
      if (nvars > 1) {
        suggestions <- matrix(suggestions, nrow = 1)
      } else {
        suggestions <- matrix(suggestions, ncol = 1)
      }
    } else {
      suggestions <- as.matrix(suggestions)
    }
    if (nvars != ncol(suggestions)) {
      stop("Provided suggestions (ncol) matrix do not match number of variables of the problem!")
    }
  }

  # check monitor arg
  if (is.logical(monitor)) {
    if (monitor) monitor <- gaMonitor
  }
  if (is.null(monitor)) monitor <- FALSE

  # if optim merge provided and default args for optim()
  if (optim) { # merge default and provided parameters
    optimArgs.default <- eval(formals(ga)$optimArgs)
    optimArgs.default$control[names(optimArgs$control)] <- optimArgs$control
    optimArgs$control <- NULL
    optimArgs.default[names(optimArgs)] <- optimArgs
    optimArgs <- optimArgs.default
    rm(optimArgs.default)
    if (any(optimArgs$method == c("L-BFGS-B", "Brent"))) {
      optimArgs$lower <- lower
      optimArgs$upper <- upper
    } else {
      optimArgs$lower <- -Inf
      optimArgs$upper <- Inf
    }
    optimArgs$poptim <- min(max(0, optimArgs$poptim), 1)
    optimArgs$pressel <- min(max(0, optimArgs$pressel), 1)
    optimArgs$control$maxit <- as.integer(optimArgs$control$maxit)
    # ensure that optim maximise the fitness
    if (is.null(optimArgs$control$fnscale)) {
      optimArgs$control$fnscale <- -1
    }
    if (optimArgs$control$fnscale > 0) {
      optimArgs$control$fnscale <- -1 * optimArgs$control$fnscale
    }
  }

  # Start parallel computing (if needed)
  if (is.logical(parallel)) {
    if (parallel) {
      parallel <- startParallel(parallel)
      stopCluster <- TRUE
    } else {
      parallel <- stopCluster <- FALSE
    }
  } else {
    stopCluster <- if (inherits(parallel, "cluster")) FALSE else TRUE
    parallel <- startParallel(parallel)
  }
  on.exit(if (parallel & stopCluster) {
    stopParallel(attr(parallel, "cluster"))
  })
  # define operator to use depending on parallel being TRUE or FALSE
  `%DO%` <- if (parallel && requireNamespace("doRNG", quietly = TRUE)) {
    doRNG::`%dorng%`
  } else if (parallel) `%dopar%` else `%do%`
  # set seed for reproducibility
  if (!is.null(seed)) set.seed(seed)
  i. <- NULL # dummy to trick R CMD check

  fitnessSummary <- matrix(as.double(NA), nrow = maxiter, ncol = 6)
  colnames(fitnessSummary) <- names(gaSummary(rnorm(10)))
  bestSol <- if (keepBest) {
    vector(mode = "list", length = maxiter)
  } else {
    list()
  }
  Fitness <- if (is.null(fitnessSuggestions)) rep(NA, popSize) else fitnessSuggestions

  object <- new("ga",
    call = call,
    type = type,
    lower = lower,
    upper = upper,
    nBits = nBits,
    names = if (is.null(names)) character() else names,
    popSize = popSize,
    iter = 0,
    run = 1,
    maxiter = maxiter,
    suggestions = suggestions,
    population = matrix(),
    elitism = elitism,
    pcrossover = pcrossover,
    pmutation = if (is.numeric(pmutation)) pmutation else NA,
    optim = optim,
    fitness = Fitness,
    summary = fitnessSummary,
    bestSol = bestSol
  )

  if (maxiter == 0) {
    return(object)
  }

  # generate beginning population
  Pop <- matrix(as.double(NA), nrow = popSize, ncol = nvars)
  ng <- min(nrow(suggestions), popSize)
  if (ng > 0) # use suggestion if provided
    {
      Pop[1:ng, ] <- suggestions
    }
  # fill the rest with a random population
  if (popSize > ng) {
    Pop[(ng + 1):popSize, ] <- population(object)[1:(popSize - ng), ]
  }
  object@population <- Pop

  # start iterations
  for (iter in seq_len(maxiter))
  {
    object@iter <- iter

    # evalute fitness function (when needed)
    if (!parallel) {
      for (i in seq_len(popSize)) {
        if (is.na(Fitness[i])) {
          fit <- do.call(fitness, c(list(Pop[i, ]), callArgs))
          if (updatePop) {
            Pop[i, ] <- attributes(fit)[[1]]
          }
          Fitness[i] <- fit
        }
      }
    } else {
      Fitness <- foreach(i. = seq_len(popSize), .combine = "c") %DO% {
        if (is.na(Fitness[i.])) {
          do.call(fitness, c(list(Pop[i., ]), callArgs))
        } else {
          Fitness[i.]
        }
      }
    }

    # update object
    object@population <- Pop
    object@fitness <- Fitness

    # Local search optimisation
    if (optim & (type == "real-valued")) {
      if (optimArgs$poptim > runif(1)) { # perform local search from random selected solution
        # with prob proportional to fitness
        i <- sample(1:popSize,
          size = 1,
          prob = optimProbsel(Fitness, q = optimArgs$pressel)
        )
        # run local search
        opt <- try(suppressWarnings(
          do.call(
            stats::optim,
            c(
              list(
                fn = fitness,
                par = Pop[i, ],
                method = optimArgs$method,
                lower = optimArgs$lower,
                upper = optimArgs$upper,
                control = optimArgs$control
              ),
              callArgs
            )
          )
        ), silent = TRUE)
        if (is.function(monitor)) {
          if (!inherits(opt, "try-error")) {
            cat(
              "\b | Local search =",
              format(opt$value, digits = getOption("digits"))
            )
          } else {
            cat("\b |", opt[1])
          }
          cat("\n")
        }
        if (!inherits(opt, "try-error")) {
          Pop[i, ] <- opt$par
          Fitness[i] <- opt$value
        }
        # update object
        object@population <- Pop
        object@fitness <- Fitness
        # update iterations summary
        fitnessSummary[iter, ] <- gaSummary(object@fitness)
        object@summary <- fitnessSummary
      }
    }

    if (keepBest) {
      object@bestSol[[iter]] <- unique(Pop[Fitness == max(Fitness, na.rm = TRUE), , drop = FALSE])
    }

    # apply a user's defined function to update the GA object
    if (is.function(postFitness)) {
      object <- do.call(postFitness, c(object, callArgs))
      Fitness <- object@fitness
      Pop <- object@population
    }

    # update iterations summary
    fitnessSummary[iter, ] <- gaSummary(object@fitness)
    object@summary <- fitnessSummary

    if (is.function(monitor)) {
      monitor(object)
    }

    # check stopping criteria
    if (iter > 1) {
      object@run <- garun(fitnessSummary[seq(iter), 1])
    }
    if (object@run >= run) break
    if (max(Fitness, na.rm = TRUE) >= maxFitness) break
    if (object@iter == maxiter) break

    ord <- order(Fitness, decreasing = TRUE)
    PopSorted <- Pop[ord, , drop = FALSE]
    FitnessSorted <- Fitness[ord]

    # selection
    if (is.function(selection)) {
      sel <- selection(object)
      # sel <- do.call(selection, c(object, callArgs))
      Pop <- sel$population
      Fitness <- sel$fitness
    } else {
      sel <- sample(1:popSize, size = popSize, replace = TRUE)
      Pop <- object@population[sel, ]
      Fitness <- object@fitness[sel]
    }
    object@population <- Pop
    object@fitness <- Fitness

    # crossover
    if (is.function(crossover) & pcrossover > 0) {
      nmating <- floor(popSize / 2)
      mating <- matrix(sample(1:(2 * nmating), size = (2 * nmating)), ncol = 2)
      for (i in seq_len(nmating))
      {
        if (pcrossover > runif(1)) {
          parents <- mating[i, ]
          Crossover <- crossover(object, parents)
          Pop[parents, ] <- Crossover$children
          Fitness[parents] <- Crossover$fitness
        }
      }
      object@population <- Pop
      object@fitness <- Fitness
    }

    # mutation
    pm <- if (is.function(pmutation)) pmutation(object) else pmutation
    if (is.function(mutation) & pm > 0) {
      for (i in seq_len(popSize))
      {
        if (pm > runif(1)) {
          Mutation <- mutation(object, i)
          Pop[i, ] <- Mutation
          Fitness[i] <- NA
        }
      }
      object@population <- Pop
      object@fitness <- Fitness
    }

    # elitism
    if (elitism > 0) # (elitism > 0 & iter > 1)
      {
        ord <- order(object@fitness, na.last = TRUE)
        u <- which(!duplicated(PopSorted, margin = 1))
        Pop[ord[1:elitism], ] <- PopSorted[u[1:elitism], ]
        Fitness[ord[1:elitism]] <- FitnessSorted[u[1:elitism]]
        object@population <- Pop
        object@fitness <- Fitness
      }
  }

  # if optim is required perform a local search from the best
  # solution at the end of GA iterations
  if (optim & (type == "real-valued")) {
    optimArgs$control$maxit <- rev(optimArgs$control$maxit)[1]
    i <- which.max(object@fitness)
    # if not provided suggest approx parscale
    # if(is.null(optimArgs$control$parscale))
    #   optimArgs$control$parscale <- 10^round(log10(abs(object@population[i,])+1))
    # run local search
    opt <- try(suppressWarnings(
      do.call(
        stats::optim,
        c(
          list(
            fn = fitness,
            par = object@population[i, ],
            method = optimArgs$method,
            lower = optimArgs$lower,
            upper = optimArgs$upper,
            control = optimArgs$control
          ),
          callArgs
        )
      )
    ), silent = TRUE)
    if (is.function(monitor)) {
      if (!inherits(opt, "try-error")) {
        cat(
          "\b | Final local search =",
          format(opt$value, digits = getOption("digits"))
        )
      } else {
        cat("\b |", opt[1])
      }
    }
    if (!inherits(opt, "try-error")) {
      object@population[i, ] <- opt$par
      object@fitness[i] <- opt$value
    }
  }

  # if(is.function(monitor))
  #   { cat("\n"); flush.console() }

  # in case of premature convergence remove NAs from summary
  # fitness evalutations
  object@summary <- na.exclude(object@summary)
  attr(object@summary, "na.action") <- NULL

  # get solution(s)
  object@fitnessValue <- max(object@fitness, na.rm = TRUE)
  valueAt <- which(object@fitness == object@fitnessValue)
  solution <- object@population[valueAt, , drop = FALSE]
  if (nrow(solution) > 1) { # find unique solutions to precision given by default tolerance
    eps <- gaControl("eps")
    solution <- unique(round(solution / eps) * eps, margin = 1)
  }
  # colnames(solution) <- parNames(object)
  object@solution <- solution
  if (keepBest) {
    object@bestSol <- object@bestSol[!sapply(object@bestSol, is.null)]
  }

  # return an object of class 'ga'
  return(object)
}

#' Function that runs one GA metaepoch. Wrapper function for GA::ga.
#'
#' @param config_ga - list of GA::ga params
#'
#' @return list with named fields: solution, population, value. A solution is a
#' value of the decision variable giving the best fitness. A population is a
#' matrix representing final population. Value is the value of a fitness
#' function for the solution.
#'
#' @export
#'
#' @examples
#' tree_height <- 3
#' empty_config_ga <- lapply(1:tree_height, function(x) {
#'   list()
#' })
#' ga_metaepoch(empty_config_ga)
ga_metaepoch <- function(config_ga) {
  function(fitness, suggestions, fitness_suggestions, lower, upper, tree_level, minimize) {
    ga_fitness <- ifelse(minimize, function(x) {
      -1 * fitness(x)
    }, fitness)
    config <- config_ga[[tree_level]]
    legal_passed_param_names <- Filter(function(name) {
      name %in% methods::formalArgs(ga_optimized)
    }, names(config))
    params <- list("maxiter" = 5, "popSize" = nrow(suggestions))
    for (param_name in legal_passed_param_names) {
      params[param_name] <- config[param_name]
    }
    # GA is used for maximization of a fitness function
    params$fitness <- ga_fitness
    params$lower <- lower
    params$upper <- upper
    params$suggestions <- suggestions
    params$type <- "real-valued"
    params$monitor <- FALSE
    params$fitnessSuggestions <- fitness_suggestions

    GA <- do.call(ga_optimized, params)
    value <- ifelse(minimize, GA@fitnessValue * -1, GA@fitnessValue)
    list(
      "solution" = c(GA@solution[1, ]),
      "population" = GA@population,
      "value" = value,
      "fitnessValues" = GA@fitness
    )
  }
}

default_ga_metaepoch <- function(tree_height) {
  empty_config_ga <- lapply(1:tree_height, function(x) {
    list()
  })
  ga_metaepoch(empty_config_ga)
}
