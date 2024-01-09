#' Train Random Forest model on BBOB dataset (data/ela_features.rda).
#'
#' @return Returns a Random Forest model trained on data/ela_features.rda dataset.
train_random_forest_model <- function() {
  load("data/ela_features.rda")
  model <-
    randomForest::randomForest(type ~ .,
      data = ela_features,
      ntree = 100,
      importance = TRUE
    )
  return(model)
}

#' Calculate selected ELA features for given fitness.
#'
#' @param fitness fitness function, that returns a numerical value.
#' @param lower numeric - lower bound of the domain, a vector of length equal
#' to the decision variables.
#' @param upper numeric - upper bound of the domain, a vector of length equal
#' to the decision variables.
#'
#' @return Returns a named list with values of selected ELA features.
#'
#' @examples
#' f <- function(x) x
#' result <- calculate_ela_features(fitness = f, lower = -5, upper = 5)
calculate_ela_features <- function(fitness, lower, upper) {
  N <- length(lower)
  X <- createInitialSample(
    n.obs = 100 * N,
    dim = N,
    list(
      "init_sample.lower" = lower,
      "init_sample.upper" = upper
    )
  )
  y <- apply(X, 1, fitness)
  feat.object <- createFeatureObject(X = X, y = y)
  features <- list()
  for (set in c("ela_meta", "nbc", "ic")) {
    feature_values <-
      flacco::calculateFeatureSet(feat.object, set = set)
    features <- c(features, feature_values)
  }
  selected_features <-
    c(
      "ic.eps.ratio",
      "nbc.nb_fitness.cor",
      "ela_meta.quad_simple.adj_r2",
      "ela_meta.lin_simple.adj_r2"
    )
  features <- features[selected_features]
  return(features)
}

#' Classify optimization problem using selected ELA features and Random Forest model trained on BBOB dataset.
#'
#' @param fitness fitness function, that returns a numerical value, to be optimized by the strategy.
#' @param lower numeric - lower bound of the domain, a vector of length equal
#' to the decision variables.
#' @param upper numeric - upper bound of the domain, a vector of length equal
#' to the decision variables.
#'
#' @return Returns one of c().
#' @export
#'
#' @examples
#' f <- function(x) x
#' result <- classify_optimization_problem(fitness = f, lower = -5, upper = 5)
classify_optimization_problem <- function(fitness, lower, upper) {
  model <- train_random_fores_model()
  features <- calculate_ela_features(fitness, lower, upper)
  return(predict(model, features))
}
