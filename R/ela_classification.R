#' Train Random Forest model on BBOB dataset (data/ela_features.rda).
#'
#' @return Returns a Random Forest model trained on ela_features dataset which is
#' stored internally in sysdata.rda.
#' The ela_features dataset, created using the flacco package, includes ELA
#' features for all BBOB functions (all available fids) across different
#' dimensions (2, 5, 10, 20) and instances (1:20).
#' Key features captured are ic.eps.ratio, nbc.nb_fitness.cor,
#' ela_meta.quad_simple.adj_r2, and ela_meta.lin_simple.adj_r2, along with
#' function type.
train_random_forest_model <- function() {
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
calculate_ela_features <- function(fitness, lower, upper) {
  N <- length(lower)
  X <- flacco::createInitialSample(
    n.obs = 100 * N,
    dim = N,
    list(
      "init_sample.lower" = lower,
      "init_sample.upper" = upper
    )
  )
  y <- apply(X, 1, fitness)
  feat.object <- flacco::createFeatureObject(X = X, y = y)
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
#' @param fitness fitness function, that returns a numerical value, to be classified. The domain should be at least two dimensional.
#' @param lower numeric - lower bound of the domain, a vector of length equal
#' to the decision variables.
#' @param upper numeric - upper bound of the domain, a vector of length equal
#' to the decision variables.
#'
#' @return Returns one of c("low-conditioning", "multimodal-adequate", "multimodal-weak", "separable", "unimodal").
#' @export
#'
#' @examples
#' f <- function(x) x[[1]] + x[[2]]
#' result <- classify_optimization_problem(fitness = f, lower = c(-5, -5), upper = c(5, 5))
classify_optimization_problem <- function(fitness, lower, upper) {
  model <- train_random_forest_model()
  features <- calculate_ela_features(fitness, lower, upper)
  return(stats::predict(model, features))
}
