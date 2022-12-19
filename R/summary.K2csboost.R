#' @name summary.K2csboost
#'
#' @aliases summary.K2csboost
#'
#' @title summarizing K2csboost fits
#'
#' @description The function is a method for class \code{K2csboost} objects that summarizes their fit.
#'
#' @usage \method{summary}{K2csboost}(object, \dots)
#'
#' @param object An object of class "\code{K2csboost}". A result of a call to the \code{K2csboost} function.
#' @param ... Further arguments passed to or from other methods.
#'
#' @details \code{summary.K2csboost} prints a summary of the "\code{K2csboost}" object.
#'
#' @export summary.K2csboost
#' @exportS3Method
#'
#' @author Kipedene Coulibaly
#'
#' @references Coulibaly, K.(2022).\emph{"Statistical modeling of construction probabilities and buildable capacities of parcels based on UrbanSimul geohistorical data"}\[Unpublished Master 2 thesis\]. INRAE and the University Franche-Comte of Besancon.
#'
#'
#' @seealso \code{\link{K2csboost}}, \code{\link{plot.K2csboost}}, \code{\link{predict.K2csboost}}
#'
#' @examples
#' library(Kcsboost)
#' data(creditcard)
#' i0 <- which(creditcard$Class == 0)
#' i1 <- which(creditcard$Class == 1)
#'
#' set.seed(2020)
#' i0_train <- sample(i0, size = 0.7 * length(i0))
#' i1_train <- sample(i1, size = 0.7 * length(i1))
#'
#' train <- creditcard[ c(i0_train, i1_train), ]
#' test  <- creditcard[-c(i0_train, i1_train), ]
#'
#' cost_matrix_train <- cost_matrix_test <- matrix(c(25, 100, 75, 2), ncol = 2, byrow = TRUE)
#'
#' K2csb<- K2csboost(formula               = Class ~ . - 1,
#'                   train                 = train,
#'                   test                  = test,
#'                   cost_matrix_train     = cost_matrix_train,
#'                   cost_matrix_test      = cost_matrix_test,
#'                   nrounds               = 300,
#'                   early_stopping_rounds = 20,
#'                   verbose               = 1,
#'                   print_every_n         = 1)
#' summary(K2csb)
#' plot(K2csb, "top")
#' predict(K2csb, newdata=test)


summary.K2csboost <- function (object, ...) {
  # check inputs
  if (missing(object)) {
    stop("argument 'object' is missing, with no default")
  }
  if (class(object) != "K2csboost") {
    stop("argument 'object' must be of class 'K2csboost'")
  }

  # print summary
  xgbmodel <- object$xgbmodel
  params <- xgbmodel$params
  best_performance <- xgbmodel$evaluation_log[xgbmodel$best_iteration, ]

  cat("SETTINGS -------------------------------------------------------------------------------\n")
  if (!is.null(params$nrounds)) {
    cat(paste("  - nrounds =",               params$nrounds,               "\n"))
  }
  if (!is.null(params$early_stopping_rounds)) {
    cat(paste("  - early_stopping_rounds =", params$early_stopping_rounds, "\n"))
  }
  if (!is.null(params$booster)) {
    cat(paste("  - booster =",               params$booster,               "\n"))
  }
  if (!is.null(params$eta)) {
    cat(paste("  - etas =",                  params$eta,                   "\n"))
  }
  if (!is.null(params$gamma)) {
    cat(paste("  - gamma =",                 params$gamma,                 "\n"))
  }
  if (!is.null(params$max_depth)) {
    cat(paste("  - max_depth =",             params$max_depth,             "\n"))
  }
  if (!is.null(params$min_child_weight)) {
    cat(paste("  - min_child_weight =",      params$min_child_weight,      "\n"))
  }
  if (!is.null(params$max_delta_step)) {
    cat(paste("  - max_delta_step =",        params$max_delta_step,        "\n"))
  }
  if (!is.null(params$subsample)) {
    cat(paste("  - subsample =",             params$subsample,             "\n"))
  }
  if (!is.null(params$colsample_bytree)) {
    cat(paste("  - colsample_bytree =",      params$colsample_bytree,      "\n"))
  }
  if (!is.null(params$colsample_bylevel)) {
    cat(paste("  - colsample_bylevel =",     params$colsample_bylevel,     "\n"))
  }
  if (!is.null(params$colsample_bynode)) {
    cat(paste("  - colsample_bynode =",      params$colsample_bynode,      "\n"))
  }
  if (!is.null(params$lambda)) {
    cat(paste("  - lambda =",                params$lambda,                "\n"))
  }
  if (!is.null(params$alpha)) {
    cat(paste("  - alpha =",                 params$alpha,                 "\n"))
  }
  if (!is.null(params$scale_pos_weight)) {
    cat(paste("  - scale_pos_weight =",      params$scale_pos_weight,      "\n"))
  }
  if (!is.null(params$base_score)) {
    cat(paste("  - base_score =",            params$base_score,            "\n"))
  }
  if (!is.null(params$nthread)) {
    cat(paste("  - nthread =",               params$nthread,               "\n"))
  }
  cat(paste("  - cost matrix (example) = \n"))
  print(params$example_cost_matrix)
  cat(paste("  -", xgbmodel$nfeatures, "features:\n      "))
  cat(paste(xgbmodel$feature_names), sep = "\n      ")

  cat("\n")
  cat("RESULTS --------------------------------------------------------------------------------\n")
  cat(paste("  - iterations =",      xgbmodel$niter, "\n"))
  cat(paste("  - time =",            object$time, "seconds\n"))
  cat(paste("  - best iteration =",  xgbmodel$best_iteration, "\n"))
  cat(paste("  - best ntreelimit =", xgbmodel$best_ntreelimit, "\n"))
  cat(paste("  - best average expected cost (train) =",
            round(best_performance$train_AEC, 6), "\n"))
  if (NCOL(xgbmodel$evaluation_log) == 3) {
    cat(paste("  - best average expected cost (test) =",
              round(best_performance$test_AEC, 6), "\n"))
  }
  cat("\n")
}
