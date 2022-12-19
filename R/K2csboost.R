#' @name K2csboost
#'
#' @aliases K2csboost
#'
#' @title Instance-dependent cost-sensitive extreme gradient boosting
#'
#' @description Instance-dependent cost-sensitive extreme gradient boosting.
#'
#' @usage K2csboost(formula, train, test = NULL, cost_matrix_train, cost_matrix_test = NULL, nrounds,
#'                  params = list(), verbose = 1, print_every_n = 1L, early_stopping_rounds = NULL,
#'                  save_period = NULL, save_name = "xgboost.model", xgb_model = NULL, ...)
#'
#' @param formula An object of class "\code{\link{formula}}": a symbolic description of the model to be fitted. An intercept can not be included.
#' @param train A training set as a data frame containing the variables in the model.
#' @param test A test set (if provided) as a data frame containing the variables in the model (default is \code{NULL}).
#' @param cost_matrix_train A matrix of dimension 2 x 2. The first (second) row contains the cost of TP & FN (FP & TN) of the instance.
#' @param cost_matrix_test A matrix of dimension 2 x 2 (if provided). The first (second) row contains the cost of TP & FN (FP & TN) of the instance. (Default is \code{NULL}).
#' @param nrounds Max number of boosting iterations.
#' @param params The list of parameters. The complete list of parameters is available at \href{https://xgboost.readthedocs.io/en/latest/parameter.html}{https://xgboost.readthedocs.io/en/latest/parameter.html} A short summary is available in the documentation \code{help(xgb.train)}.
#' @param verbose If 0, xgboost will stay silent. If 1, it will print information about performance. If 2, some additional information will be printed out.
#' @param print_every_n Print each n-th iteration evaluation messages when \code{verbose > 0}. Default is 1 which means all messages are printed.
#' @param early_stopping_rounds If NULL, the early stopping function is not triggered. If set to an integer k, training with a validation set will stop if the performance doesn't improve for k rounds.
#' @param save_period When it is non-NULL, model is saved to disk after every save_period rounds, 0 means save at the end.
#' @param save_name The name or path for periodically saved model file.
#' @param xgb_model A previously built model to continue the training from. Could be either an object of class \code{xgb.Booster}, or its raw data, or the name of a file with a previously saved model.
#' @param ... Other parameters to pass to \code{params}.
#'
#' @details This method introduces instance-dependent costs into extreme gradient boosting by changing the objective function of the model to one that is cost-sensitive.
#'
#' @return \code{K2csboost} returns an object of class "\code{K2csboost}" which is a list containing the following components:
#' \item{call}{The matched call.}
#' \item{time}{The number of seconds passed to execute the K2csboost algorithm.}
#' \item{xgbmodel}{An object of class \code{xgb.Booster} with the following elements:}
#' \itemize{
#'   \item \code{handle} \cr a handle (pointer) to the xgboost model in memory.
#'   \item \code{raw} \cr a cached memory dump of the xgboost model saved as R's \code{raw} type.
#'   \item \code{niter} \cr number of boosting iterations.
#'   \item \code{evaluation_log} \cr evaluation history stored as a \code{data.table} with the
#'               first column corresponding to iteration number and the rest corresponding to evaluation
#'               metrics' values.
#'   \item \code{call} \cr a function call.
#'   \item \code{params} \cr parameters that were passed to the xgboost library.
#'   \item \code{callbacks} \cr callback functions that were either automatically assigned or
#'               explicitly passed.
#'   \item \code{best_iteration} \cr iteration number with the best evaluation metric value
#'              (only available with early stopping).
#'   \item \code{best_ntreelimit} the \code{ntreelimit} value corresponding to the best iteration,
#'               which could further be used in \code{predict} method (only available with early stopping).
#'   \item \code{best_score} \cr the best evaluation metric value during early stopping.(only available with early stopping).
#'   \item \code{feature_names} \cr names of the training dataset features (only when column names were defined in training data).
#'   \item \code{nfeatures} \cr number of features in training data.
#' }
#'
#' @export
#' @importFrom hmeasure relabel
#' @importFrom xgboost getinfo xgb.DMatrix xgb.train
#' @importFrom stats model.matrix model.response
#'
#' @author Kipedene Coulibaly
#'
#' @references Coulibaly, K.(2022).\emph{"Statistical modeling of construction probabilities and buildable capacities of parcels based on UrbanSimul geohistorical data"}\[Unpublished Master 2 thesis\]. INRAE and the University Franche-Comte of Besancon.
#'
#'
#' @seealso \code{\link{summary.K2csboost}}, \code{\link{plot.K2csboost}}, \code{\link{predict.K2csboost}}
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

K2csboost <- function(formula, train, test = NULL, cost_matrix_train, cost_matrix_test = NULL,
                      nrounds, params = list(), verbose = 1, print_every_n = 1L,
                      early_stopping_rounds = NULL, save_period = NULL, save_name = "xgboost.model",
                      xgb_model = NULL, ...) {
  # start timer
  t_start <- proc.time()

  # original call
  call <- match.call()

  # check inputs
  check <- checkInputs_K2csboost(formula, train, test, cost_matrix_train, cost_matrix_test)

  # convert data to xgb.DMatrix & build watchlist
  labels_train <- hmeasure::relabel(model.response(check$mf_train))
  Y_Ybar_train <- cbind(labels_train, 1-labels_train)
  dtrain <- xgboost::xgb.DMatrix(data = model.matrix(formula, train), label = labels_train)
  watchlist <- list(train = dtrain)

  dtest <- NULL
  if (!is.null(test)) {
    labels_test <- hmeasure::relabel(model.response(check$mf_test))
    Y_Ybar_test <- cbind(labels_test, 1-labels_test)
    dtest <- xgboost::xgb.DMatrix(data = model.matrix(formula, test), label = labels_test)
    watchlist <- c(watchlist, list(test = dtest))
  }

  # rearrange cost matrix & define auxilary vectors
  cost_matrix_train_obj <- Y_Ybar_train%*%cost_matrix_train
  diff_costs_train <- cost_matrix_train_obj[, 1] - cost_matrix_train_obj[, 2]

  if (!is.null(test)) {
    cost_matrix_test_obj <- Y_Ybar_test%*%cost_matrix_test
    diff_costs_test <- cost_matrix_test_obj[, 1] - cost_matrix_test_obj[, 2]
  }

  # example cost matrix (used by summary.K2csboost)
  example_cost_matrix <- matrix(c(cost_matrix_train_obj[which(labels_train == 0)[1], c(2, 1)],
                                  cost_matrix_train_obj[which(labels_train == 1)[1], c(2, 1)]),
                                nrow = 2, ncol = 2)
  colnames(example_cost_matrix) <- rownames(example_cost_matrix) <- c("0", "1")
  names(dimnames(example_cost_matrix)) <- c("      Prediction", "Reference")

  # define objective function
  AecGradHess_M <- function (scores, dtrain) {
    scores <- 1 / (1 + exp(-scores))
    grad <- scores * (1 - scores) * diff_costs_train
    hess <- abs((1 - 2 * scores) * grad)
    return(list(grad = grad, hess = hess))
  }

  # define evaluation function
  AEC_M <- function (scores, DMatrix) {
    scores <- 1 / (1 + exp(-scores))
    if (length(scores) == NROW(dtrain)) {
      cost_matrix <- as.matrix(c(cost_matrix_train[1,1], cost_matrix_train[1,2],
                                 cost_matrix_train[2,1], cost_matrix_train[2,2]))
      label <- labels_train
    } else if (length(scores) == NROW(dtest)) {
      cost_matrix <- as.matrix(c(cost_matrix_test[1,1], cost_matrix_test[1,2],
                                 cost_matrix_test[2,1], cost_matrix_test[2,2]))
      label <- labels_test
    }
    YS <- cbind(label*scores, label*(1-scores),
                (1-label)*scores, (1-label)*(1-scores))
    average_expected_cost <- mean(YS%*%cost_matrix)
    return(list(metric = "AEC", value = average_expected_cost))
  }

  # fit xgboost
  params$objective <- AecGradHess_M
  params$eval_metric <- AEC_M

  xgbmodel <- xgboost::xgb.train(params, dtrain, nrounds, watchlist,
                                 verbose               = verbose,
                                 print_every_n         = print_every_n,
                                 early_stopping_rounds = early_stopping_rounds,
                                 maximize              = FALSE,
                                 save_period           = save_period,
                                 save_name             = save_name,
                                 xgb_model             = xgb_model, ...)

  # end timer
  t_end <- proc.time() - t_start

  # output
  xgbmodel$params <- c(xgbmodel$params, list(formula               = formula,
                                             nrounds               = nrounds,
                                             early_stopping_rounds = early_stopping_rounds,
                                             example_cost_matrix   = example_cost_matrix))
  output <- list(call     = call,
                 time     = round(t_end[3], 3),
                 xgbmodel = xgbmodel)
  class(output) <- "K2csboost"
  return(output)
}
