#' @name predict.K2csboost
#'
#' @aliases predict.K2csboost
#'
#' @title predict method for K2csboost fits
#'
#' @description Obtains predictions from a fitted \code{K2csboost} object.
#'
#' @usage \method{predict}{K2csboost}(object, newdata, ntreelimit = NULL, \dots)
#'
#' @param object A fitted object of class "\code{K2csboost}".
#' @param newdata A data frame in which to look for variables with which to predict.
#' @param ntreelimit Limit the number of model's trees or boosting iterations used in prediction. It will use all the trees by default (\code{NULL} value).
#' @param ... Further arguments passed to \code{predict.xgb.Booster}.
#'
#' @return \code{predict.K2csboost} returns a vector of predicted probabilities.
#'
#' @export predict.K2csboost
#' @exportS3Method
#' @importFrom xgboost xgb.DMatrix
#' @importFrom stats predict
#'
#' @author Kipedene Coulibaly
#'
#' @references Coulibaly, K.(2022).\emph{"Statistical modeling of construction probabilities and buildable capacities of parcels based on UrbanSimul geohistorical data"}\[Unpublished Master 2 thesis\]. INRAE and the University Franche-Comte of Besancon.
#'
#' @seealso \code{\link{K2csboost}}, \code{\link{summary.K2csboost}}, \code{\link{plot.K2csboost}}
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
#'
#'
predict.K2csboost <- function (object, newdata, ntreelimit = NULL, ...) {
  # check inputs
  if (missing(object)) {
    stop("argument 'object' is missing, with no default")
  } else if (class(object) != "K2csboost") {
    stop("argument 'object' must be of class 'K2csboost'")
  }
  if (missing(newdata)) {
    stop("argument 'newdata' is missing, with no default")
  } else if(!is.data.frame(newdata)) {
    stop("argument 'newdata' must be a data frame")
  }

  # predict scores
  dnewdata <- xgboost::xgb.DMatrix(data = model.matrix(object$xgbmodel$params$formula, newdata))
  etas <- predict(object$xgbmodel, newdata = dnewdata, ntreelimit = ntreelimit, ...)
  scores <- 1 / (1 + exp(-etas))
  return(scores)
}
