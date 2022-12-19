# @title checkInputs_K2csboost
#
# @description A function for checking the conformity of the arguments.
#
# @param formula An object of class "\code{\link{formula}}": a symbolic description of the model to be fitted. An intercept can not be included.
# @param train A training set as a data frame containing the variables in the model.
# @param test A test set (if provided) as a data frame containing the variables in the model (default is \code{NULL}).
# @param cost_matrix_train A matrix of dimension 2 x 2. The first (second) row contains the cost of TP & FN (FP & TN) of the instance.
# @param cost_matrix_test A matrix of dimension 2 x 2 (if provided). The first (second) row contains the cost of TP & FN (FP & TN) of the instance. (Default is \code{NULL}).
#'
#' @importFrom stats model.frame
#'
checkInputs_K2csboost <- function (formula, train, test, cost_matrix_train, cost_matrix_test) {
  # check inputs
  if (missing(formula)) {
    stop("argument 'formula' is missing, with no default")
  } else if (class(formula) != "formula") {
    stop("argument 'formula' must be a formula")
  }
  if (missing(train)) {
    stop("argument 'train' is missing, with no default")
  } else if(!is.data.frame(train)) {
    stop("argument 'train' must be a data frame")
  }
  if (missing(cost_matrix_train)) {
    stop("argument 'cost_matrix_train' is missing, with no default")
  } else if (!is.matrix(cost_matrix_train) | any(dim(cost_matrix_train) != c(2, 2))) {
    stop("argument 'cost_matrix_train' must be a matrix of dimension 2 x 2")
  }
  if (!is.null(test)) {
    if (!is.data.frame(test)) {
      stop("argument 'test' must be a data frame")
    }
    if (is.null(cost_matrix_test)) {
      stop("argument 'cost_matrix_test' must be specified when 'test' is given")
    } else if (!is.matrix(cost_matrix_test) |
               any(dim(cost_matrix_test) != c(2, 2))) {
      stop("argument 'cost_matrix_test' must be a matrix of dimension 2 x 2")
    }
  }
  if (!is.null(cost_matrix_test) & is.null(test)) {
    stop("argument 'test' must be specified when 'cost_matrix_test' is given")
  }

  # check response variable of training data
  mf_train <- model.frame(formula, train)
  if (attr(attr(mf_train, "terms"), "intercept") != 0) {
    stop("argument 'formula' can not contain an intercept")
  }
  if (length(unique(mf_train[, 1])) == 1L) {
    stop("response variable in 'train' has no variation")
  }
  if (!is.factor(mf_train[, 1])) {
    stop("response variable in 'train' is not a factor")
  } else if (length(levels(mf_train[, 1])) != 2L) {
    stop("response variable in 'train' has ", length(levels(mf_train[, 1])), " level(s) instead of 2")
  }

  # check response variable of test data
  if (is.null(test)) {
    mf_test <- NULL
  } else {
    mf_test <- model.frame(formula, test)
    if (!is.factor(mf_test[, 1])) {
      stop("response variable in 'test' is not a factor")
    } else if (length(levels(mf_test[, 1])) != 2L) {
      stop("response variable in 'test' has ", length(levels(mf_test[, 1])),
           " level(s) instead of 2")
    }
  }

  return(list(mf_train = mf_train, mf_test = mf_test))
}
