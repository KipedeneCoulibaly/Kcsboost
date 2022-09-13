
checkInputs_K2CSboost <- function (formula, train, test, cost_matrix_train, cost_matrix_test) {
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


K2_csboost <- function (formula, train, test = NULL,
                        cost_matrix_train, cost_matrix_test = NULL,
                        nrounds, params = list(),
                        verbose = 1, print_every_n = 1L, early_stopping_rounds = NULL,
                        save_period = NULL, save_name = "xgboost.model",
                        xgb_model = NULL, ...) {
  # start timer
  t_start <- proc.time()
  
  # original call
  call <- match.call()
  
  # check inputs
  check <- checkInputs_K2CSboost(formula, train, test, cost_matrix_train, cost_matrix_test)
  
  # convert data to xgb.DMatrix & build watchlist
  labels_train <- hmeasure::relabel(model.response(check$mf_train))
  Y_Ybar_train <- cbind(labels_train, 1-labels_train)
  dtrain       <- xgboost::xgb.DMatrix(data = model.matrix(formula, train), label = labels_train)
  watchlist    <- list(train = dtrain)
  
  dtest <- NULL
  if (!is.null(test)) {
    labels_test <- hmeasure::relabel(model.response(check$mf_test))
    Y_Ybar_test <- cbind(labels_test, 1-labels_test)
    dtest       <- xgboost::xgb.DMatrix(data = model.matrix(formula, test), label = labels_test)
    watchlist   <- c(watchlist, list(test = dtest))
  }
  
  # rearrange cost matrix & define auxilary vectors
  cost_matrix_train_obj <- Y_Ybar_train%*%cost_matrix_train
  diff_costs_train      <- cost_matrix_train_obj[, 1] - cost_matrix_train_obj[, 2]
  
  if (!is.null(test)) {
    cost_matrix_test_obj <- Y_Ybar_test%*%cost_matrix_test
    diff_costs_test      <- cost_matrix_test_obj[, 1] - cost_matrix_test_obj[, 2]
  }
  
  # example cost matrix (used by summary.K2_csboost)
  example_cost_matrix <- matrix(c(cost_matrix_train_obj[which(labels_train == 0)[1], c(2, 1)],
                                  cost_matrix_train_obj[which(labels_train == 1)[1], c(2, 1)]),
                                nrow = 2, ncol = 2)
  colnames(example_cost_matrix)        <- rownames(example_cost_matrix) <- c("0", "1")
  names(dimnames(example_cost_matrix)) <- c("      Prediction", "Reference")
  
  # define objective function
  AecGradHess_M <- function (scores, dtrain) {
    scores      <- 1 / (1 + exp(-scores))
    grad        <- scores * (1 - scores) * diff_costs_train
    hess        <- abs((1 - 2 * scores) * grad)
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
  params$objective   <- AecGradHess_M
  params$eval_metric <- AEC_M
  
  xgbmodel <- xgboost::xgb.train(params, dtrain, nrounds, watchlist,
                                 verbose                 = verbose, 
                                 print_every_n           = print_every_n,
                                 early_stopping_rounds   = early_stopping_rounds, 
                                 maximize                = FALSE,
                                 save_period             = save_period,
                                 save_name               = save_name,
                                 xgb_model               = xgb_model, ...)
  
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
  class(output) <- "K2_csboost"
  return(output)
}
