#' @name plot.K2csboost
#'
#' @aliases plot.K2csboost
#'
#' @title plot method for K2csboost fits
#'
#' @description plot expected savings against the iteration index.
#'
#' @usage \method{plot}{K2csboost}(x, legend_position = NULL, \dots)
#'
#' @param x a fitted object of class "\code{K2csboost}".
#' @param legend_position string indicating the position of the legend.
#' @param ... further arguments passed to or from other methods.
#'
#' @details ```plot.K2csboost``` plots the evolution of the expected savings. Use of ggplot2 for plotting if available, otherwise basic graphics are used.
#'
#' @export plot.K2csboost
#' @exportS3Method
#' @importFrom graphics abline legend lines plot
#' @importFrom ggplot2 ggplot aes geom_line theme_dark theme geom_vline ylim labs
#'
#' @author Kipedene Coulibaly
#'
#' @references Coulibaly, K.(2022).\emph{"Statistical modeling of construction probabilities and buildable capacities of parcels based on UrbanSimul geohistorical data"}\[Unpublished Master 2 thesis\]. INRAE and the University Franche-Comte of Besancon.
#'
#'
#' @seealso \code{\link{K2csboost}}, \code{\link{summary.K2csboost}},  \code{\link{predict.K2csboost}}
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
plot.K2csboost <- function (x, legend_position = NULL, ...) {
  # check inputs
  if (missing(x)) {
    stop("argument 'x' is missing, with no default")
  }
  if (class(x) != "K2csboost") {
    stop("argument 'x' must be of class 'K2csboost'")
  }
  if (!is.null(legend_position)) {
    if (!legend_position %in% c("bottomright", "bottom", "bottomleft", "left", "topleft",
                                "top", "topright", "right", "center")) {
      stop(paste("'legend_position' should be one of \"bottomright\", \"bottom\", \"bottomleft\",",
                 "\"left\", \"topleft\", \"top\", \"topright\", \"right\", \"center\""))
    }
  }

  # plot average expected cost versus iteration

  evallog <- x$xgbmodel$evaluation_log
  ylimit <- range(evallog[, 2:NCOL(evallog)])

  # use of ggplot2 for plotting if available, otherwise basic graphics are used.

  if(base::require("ggplot2", quietly = TRUE)){
    p <- ggplot2::ggplot(data=evallog) +
      geom_line(mapping=aes(x=evallog$iter, y=unlist(evallog[,2]), color="Train"), size = 1, ...) +
      theme_dark()

    if(NCOL(evallog) == 3) {
      p <- p + geom_line(aes(x=evallog$iter, y=unlist(evallog[,3]), color="Test"), linetype = "dashed", size = 1, ...)
      if(is.null(legend_position)){
        p <- p + theme(legend.position= "right")
      }
      p <- p + geom_vline(xintercept=x$xgbmodel$best_iteration, color = "red", linetype = "dashed", size = 1.5) +
        ylim(ylimit) +
        theme(legend.position= legend_position) +
        labs(x = "iteration", y = "average expected cost")
      print(p)
    }

  }else{
    plot(evallog$iter, unlist(evallog[, 2]), type = "l", ylim = ylimit, ylab = "average expected cost", xlab = "iteration", col="green", ...)

    if (NCOL(evallog) == 3) {
      lines(evallog$iter, unlist(evallog[, 3]), lty = 2, col="orange", ...)
      if (is.null(legend_position)) {
        legend_position <- "top"
      }
      abline(v = x$xgbmodel$best_iteration, lty = 4, col="red", ...)
      legend(legend_position, legend = c("train", "test"), lty = c(1, 2), col=c("green", "orange"), lwd = 2)
    }
  }
}
