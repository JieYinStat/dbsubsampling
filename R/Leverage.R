#' Subsampling Based on Leverage Scores
#'
#' A subsampling methods based on leverage scores proposed by Ma et.al. (2015).
#'
#' @param X A data.frame or matrix of explanatory variables.
#' @param n Subsample size.
#' @param shrinkage_alpha Shrinkage for SLEV, default to 1 (do not shrinkage).
#' @param replace With replacement or without replacement, default to TRUE.
#' @param seed Random seed for the sampling.
#'
#' @return Subsample index.
#' @references Ping Ma, Michael W. Mahoney & Bin Yu (2015)
#' \emph{A Statistical Perspective on Algorithmic Leveraging,
#' Journal of Machine Learning Research, 16:27, 861−911},
#' \url{https://jmlr.csail.mit.edu/papers/v16/ma15a.html}.
#'
#' @examples
#' data <- data_numeric_regression
#' X <- data[-which(names(data) == "y")]
#' Leverage(X, n = 100, shrinkage_alpha = 0.9, replace = TRUE, seed = NULL)
#' @export
Leverage <- function(X, n, shrinkage_alpha = 1, replace = TRUE, seed = NULL) {
  if (!is.null(seed)) withr::local_seed(seed)

  lev_prob <- apply(svd(X)[["u"]] , 1, crossprod) / ncol(X)

  if (shrinkage_alpha == 1) {
    prob <- lev_prob
  } else {
    prob <- shrinkage_alpha * lev_prob + (1 - shrinkage_alpha) / nrow(X)
  }

  index <- sample.int(nrow(X), size = n, replace = replace, prob = prob)
  return(index)
}
