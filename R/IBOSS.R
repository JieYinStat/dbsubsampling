#' Information-Based Optimal Subdata Selection for Big Data Linear Regression (IBOSS)
#'
#' A subsampling method based on D-optiaml criterion inspired by optimal experimental design
#' used for linear regression.
#'
#' @param n subsample size.
#' @param X A data.frame or matrix consists of explanatory variables.
#'
#' @return subsample index.
#' @references HaiYing Wang, Min Yang & John Stufken (2019)
#' \emph{Information-Based Optimal Subdata Selection for Big Data Linear Regression,
#' Journal of the American Statistical Association, 114:525, 393-405},
#' \url{https://www.tandfonline.com/doi/full/10.1080/01621459.2017.1408468},
#' \url{https://github.com/Ossifragus/IBOSS}.
#' @export
#'
#' @examples
#' data <- data_numeric_regression
#' X <- data[-which(names(data) == "y")]
#' IBOSS(n = 100, X = X)
IBOSS <- function(n, X) {
  X = as.matrix(X)
  d = ncol(X)
  r <- as.integer(n / d / 2)
  idx <- getIdx_cpp(r, X[,1])
  for(j in 2:d) {
    tmp <- getIdxR_cpp(r, X[,j], idx)
    idx <- c(idx, tmp)
  }
  return(idx)
}
