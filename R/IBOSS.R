#' Information-Based Optimal Subdata Selection for Big Data Linear Regression (IBOSS, Rcpp-c++-style)
#'
#' A subsampling method based on D-optiaml criterion inspired by optimal experimental design
#' used for linear regression.
#'
#' @param n Subsample size.
#' @param X A data.frame or matrix consists of explanatory variables.
#'
#' @return Subsample index.
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

#' IBOSS with `Rcpp` by the package itself.
#'
#' @param n Subsample size.
#' @param X A data.frame or matrix consists of explanatory variables.
#'
#' @return Subsample index.
#' @export
myRcpp_IBOSS <- function(n, X){
  if (floor(n / 2 / ncol(X)) == 0) stop("Subsample size too small. n/2/ncol(X) must >= 1")
  X = as.matrix(X)
  rcppIBOSS(X, n)
}

rbottom_r_index <- function(x, r){
  return(which(x <= sort(x)[r]))
}
rtop_r_index <- function(x, r){
  return(which(x >= sort(x, decreasing = TRUE)[r]))
}

#' IBOSS with `base R` by the package itself.
#'
#' @param n Subsample size.
#' @param X A data.frame or matrix consists of explanatory variables.
#'
#' @return Subsample index.
#' @export
myR_IBOSS <- function(n, X){
  X <- as.matrix(X)
  r <- floor(n / 2 / ncol(X))
  if (r == 0) stop("Subsample size too small. n/2/ncol(X) must >= 1")

  index <- rtop_r_index(X[,1], r)
  index <- c(index, rbottom_r_index(X[,1], r))

  for (j in 2:ncol(X)){
    temp <- setdiff(1:nrow(X), index)
    index <- c(index, temp[rtop_r_index(X[temp, j], r)])
    index <- c(index, temp[rbottom_r_index(X[temp, j], r)])
  }
  return(index)
}

#' IBOSS with `RcppArmadillo` by the package itself.
#'
#' @param n Subsample size.
#' @param X A data.frame or matrix consists of explanatory variables.
#'
#' @return Subsample index.
#' @export
myArma_IBOSS <- function(n, X){
  if (floor(n / 2 / ncol(X)) == 0) stop("Subsample size too small. n/2/ncol(X) must >= 1")
  X = as.matrix(X)
  armarcppIBOSS(X, n)
}

