getalpha = function(n,s){
  alpha = NULL
  for (i in 2:n){
    if ( (primes::gcd(i, n+1) == 1)  & (length(unique(i^(1:s) %% (n+1))) == s) ) alpha = c(alpha, i)
  }
  return(alpha)
}

getMD = function(x){
  n = dim(x)[1]
  d = dim(x)[2]
  temp1 = (19/12)^d - 2/n*sum(apply(5/3-abs(x-0.5)/4-(x-0.5)^2/4, 1, prod))
  temp2 = 0
  for (i in 1:n){
    for (k in 1:n){
      temp2 = temp2 + prod(15/8 - abs(x[i,]-0.5)/4 - abs(x[k,]-0.5)/4
                           - 3/4*abs(x[i,]-x[k,]) + (x[i,]-x[k,])^2/2)
    }
  }
  value = temp1 + temp2/(n^2)
  return(value)
}

getUD <- function(n, d) {
  alpha <- getalpha(n, d)
  MD <- numeric(length(alpha))
  for (k in length(alpha)){
    design <- ((1:n %*% (alpha[k]^(t(0:(d-1))))) %% (n+1)) / n - 1 / (2*n)
    MD[k] <- getMD(design)
  }
  uniform_design <- ((1:n %*% (alpha[which.min(MD)]^(t(0:(d-1))))) %% (n+1)) / n - 1 / (2*n)
  return(uniform_design)
}

#' Data-Driven Subsampling Based on Uniform Design
#'
#' A model-free subsampling method based on uniform design proposed by Zhang et.al. (2023).
#'
#' @param X A data.frame or matrix of explanatory variables.
#' @param n Subsample size.
#' @param ratio Dimensionality reduction ratio of PCA, default to 0.85.
#'
#' @return Subsample index.
#' @details
#' The uniform design is generated according to the method described in part 5 of Zhang's article.
#' The uniformity measure uses mixed divergence.
#'
#' @references Mei Zhang, Yongdao Zhou, Zheng Zhou & Aijun Zhang (2023)
#' \emph{Model-Free Subsampling Method Based on Uniform Designs,
#' IEEE Transactions on Knowledge and Data Engineering, 36:3, 1210-1220},
#' \url{https://ieeexplore.ieee.org/abstract/document/10192374}.
#' @export
#'
#' @examples
#' data <- data_numeric_regression
#' X <- data[-which(names(data) == "y")]
#' DDS(X, n = 100, ratio = 0.85)
DDS <- function(X, n, ratio = 0.85) {
  X_svd <- svd(scale(X))
  lambda <- (X_svd$d)^2
  d <- which((cumsum(lambda) / sum(lambda)) >= ratio)[1]
  Z <- X_svd$u[, 1:d]

  D <- getUD(n, d)
  Q <- matrix(NA, n, d)
  for (j in 1:d){
    Q[,j] <- stats::quantile(Z[,j], D[,j])
  }

  index <- RANN::nn2(data = Z, query = Q, k = 1, treetype = "kd")$nn.idx
  return(t(index))
}
