## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
##
## R main function `IES`.  `RcppArmadillo`-Version.
##
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------

#' Scale data to \eqn{[-1, 1]}
#'
#' @param X A data.frame or matrix.
#'
#' @return Scaled X.
#' @export
#'
#' @examples
#' X <- matrix(1:20, 5, 4)
#' scale01(X)
scale01 <- function(X) {
  apply(X, 2, function(.col) (.col - min(.col)) / (max(.col)-min(.col)))
}

#' Independence-Encouraging Subsampling for Nonparametric Additive Models (IES, Proposed by Zhang et.al. (2024))
#'
#' A subsampling method for nonparameter additive model based on Orthogonal Array.
#'
#' @param X A data.frame or matrix consists of explanatory variables.
#' @param n Subsample size.
#' @param q Hyperparamter of how to divide the axes. Default to 16.
#' @param seed Random seed for the sampling.
#'
#' @return Subsample index.
#' @references Yi Zhang, Lin Wang, Xiaoke Zhang & HaiYing Wang (2024)
#' \emph{Independence-Encouraging Subsampling for Nonparametric Additive Models,
#' Journal of Computational and Graphical Statistics},
#' \url{https://www.tandfonline.com/doi/full/10.1080/10618600.2024.2326136}.
#'
#' @examples
#' data <- data_numeric_regression
#' X <- data[-which(names(data) == "y")]
#' IES(X, n = 100, q = 16, seed = NULL)
#' @export
IES <- function(X, n, q = 16, seed = NULL) {
  if (!is.null(seed)) withr::local_seed(seed)
  X <- scale01(as.matrix(X))
  attributes(X) <- attributes(X)["dim"]

  index <- armaIES(X, n, q)
  return(as.vector(index))
}


#' IES C++-Version for Benchmarking (R-Wrap Code)
#'
#' There is no randomness, all parts that need to be randomly selected are selected first indexed.
#'
#' @param X X A data.frame or matrix consists of explanatory variables.
#' @param n Subsample size.
#' @param q Hyperparamter of how to divide the axes.
#'
#' @export
c_IES_compare <- function(X, n, q) {
  X <- scale01(as.matrix(X))
  attributes(X) <- attributes(X)["dim"]

  index <- armaIES_compare(X, n, q)
  return(as.vector(index))
}

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
##
## IES R-Version.
##
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------

r_Compute_IES_Loss <- function(candi, last_index, X, q) {
  x_last <- floor(X[last_index, ]*q)
  loss <- apply(X[candi, ], 1, function(.row) (sum(floor(.row*q) == x_last))^2)
  return(loss)
}

#' R Version of IES for Testing
#'
#' @param X A data.frame or matrix consists of explanatory variables.
#' @param n Subsample size.
#' @param q Hyperparamter of how to divide the axes.
#' @param seed Random seed for the sampling.
#'
#' @export
r_IES <- function(X, n, q, seed = NULL) {
  if (!is.null(seed)) withr::local_seed(seed)
  X <- scale01(as.matrix(X))
  attributes(X) <- attributes(X)["dim"]
  N <- nrow(X)

  index <- numeric(n)
  candi <- 1:N
  loss <- numeric(N)

  # Initial
  index[1] <- sample(N, 1)
  # index[1] <- 1
  candi <- candi[-index[1]]
  loss <- loss[-index[1]]
  loss <- r_Compute_IES_Loss(candi, index[1], X, q)
  # paste0("---Step: 1---")
  # print(loss)

  for (i in 2:n) {
    # Election
    temp_vec <- which(loss == min(loss))
    temp <- temp_vec[sample(length(temp_vec), 1)]
    # temp <- temp_vec[1]
    index[i] <- candi[temp]
    # print(temp)

    # Update
    candi <- candi[-temp]
    loss <- loss[-temp]
    loss <- loss + r_Compute_IES_Loss(candi, index[i], X, q)
    # paste0("---Step: ", i, "---")
    # print(loss)
  }

  return(index)
}

#' IES R-Version for Benchmarking
#'
#' There is no randomness, all parts that need to be randomly selected are selected first indexed.
#'
#' @param X X A data.frame or matrix consists of explanatory variables.
#' @param n Subsample size.
#' @param q Hyperparamter of how to divide the axes.
#'
#' @export
r_IES_compare <- function(X, n, q) {
  X <- scale01(as.matrix(X))
  attributes(X) <- attributes(X)["dim"]
  N <- nrow(X)

  index <- numeric(n)
  candi <- 1:N
  loss <- numeric(N)

  # Initial
  # index[1] <- sample(N, 1)
  index[1] <- 1
  candi <- candi[-index[1]]
  loss <- loss[-index[1]]
  loss <- r_Compute_IES_Loss(candi, index[1], X, q)
  # paste0("---Step: 1---")
  # print(loss)
  for (i in 2:n) {
    # Election
    temp_vec <- which(loss == min(loss))
    # temp <- temp_vec[sample(length(temp_vec), 1)]
    temp <- temp_vec[1]
    index[i] <- candi[temp]
    # print(temp)

    # Update
    candi <- candi[-temp]
    loss <- loss[-temp]
    loss <- loss + r_Compute_IES_Loss(candi, index[i], X, q)
    # paste0("---Step: ", i, "---")
    # print(loss)
  }

  return(index)
}
