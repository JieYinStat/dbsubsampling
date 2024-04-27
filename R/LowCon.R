#' Transform data to \eqn{[-1,1]^p}
#'
#' @param X A data.frame or matrix of explanatory variables.
#'
#' @return Scaled `X` to \eqn{[-1,1]^p}.
#' @export
#'
#' @examples
#' X <- matrix(rnorm(100), nrow = 20, ncol = 5)
#' scaled_X <- scale_neg_pos_1(X)
#' apply(scaled_X, 2, range)
scale_neg_pos_1 <- function(X) {
  apply(X, 2, function(.col) (( (.col - min(.col)) / (max(.col)-min(.col)) ) *2 ) -1 )
}

trans_LHD_adapt_X <- function(LHD_data, X, theta) {
  p <- ncol(X)
  space <- apply(X, 2, stats::quantile, probs = c(theta / 100, 1 - theta / 100))
  width <- space[2,] - space[1,]
  for (j in 1:p) {
    LHD_data[,j] <- LHD_data[,j]*width[j] + space[1, j]
  }
  return(LHD_data)
}

#' LowCon: A Design-based Subsampling Approach in a Misspecified Linear Model
#'
#' A subsampling method based space-filling design for misspecified linear model proposed by Meng et.al. (2021).
#'
#' @param X A data.frame or matrix of explanatory variables.
#' @param n Subsample size.
#' @param theta Percentage of data shrinkage. Default to 1.
#' @param space_method The generation method of initial space-filling design.
#' @param seed Random seed for the sampling.
#'
#' @return Subsample index.
#' @references Cheng Meng, Rui Xie, Abhyuday Mandal, Xinlian Zhang, Wenxuan Zhong & Ping Ma (2021)
#' \emph{LowCon: A Design-based Subsampling Approach in a Misspecified Linear Model,
#' Journal of Computational and Graphical Statistics, 30:3, 694-708},
#' \url{https://www.tandfonline.com/doi/full/10.1080/10618600.2020.1844215}.
#'
#' @examples
#' data <- data_numeric_regression
#' X <- data[-which(names(data) == "y")]
#' LowCon(X, n = 100, space_method = "randomLHS", theta = 1, seed = NULL)
#' @export

LowCon <- function(X, n, theta = 1, space_method = "randomLHS", seed = NULL){
  if (!is.null(seed)) withr::local_seed(seed)

  X <- scale_neg_pos_1(as.matrix(X))
  attributes(X) <- attributes(X)["dim"]
  p <- ncol(X)

  if (space_method == "randomLHS") {
    LHD_data <- lhs::randomLHS(n, p)
  } else if (space_method == "sobol") {
    LHD_data <- spacefillr::generate_sobol_set(n, p)
  } else {
    LHD_data <- lhs::randomLHS(n, p)
  }
  LHD_data <- trans_LHD_adapt_X(LHD_data, X, theta)

  index <- RANN::nn2(X, LHD_data, k=1, treetype = "kd")$nn.idx
  return(as.vector(index))
}


