#' MLE for Logistic regression
#'
#' Maximum likelihood estimation for logistic regression.
#'
#' @param x A matrix of explanatory variables.
#' @param y A numeric vector. Response variable.
#' @param w A numeric vector. The weight of each sample.
#'
#' @return A list.
#'  * `par` : parameter estimation.
#'  * `message` : message during iteration.
#'  * `iter` : iteration times.
get_Logistic_MLE <- function(x, y, w) {
  d <- ncol(x)
  beta <- rep(0, d)
  loop  <- 1
  Loop  <- 100
  msg <- "NA"
  while (loop <= Loop) {
    pr <- c(1 - 1 / (1 + exp(x %*% beta)))
    H <- t(x) %*% (pr * (1 - pr) * w * x)
    S <- colSums((y - pr) * w * x)
    tryCatch(
      {shs <- NA
      shs <- solve(H, S) },
      error=function(e){
        cat("\n ERROR :", loop, conditionMessage(e), "\n")})
    if (is.na(shs[1])) {
      msg <- "Not converge"
      beta <- loop <- NA
      break
    }
    beta.new <- beta + shs
    tlr  <- sum((beta.new - beta)^2)
    beta  <- beta.new
    if(tlr < 0.000001) {
      msg <- "Successful convergence"
      break
    }
    if (loop == Loop)
      warning("Maximum iteration reached")
    loop  <- loop + 1
  }
  list(par=beta, message=msg, iter=loop)
}

#' Optimal Subsampling for Large Sample Logistic Regression(OSMAC)
#'
#' A subsampling method based on A- / L- optimal for logistic regression proposed by Wang et.al. (2018).
#'
#' @param X A data.frame or matrix of explanatory variables.
#' @param Y A numeric vector. Response variable.
#' @param r1 Sample size for pilot sample.
#' @param r2 Subsample size.
#' @param method Sampling methods:
#'  * `mmse`: A-optimal.
#'  * `mvc`: L-optimal.
#' @param seed_1 Random seed for the first stage sampling.
#' @param seed_2 Random seed for the second stage sampling.
#'
#' @return A numeric vector with length `r2` which represent the subsample index.
#'
#' @references HaiYing Wang, Rong Zhu and Ping Ma (2018)
#' \emph{Optimal Subsampling for Large Sample Logistic Regression, Journal of the American
#' Statistical Association, 113:522, 829-844},
#' \url{https://www.tandfonline.com/doi/full/10.1080/01621459.2017.1292914},
#' \url{https://github.com/Ossifragus/OSMAC}.
#'
#' @examples
#' data <- data_binary_class
#' y <- data[["y"]]
#' x <- data[-which(names(data) == "y")]
#'
#' OSMAC(X = x, Y = y, r1 = 100, r2 = 5, method="mmse", seed_1 = 123, seed_2 = 456)
#' OSMAC(X = x, Y = y, r1 = 100, r2 = 5, method="mvc", seed_1 = 123, seed_2 = 456)
#' @export
OSMAC <- function(X, Y, r1, r2, method=c("mmse", "mvc"), seed_1 = NULL, seed_2 = NULL){
  X <- as.matrix(X)
  Y <- as.integer(Y)
  n <- length(Y)
  n1 <- sum(Y)
  n0 <- n - n1
  PI.prop <- rep(1/(2*n0), n)
  PI.prop[Y==1] <- 1/(2*n1)
  if (!is.null(seed_1)) {
    idx.prop <- withr::with_seed(seed_1, sample(1:n, r1, T, PI.prop))
  } else {
    idx.prop <- sample(1:n, r1, T, PI.prop)
  }
  x.prop <- X[idx.prop,]
  y.prop <- Y[idx.prop]
  pinv.prop <- n
  pinv.prop <- 1/PI.prop[idx.prop]
  fit.prop <- get_Logistic_MLE(x=x.prop, y=y.prop, w=pinv.prop)
  beta.prop <- fit.prop$par
  if (is.na(beta.prop[1]))
    return(list(opt=NA, msg="first stage not converge"))

  if (method == "mmse") {
    P.prop  <- 1 - 1 / (1 + exp(X %*% beta.prop))
    p.prop <- P.prop[idx.prop]
    w.prop <- p.prop * (1 - p.prop)
    W.prop <- solve(t(x.prop) %*% (x.prop * w.prop * pinv.prop))
    PI.mMSE <- sqrt((Y - P.prop)^2 * rowSums((X%*%W.prop)^2))
    PI.mMSE <- PI.mMSE / sum(PI.mMSE)
    if (!is.null(seed_2)) {
      idx.mMSE <- withr::with_seed(seed_2, sample(1:n, r2, T, PI.mMSE))
    } else {
      idx.mMSE <- sample(1:n, r2, T, PI.mMSE)
    }

    return(idx.mMSE)
  }

  if (method == "mvc") {
    P.prop  <- 1 - 1 / (1 + exp(X %*% beta.prop))
    PI.mVc <- sqrt((Y - P.prop)^2 * rowSums(X^2))
    PI.mVc <- PI.mVc / sum(PI.mVc)
    if (!is.null(seed_2)) {
      idx.mVc <- withr::with_seed(seed_2, sample(1:n, r2, T, PI.mVc))
    } else {
      idx.mVc <- sample(1:n, r2, T, PI.mVc)
    }

    return(idx.mVc)
  }
}




