#' Get subsample index.
#'
#' A unified interface for retrieving subsample indexes
#'
#' @param y_name A character. The name of the response variable in the data frame.
#' @param x_name A character. The name of the explanatory variable in the data frame.
#'   Default to all variables except the response variable
#' @param data A data frame containing response variables and explanatory variables
#' @param n Subsample size.
#' @param pilot_n pilot sample size (for some method)
#' @param method Subsamling methods:
#'  * `Unif`: Random sampling.
#'  * `OSMAC_A`: A subsampling method based on A-optimal for logistic regression proposed by Wang et.al. (2018).
#'  * `OSMAC_L`: A subsampling method based on L-optimal for logistic regression proposed by Wang et.al. (2018).
#'  * `IBOSS`: A subsampling method based on D-optimal for linear regression proposed by Wang et.al. (2019).
#'  * `OSS` : A subsampling method based on Orthogonal Array proposed by Wang et.al.(2021).
#'  * `LowCon`: A subsampling method based on Space-filling designs proposed by Meng et.al.(2021).
#'  * `IES`: A subsampling method based on Orthogonal Array proposed by Zhang et.al.(2024).
#' @param replace A boolean.
#'  * `TRUE` (the default): Sampling with replace.
#'  * `FALSE`: Sampling without replace
#' @param seed Random seed. Default to NULL.
#' @param seed_1 Random seed for the first stage sampling (When two-phase subsampling are needed). Default to NULL.
#' @param seed_2 Random seed for the second stage sampling (When two-phase subsampling are needed). Default to NULL.
#' @param na_method Method to handle NA.
#' @param ... Additional parameters setting for some methods.
#'  * `theta`: Percentage of data shrinkage for `LowCon`. Default to 1.
#'  * `q` : Hyperparamter of how to divide the axes for `IES`. Default to 16.
#'
#' @return A numeric vector with length `n` which represent the subsample index.
#' @export
#'
#' @examples
#' data_binary <- data_binary_class
#' subsampling(y_name = "y", data = data_binary, n = 30, method = "Unif", seed = 123)
#' subsampling(y_name = "y", data = data_binary, n = 30, pilot_n = 100, method = "OSMAC_A",
#'   seed_1 = 123, seed_2 = 456)
#' subsampling(y_name = "y", data = data_binary, n = 30, pilot_n = 100, method = "OSMAC_L",
#'   seed_1 = 123, seed_2 = 456)
#'
#' data_numeric <- data_numeric_regression
#' subsampling(y_name = "y", data = data_numeric, n = 100, method = "IBOSS")
#' subsampling(y_name = "y", data = data_numeric, n = 30, method = "OSS")
#' subsampling(y_name = "y", data = data_numeric, n = 10, method = "LowCon", seed = 123, theta = 1)
#' subsampling(y_name = "y", data = data_numeric, n = 10, method = "IES", seed = 123, q = 16)

subsampling <- function(y_name, x_name = NULL, data, n, pilot_n = NULL, method = "Unif",
                        replace = TRUE, seed = NULL, seed_1 = NULL, seed_2 = NULL, na_method = NULL, ...) {

  # CheckPara(y_name, x_name, data, n, pilot_n, method, replace, seed_1, seed_2, na_method)

  y = data[[y_name]]
  if (is.null(x_name)) {x <- data[-which(names(data) == y_name)]} else {x <- data[x_name]}
  N = nrow(data)

  # replace
  subsample_index <- switch(method,
         Unif = Unif(N = N, n = n, seed = seed, replace = TRUE),
         OSMAC_A = OSMAC(X = x, Y = y, r1 = pilot_n, r2 = n, method = "mmse", seed_1 = seed_1, seed_2 = seed_2),
         OSMAC_L = OSMAC(X = x, Y = y, r1 = pilot_n, r2 = n, method = "mvc", seed_1 = seed_1, seed_2 = seed_2),
         IBOSS = IBOSS(n = n, X = x),
         Leverage = Leverage(X = x, n = n, replace = replace, seed = seed, ...), # shrinkage in ...
         OSS = OSS(n = n, X = x),
         LowCon = LowCon(X = x, n = n, seed = seed, ...), # theta in ...
         IES = IES(X = x, n = n, seed = seed, ...), # q in ...
         DDS = DDS(X = x, n = n, ...) # ratio in ...
         # Support
    )
  return(subsample_index)
}
