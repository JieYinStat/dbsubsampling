#' Uniform sampling.
#'
#' Random sampling with equal probability.
#'
#' @param N Total sample size.
#' @param n Subsample size.
#' @param replace A boolean.
#'  * `TRUE` (the default): Sampling with replace.
#'  * `FALSE`: Sampling without replace
#' @param seed Random seed which is an integer (default NULL). This random seed is only valid for this sampling and
#'  will not affect the external environment
#'
#' @return A numeric vector with length `n` which represent the subsample index.
#' @export
#'
#' @examples
#' N <- 1000
#' n <- 10
#' Unif(N = 1000, n = 10)
#' Unif(N = 1000, n = 10)
#' Unif(N = 1000, n = 10) != Unif(N = 1000, n = 10)
#'
#' Unif(N = 1000, n = 10, seed = 1)
#' Unif(N = 1000, n = 10, seed = 123)
#'
#' Unif(N = 1000, n = 10, seed = 123, replace = TRUE)
#' Unif(N = 1000, n = 10, seed = 123, replace = FALSE)
Unif <- function(N, n, seed = NULL, replace = TRUE) {
  if(!is.null(seed)) withr::local_seed(seed)
  sample(N, n, replace)
}
