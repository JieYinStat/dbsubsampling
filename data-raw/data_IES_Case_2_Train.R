## code to prepare `data_IES_Case_2_Train` dataset goes here
m1 <- function(x) {
  8 / (4 + x)
}

m2 <- function(x) {
  exp(3 - x^2) / 4
}

m3 <- function(x) {
  1.5*sin(pi/2*x)
}

trans_exp_data <- function(density, lower, upper) {
  out <- qexp( pexp(lower) + density*(pexp(upper) - pexp(lower)) )
  return(out)
}

gene_data <- function(N, rho, lower, upper, sd, distribute = "normal") {
  cov <- matrix(rho, 3, 3) + diag(1-rho, 3)
  if (distribute == "normal") {
    x <- TruncatedNormal::rtmvnorm(N, rep(0, 3), cov, lb = rep(lower, 3), ub = rep(upper, 3))
  } else if (distribute == "exp") {
    copula_def <- copula::ellipCopula(family = "normal", dim = 3, dispstr = "ex", param = rho)
    temp_quan <- copula::rCopula(N, copula_def)
    x <- apply(temp_quan, 2, trans_exp_data, 0, (upper-lower)) - (upper-lower)/2
  }

  colnames(x) <- c("X1", "X2", "X3")
  m <- 1 + m1(x[, "X1"]) + m2(x[, "X2"]) + m3(x[, "X3"])
  epi <- rnorm(N, 0, sd)
  y <- m + epi
  data <- as.data.frame(cbind(x, m = m , y = y))
  return(data)
}

N <- 10000; rho <- 0.3; lower <- -2; upper <- 2; sd = 0.5;
data_IES_Case_2_Train <- gene_data(N, rho, lower, upper, sd, "exp")
usethis::use_data(data_IES_Case_2_Train, overwrite = TRUE)
