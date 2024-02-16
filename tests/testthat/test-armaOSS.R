test_that("OSS with RcppArmadill get the same result with Rcpp", {
  data_numeric_regression["y"] <- NULL
  X <- as.matrix(data_numeric_regression)
  attributes(X) <- attributes(X)["dim"]

  expect_equal(as.vector(armaOSS(X, 100)), OSS(100, X))
  expect_equal(as.vector(armaOSS(X, 100)), as.vector(armaOSS(X, 100)))
})



