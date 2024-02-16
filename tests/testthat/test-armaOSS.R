test_that("OSS with RcppArmadill get the same result with Rcpp", {
  data_numeric_regression["y"] <- NULL
  X <- data_numeric_regression

  expect_equal(myArma_OSS(100, X), OSS(100, X))
  expect_equal(myArma_OSS(100, X), myArma_OSS(100, X))
})



