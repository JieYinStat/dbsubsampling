test_that("OSS with Rcpp works well and get the same result with r-version", {
  data_numeric_regression["y"] <- NULL
  X <- data_numeric_regression

  # X <- scale(as.matrix(data_numeric_regression))
  # attributes(X) <- attributes(X)["dim"]
  # expect_equal(L2norm(X), rowSums(X^2))
  # expect_equal(bottom_t_index(X[,1], 20) + 1, which(X[,1] <= sort(X[,1])[20]))

  expect_equal(OSS(100, X), rOSS(100, X))
  expect_equal(OSS(100, X), OSS(100, X))
})
