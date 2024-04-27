test_that("IES with RcppArmadillo works", {
  data <- data_numeric_regression
  X <- data[-which(names(data) == "y")]
  n <- 100
  q <- 16
  expect_equal(IES(X, n, q, seed = 123), IES(X, n, q, seed = 123))
})
test_that("IES with R works", {
  data <- data_numeric_regression
  X <- data[-which(names(data) == "y")]
  n <- 100
  q <- 16
  expect_equal(r_IES(X, n, q, seed = 123), r_IES(X, n, q, seed = 123))
})
test_that("IES get the same result with R and RcppArmadillo", {
  data <- data_numeric_regression
  X <- data[-which(names(data) == "y")]
  n <- 100
  q <- 16
  expect_equal(r_IES_compare(X, n, q), c_IES_compare(X, n, q))
})
test_that("IES R-version works", {
  data <- data_numeric_regression
  X <- data[-which(names(data) == "y")]
  n <- 100
  q <- 16
  expect_true(length(r_IES(X, n, q)) == 100)
})
