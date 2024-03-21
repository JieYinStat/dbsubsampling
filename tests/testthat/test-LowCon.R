test_that("LowCon works", {
  data <- data_numeric_regression
  X <- data[-which(names(data) == "y")]

  n <- 1000
  theta <- 1

  expect_equal(length(LowCon(X, n, theta, seed = 123)), n)
})
