test_that("leverage works", {
  data <- data_numeric_regression
  X <- data[-which(names(data) == "y")]

  expect_equal(Leverage(X, n = 100, shrinkage_alpha = 1, replace = TRUE, seed = 123),
               Leverage(X, n = 100, shrinkage_alpha = 1, replace = TRUE, seed = 123))
  expect_equal(Leverage(X, n = 100, shrinkage_alpha = 0.9, replace = TRUE, seed = 123),
               Leverage(X, n = 100, shrinkage_alpha = 0.9, replace = TRUE, seed = 123))
})
