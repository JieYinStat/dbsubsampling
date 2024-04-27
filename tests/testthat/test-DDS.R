test_that("DDS works", {
  data <- data_numeric_regression
  X <- data[-which(names(data) == "y")]
  expect_equal(DDS(X, n = 100, ratio = 0.85), DDS(X, n = 100, ratio = 0.85))
})
