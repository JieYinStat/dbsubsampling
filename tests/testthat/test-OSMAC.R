test_that("OSMAC subsampling with the same seed_1 and seed_2 is identical", {
  data <- data_binary_class
  y <- data[["y"]]
  x <- data[-which(names(data) == "y")]
  expect_equal(OSMAC(X = x, Y = y, r1 = 100, r2 = 5, method="mmse", seed_1 = 123, seed_2 = 456),
               OSMAC(X = x, Y = y, r1 = 100, r2 = 5, method="mmse", seed_1 = 123, seed_2 = 456))
  expect_equal(OSMAC(X = x, Y = y, r1 = 100, r2 = 5, method="mvc", seed_1 = 123, seed_2 = 456),
               OSMAC(X = x, Y = y, r1 = 100, r2 = 5, method="mvc", seed_1 = 123, seed_2 = 456))
})
test_that("OSMAC-A subsampling with some seeds is effective", {
  data <- data_binary_class
  y <- data[["y"]]
  x <- data[-which(names(data) == "y")]
  expect_equal(OSMAC(X = x, Y = y, r1 = 100, r2 = 5, method="mmse", seed_1 = 123, seed_2 = 456),
               c(5684,1620,5372,8297,8863))
})
test_that("OSMAC-L subsampling with some seeds is effective", {
  data <- data_binary_class
  y <- data[["y"]]
  x <- data[-which(names(data) == "y")]
  expect_equal(OSMAC(X = x, Y = y, r1 = 100, r2 = 5, method="mvc", seed_1 = 123, seed_2 = 456),
               c(5813,1681,5372,8313,8863))
})
