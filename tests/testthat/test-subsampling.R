test_that("Unif subsampling", {
  data <- data_binary_class
  expect_equal(subsampling(y_name = "y", data = data, n = 10, method = "Unif", seed = 123),
               c(2463,2511,8718,2986,1842,9334,3371,4761,6746,9819))
})

test_that("OSMAC-A subsampling", {
  data <- data_binary_class
  expect_equal(subsampling(y_name = "y", data = data, n = 10, pilot_n = 100,
                           method = "OSMAC_A", seed_1 = 123, seed_2 = 456),
               c(5684,1620,5372,8297,8863,9783,6483,6103,2702,5735))

})

test_that("OSMAC-L subsampling", {
  data <- data_binary_class
  expect_equal(subsampling(y_name = "y", data = data, n = 10, pilot_n = 100,
                           method = "OSMAC_L", seed_1 = 123, seed_2 = 456),
               c(5813,1681,5372,8313,8863,9780,1630,6103,2702,5888))

})

test_that("IBOSS subsampling", {
  data_numeric <- data_numeric_regression
  expect_equal(subsampling(y_name = "y", data = data_numeric, n = 100, method = "IBOSS", seed = 123),
               subsampling(y_name = "y", data = data_numeric, n = 100, method = "IBOSS", seed = 123))

})

test_that("Leverage subsampling", {
  data_numeric <- data_numeric_regression
  expect_equal(subsampling(y_name = "y", data = data_numeric, n = 100, method = "Leverage", replace = TRUE, seed = 123,
                           shrinkage = 0.9),
               subsampling(y_name = "y", data = data_numeric, n = 100, method = "Leverage", replace = TRUE, seed = 123,
                           shrinkage = 0.9))
})

test_that("OSS subsampling", {
  data_numeric <- data_numeric_regression
  expect_equal(subsampling(y_name = "y", data = data_numeric, n = 100, method = "OSS", seed = 123),
               subsampling(y_name = "y", data = data_numeric, n = 100, method = "OSS", seed = 123))
})

test_that("LowCon subsampling", {
  data_numeric <- data_numeric_regression
  expect_equal(subsampling(y_name = "y", data = data_numeric, n = 100, method = "LowCon", seed = 123, theta = 1),
               subsampling(y_name = "y", data = data_numeric, n = 100, method = "LowCon", seed = 123, theta = 1))
})

test_that("IES subsampling", {
  data_numeric <- data_numeric_regression
  expect_equal(subsampling(y_name = "y", data = data_numeric, n = 100, method = "IES", seed = 123, q = 16),
             subsampling(y_name = "y", data = data_numeric, n = 100, method = "IES", seed = 123, q = 16))
})

test_that("DDS subsampling", {
  data_numeric <- data_numeric_regression
  expect_equal(subsampling(y_name = "y", data = data_numeric, n = 100, method = "DDS", ratio = 0.85),
               subsampling(y_name = "y", data = data_numeric, n = 100, method = "DDS", ratio = 0.85))
})


