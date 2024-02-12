test_that("Unif subsampling", {
  data <- data_binary_class
  expect_equal(subsampling(y_name = "y", data = data, n = 10, method = "Unif", seed_1 = 123),
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

