test_that("uniform subsampling with the same seed is identical", {
  expect_equal(Unif(1000, 10, 123), Unif(1000, 10, 123))
})
test_that("uniform subsampling with some seed is effective", {
  expect_equal(Unif(1000, 10, 1), c(836,679,129,930,509,471,299,270,978,187))
  expect_equal(Unif(1000, 10, 123), c(415,463,179,526,195,938,818,118,299,229))
})
test_that("uniform subsampling with replace or without replace is different", {
  expect_equal(Unif(10, 10, 123, TRUE), c(3,3,10,2,6,5,4,6,9,10))
  expect_equal(Unif(10, 10, 123, FALSE), c(3,10,2,8,6,9,1,7,5,4))
})
