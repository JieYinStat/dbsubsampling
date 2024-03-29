test_that("IBOSS works", {
  data <- data_numeric_regression
  X <- data[-which(names(data) == "y")]
  expect_equal(IBOSS(n = 100, X = X),
               c(183,226,395,419,584,666,711,758,1027,1144,1324,1445,1940,1946,1978,2018,2673,2982,3190,3395,3484,
                 3612,3632,3638,3696,3816,3835,3896,3921,4256,4312,4405,4523,4551,4729,4938,5121,5226,5342,5410,5679,5770,
                 5995,6089,6163,6170,6203,6250,6525,6964,6979,7053,7198,7407,7564,7633,7915,7935,7967,7992,8026,8088,8106,
                 8156,8161,8267,8306,8501,8503,8521,8534,8694,8805,8841,9117,9211,9302,9364,9398,9456,9676,9946,9971,9989,
                 1173,2344,5394,8438,8567,9239,1787,2104,2215,3121,7159,9133))
  expect_equal(sort(IBOSS(100,X)), sort(myRcpp_IBOSS(100,X)))
  expect_equal(sort(IBOSS(100,X)), sort(myR_IBOSS(100,X)))
  expect_equal(sort(IBOSS(100,X)), sort(myArma_IBOSS(100,X)))
  expect_equal(sort(IBOSS(100,X)), sort(myRcpp_cstyle_IBOSS(100,X)))
  expect_error(myR_IBOSS(10,X))
  expect_error(myArma_IBOSS(10,X))
  expect_error(myRcpp_cstyle_IBOSS(10,X))
})

