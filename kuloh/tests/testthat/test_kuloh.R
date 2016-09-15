library(kuloh)
library(testthat)
context("euclidian")

test_that("The code gives correct output", {
  expect_that(euclidian(123612, 13892347912), equals(4))
  expect_that(euclidian(2,8), equals(2))
  expect_that(euclidian(3,5), equals(1))
  expect_that(euclidian(123612, 13892347912), equals(4))
})
