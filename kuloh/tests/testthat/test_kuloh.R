library(kuloh)
library(testthat)

############################################################################################

context("Euclidian Output Test")

test_that("The code gives correct output", {
  expect_that(euclidian(123612, 13892347912), equals(4))
  expect_that(euclidian(2,8), equals(2))
  expect_that(euclidian(3,5), equals(1))
  expect_that(euclidian(123612, 13892347912), equals(4))
  })

############################################################################################

context("Euclidian Error Test")

test_that("The code throws an error", {
  expect_that(euclidian(c(1000,"a"),c("1000",1000)), throws_error("Insert a numeric value of lenght 1"))
  expect_that(euclidian(TRUE,c("1000",1000)), throws_error("Insert a numeric value of lenght 1"))
  expect_that(euclidian(5>2,FALSE), throws_error("Insert a numeric value of lenght 1"))
  expect_that(euclidian(2,1:10), throws_error("Insert a numeric value of lenght 1"))
  expect_that(euclidian(1,"hello"), throws_error("Insert a numeric value of lenght 1"))
})

############################################################################################

context("Dijkstra Output Test")
data(wiki_graph)
test_that("The code gives correct output", {
  expect_that(dijkstra(wiki_graph, 1), equals(c(0, 7, 9, 20, 20, 11)))
  expect_that(dijkstra(wiki_graph, 3), equals(c(9, 10, 0, 11, 11, 2)))
})

############################################################################################

context("Dijkstra Input Test")
data("wiki_graph")
test_that("Correct Input", {
  expect_that(dijkstra(wiki_graph, "a"), throws_error("Insert a correct dataframe or a numeric value as initial node"))
  expect_that(dijkstra(data.frame("a" = c(1:4),
                                  "b" = c(2)), "a"), throws_error("Insert a correct dataframe or a numeric value as initial node"))
})
