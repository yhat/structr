library(testthat)

context("utilities")

test_that("can combine 2 lists", {
  x <- list(1, 2, 3)
  y <- list(4, 5, 6)
  expect_that(merge.list(x, y), equals(list(1, 2, 3, 4, 5, 6)))
})

test_that("can't combine null w/ list", {
  x <- list(1, 2, 3)
  y <- NULL
  expect_that(merge.list(x, y), equals(list(1, 2, 3)))
})

test_that("can combine 2 lists w/ the same items", {
  x <- list(1, 2, 3)
  y <- list(1, 2, 3)
  expect_that(merge.list(x, y), equals(list(1, 2, 3, 1, 2, 3)))
})

test_that("encapsulate adds '' to characters", {
  x <- list("hello", "goodbye")
  expect_that(encapsulate(x), equals(list("'hello'", "'goodbye'")))
})

test_that("encapsulate doesn't alter numerics", {
  x <- list(1, 2, 3)
  expect_that(encapsulate(x), equals(list(1 ,2, 3)))
})

test_that("encapsulate doesn't alter numerics in multitype lists", {
  x <- list(1, 2, "a")
  expect_that(encapsulate(x), equals(list(1, 2, "'a'")))
})