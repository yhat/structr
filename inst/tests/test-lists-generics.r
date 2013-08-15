library(testthat)

context("pylist-generics")

test_that("[] getters work for single number", {
  x <- .list(1, 2, 3)
  expect_that(x[1], equals(1))
  expect_that(x[2], equals(2))
  expect_that(x[3], equals(3))
})

test_that("[] getters work for range", {
  x <- .list(1, 2, 3)
  expect_that(x[1:2]$data, equals(list(1, 2)))
  expect_that(x[2:3]$data, equals(list(2, 3)))
  expect_that(x[1:3]$data, equals(list(1, 2, 3)))
})

# test_that("seq returns an interable", {
#   x <- .list(1, 2, 3)
#   for (i in seq(x)) {
#     expect_that(i, is_a("integer"))
#   }
# })

test_that("toString returns the string representation of pylist", {
  x <- .list(1, 2, 3)
  expect_that(toString(x), equals("[1, 2, 3]"))
})

test_that("as.character returns the string representation of pylist", {
  x <- .list(1, 2, 3)
  expect_that(as.character(x), equals("[1, 2, 3]"))
})

test_that("paste returns the string representation of pylist", {
  x <- .list(1, 2, 3)
  expect_that(paste(x), equals("[1, 2, 3]"))
})

test_that("sum returns the sum of the items in a list", {
  x <- .list(1, 2, 3)
  expect_that(sum(x), is_equivalent_to(6))
})

test_that("sum fails if the items aren't numerical", {
  x <- .list(1, 2, "a")
  expect_that(sum(x), throws_error())
})

test_that("cumsum returns the cumsum of the items in a list", {
  x <- .list(1, 2, 3)
  expect_that(cumsum(x), is_equivalent_to(c(1, 3, 6)))
})

test_that("cumsum yields NA if the items aren't numerical", {
  x <- .list(1, 2, "a")
  expect_that(cumsum(x), equals(c(1, 3, NA)))
  expect_that(cumsum(x), gives_warning())
})

test_that("sin returns the sin of the items in a list", {
  x <- .list(1, 2, 3)
  expect_that(sin(x), is_equivalent_to(sin(1:3)))
})

test_that("sin fails if the items aren't numerical", {
  x <- .list(1, 2, "a")
  expect_that(sin(x), throws_error())
})

test_that("cos returns the cos of the items in a list", {
  x <- .list(1, 2, 3)
  expect_that(cos(x), is_equivalent_to(cos(1:3)))
})

test_that("cos fails if the items aren't numerical", {
  x <- .list(1, 2, "a")
  expect_that(cos(x), throws_error())
})

test_that("sign returns the sign of the items in a list", {
  x <- .list(1, 2, 3)
  expect_that(sign(x), is_equivalent_to(c(1, 1, 1)))
})

test_that("sign fails if the items aren't numerical", {
  x <- .list(1, 2, "a")
  expect_that(sign(x), throws_error())
})

test_that("lapply is invoked on the data in the list", {
  x <- .list(1, 2, 3)
  expect_that(lapply(x, function(i) { i + 3 }), equals(list(4, 5, 6)))
  expect_that(x$data, equals(list(1, 2, 3)))
})

test_that("sapply is invoked on the data in the list", {
  x <- .list(1, 2, 3)
  expect_that(sapply(x, function(i) { i + 3 }), equals(c(4, 5, 6)))
  expect_that(x$data, equals(list(1, 2, 3)))
})

test_that("length retursn the number of items in a list", {
  x <- .list(1, 2, "a")
  expect_that(length(x), equals(3))
})

test_that(".list creates an instance of a pylist", {
  x <- .list(1, 2, "a")
  expect_that(x, is_a("pylist"))
})

test_that("+ combines 2 lists", {
  x <- .list(1, 2, "a")
  y <- .list(4, 5, "b")
  z <- x + y
  expect_that(z$data, equals(list(1, 2, "a", 4, 5, "b")))  
})

test_that("is..list can identify a list", {
  x <- .list(1, 2, "a")
  y <- 1:100
  z <- list(1, 2, 3)
  expect_that(is..list(x), is_true())
  expect_that(is..list(y), is_false())
  expect_that(is..list(z), is_false())
})

test_that("+ still works as usual", {
  expect_that(1 + 2, equals(3))  
})
