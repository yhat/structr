library(testthat)

context("pylist")

test_that("string method works for numbers", {
  x <- list.py(1 ,2 ,3)
  expect_that(x$string(), equals("[1, 2, 3]"))  
})

test_that("string method works for characters", {
  x <- list.py("a", "b", "c")
  expect_that(x$string(), equals("['a', 'b', 'c']"))  
})

test_that("string method works for numbers and characters", {
  x <- list.py("a", "b", 1)
  expect_that(x$string(), equals("['a', 'b', 1]"))  
})

test_that("append adds an item to the last position", {
  x <- list.py("a", "b", "c")
  x$append("d")
  expect_that(x[4], equals("d"))  
})

test_that("append adds an item to the last position", {
  x <- list.py("a", "b", "c")
  x$append("d")
  expect_that(x[4], equals("d"))  
})

test_that("push adds an item to the first position", {
  x <- list.py("a", "b", "c")
  x$push("d")
  expect_that(x[1], equals("d"))  
})

test_that("pop gets the last item from the list and removes it", {
  x <- list.py("a", "b", "c")
  y <- x$pop()
  expect_that(y, equals("c"))
  expect_that(length(x), equals(2))  
})

test_that("pop does not return a nested list", {
  x <- list.py("a", "b", "c")
  y <- x$pop()
  expect_that(y, is_a("character"))
})

test_that("reverse returns a reversed list but does not modify the list in place", {
  x <- list.py("a", "b", "c")
  expect_that(x$reverse()$data, equals(list("c", "b", "a")))
  expect_that(x$data, equals(list("a", "b", "c")))
})

test_that("count returns the correct length", {
  x <- list.py("a", "b", "c")
  expect_that(x$count(), equals(3))
})

test_that("insert returns FALSE if the position is not possible", {
  x <- list.py("a", "b", "c")
  outcome <- x$insert("d", 100)
  expect_that(outcome, is_false())
  outcome <- x$insert("d", 0)
  expect_that(outcome, is_false())
})

test_that("insert, inserts items in the correct position", {
  x <- list.py("a", "b", "c")
  outcome <- x$insert("d", 2)
  expect_that(x[3], equals("d"))
})

test_that("insert returns TRUE when successful", {
  x <- list.py("a", "b", "c")
  outcome <- x$insert("d", 2)
  expect_that(outcome, is_true())
})

# test_that("index returns NA when given an item not in the list", {
#   x <- list.py("a", "b", "c")
#   expect_that(x$index("blah"), is_identical_to(c(NA)))
# })

test_that("index returns the index of an item", {
  x <- list.py("a", "b", "c")
  expect_that(x$index("b"), equals(2))
})

test_that("sort sorts a list in place", {
  x <- list.py("c", "b", "a")
  x$sort()
  expect_that(x$data, equals(list("a", "b", "c")))
})

test_that("map modifies the list in place", {
  x <- list.py("a", "b", "c")
  x$map(function(i) { paste(i, "--test") })
  expect_that(x$data, equals(list("a --test", "b --test", "c --test")))  
})

test_that("find takes a regular expression and returns vaild items", {
  x <- list.py("apple", "apples", "berries")
  expect_that(x$find("^apple")$data, equals(list("apple", "apples")))  
})

test_that("find takes a function and returns vaild items", {
  x <- list.py(1, 2, 3, 4)
  expect_that(x$find(function(i) { i > 2} )$data, equals(list(3, 4)))  
})

test_that("find returns an instance of pylist", {
  x <- list.py(1, 2, 3, 4)
  expect_that(x$find(function(i) { i > 2} ), is_a("pylist"))  
})

test_that("contains returns a boolean", {
  x <- list.py(1, 2, 3, 4)
  expect_that(x$contains(1), is_a("logical"))
  expect_that(x$contains("horse"), is_a("logical"))  
})

test_that("contains can find items in the list", {
  x <- list.py(1, 2, 3, 4)
  expect_that(x$contains(1), is_true())
})

test_that("items returns a native list of the data", {
  x <- list.py(1, 2, 3, 4)
  expect_that(x$items(), is_a("list"))
})

test_that("items returns the same items as those in the pylist", {
  x <- list.py(1, 2, 3, 4)
  expect_that(x$items(), equals(list(1, 2, 3, 4)))
})

test_that("iteritems returns an iterable list of the data", {
  x <- list.py(1, 2, 3, 4)
  expect_that(x$iteritems(), is_a("integer"))
})

test_that("iteritems returns the same items as those in the pylist", {
  x <- list.py(1, 2, 3, 4)
  expect_that(x$iteritems(), equals(c(1, 2, 3, 4)))
})


