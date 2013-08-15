library(testthat)
library(digest)

context("dicts")

test_that("string method works for character keys", {
  x <- .dict("a"=1 ,"b"=2 ,"c"=3)
  expect_that(x$string(), equals("{'a': 1, 'b': 2, 'c': 3}"))  
})

test_that("string method works for numbers", {
  x <- .dict()
  x[100] = 100
  expect_that(x$string(), equals("{100: 100}"))  
})

test_that("string method works for numbers & characters", {
  x <- .dict()
  x[100] = 100
  x['a'] = 200
  expect_that(x$string(), equals("{100: 100, 'a': 200}"))  
})

test_that("count returns the right number of items", {
  x <- .dict()
  x['a'] = 1
  x['b'] = 2
  x['c'] = 3
  expect_that(x$count(), equals(3))
})

test_that("count returns the right number of items after removing a key/value", {
  x <- .dict()
  x['a'] = 1
  x['b'] = 2
  x['c'] = 3
  x$popitem()
  expect_that(x$count(), equals(2))
})

test_that("keys return a list of the keys", {
  x <- .dict()
  x['a'] = 1
  x['b'] = 2
  x['c'] = 3
  expect_that(x$keys(), equals(.list('a', 'b', 'c')))
})

test_that("keys returns an empty list when dict is blank", {
  x <- .dict()
  expect_that(x$keys(), equals(.list()))
})

# test_that("iterkeys returns a sequence of keys", {
#   x <- .dict()
#   x[1] = 1
#   x[2] = 2
#   x[3] = 3
#   expect_that(x$iterkeys(), is_a("integer"))
# })

test_that("digested_keys returns a vector of hashes", {
  x <- .dict()
  x['a'] = 1
  x['b'] = 2
  x['c'] = 3
  expect_that(x$digested_keys(), is_a("character"))
})

test_that("values returns a pylist of the values", {
  x <- .dict()
  x['a'] = 1
  x['b'] = 2
  x['c'] = 3
  expect_that(x$values(), equals(.list(1, 2, 3)))
})

test_that("values the right values", {
  x <- .dict()
  x['a'] = 1
  x['b'] = 2
  x['c'] = 3
  expect_that(x$values(), equals(.list(1, 2, 3)))
  x$pop('b')
  expect_that(x$values(), equals(.list(1, 3)))
})

# test_that("itervalues returns a sequence of keys", {
#   x <- .dict()
#   x[1] = 1
#   x[2] = 2
#   x[3] = 3
#   expect_that(x$iterkeys(), is_a("integer"))
# })

test_that("get returns a value if the key is present", {
  x <- .dict()
  x['a'] = 1
  x['b'] = 2
  x['c'] = 3
  expect_that(x$get('c'), equals(3))
})

test_that("get returns a defaultvalue if the key is not present", {
  x <- .dict()
  x['a'] = 1
  x['b'] = 2
  x['c'] = 3
  expect_that(x$get('d', 100), equals(100))
})

test_that("get returns a previously specified defaultvalue if the key is not present", {
  x <- .dict()
  x$defaultvalue <- 200
  x['a'] = 1
  x['b'] = 2
  x['c'] = 3
  expect_that(x$get('d'), equals(200))
})

test_that("pop removes & returns a value if the key exists", {
  x <- .dict()
  x['a'] = 1
  x['b'] = 2
  x['c'] = 3
  expect_that(x$pop('c'), equals(3))
  x$defaultvalue <- -1
  expect_that(x$get('c'), equals(-1))
})

test_that("pop returns a defaultvalue if the key doesn't exist", {
  x <- .dict()
  x$defaultvalue <- -1
  x['a'] = 1
  x['b'] = 2
  x['c'] = 3
  expect_that(x$pop('d'), equals(-1))
})

test_that("popitem removes & returns a value if the key exists", {
  x <- .dict()
  x['a'] = 1
  x['b'] = 2
  x['c'] = 3
  expect_that(x$popitem(), equals(.list("a", 1)))
  x$defaultvalue <- -1
  expect_that(x$get('a'), equals(-1))
})

test_that("popitem returns a defaultvalue if the key doesn't exist", {
  x <- .dict()
  x$defaultvalue <- -1
  expect_that(x$popitem(), equals(NA))
})

test_that("setdefault works", {
  x <- .dict()
  x$setdefault(100)
  expect_that(x$defaultvalue, equals(100))
})

test_that("has_key confirms regular key is in dict", {
  x <- .dict()
  x['a'] = 100
  expect_that(x$has_key('a'), is_true())
})

test_that("has_key confirms complex key is in dict", {
  x <- .dict()
  x[iris] = 100
  expect_that(x$has_key(iris), is_true())
})

test_that("update combines 2 dicts but keeps keys in 1st dict", {
  x <- .dict("a"=1, "b"=2)
  y <- .dict("c"=3, "b"=1)
  x$update(y)
  expect_that(x['a'], equals(1))
  expect_that(x['b'], equals(2))
  expect_that(x['c'], equals(3))
})

test_that("clear removers all data", {
  x <- .dict("a"=1, "b"=2)
  x$clear()
  expect_that(x$data, equals(list()))
  expect_that(x$keymap, equals(list()))
})

test_that("items returns a list of zipped keys/values", {
  x <- .dict("a"=1, "b"=2)
  expect_that(x$items(), equals(.list(.list("a", 1), .list("b", 2))))
})

# test_that("iteritems returns an iterable zipped dict", {
#   x <- .dict("a"=1, "b"=2)
#   expect_that(x$items(), equals(.list(.list("a", 1), .list("b", 2))))
# })

test_that("add_key adds the key to the keymap and digests the key", {
  x <- .dict()
  x['a'] = 1
  expect_that(digest('a') %in% names(x$data), is_true())
  expect_that(digest('a') %in% names(x$keymap), is_true())
  expect_that('a' %in% x$keymap, is_true())
})

test_that("get_key returns a previously specified defaultvalue if the key is not present", {
  x <- .dict()
  x['a'] = 1
  x['b'] = 2
  x['c'] = 3
  expect_that(x$get_key('a'), equals(1))
})
