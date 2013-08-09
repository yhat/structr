library(testthat)

context("syntactic sugar")

test_that("%in% works for lists", {
	x <- list.py(1, 2, 3)
	expect_that(2 %in% x, is_true())
	expect_that(4 %in% x, is_false())	
})

test_that("%in% works for dicts", {
	x <- dict("a"=1, "b"=2, "c"=3)
	expect_that("a" %in% x, is_true())
	expect_that("d" %in% x, is_false())	
})

test_that("%in% works for other stuff", {
	x <- c(1, 2, 3, 4, 5)
	expect_that(1 %in% x, is_true())
	expect_that(-1 %in% x, is_false())	
})

test_that("zip.dict happy path", {
	x <- list.py("a", "b", "c")
	y <- list.py(1, 2, 3)
	test_that(zip.dict(x, y), equals(dict("a"=1, "b"=2, "c"=3)))
})

test_that("zip.dict default to shortest list", {
	x <- list.py("a", "b")
	y <- list.py(1, 2, 3)
	test_that(zip.dict(x, y), equals(dict("a"=1, "b"=2)))
})

test_that("zip.tuple happy path", {
	x <- list.py("a", "b", "c")
	y <- list.py(1, 2, 3)
	test_that(zip.dict(x, y), equals(list.py(list.py("a", 1), list.py("b", 2), list.py("c", 3))))
})

test_that("zip.tuple defaults to shortest list", {
	x <- list.py("a", "b")
	y <- list.py(1, 2, 3)
	test_that(zip.dict(x, y), equals(list.py(list.py("a", 1), list.py("b", 2))))
})