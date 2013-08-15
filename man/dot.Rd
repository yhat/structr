\name{dot}
\alias{dot}
\alias{dot}
\docType{package}
\title{
Sane data structures for R
}
\description{
	\code{dot} provides you with data structures that are similar to 
	data structures commonly found in Python, Ruby, PHP, and other
	scripting languages. While nearly all of the functionality provided
	by \code{dot} is possible in base R, \code{dot} provides a similar look
	and feel to other languages that many developers are more comfortable
	using (\code{\link{.list}}, \code{\link{.dict}}). structur also provides a set of functions
	to interact with these data structures. Most of the methods and functions
	come directly from the Python API. For more informaiton on using structur see
	below, or visit the github page.
}
\details{
	Lists

	R has it's own list structure, called \code{\link{list}}. It's also
	functions as a sort of .dictionary, but it's not exactly clear (especially
	for first time R users) when and where to use it. \code{\link{.list}} provides
	a more familiar API for those of you coming from Python, Ruby, Javascript, etc. It has
	an OOP feel and allows methods to be accessed via the \code{$} operator 
	(similiar to . in other OOP langauges).

	.dicts

	\code{\link{.dict}} is similar to a Python .dictionary, a Java HashMap, or a PHP array. It's
	an abstraction on top of a regular R \code{\link{list}} that functions as a basic key, value store.

	Other functions
	\tabular{ll}{
		\code{\link{zip.tuple}} \tab similar to zip() in Python\cr
		\code{\link{zip.dict}} \tab similar to .dict(zip()) in Python\cr
		\code{\link{is..dict}} \tab check if a data structure is a .dict\cr
		\code{\link{is..list}} \tab check if a data structure is a list\cr
	}

See \url{http://www.yhathq.com/} for more detials.
}
\author{
Greg Lamp <greg@yhathq.com>
}
\references{
  \url{http://www.yhathq.com/}
  \tabular{ll}{
	Package: \tab \code{dot}\cr
	Type: \tab Package\cr
	Version: \tab 0.1\cr
	Date: \tab 2013-08-10\cr
	License: \tab FreeBSD\cr
	}
}
\keyword{ package }
\seealso{
  \url{http://blog.yhathq.com/posts/dot}
}
\examples{
x <- .dict()
x["one"] = 1
x["two"] = 2
x["three"] = 3
print(x)
x[iris] = iris
x["iris"] = iris
x["iris"]
x[iris]

y <- .list(1, 2, 3, 4)
y$append(100)
y$pop()
y <- .list()
y$append(.list(1, 2))
y$append(.list(3, 4))
y$append(.list(5, 6))
print(y)

y <- .list(1, 2, 3)
x <- .list(3, 4, 6)
zip.dict(x, y)
zip.tuple(x, y)

d <- .list(1, 2, 3)

# .list(1, 2, 3) + .list(4, 5, 6)

# 2 %in% .list(1, 2, 3)


test <- .list(100, 200)
test$append(300)
test$append("hello")
test$pop()
test$push("hello")
test$reverse()
test$count()
test$insert("hello", 100)
test$insert("goodbye", 2)
test$insert(TRUE, 2)
test$index("goodbye")
test$sort()
test
test <- .list(100, 200, 300, 400)
test$map(function(x) {
  x * 1.5
})
test

test <- .list("greg", "sam", "stan", "paul", "sammy")
test$find("sam")
test$find("^s")
test$find("^sa")
test <- .list(1, 2, 3, 4, 5)
test$find(function(x) { x > 2.5 })

test[1]
test <- .list(1, 2, 3, 4)
sum(test)

nested <- .list(
  .list(100, 200, 300),
  .list(400, 500)
)
nested
nested[1][2]
nested[2][1:2]
nested[1][2]
nested[1]
nested


x <- .list(1, 2, 4, 5)
x[1:2]
nested[1][1:2]

summary(.list(100, "austin", 200, 400, "austin", "greg"))
summary(nested)


for (i in seq(x)) {
  print(i)
}

for (i in x$items()) {
  print(i)
}


summary(d)
summary(nested)

d <- zip.tuple(1:10, 1:100 * 10)
d

d <- zip.tuple(.list(1, 2, 3), .list("a", "b", "c"))
d

d <- zip.tuple(1:100, 1:100 * 10)
d


d <- zip.dict(1:10, 1:100 * 10)
d

d <- zip.dict(letters, 1:100 * 10)
d


for (item in seq(zip.tuple(1:10, 1:10))) {
  print(item)
}

d <- zip.dict(1:10, 1:100 * 10)
d

d <- .dict(a=1, b=2)
d['a'] <- 1
d['c'] <- 3
d
d$keys()
sum(d$values())
d$get("a")
d$get("fake", 10)
d$pop("a")
#d$popitem()
d
d$setdefault(100)
d$get("fakekey")

a <- .dict(a=1, b=2)
b <- .dict(c=1, b=0)
a$update(b)
a
b$clear()
b

a$iteritems()

test <- .dict()
for (l in letters) {
  for (i in 1:10) {
    key <- paste(l, i, sep="")
    test[key] = i
  }
}
test['a2']
#"x8" %in% test
#"fakething" %in% test

x <- .dict((a=1), b=21)
for(key in seq(x)) {
  print(key)
}

for (item in x$iteritems()) {
  print(item)
}

d <- .dict()
d[iris] = "hello"
d
d[1] = 100
}

