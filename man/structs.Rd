\name{structs}
\alias{structs}
\alias{structs}
\docType{package}
\title{
Sane data structures for R
}
\description{
	\code{structs} provides you with data structures that are similar to 
	data structures commonly found in Python, Ruby, PHP, and other
	scripting languages. While nearly all of the functionality provided
	by \code{structs} is possible in base R, \code{structs} provides a similar look
	and feel to other languages that many developers are more comfortable
	using (\code{\link{list.py}}, \code{\link{dict}}). structur also provides a set of functions
	to interact with these data structures. Most of the methods and functions
	come directly from the Python API. For more informaiton on using structur see
	below, or visit the github page.
}
\details{
	Lists

	R has it's own list structure, called \code{\link{list}}. It's also
	functions as a sort of dictionary, but it's not exactly clear (especially
	for first time R users) when and where to use it. \code{\link{list.py}} provides
	a more familiar API for those of you coming from Python, Ruby, Javascript, etc. It has
	an OOP feel and allows methods to be accessed via the \code{$} operator 
	(similiar to . in other OOP langauges).

	Dicts

	\code{\link{dict}} is similar to a Python dictionary, a Java HashMap, or a PHP array. It's
	an abstraction on top of a regular R \code{\link{list}} that functions as a basic key, value store.

	Other functions
	\tabular{ll}{
		\code{\link{zip.tuple}} \tab similar to zip() in Python\cr
		\code{\link{zip.dict}} \tab similar to dict(zip()) in Python\cr
		\code{\link{is.dict}} \tab check if a data structure is a dict\cr
		\code{\link{is.list.py}} \tab check if a data structure is a list\cr
	}

See \url{http://www.yhathq.com/} for more detials.
}
\author{
Greg Lamp <greg@yhathq.com>
}
\references{
  \url{http://www.yhathq.com/}
  \tabular{ll}{
	Package: \tab \code{structs}\cr
	Type: \tab Package\cr
	Version: \tab 0.1\cr
	Date: \tab 2013-08-10\cr
	License: \tab FreeBSD\cr
	}
}
\keyword{ package }
\seealso{
  \url{http://blog.yhathq.com/posts/structs}
}
\examples{
x <- dict()
x["one"] = 1
x["two"] = 2
x["three"] = 3
print(x)
x[iris] = iris
x["iris"] = iris
x["iris"]
x[iris]

y <- list.py(1, 2, 3, 4)
y$append(100)
y$pop()
y <- list.py()
y$append(list.py(1, 2))
y$append(list.py(3, 4))
y$append(list.py(5, 6))
print(y)

y <- list.py(1, 2, 3)
x <- list.py(3, 4, 6)
zip.dict(x, y)
zip.tuple(x, y)
}

