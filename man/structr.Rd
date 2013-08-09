\name{structr}
\alias{structr}
\alias{structr}
\docType{package}
\title{
Python/Ruby/PHP like data structures for R
}
\description{
structr does blah blah blah.
}
\details{
\tabular{ll}{
Package: \tab structr\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2013-08-10\cr
License: \tab FreeBSD\cr
}
See \url{http://www.yhathq.com/} for more detials.
}
\author{
Greg Lamp <greg@yhathq.com>
}
\references{
  \url{http://www.yhathq.com/}
}
\keyword{ package }
\seealso{
  \url{http://blog.yhathq.com/posts/structr}
}
\examples{
x <- dict.py()
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



