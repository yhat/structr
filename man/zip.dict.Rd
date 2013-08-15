\name{zip.dict}
\alias{zip.dict}
\title{Combine 2 lists into a .dict of key/values}
\usage{
  zip.dict(x, y)
}
\arguments{
  \item{x}{a list, vector, or .list}

  \item{y}{a second list, vector, or .list}
}
\description{
  Takes 2 lists and converts them into a key => value
  mapping, which takes the form of a \code{\link{.dict}}.
}
\examples{
x <- .list(1, 2, 3)
y <- .list("a", "b", "c")
zip.dict(x, y)
#{1: 'a', 2: 'b', 3: 'c'}
zip.dict(y, x)
#{'a': 1, 'b': 2, 'c': 3}
}
\keyword{.dict,}
\keyword{lists}
\keyword{zip,}

