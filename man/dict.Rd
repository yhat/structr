\name{dict}
\alias{dict}
\title{Creates an instance of a dict}
\usage{
  dict(...)
}
\arguments{
  \item{...}{a series of key/value pairs in the form
  \code{key=value}}
}
\description{
  This is a wrapper function around the \code{pydict$new}
  that is a little more R friendly.
}
\examples{
(x <- dict("a"=1, "b"=2, "c"=3))
#{a: 1, b: 2, c: 3}
}
\keyword{dict,}
\keyword{key/value}

