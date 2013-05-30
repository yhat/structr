\name{dict.py}
\alias{dict.py}
\title{Creates an instance of a dict}
\usage{
  dict.py(...)
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
(x <- dict.py("a"=1, "b"=2, "c"=3))
#{a: 1, b: 2, c: 3}
}
\keyword{dict,}
\keyword{dict.py,}
\keyword{key/value}

