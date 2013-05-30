\name{list.py}
\alias{list.py}
\title{Creates an instance of a list}
\usage{
  list.py(...)
}
\arguments{
  \item{...}{a series of values seperated by a comma. NOTE:
  a vector will be treated as an individual item. i.e.
  \code{list.py(1:100)} will yield a list with 1 item,
  whereas \code{list.py(1, 2, 3, 4)} will yield a list with
  4 items}
}
\description{
  This is a wrapper function around the \code{pylist$new}
  that is a little more R friendly.
}
\examples{
x <- list.py(1, 2, 3, 4)
#[1, 2, 3, 4]
}
\keyword{list,}
\keyword{list.py}

