\name{zip.tuple}
\alias{zip.tuple}
\title{Combine 2 lists into a list of lists}
\usage{
  zip.tuple(x, y)
}
\arguments{
  \item{x}{a list, vector, or .list}

  \item{y}{a second list, vector, or .list}
}
\description{
  Return a list of 2 item lists, where each list contains
  the i-th element from each of the argument sequences.
  The returned list is truncated in length to the length of
  the shortest argument sequence.
}
\examples{
x <- .list(1, 2, 3)
y <- .list(4, 5, 6)
zip.tuple(x, y)
#[[1, 4], [2, 5], [3, 6]]
y <- .list("a", "b", "c")
zip.tuple(x, y)
#[[1, 'a'], [2, 'b'], [3, 'c']]
}
\keyword{lists,}
\keyword{zip}

