\name{is..list}
\alias{is..list}
\title{Determines whether or not an object is an instance of a
list}
\usage{
  is..list(object)
}
\arguments{
  \item{object}{any object}
}
\description{
  Determines the class of an object and checks to see if
  it's a list
}
\examples{
x <- .list("a")
is..list(x)
#TRUE
x <- 1:10
is..dict(x)
#FALSE
}

