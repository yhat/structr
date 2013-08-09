\name{is.dict}
\alias{is.dict}
\title{Determines whether or not an object is an instance of a
dictionary.}
\usage{
  is.dict(object)
}
\arguments{
  \item{object}{any object}
}
\description{
  Determines the class of an object and checks to see if
  it's a dictionary.
}
\examples{
x <- dict("a"=1)
is.dict(x)
#TRUE
x <- list(1, 2, 3, 4)
is.dict(x)
#FALSE
}

