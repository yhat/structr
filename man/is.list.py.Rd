\name{is.list.py}
\alias{is.list.py}
\title{Determines whether or not an object is an instance of a
list}
\usage{
  is.list.py(object)
}
\arguments{
  \item{object}{any object}
}
\description{
  Determines the class of an object and checks to see if
  it's a list
}
\examples{
x <- list.py("a")
is.list.py(x)
#TRUE
x <- 1:10
is.dict(x)
#FALSE
}

