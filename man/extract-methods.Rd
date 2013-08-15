\docType{methods}
\name{[}
\alias{[}
\alias{[,pydict-method}
\alias{[,pylist-method}
\alias{[<-,pydict-method}
\alias{[<-,pylist-method}
\alias{as.character}
\alias{as.character,pylist-method}
\alias{cos}
\alias{cos,pylist-method}
\alias{cumsum}
\alias{cumsum,pylist-method}
\alias{length}
\alias{length,pydict-method}
\alias{length,pylist-method}
\alias{sign}
\alias{sign,pylist-method}
\alias{sin}
\alias{sin,pylist-method}
\alias{sum}
\alias{sum,pylist-method}
\title{Get the value of a key associated with a .dictionary.}
\arguments{
  \item{x}{a list}

  \item{...}{named args}
}
\description{
  You can use the a.dict['key'] syntax to access key/values
  from within a .dictionary--much like Python, Ruby, or
  Perl.

  You can use the a.dict['key'] syntax to set key/values
  from within a .dictionary--much like Python, Ruby, or
  Perl.

  Use much like length(list()) or length(c(1, 2, 3)).

  You can use the adict[idx] syntax to access items from
  within a list--much like Python, Ruby, or Perl.

  You can use the alist[idx] syntax to set items within a
  list--much like Python, Ruby, or Perl.

  Generic function that calls the string method for a list.

  Generic function for caclulating the sum of the items in
  a list. If an item is not numeric an error occurs.

  Generic function for caclulating the cumsum of the items
  in a list. If an item is not numeric an error occurs.

  Generic function for caclulating the sin of the items in
  a list. If an item is not numeric an error occurs.

  Generic function for caclulating the cos of the items in
  a list. If an item is not numeric an error occurs.

  Generic function for caclulating the sign of the items in
  a list. If an item is not numeric an error occurs.

  Use much like length(list(1, 2, 3)) or length(c(1, 2,
  3)).
}
\examples{
as.character(.list(1, 2, 3, 4))
}

