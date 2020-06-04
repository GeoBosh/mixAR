\name{ragged}
\alias{rag_modify}
\alias{ragged2vec}
%\alias{ragged2char}
\title{
  Small utilities for ragged objects
}
\description{
  Small utilities for ragged objects.
  Modify the elements of raggedCoef objects, extract them as a vector.
}
\usage{
rag_modify(rag, v)
ragged2vec(x)
}
\arguments{
  \item{rag}{the \code{raggedCoef} object to be modified.}
  \item{v}{vector of values to replace the old ones.}
  \item{x}{a \code{raggedCoef} object.}
}
\details{
  An error will occur if the length of \code{v} is not equal to
  \code{sum(rag@p)}.

  \code{rag_modify} is, in a sense, the inverse of \code{ragged2vec}.
}
\value{
  for \code{rag_modify}, a \code{raggedCoef} object of the same order as
  \code{rag} but with coefficients replaced by the new values.

  for \code{ragged2vec}, a numeric vector.

}
\examples{
rag1 <- new("raggedCoef", list(1, 2:3, 4:6))
a1 <- (1:6)^2
rag1a <- rag_modify(rag1, a1)

rag2 <- new("raggedCoef", list(1, numeric(0), 4:6)) # a zero-length ccomponent
a2 <- (1:4)^2
rag2a <- rag_modify(rag2, a2)

}
\keyword{ragged}
