\name{lastn}
\alias{lastn}
\title{
  Extract the last n elements of a vector
}
\description{
  Extract the last n elements of a vector
}
\usage{
lastn(x, n)
}
\arguments{
  \item{x}{a vector}
  \item{n}{number of elements to keep, an integer}
}
\details{
  I did not know about the function \code{tail()} when I did this one,
  but the two functions are not completely equivalent (and \code{tail}
  is generic).

  If \code{n} is equal to \code{length(x)}, \code{x} is returned as is.
  If \code{n} is equal to zero or is negative, a length zero vector is
  returned. 

  It is an error for \code{n} to be larger than \code{length(x)}.
  
}
\value{
  a vector containing the last \code{n} elements of \code{x},
  see Details 
}
\seealso{
  See Also as \code{\link{tail}}
}
\examples{
lastn(1:10, 3) # 8:10
lastn(letters, 5) # "v" "w" "x" "y" "z"
}
\keyword{internal}
