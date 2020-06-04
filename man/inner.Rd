\name{inner}
\docType{methods}
\alias{inner}
\alias{inner-methods}
\alias{inner,ANY,ANY,ANY,ANY-method}
\alias{inner,mixComp,missing,missing,missing-method}
\alias{inner,mixComp,numeric,missing,missing-method}
\alias{inner,numeric,mixComp,missing,missing-method}
\alias{inner,mixComp,numeric,ANY,ANY-method}
\alias{inner,mixComp,numeric,ANY,missing-method}
\title{
  Generalised inner product and methods for class mixComp
}
\description{
  Generalised inner product and methods for class mixComp. The methods
  for mixComp provide for very convenient computing with mixAR models.
}
\usage{
inner(x, y, star = "*", plus = .mplus)
}
\arguments{
  \item{x}{the first argument.}
  \item{y}{the second argument.}
  \item{star}{
    function to apply to pairs of elements from \code{x} and \code{y},
    default is multiplication, as for the usual inner product.
  }
  \item{plus}{
    function to apply to combine the results from the pairs,
    default  is addition, as for the usual inner product.
  }
}
\details{

  \code{inner} computes a generalised inner product \code{x . y}, where
  multiplication and summation can be replaced by other functions.

  The default method of \code{inner} applies \code{star} to the
  corresponding pairs of elements and combines them with \code{plus}.
  There is no recycling, if \code{x} and \code{y} have different
  lengths, an error is raised.  The elements of \code{x} and \code{y}
  are accessed with "[[". \code{plus} should be \code{n}-ar operation.
}
\value{
  the inner product, the type of the result depends on the arguments
}

\section{Methods}{
  % This is similar to a product between a matrix and a vector.
  % % commented out on 2011-07-12: does not correspond to the R code.
  % %
  % % but the definition here is commutative, i.e. it does not matter in
  % % what order the two objects, the \code{mixComp} and the vector, are
  % % given.

  Methods for inner product between a \code{"mixComp"} object and a
  vector are similar to a product between a matrix and a vector but
  comply with the conventions of class \code{"mixComp"}. For this reason
  they are described in the help page for class
  \code{"\linkS4class{mixComp}"}, along with methods for other functions
  and operators applied to \code{"mixComp"} objects.
  
  \describe{

    \item{\code{signature(x = "ANY", y = "ANY", star = "ANY", plus = "ANY")}}{
      This is the default method, see section Details.
    }

    \item{\code{signature(x = "mixComp", y = "missing", star = "missing", plus = "missing")}}{
      see \code{\linkS4class{mixComp}}.
    }

    \item{\code{signature(x = "mixComp", y = "numeric", star = "missing", plus = "missing")}}{
      see \code{\linkS4class{mixComp}}.
    }

    \item{\code{signature(x = "numeric", y = "mixComp", star = "missing", plus = "missing")}}{
      see \code{\linkS4class{mixComp}}.
    }
    
    \item{\code{signature(x = "mixComp", y = "numeric", star = "ANY", plus = "ANY")}}{{
	see \code{\linkS4class{mixComp}}.
      }
    }

    \item{\code{signature(x = "mixComp", y = "numeric", star = "ANY", plus = "missing")}}{
	see \code{\linkS4class{mixComp}}.
    }
    
  }
}
\seealso{
  \code{\linkS4class{mixComp}}
}
\examples{
inner(1:3, 2:4) # [1] 20
class(inner(1:3, 2:4)) # [1] "integer"
## compare to:
1:3 \%*\% 2:4        # 20, but (1,1)-matrix
class(1:3 \%*\% 2:4) # matrix
}
\keyword{methods}
\keyword{mixComp}
\keyword{math}
