\name{percent_of}
\docType{methods}
\alias{\%of\%}
\alias{\%of\%-methods}
\alias{\%of\%,ANY,ANY-method}
\alias{\%of\%,character,MixComp-method}
\alias{\%of\%,function,MixComp-method}
\alias{\%of\%,list,MixComp-method}
\title{
  Infix operator to apply functions to matrix-like objects
}
\description{
 
  The infix operator \code{\%of\%} is a generic function which applies
  functions to objects.  This page describes the function and the
  methods defined in package \pkg{mixAR}.

}
%
% FIXME: todo: (for Rdpack !)
%        (1) automatic generation of this field does not put \%.
%        (2) Ne dava infix-nata forma!
%        (3) R CMD CHECK is not happy with name of file containing %.
\usage{
"\%of\%"(e1, e2)
e1 \%of\% e2
}
\arguments{
  \item{e1}{
    usually a function, the name of a function, a character vector, or a
    list of functions, see Details.
  }
  \item{e2}{an object, usually matrix-like.}
}
\details{

  \code{\%of\%} is a generic function with dispatch on both arguments.
  It is intended to be used mainly in infix form.

  \code{\%of\%} transforms each \dQuote{column} of a matrix-like object by a
  function. If \code{e1} specifies a single function, that is applied to all
  columns. Otherwise \code{length(e1)} should equal the number of 
  \dQuote{columns} of \code{e2} and \code{e1[[i]]} is applied to the
  \code{i}-th \dQuote{column} of \code{e2}.

  The mental model is that the first argument, \code{e1}, is (converted
  to) a list of functions containing one function for each column of
  \code{e2}. The i-th function is applied to each element of the i-th
  column.

  The methods for \code{"MixComp"} objects allow for very transparent
  and convenient computing with \code{"MixAR"} objects.

}
\value{
  for the default method, a matrix;

  for methods with \code{e2} from class \code{MixComp}, a \code{MixComp}
  object with its slot \code{m} replaced by the result of applying
  \code{e1} to its elements, see the descriptions of the individual
  methods for details;

}
\section{Methods}{

  Below are the descriptions of the methods for \code{\%of\%} defined by
  package \pkg{mixAR}.
  
  \describe{
    \item{\code{signature(e1 = "ANY", e2 = "ANY")}}{

      This is the default method. It uses \code{apply()} to evaluate
      \code{e1} for each element of the matrix \code{e2}, without
      checking the arguments. If the arguments are not suitable for
      \code{apply()}, any error messages will come from it. So, for this
      method \code{e1} is a function (or the name of a function) and
      \code{e2} is a matrix or array.

    }
    \item{\code{signature(e1 = "function", e2 = "MixComp")}}{

      Create (and return) a \code{MixComp} object with its slot \code{m}
      replaced by the result of applying the function \code{e1} to each
      element of the \code{MixComp} object \code{e2}, see
      \code{\linkS4class{MixComp}}.

    }
    \item{\code{signature(e1 = "character", e2 = "MixComp")}}{

      Here \code{e1} contains the names of one or more functions.  If
      \code{length(e1) = 1}, this is equivalent to the method for
      \code{e1} of class \code{"function"}.

      If \code{length(e1) > 1}, then for each \code{i} the function
      specified by \code{e1[i]} is applied to the \code{i}th column of
      \code{e2@m}.  In this case there is no recycling: \code{e1} must
      have \code{ncol(e2@m)} elements.

    }
    \item{\code{signature(e1 = "list", e2 = "MixComp")}}{

      Here each element of \code{e1} is a function or the name of a
      function. It works analogously to the method with \code{e1} from
      class \code{"character"}.  If \code{length(e1) = 1}, then
      \code{e1[[1]]} is applied to each element of
      \code{e1@m}. Otherwise, if \code{length(e1) > 1}, then
      \code{e1[[i]]} is applied to the \code{i}th column of \code{e2@m}.
    
    }
}}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
\section{Todo}{
  The code is rather inefficient for some of the methods. 

  Maybe should require that the functions in the first argument are
  vectorised. (Some methods effectively assume it.)
  
}
\seealso{
  class \code{"\linkS4class{MixComp}"}
}
\examples{
m <- matrix(rnorm(18), ncol = 3)
## defult method
pm1 <- pnorm \%of\% m
f3 <- list(pnorm, function(x, ...) pnorm(x, mean = 0.1),
                  function(x, ...) pnorm(x, mean = -0.1) )
## no method for f from "list" yet:
## pm2 <- f3 \%of\% m

mc <- new("MixComp", m = m)
pnorm \%of\% mc
pmc3 <- f3 \%of\% mc
## result is equivalent to applying f3[[i] to m[ , i]:
all.equal(pmc3@m, cbind(f3[[1]](m[ , 1]), f3[[2]](m[ , 2]), f3[[3]](m[ , 3])))
}
\keyword{math}
\keyword{methods}
\keyword{MixComp}
