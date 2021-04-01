\name{stdnormmoment}
\alias{stdnormmoment}
\alias{stdnormabsmoment}
\alias{stdtmoment}
\alias{stdtabsmoment}
\alias{tabsmoment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute moments and absolute moments of standardised-t and normal distributions}
\description{
  Compute moments and absolute moments of standardised-t, t and normal
  distributions.
}
\usage{
stdnormmoment(k)
stdnormabsmoment(k)
stdtmoment(nu, k)
stdtabsmoment(nu, k)
tabsmoment(nu, k)
}
\arguments{
  \item{k}{numeric vector, moments to compute.}
  \item{nu}{a number, degrees of freedom.}
}
\details{

  These functions compute moments of standardised-t and standard normal
  distibutions. These distributions have mean zero and variance 1.
  Standardised-t is often prefferred over Student-t for innovation
  distributions, since its variance doesn't depend on its parameter
  (degrees of freedom). The absolute moments of the usual
  t-distributions are provided, as well.

  The names of the functions start with an abbreviated name of the
  distribution concerned: \code{stdnorm} (N(0,1)), \code{stdt}
  (standardised-t), \code{t} (Student-t). 

  The functions with names ending in \code{absmoment()}
  (\code{stdnormabsmoment()}, \code{stdtabsmoment()} and \code{tabsmoment()})
  compute absolute moments, The rest (\code{stdnormmoment()} and
  \code{stdtmoment()}) compute ordinary moments.

  The absolute moments are valid for (at least) \code{k >= 0}, not
  necessarily integer. The ordinary moments are currently intended only
  for integer moments and return NaN's for fractional ones, with
  warnings.

  Note that the Student-t and standardised-t with \eqn{\nu}{nu} degrees
  of freedom have finite (absolute) moments only for \eqn{k<\nu}{k < nu}.
  As a consequence, standardised-t is defined only for \eqn{\nu>2}{nu > 2} 
  (otherwise the variance is infinite).

  \code{stdtabsmoment} returns \code{Inf} for any \eqn{k \ge \nu}{k >= nu}. 
  \code{stdtmoment} returns \code{Inf} for even integer \code{k}'s, such
  that \eqn{k \ge \nu}{k >= nu}. However, for odd integers it returns
  zero and for non-integer moments it returns \code{NaN}.
  Here is an example, where the first two k's are smaller than
  \code{nu}, while the others are not:
  \printExample{stdtabsmoment(nu = 5, k = c(4, 4.5, 5, 5.5));stdtmoment(nu = 5, k = c(4, 4.5, 5, 5.5))}

  These functions are designed to work with scalar \code{nu} but this
  is not enforced.
  
}
\value{
  numeric vector of the same length as \code{k}.
}
\references{
  \insertRef{WuertzEtAll2006}{mixAR}
}
\author{Georgi N. Boshnakov}
% \note{
% %%  ~~further notes~~
% }
% 
% %% ~Make other sections like Warning with \section{Warning }{....} ~
% 
% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
## some familiar positive integer moments
stdnormmoment(1:6)
## fractional moments of N(0,1) currently give NaN
stdnormmoment(seq(1, 6, by = 0.5))
## abs moments don't need to be integer
curve(stdnormabsmoment, from = 0, to = 6, type = "l", col = "blue")

## standardised-t
stdtmoment(5, 1:6)
stdtabsmoment(5, 1:6)
stdtabsmoment(5, 1:6)

## Student-t
tabsmoment(5, 1:6)
}
\keyword{distribution}
