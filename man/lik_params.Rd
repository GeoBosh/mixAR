\name{lik_params}
\docType{methods}
\alias{lik_params}
\alias{lik_params-methods}
\alias{lik_params,mixAR-method}
\alias{lik_params,mixARgen-method}

\title{  Vector of parameters of a mixAR model }
\description{

  Give a numeric vector containing non-redundant parameters of a mixAR
  model in a form suitable for use by optimisation routines.  The
  methods defined in package \pkg{mixAR} for this generic function are
  described here.

}
\usage{
lik_params(model)
}
\arguments{
  \item{model}{a mixAR model.}
}
\details{
  \code{lik_params} gives the parameters of a mixAR model as a numeric
  vector.

  This is a generic function.  Parameters common to all mixAR models are
  arranged as described below. There are no other parameters when the error
  distributions do not contain parameters of their own.  Methods for
  sub-classes with additional parameters should append them after the
  common parameters.

  If \eqn{k} is the number of components and \eqn{\pi_i}{pi[i]} is the
  probability associated with the \eqn{i}{i-}th component, then the
  parameters are put in a vector as follows:
  \enumerate{
    \item{
      component probabilities, \eqn{\pi_1,\ldots,\pi_{k-1}}{pi[1],\ldots,pi[k-1]},
      (note: \eqn{\pi_{k}}{pi[k]} is not included)
    }
    \item{
      scales, \eqn{\sigma_1,\ldots,\sigma_{k}}{sigma[1],\ldots,sigma[k]},
    }
    \item{
      shifts, \eqn{\mu_1,\ldots,\mu_{k}}{mu[1],\ldots,mu[k]},
    }
    \item{
      AR coefficients of the 1st component,
    }
    \item{
      AR coefficients of the 2nd component,
    }
    \item{
      \ldots
    }
    \item{
      AR coefficients of the \eqn{k}{k-}th component.
    }
  }
}
\value{
  A numeric vector containing all parameters except the probability
  associated with the last component.
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
\note{
  The probability associated with the \eqn{k}{k-}th component is omitted
  as it is redundant. This makes it possible to try unconstrained
  optimisation though it is not likely to give useful results since
  there are other restrictions on the probabilities.

  todo: add parameter \code{omit_last=TRUE}?
}

\section{Methods}{
\describe{

\item{\code{signature(model = "mixAR")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(model = "mixARgen")}}{
%%  ~~describe this method here~~
}
}}
%\seealso{
%  \code{\link{}}
%}
%\examples{}
\keyword{methods}
\keyword{mixAR}
