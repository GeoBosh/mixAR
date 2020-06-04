\name{mixSubsolve}
\alias{mixSubsolve}
\title{
  Support for EM estimation of mixAR models, internal function.
}
\description{
  Solve the linear sub-system arising in the M-step of the EM algorithm
  for mixAR models with Gaussian noise components. This gives estimates
  of the parameters of the \eqn{k}{k-}th AR component of the model.
}
\usage{
mixSubsolve(k, pk, Stau, Stauy, Stauyy, shift, tol = 1e-07)
}
\arguments{
  \item{k}{which component, an integer.}
  \item{pk}{AR order of the \eqn{k}{k-}th component.}
  \item{Stau,Stauy,Stauyy}{
    sums and cross-sums needed to form the system.
  }
  \item{shift}{
    If \code{TRUE}, estimate also a shift, otherwise set the shift to 0.
  }
  \item{tol}{tolerance, only used in case of trouble, see Details.}
}
\details{

  \code{mixSubsolve} forms and solves a linear subsytem to obtain
  estimates of the AR parameters (and the shift, if \code{shift} is
  \code{TRUE})  for the \code{k}-th mixAR component.

  First, \code{solve()} is tried. If it reports that the system is
  (numerically) singular, a solution is computed using SVD with
  tolerance \code{tol}.

  Note that argument \code{tol} is not used in the call to
  \code{solve()}. The net effect is that \code{solve()} computes the
  solution with its very small default tollerance. Probably I wanted
  discontinuity during optimisation, which could result if using a
  larger tolerance, and only resort to that if absolutely needed.

  I don't remember why I used pseudo-inverse in \code{mixSubsolve()},
  when \code{solve()} has a similar \code{tol} argument for its
  QR-decomposition.
  
}
\author{Georgi N. Boshnakov}


%% ~Make other sections like Warning with \section{Warning }{....} ~


% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{em}
\keyword{internal}
