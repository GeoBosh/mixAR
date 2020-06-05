\name{em_est_sigma}
\alias{em_est_sigma}
\alias{tauetk2sigmahat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Update the scale parameters of MixAR models
}
\description{
  Calculates estimates of scale parameters of MixAR models from
  conditional probabilities and mixture `residuals'.
  Used in EM algorithm.
}
\usage{
tauetk2sigmahat(tau, etk)

em_est_sigma(tau, etk, Fscore, sigma,
             dontfix = rep(TRUE, length(sigma)), compwise = FALSE)
}
\arguments{
  \item{tau}{
    the conditional probabilities for the groups, a \code{"MixComp"}
    object.
  }
  \item{etk}{component "residuals", MixComp object(?).}
  \item{Fscore}{the score function(s) of the noise distributions.}
  \item{sigma}{current values of the scale parameters.}
  \item{compwise}{if \code{TRUE} solve the equations component-wise, see
  'Details.}
  \item{dontfix}{a logical vector containig \code{TRUE} in the positions
             of elements of \code{sigma} that are to be estimated.}
}
\details{
  \code{tauetk2sigmahat} calculates estimates of the scale parameters
  for a MixAR time series with Gaussian components. There is an explicit
  formula in that case.

  \code{em_est_sigma} calculates estimates of the scale parameters in
  the general case.  The non-linear equations are solved using functions
  from package \code{BB}. The equations for the components can often be
  solved independently. When that is the case, \code{compwise} may speed
  things up a little.
}
\value{
  The new values of the scale parameters, a numeric vector
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{em}
