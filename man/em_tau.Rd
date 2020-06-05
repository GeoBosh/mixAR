\name{em_tau}
\alias{em_tau}
\alias{em_tau_safe}
\title{
  Compute probabilities for the observations to belong to each of the components
}
\description{
  Supporting function for EM algorithm. Update the conditional
  probabilities of the components of the MixAR model
  (for E-step of EM algorithm).
}
\usage{
em_tau(stdetk, prob, scale, pdf = dnorm)

em_tau_safe(stdetk, prob, scale, pdf = dnorm)
}
\details{

  \code{em_tau} and \code{em_tau_safe} compute the conditional
  probabilities of the components of the MixAR model (for the E-step of
  the EM algorithm). The two functions do the same computations but
  \code{em_tau_safe}, in addition, protects agains \code{NaN}'s and
  infinite values in argument \code{stdetk} or obtained during
  computations.
  
}
\arguments{
  \item{stdetk}{
    standardised component residuals, a \code{MixComp} object.
  }
  \item{prob}{
    current estimates of the probabilities of the components, a numeric
    vector of length equal to the number of components in the model.
  }
  \item{scale}{
    scales (standard deviations) of the noise components, a numeric
    vector of length equal to the number of components in the model.
  }
  \item{pdf}{
    densities of the noise components.
  }
}
\author{Georgi N. Boshnakov}
\keyword{em}
\keyword{internal}
