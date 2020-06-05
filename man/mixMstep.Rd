\name{mixMstep}
\alias{mixMstep}
\alias{tauCorrelate}
\alias{tau2arcoef}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Internal functions for estimation of MixAR models with Gaussian components
}
\description{
  Internal functions for EM estimation of MixAR models with Gaussian components:
    sums of products and crossproducts;
    M-step for MixAR estimation;
    estimation of autoregressive part of the model.
}
\usage{
tauCorrelate(y, tau, order)
tau2arcoef(y, tau, order, est_shift = TRUE)
mixMstep(y, tau, order, index, est_shift = TRUE)
}
\arguments{
  \item{y}{time series.}
  \item{tau}{
    conditional probabilties for the observations to belong to each of
    the components, a \code{MixComp} object.
  }
  \item{order}{
    order of the MixAR model, numeric vector of length the number of
    mixture components.
  }
  \item{index}{
    indices of the observations to include in the likelihood
    calculations, typically \code{(p+1):n}, where \code{p} is
    \code{max(order)} and \code{n=length(y)}.
  }
  \item{est_shift}{
    if TRUE include shifts (intercepts) in the AR components, otherwise
    set them to zero.
  }
}
\details{

  \code{mixMstep} performs an M-step for estimation of MixAR models with
  Gaussian components.

  \code{tauCorrelate} computes crossproducts needed for EM estimation
  of MixAR models with Gaussian components.

  \code{tau2arcoef} computes the AR coefficients by solving
  Yule-Walker-type equations for each component.
}
\value{
  For \code{mixMstep}, a MixAR model, an object of class \code{MixARGaussian}.

  For \code{tauCorrelate}, a named list with the following components:
  \item{Stau}{Todo: describe this item.}
  \item{Stauy}{Todo: describe this item.}
  \item{Stauyy}{Todo: describe this item.}

  For \code{tau2arcoef},   a list with two components:
  \item{shift}{the shift (intercept) terms, a numeric vector}
  \item{arcoef}{the AR coefficients as a list, whose i-th component contains
    the coefficients for component i (as a numeric vector)}
}




%% ~Make other sections like Warning with \section{Warning }{....} ~



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{em}
