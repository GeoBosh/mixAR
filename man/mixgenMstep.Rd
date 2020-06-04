\name{mixgenMstep}
\alias{mixgenMstep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  M-step for models from class mixARgen
}
\description{
  M-step for models from class mixARgen. This function is for use by
  other functions.
}
\usage{
mixgenMstep(y, tau, model, index, fix = NULL, comp_sigma = FALSE,
            method = "BBsolve", maxit = 100, trace = FALSE,
            lessverbose = TRUE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{time series, a numeric vector.}
  \item{tau}{conditional probabilities, an object of class "mixComp".}
  \item{model}{the current model, an object from a subclass of class
    "mixAR".}
  \item{index}{indices of observations for which to compute residuals, a
    vector of positive integers, see 'Details'.}
  \item{method}{ optimisation or equation  solving method for package BB}
  \item{\dots}{arguments to pass on to optimisation functions, not
    thought over yet. Do not use until this notice is removed.}
  \item{comp_sigma}{If TRUE optimise the scale parameters using
    univariate optimisation. (todo: does not work with argument 'fix'
    yet.)}
  \item{fix}{specify parameters to be held fixed during optimisation,
    see 'Details'.}
  \item{maxit}{maximal number of iterations for BB optimisers and
    solvers. Meant mainly for testing.}
  \item{trace}{if TRUE, BB optimisers and
    solvers will print information about their proceedings. Meant mainly
    for testing.}
  \item{lessverbose}{if TRUE, print a dot instead of more verbose
    information.}
}
\details{

  \code{mixgenMstep} is an implementation of the M-step of the EM
  algorithm for mixture autoregressive models specified by objects of
  class "mixARgen". The function was build and modified incrementally
  with the main goal of providing flexibility. Speed will be addressed
  later.

  By default optimisation is done with respect to all parameters.
  Argument \code{fix} may be a list with elements "prob", "shift",
  "scale" and "arcoef". These elements should be logical vectors
  containing \code{TRUE} in the positions of the fixed parameters.
  Elements with no fixed parameters may be omitted. (Currently the
  "prob" element is ignored, i.e. it is not possible to fix any of the
  component probabilities.)

  If \code{fix = "shift"} the shift parameters are kept fixed. This is
  equivalent to \code{fix = list(shift = rep(TRUE,g))}.

  The parameters (if any) of the distributions of the error components
  are estimated by default. Currently the above method cannot be used to
  fix some of them (todo:?). This can be achieved however by modifying
  the distribution part of the model since that incorporates information
  about the parameters and whether they are fixed or not, see (todo: put
  a cross-reference here).

}

\seealso{
  \code{\link{fit_mixAR}} and
  \code{\link{mixARgenemFixedPoint}}
  which are meant to be called by users.
}

\keyword{em}
