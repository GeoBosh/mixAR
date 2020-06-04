\name{mixARemFixedPoint}
\alias{mixARemFixedPoint}
\alias{mixARgenemFixedPoint}
\title{
  EM estimation for mixture autoregressive models
}
\description{
  Fit a mixture autoregressive model to a univariate time series using the
  EM algorithm.
}
\usage{
mixARemFixedPoint(y, model, est_shift = TRUE, crit = 1e-14,
                  maxniter = 200, minniter = 10, verbose = FALSE)

mixARgenemFixedPoint(y, model, crit = 1e-14, maxniter = 200,
                     minniter = 10, \dots)
}
\arguments{
  \item{y}{a univariate time series.}
  \item{model}{an object of class mixAR, a mixture autoregressive model providing the model
    specifications and initial values for the parameters.
  }
  \item{est_shift}{if TRUE optimise also w.r.t. the shift (constant) terms of the AR
    components, if FALSE keep the shift terms fixed.
  }
  \item{crit}{stop iterations when the relative change in the
    log-likelihood becomes smaller than this value.
  }
  \item{maxniter}{maximum number of iterations.}
  \item{minniter}{minimum number of iterations, do  at leat that many
                     iterations.}
  \item{\dots}{further arguments to be passed on to the M-step
                     optimiser.}
  \item{verbose}{print more details during optimisation.}
}
\details{

  \code{mixARemFixedPoint} and \code{mixARgenemFixedPoint} estimate
  mixAR models with the EM algorithm.  For \code{mixARemFixedPoint}, the
  distribution of the components are Gaussian. For
  \code{mixARgenemFixedPoint}, the distributions are arbitrary (well, to
  a point).

  Starting with \code{model}, the expectation and maximisation steps of
  the EM algorithm are repeated until convergence is detected or the
  maximum number of iterations, \code{maxniter} is exceeded.

  Currently the convergence check is very basic---the iterations stop
  when the relative change in the log-likelihood in the last two
  iterations is smaller than the threshold value specified by
  \code{crit} and at least \code{minniter} iterations have been done.

  The EM algorithm may converge very slowly. To do additional iterations
  use the returned value in another call of this function.

}
\value{
  the fitted model as an object inheriting from "mixAR".
}

\author{Georgi N. Boshnakov}
% \note{
% %%  ~~further notes~~
% }
%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Todo}{
  This function was not intended to be called directly by the user
  (hence the inconvenient name).

  The examples are not complete!!!

  The examples section below is mostly for testing.
}
\seealso{
  \code{\link{fit_mixAR}} which uses these functions for estimation,
  \code{\linkS4class{mixARGaussian}},
  \code{\linkS4class{mixARgen}}
}
\examples{
## data(ibmclose, package = "fma") # ibm data from BJ

m0 <- exampleModels$WL_ibm
m1 <- mixARemFixedPoint(fma::ibmclose, m0)
m1a <- mixARemFixedPoint(fma::ibmclose, m1$model)
show_diff(m1$model, m1a$model)

mixARemFixedPoint(fma::ibmclose, m0, est_shift = FALSE)

\dontrun{
## (not run to maybe safe a couple of seconds on CRAN)    
## simulate a continuation of ibmclose, assuming m0
ts1 <- mixAR_sim(m0, n = 1000, init = c(346, 352, 357), nskip = 0)
m2a <- mixARemFixedPoint(ts1,       m0, est_shift = FALSE)$model
m2b <- mixARemFixedPoint(diff(ts1), m0, est_shift = FALSE)$model
}
}
\keyword{optimize}
\keyword{mixAR}
\keyword{em}
