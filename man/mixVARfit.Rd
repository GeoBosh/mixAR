\name{mixVARfit}
\alias{mixVARfit}
\title{
  Fit mixture vector autoregressive models
}
\description{
  Provides EM-estimation of mixture autoregressive models for
  multivariate time series
}
\usage{
mixVARfit(y, model, fix = FALSE, tol = 10^-6, verbose = FALSE)
}
\arguments{
  \item{y}{a data matrix.}
  \item{model}{
    an object of class \code{"MixVAR"} with initial values of parameter
    for EM estimation.
  }
  \item{tol}{
    Threshold for convergence criterion.
  }

  \item{fix}{if TRUE, fix the shift parameters.}
  \item{verbose}{if \code{TRUE} print information during the optimisation.}
}
\details{

  Estimation is done under the assumption of multivariate Gaussian
  innovations.

}
\value{
  An object of class \code{MixVARGaussian} with EM estimates of model
  parameters.  
}
\references{
\insertRef{10.2307/20445243}{mixAR}
}
\author{Davide Ravagli}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{fit_mixVAR-methods}} for examples
}
%\examples{
%}
