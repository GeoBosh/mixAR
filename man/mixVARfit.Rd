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
mixVARfit(y, model, fix = FALSE, tol = 10^-6)
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

  \item{fix}{~~ TODO: describe this argument. ~~}
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
AR <- list()
AR[[1]] <- array(c(0.5,-0.3,-0.6,0,0,0.5,0.4,0.5,-0.3), dim = c(3,3,1))
AR[[2]] <- array(c(-0.5,0.3,0,1,0,-0.5,-0.4,-0.2, 0.5), dim = c(3,3,1))

prob <- c(0.75, 0.25)
shift <- cbind(c(0,0,0), c(0,0,0))

Sigma1 <- cbind(c(1,0.5, -0.4), c(0.5,2, 0.8), c(-0.4,0.8,4))
Sigma2 <- cbind(c(1,0.2, 0), c(0.2,  2, -0.15), c(0,-0.15,4))
Sigma <- array(c(Sigma1, Sigma2), dim = c(3,3,2))

m <- new("MixVARGaussian", prob = prob, vcov = Sigma, arcoef = AR, shift = shift)
y <- mixVAR_sim(m, n = 500, init = matrix(rep(0,3), ncol = 3), nskip = 100, flag = FALSE)

mixVARfit(y, m, tol = 10^-6)
}
