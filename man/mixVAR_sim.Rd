\name{mixVAR_sim}
\alias{mixVAR_sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulate from multivariate mixAR models
%%  ~~function to do ... ~~
}
\description{Simulate data from multivariate mixAR models
under the assumptions of multivariate Gaussian innovarion
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mixVAR_sim(model, n, init, nskip = 100, flag = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
    model from which to simulate, an object inheriting from class
    \code{mixVAR}.
  }
  \item{n}{size of simulated multivariate series.}
  \item{init}{
    initial values, a numeric matrix. If \code{missing}, a matrix of 0
    values is generated.
  }
  \item{nskip}{number of burn-in values.}
  \item{flag}{
    if \code{TRUE} returns also the regimes.
  }
%%     ~~Describe \code{x} here~~
}
\details{

  mixVAR_sim simulates a series of length \code{nskip + n} and returns
  the last \code{n} values.  \code{init} provides initial values for the
  algorithm. Each row is considered as a time point. The number of rows
  must be at least equal to the maximal AR order.

}
\value{
  a numeric \code{matrix} with \code{n} rows. 
}
\author{Davide Ravagli}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{mixAR_sim}}
}
\examples{
AR <- list()
AR[[1]] <- array(c(0.5,-0.3,-0.6,0,0,0.5,0.4,0.5,-0.3), dim = c(3,3,1))
AR[[2]] <- array(c(-0.5,0.3,0,1,0,-0.5,-0.4,-0.2, 0.5), dim = c(3,3,1))

prob <- c(0.75, 0.25)
shift <- cbind(c(0,0,0), c(0,0,0))

Sigma1 <- cbind(c(1, 0.5, -0.4), c(0.5, 2, 0.8), c(-0.4, 0.8, 4))
Sigma2 <- cbind(c(1,0.2, 0), c(0.2, 2, -0.15), c(0, -0.15, 4))
Sigma <- array(c(Sigma1, Sigma2), dim = c(3,3,2))

m <- new("mixVARGaussian", prob=prob, vcov=Sigma, arcoef=AR, shift=shift)
mixVAR_sim(m, n=500, init=matrix(rep(0,3), ncol=3), nskip=100, flag=FALSE)
}
\keyword{ sim }% use one of  RShowDoc("KEYWORDS")
\keyword{ mixVAR }% __ONLY ONE__ keyword per line
