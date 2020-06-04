\name{fit_mixVAR-methods}
\docType{methods}

\alias{fit_mixVAR}
\alias{fit_mixVAR-methods}
\alias{fit_mixVAR,ANY,mixVAR-method}
\alias{fit_mixVAR,ANY,ANY-method}

\title{Fit Mixture Vector Autoregressive Models}
\description{
  Estimate a mixVAR model for a multivariate time series. This is a generic function.
  The methods defined in package mixAR are described here.
}

\usage{
fit_mixVAR(x, model, fix, \dots)
}

\arguments{
  \item{x}{
    a multivariate time series (currently a numeric matrix).
  }
  \item{model}{
    model, object inheriting from mixVAR class.
  }
  \item{fix}{~~ TODO: describe this argument. ~~}
  \item{\dots}{
    additional arguments for the methods (not currently used).
  }
}

\value{
  a mixVAR model.
}

\details{

  \code{model} specifies the model to fit. If \code{model} inherits from
  \code{"mixVAR"}, it is used as a template.  Estimation is done via
  EM-Algorithm, using the function \code{mixVARfit}.

  Currently the default method for \code{fit_mixAR} just throws error,
  since there seems no suitable default task to do. 
  
  \bold{TODO:} Define more methods to handle different cases (\code{fix
    = "shift"}, etc..)

}

\section{Methods}{
\describe{

  \item{\code{signature(x = "ANY", model = "mixVAR")}}{
    This is currently the only available method for \code{fit_mixVAR}}.
  \item{\code{signature(x = "ANY", model = "ANY")}}{{
%%  ~~describe this method here~~
}
}}
}

\seealso{
  \code{\link{mixVARfit}}
}
\examples{
AR <- list()
AR[[1]] <- array(c(0.5,-0.3,-0.6,0,0,0.5,0.4,0.5,-0.3),dim=c(3,3,1))
AR[[2]] <- array(c(-0.5,0.3,0,1,0,-0.5,-0.4,-0.2, 0.5),dim=c(3,3,1))

prob <- c(0.75,0.25)
shift <- cbind(c(0,0,0),c(0,0,0))

Sigma1 <- cbind(c(1,0.5, -0.4),c(0.5,2, 0.8),c(-0.4,0.8,4))
Sigma2 <- cbind(c(1,0.2, 0),c(0.2,  2, -0.15),c(0,-0.15,4))
Sigma <- array(c(Sigma1, Sigma2), dim=c(3,3,2))

m <- new("mixVARGaussian", prob=prob, vcov=Sigma, arcoef=AR, shift=shift)
y <- mixVAR_sim(m, n=500, init=matrix(rep(0,3), ncol=3), nskip=100, flag=FALSE)

fit_mixVAR(y, m)
}
