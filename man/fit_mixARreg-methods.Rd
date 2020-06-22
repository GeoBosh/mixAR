\name{fit_mixARreg-methods}
\docType{methods}

\alias{mixARreg}
\alias{fit_mixARreg}
\alias{fit_mixARreg-methods}
\alias{fit_mixARreg,ANY,data.frame,MixAR,missing-method}
\alias{fit_mixARreg,ANY,matrix,MixAR,missing-method}
\alias{fit_mixARreg,ANY,data.frame,missing,list-method}
\alias{fit_mixARreg,ANY,matrix,missing,list-method}
\alias{fit_mixARreg,ANY,numeric,MixAR,missing-method}
\alias{fit_mixARreg,ANY,numeric,missing,list-method}
\alias{fit_mixARreg,ANY,ANY,MixAR,list-method}
%\alias{fit_mixARreg,ANY,ANY,ANY,ANY-method}
\alias{fit_mixARreg,ANY,ANY,missing,list-method}

\title{
  Fit time series regression models with mixture autoregressive residuals
}
\description{
  Estimate a linear regression model for time series with residuals
  from a mixture autoregressive process.
}
\usage{
fit_mixARreg(x, y, mixARmodel, EMinit, \dots)

mixARreg(x, y, mixARmodel, tol = 1e-6, niter = 200)
}
\arguments{
  \item{x}{the response time series (currently a numeric vector).}
  \item{y}{
    \code{data.frame}, \code{matrix} or \code{numeric} vector. If either
    of the first two, each column must contain one covariate (currently
    numeric). A check for matching lengths between \code{x} and \code{y}
    is done. 
  }
  \item{mixARmodel}{
    An object inheriting from class \code{"MixAR"}, giving initial
    values for EM-estimation of mixture autoregressive
    parameters. Currenlty only \code{"MixARGaussian"} is supported.
  }
  \item{EMinit}{
    starting values for EM estimation of MixAR parameters. If present,
    must be a named list, containing at least \code{prob} and
    \code{scale} as numeric vectors, and a list for \code{arcoef}.
  }
  \item{tol}{
    threshold for convergence criterion.
  }
  \item{...}{passed on to \code{MixARreg}.}
  \item{niter}{maximal number of iterations.}
}
\details{

  \code{fit_mixARreg} is a generic function. 
  Currently there is no default method for \code{fit_mixARreg}.
  Arguments \code{y}, \code{mixARmodel}, \code{EMinit} can be given in a
  number of ways, see individual methods for details. 

  Argument \code{mixARmodel} gives the details of the the MixAR part of
  the model and initial values for the parameters. For
  \code{fit_mixARreg} this can alternatively be done with a list using 
  argument \code{EMinit}. Currently, at least one of the two must be
  supplied, and if both are present \code{EMinit} is ignored.


  \code{mixARreg} performs a two-step estimation of a linear regression
  model with mixture autoregressive residuals.  It is the workhorse for
  \code{fit_mixARreg} which calls it to do the computations.

}
\value{  
  \item{reg}{The summary output of the regression part of the model.}
  \item{mixARmodel}{
    Estimates of the mixture autoregressive part of the model. 
  }
  \item{niter}{The number of iterations until convergence.}
}
\section{Methods}{
  \describe{
    \item{\code{signature(x = "ANY", y = "data.frame", mixARmodel =
	"MixAR", EMinit = "missing")
    }}{Covariates \code{y} are supplied as \code{data.frame}: each column
    corresponds to one covariate. Initialization of \code{MixAR} paramters is
    done using input \code{mixARmodel}}
    
    \item{\code{signature(x = "ANY", y = "matrix",
        mixARmodel = "MixAR", EMinit = "missing")}
    }{Covariates \code{y} are supplied as \code{matrix}: each column
    corresponds to one covariate. Initialization of \code{MixAR} paramters is
    done using input \code{mixARmodel}}
    
    \item{\code{signature(x = "ANY", y = "numeric", mixARmodel = "MixAR", EMinit = "missing")}
    }{Covariates \code{y} is supplied as \code{numeric}: this method handles the
    simple regression case with a single covairate. 
    Initialization of \code{MixAR} paramters is done using input \code{mixARmodel}}
    
    %\item{\code{signature(x = "ANY", y = "ANY", mixARmodel = "ANY", EMinit = "ANY")}}{}
    
    \item{\code{signature(x = "ANY", y = "ANY", mixARmodel = "missing", EMinit = "list")}}{
    \code{EMinit} must be a named list (see 'Arguments').
      %%  ~~describe this method here~~
    }
    \item{\code{signature(x = "ANY", y = "ANY", mixARmodel = "MixAR", EMinit = "list")}}{
    When both \code{mixARmodel} and \code{EMinit} are supplied, the second is ignored.}
  }
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Davide Ravagli and Georgi N. Boshnakov}
\note{
  Estimation is done using the function \code{mixARreg} within each
  method.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{fit_mixAR}}
}
\examples{
## Simulate covariates
set.seed(1234)
n <- 50 # for CRAN
y <- data.frame(rnorm(n, 7, 1), rt(n, 3), rnorm(n, 3, 2))

## Build mixAR part
model <- new("MixARGaussian", 
             prob   = exampleModels$WL_At@prob,      # c(0.5, 0.5)
             scale  = exampleModels$WL_At@scale,     # c(1, 2)        
             arcoef = exampleModels$WL_At@arcoef@a ) # list(-0.5, 1.1)

## Simulate from MixAR part
u <- mixAR_sim(model, n, 0)

x <- 10 + y[, 1] + 3 * y[, 2] + 2 * y[, 3] + u

## Fit model

## Using MixARGaussian
fit_mixARreg(x = x, y = y, mixARmodel = model, niter = 3)

## Using EMinit
EMinit <- list(prob = exampleModels$WL_At@prob, scale = exampleModels$WL_At@scale,
               arcoef = exampleModels$WL_At@arcoef@a)
fit_mixARreg(x = x, y = y, EMinit = EMinit, niter = 3)
}
\keyword{ regression }
\keyword{ residuals }
\keyword{ MixAR }
