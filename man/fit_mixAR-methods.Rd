\name{fit_mixAR-methods}
\docType{methods}
\alias{fit_mixAR}
\alias{fit_mixAR-methods}
\alias{fit_mixAR,ANY,ANY,ANY-method}
\alias{fit_mixAR,ANY,MixAR,list-method}
\alias{fit_mixAR,ANY,MixAR,missing-method}
\alias{fit_mixAR,ANY,MixAR,MixAR-method}
\alias{fit_mixAR,ANY,MixAR,numeric-method}
\alias{fit_mixAR,ANY,MixARGaussian,MixAR-method}
\alias{fit_mixAR,ANY,numeric,missing-method}
\alias{fit_mixAR,ANY,numeric,numeric-method}
\title{
  Fit mixture autoregressive models
}
\description{
  Estimate a MixAR model for a time series. This is a generic function.
  The methods defined in package \pkg{mixAR} are described here.
}
\usage{
fit_mixAR(x, model, init, fix, ...)
}
\arguments{
  \item{x}{a time series.}
  \item{model}{model, object inheriting from MixAR class.}
  \item{init}{what initializations to do, see Details.}
  \item{fix}{which parameters to fix, see Details.}
  \item{\dots}{additional arguments for the methods.}
}
\details{

  Method dispatch is done on the first three arguments:
  \code{x}, \code{model} and \code{init}.

  \code{model} specifies the model to fit. If \code{model} inherits from
  \code{"MixAR"}, it is used as a template.  If \code{init} is missing,
  the parameters of \code{model} are also used as initial values. 
  \code{model} can also be a numeric vector specifying the order of a
  MixAR model with Gaussian components. 

  Argument \code{init} can be used to give initial values in  variety of
  ways. If it is a MixAR object it doesn't need to be of the same class
  as \code{model}, to allow using as initial values common parameters
  of different MixAR models. A positive integer value of \code{init}
  asks to run the fitting procedure \code{init} times, each time
  generating random initial values.  

  \code{init} can also be a list. In that case, each component of the
  list should itself be an acceptable value for \code{init} and the
  fitting procedure is run with each component of \code{init}.

  Argument \code{fix} can be given in a number of ways. Note however
  that currently there is no method dispatch on it.

  Currently the default method for \code{fit_mixAR} just throws error,
  since there seems no suitable default task to do. 

  See individual methods for further details.
  
  % TODO: This is work in progress, things may change.
}
\value{
  a MixAR model or a list of MixAR models, depending on the arguments.
}
\section{Methods}{
\describe{

  \item{\code{signature(x = "ANY", model = "ANY", init = "ANY")}}{
    The default method throws error.
  }

  \item{\code{signature(x = "ANY", model = "MixAR", init = "missing")}}{
    This is equivalent to setting \code{init = model}.
  }

  \item{\code{signature(x = "ANY", model = "MixAR", init = "MixAR")}}{
    \code{model} is a template for the result, \code{init} specifies
    initial values for the parameters. In principle, \code{model} and
    \code{init} may be from different classes, to allow for example
    using AR coefficients from a Gaussian fit for other distributions.

    % TODO: this method is not finished, see the source code for further
    % comments. 

  }

  \item{\code{signature(x = "ANY", model = "MixAR", init = "numeric")}}{
 
    \code{init} must be a single positive integer here. The model is
    fitted \code{init} times, each time starting with a new set of
    randomly generated initial values. If \code{select} is \code{TRUE},
    the default, the model with the largest likelihood is returned,
    otherwise a list containing the \code{init} fitted models is
    returned.

  }

  \item{\code{signature(x = "ANY", model = "MixAR", init = "list")}}{
    
    Each element of the list \code{init} should be an acceptable value
    for \code{init}. The model is fitted with the initial value  set to each
    element of \code{init}. A list containing the fitted models is
    returned. 
    
  }

  \item{\code{signature(x = "ANY", model = "MixARGaussian", init = "MixAR")}}{
      %%  ~~describe this method here~~
  }
  \item{\code{signature(x = "ANY", model = "numeric", init = "missing")}}{
      This is equivalent to setting \code{init = 1}.
  }
  \item{\code{signature(x = "ANY", model = "numeric", init = "numeric")}}{

      A numeric \code{model} should be a vector of non-negative integers
      specifying the order of the MixAR model. The distribution of the
      components is assumed Gaussian.

  }
}}
\examples{
## model coefficients from Wong&Li (IBM fit)
prob  <- exampleModels$WL_ibm@prob     # c(0.5439, 0.4176, 0.0385)
sigma <- exampleModels$WL_ibm@scale    # c(4.8227, 6.0082, 18.1716)
ar    <- exampleModels$WL_ibm@arcoef@a # list(c(0.6792, 0.3208), c(1.6711, -0.6711), 1)

## data(ibmclose, package = "fma")  # `ibmclose'

mot30 <- new("MixARgen", prob = prob, scale = sigma, arcoef = ar,
             dist = distlist("stdt", c(30, 30, 30)))

mot20_30_40 <- new("MixARgen", prob = prob, scale = sigma, arcoef = ar,
                   dist = distlist("stdt", c(20, 30, 40)))

mo_t20_t30_norm <- new("MixARgen", prob = prob, scale = sigma, arcoef = ar,
                   dist = distlist(c("stdt", "stdt", "stdnorm"), c(20, 30)))

## Gaussian components
fi0 <- fit_mixAR(fma::ibmclose, exampleModels$WL_ibm, fix = "shift", crit = 1e-4)
fi0$model

if(FALSE){ # don't run on CRAN to save a couple of seconds
## remove minniter/maxniter below for realistic results.

## std-t components
fi1 <- fit_mixAR(fma::ibmclose, mot30, fix = "shift",
                 crit = 1e-4, minniter = 1, maxniter = 3)
fi1$model

## 1st and 2nd components std-t, 3rd Gaussian
fi2 <- fit_mixAR(fma::ibmclose, mo_t20_t30_norm, fix = "shift",
                 crit = 1e-4, minniter = 1, maxniter = 3)
fi2$model
}
}
\keyword{methods}
\keyword{MixAR}
\keyword{estimation}
