\name{fit_mixAR-methods}
\docType{methods}
\alias{fit_mixAR}
\alias{fit_mixAR-methods}
\alias{fit_mixAR,ANY,ANY,ANY-method}
\alias{fit_mixAR,ANY,mixAR,list-method}
\alias{fit_mixAR,ANY,mixAR,missing-method}
\alias{fit_mixAR,ANY,mixAR,mixAR-method}
\alias{fit_mixAR,ANY,mixAR,numeric-method}
\alias{fit_mixAR,ANY,mixARGaussian,mixAR-method}
\alias{fit_mixAR,ANY,numeric,missing-method}
\alias{fit_mixAR,ANY,numeric,numeric-method}
\title{
  Fit mixture autoregressive models
}
\description{
  Estimate a mixAR model for a time series. This is a generic function.
  The methods defined in package mixAR are described here.
}
\usage{
fit_mixAR(x, model, init, fix, ...)
}
\arguments{
  \item{x}{a time series.}
  \item{model}{model, object inheriting from mixAR class.}
  \item{init}{what initializations to do, see Details.}
  \item{fix}{which parameters to fix, see Details.}
  \item{\dots}{additional arguments for the methods.}
}
\details{

  Method dispatch is done on the first three arguments:
  \code{x}, \code{model} and \code{init}.

  \code{model} specifies the model to fit. If \code{model} inherits from
  \code{"mixAR"}, it is used as a template.  If \code{init} is missing,
  the parameters of \code{model} are also used as initial values. 
  \code{model} can also be a numeric vector specifying the order of a
  mixAR model with Gaussian components. 

  Argument \code{init} can be used to give initial values in  variety of
  ways. If it is a mixAR object it doesn't need to be of the same class
  as \code{model}, to allow using as initial values common parameters
  of different mixAR models. A positive integer value of \code{init}
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
  
  TODO: This is work in progress, things may change.
}
\value{
  a mixAR model or a list of mixAR models, depending on the arguments.
}
\section{Methods}{
\describe{

  \item{\code{signature(x = "ANY", model = "ANY", init = "ANY")}}{
    The default method throws error.
  }

  \item{\code{signature(x = "ANY", model = "mixAR", init = "missing")}}{
    This is equivalent to setting \code{init = model}.
  }

  \item{\code{signature(x = "ANY", model = "mixAR", init = "mixAR")}}{
    \code{model} is a template for the result, \code{init} specifies
    initial values for the parameters. In principle, \code{model} and
    \code{init} may be from different classes, to allow for example
    using AR coefficients from a Gaussian fit for other distributions.

    TODO: this method is not finished, see the source code for further
    comments. 

  }

  \item{\code{signature(x = "ANY", model = "mixAR", init = "numeric")}}{
 
    \code{init} must be a single positive integer here. The model is
    fitted \code{init} times, each time starting with a new set of
    randomly generated initial values. If \code{select} is \code{TRUE},
    the default, the model with the largest likelihood is returned,
    otherwise a list containing the \code{init} fitted models is
    returned.

  }

  \item{\code{signature(x = "ANY", model = "mixAR", init = "list")}}{
    
    Each element of the list \code{init} should be an acceptable value
    for \code{init}. The model is fitted with the initial value  set to each
    element of \code{init}. A list containing the fitted models is
    returned. 
    
  }

  \item{\code{signature(x = "ANY", model = "mixARGaussian", init = "mixAR")}}{
      %%  ~~describe this method here~~
  }
  \item{\code{signature(x = "ANY", model = "numeric", init = "missing")}}{
      This is equivalent to setting \code{init = 1}.
  }
  \item{\code{signature(x = "ANY", model = "numeric", init = "numeric")}}{

      A numeric \code{model} should be a vector of non-negative integers
      specifying the order of the mixAR model. The distribution of the
      components is assumed Gaussian.

  }
}}
\examples{
## model coefficients from Wong&Li (IBM fit)
prob  <- exampleModels$WL_ibm@prob     # c(0.5439, 0.4176, 0.0385)
sigma <- exampleModels$WL_ibm@scale    # c(4.8227, 6.0082, 18.1716)
ar    <- exampleModels$WL_ibm@arcoef@a # list(c(0.6792, 0.3208), c(1.6711, -0.6711), 1)

## data(ibmclose, package = "fma")  # `ibmclose'

mo_WLt3 <- new("mixARgen", prob = prob, scale = sigma, arcoef = ar,
               dist = list(fdist_stdt(3)))
mo_WLt30 <- new("mixARgen", prob = prob, scale = sigma, arcoef = ar,
                dist = list(fdist_stdt(30)))

\dontrun{
## running all examples would take too long on CRAN
fi0    <- fit_mixAR(fma::ibmclose, exampleModels$WL_ibm,    fix = "shift")
fi0gen <- fit_mixAR(fma::ibmclose, exampleModels$WL_ibm_gen, fix = "shift")
fit3   <- fit_mixAR(fma::ibmclose, mo_WLt3,  fix = "shift")
fi30   <- fit_mixAR(fma::ibmclose, mo_WLt30, fix = "shift")

stdt3v <- fdist_stdt(3, fixed = FALSE)
mo_WLt3v <- new("mixARgen", prob = prob, scale = sigma, arcoef = ar,
                dist = list(fdist_stdt(3, fixed = FALSE)))
fit3v <- fit_mixAR(fma::ibmclose, mo_WLt3v, fix = "shift")

fit_mixAR(fma::ibmclose, new("mixARgen", prob = prob, scale = sigma, arcoef = ar,
                        dist = list(fdist_stdt(10))), fix = "shift")
fit_mixAR(fma::ibmclose, new("mixARgen", prob = prob, scale = sigma, arcoef = ar,
                        dist = list(fdist_stdt(5))), fix = "shift")
fit_mixAR(fma::ibmclose, new("mixARgen", prob = prob, scale = sigma, arcoef = ar,
                        dist = list(fdist_stdt(4))), fix = "shift")
fit_mixAR(fma::ibmclose, new("mixARgen", prob = prob, scale = sigma, arcoef = ar,
                        dist = list(fdist_stdt(3))), fix = "shift")
fit_mixAR(fma::ibmclose, new("mixARgen", prob = prob, scale = sigma, arcoef = ar,
                        dist = list(fdist_stdt(3.5))), fix = "shift")
fit_mixAR(fma::ibmclose, new("mixARgen", prob = prob, scale = sigma, arcoef = ar,
                        dist = list(fdist_stdt(3.8))), fix = "shift")

mo_WLt30v <- new("mixARgen", prob = prob, scale = sigma, arcoef = ar,
                 dist = list(fdist_stdt(30, fixed = FALSE)))

fit30v <- fit_mixAR(fma::ibmclose, mo_WLt30v, fix = "shift")

mo_WLt20v30v40 <- new("mixARgen", prob = prob, scale = sigma, arcoef = ar,
                      dist = list(fn_stdt(c(20, 30, 40), fixed = FALSE)))
fit20v30v40 <- fit_mixAR(fma::ibmclose, mo_WLt20v30v40, fix = "shift")

mo_WLt20v30v40 <- new("mixARgen", prob = prob, scale = sigma, arcoef = ar,
                      dist = fn_stdt(c(20, 30, 40), fixed = FALSE))
fit20v30v40 <- fit_mixAR(fma::ibmclose, mo_WLt20v30v40, fix = "shift")

mo_WLtf <- new("mixARgen", prob = prob, scale = sigma, arcoef = ar, 
               dist = list(generator = function(par) fn_stdt(par, fixed = FALSE), 
                           param = c(20, 30, 40)))

fitmo_WLtf <- fit_mixAR(fma::ibmclose, mo_WLtf, fix = "shift")
}
}
\keyword{methods}
\keyword{mixAR}
\keyword{estimation}
