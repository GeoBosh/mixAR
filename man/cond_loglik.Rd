\name{cond_loglik}
\alias{cond_loglik}
\alias{cond_loglikS}
\title{
  Log-likelihood of MixAR models
}
\description{
  Compute the log-likelihood of a MixAR model for a univariate time
  series.
}
\usage{
cond_loglik(model, x, index)
cond_loglikS(model, x, index)
}
\arguments{
  \item{model}{a MixAR model.}
  \item{x}{a time series or numeric vector.}
  \item{index}{

    a vector of integers giving the indices in \code{x} over which to
    compute the sum for the log-likelihood, default is
    \code{(p+1):length(x)}, where \code{p} is the maximum AR order of
    the components of the model.
  }
}
\details{

  \code{cond_loglik} computes the conditional log-likelihood of a MixAR
  model. Conditional here means conditional on the first \code{p} values
  being fixed, where \code{p} is the maximum AR order of the components
  of the model.

  Argument \code{index} can be used to compute the sum over a subset of
  time points. 
  
  \code{cond_loglikS} is a variant of \code{cond_loglik} for the case
  when the input model contains seasonal AR coefficients.

}
\value{
  the log-likelihood, a numeric value
}
\author{Georgi N. Boshnakov and Davide Ravagli}
\examples{
## data(ibmclose, package = "fma") # doesn't work with fma v2.4, using '::'
cond_loglik(exampleModels$WL_ibm,     as.numeric(fma::ibmclose))
cond_loglik(exampleModels$WL_ibm_gen, as.numeric(fma::ibmclose))

data(lynx)  # for 'lynx' data
sar <- new("raggedCoefS", a = list(c(1.1022, -0.2835), c(1.5279, -0.8871)),
                          as = list(c(0, 0), 0), s = 10)

## SMAR(2; 2, 2)(2, 1)_10
model_s10 <- new("MixARGaussian", prob = c(.3, .7), scale = c(.08, .202),
                                  arcoef = sar, shift = c(.7,1)) 
cond_loglikS(model_s10, log(lynx))
cond_loglikS(model_s10, log(lynx), index = 45:114) # on reduced dataset
}
\keyword{ts}
\keyword{MixAR}
