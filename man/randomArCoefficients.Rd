\name{randomArCoefficients}
\alias{randomArCoefficients}
\alias{randomMarParametersKernel}
\alias{randomMarResiduals}
\alias{tsDesignMatrixExtended}
\title{Random initial values for MixAR estimation}
\description{

  Translations of functions from my Mathematica sources. Not used currently?

}
\usage{
randomArCoefficients(ts, wv, pk, pmax, partempl, sub_size = 10,
                     condthr = 10, nattempt = 10, startfrom = pmax + 1)

randomMarParametersKernel(ts, ww, pk, pmax, partempl, \dots)

randomMarResiduals(ts, p, partempl)

tsDesignMatrixExtended(ts, p, ind, partempl)
}
\arguments{
  \item{ts}{time series.}
  \item{wv,ww}{a vector of weights (?).}
  \item{pk}{the AR order of the requested component.}
  
  \item{pmax}{
    the maximal AR order in the model. Needed since it cannot be
    determined by functions working on a single component.
  }
  \item{partempl}{
    parameter template, a list containing one element for each mixture
    component, see Details.
  }
  \item{sub_size}{
    the size of the subsample to use, default is 10.
  }
  \item{condthr}{
    threshold for the condition number.
  }
  \item{nattempt}{
    if \code{condthr} is not reached after \code{nattempt} attempts, the
    function returns the results from the last subset tried.
  }
  \item{startfrom}{
    the starting index (in \code{ts}) to use for subsampling, default is
    \code{pmax + 1}.
  }
  \item{...}{arguments to pass on to \code{randomArCoefficients()}.}
  \item{p}{a vector of non-negative integers, the MixAR order.}
  \item{ind}{
    a vector of positive integers specifying the indices of the
    observations to use for the \dQuote{response} variable.
  }
}
\details{

  \code{randomArCoefficients} tries small subsamples (not necessarilly
  contiguous) from the observations in search of a cluster hopefully
  belonging to one mixture component and estimates the corresponding
  shift and AR parameters.

  \code{randomMarResiduals} selects random parameters for each mixture
  component and returns the corresponding residuals.
  \code{randomMarParametersKernel} is a helper function which does the
  computation for one component.

  \code{tsDesignMatrixExtended} forms the extended design matrix
  corresponding to a subsample. This is used for least square estimation
  of the parameters. 

}
\author{Georgi N. Boshnakov}
\seealso{
  \code{\link{tomarparambyComp}}
}
\keyword{ MixAR }
