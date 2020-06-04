\name{mix_se-methods}
\docType{methods}
\alias{mix_se}
\alias{mix_se-methods}
\alias{mix_se,ANY,list-method}
\alias{mix_se,ANY,mixAR-method}
\alias{mix_se,ANY,mixARGaussian-method}
\title{Compute standard errors of estimates of mixAR models}
\description{
  Compute standard errors of estimates of mixAR models.
}
\usage{
mix_se(x, model, fix_shift)
}
\arguments{
  \item{x}{time series.}
  \item{model}{
    mixAR model, an object inheriting from class \dQuote{mixAR}.
  }
  \item{fix_shift}{
    \code{logical}. Should the shift paramters be fixed? Default is
    \code{FALSE}.
  }
}
\details{

  :TODO:

}
\value{
  a list with components:
  \item{standard_errors}{
    Standard error of parameter estimates,
  }
  \item{covariance_matrix}{
    The covariance matrix, obtained as inverse of the information matrix,
  }
  \item{Complete_Information}{
    Complete information matrix,
  }
  \item{Missing_Information}{
    Missing information matrix.
  }
}
\references{For formulas used in the computation, see:

  \insertRef{WongPhD}{mixAR}
}
\author{
  Davide Ravagli
}
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\section{Methods}{
\describe{

\item{\code{signature(x = "ANY", model = "list")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "ANY", model = "mixAR")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "ANY", model = "mixARGaussian")}}{
%%  ~~describe this method here~~
}
}}
\examples{
## Example with IBM data

## data(ibmclose, package = "fma")

## Wong&Li estimates as starting values
## TODO: @Davide - can you give exact reference (paper, page)
##                 To me 0.5339 below seems different from WongLi; 0.5439?.
##                 Apparently this is a different model.
##
moWLprob <- exampleModels$WL_ibm@prob    # 2019-12-15; was: c(0.5339,0.4176,0.0385)     
moWLsigma <- exampleModels$WL_ibm@scale  #                  c(4.8227,6.0082,18.1716)
moWLar <- list(-0.3208, 0.6711,0)        # @Davide - is this from some model?

moWLibm <- new("mixARGaussian", prob = moWLprob, scale = moWLsigma, arcoef = moWLar)

IBM <- diff(fma::ibmclose)
mix_se(as.numeric(IBM), moWLibm, fix_shift = TRUE)$'standard_errors'
}
\keyword{methods}
\keyword{mixAR}
