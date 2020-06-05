\name{mix_moment}
\alias{mix_location}
\alias{mix_variance}
\alias{mix_central_moment}
\alias{mix_moment}
\alias{mix_kurtosis}
\alias{mix_ekurtosis}
\title{Conditional moments of MixAR models}
\description{Conditional moments of MixAR models.}
\usage{
mix_location(model, x, index, xcond)
mix_variance(model, x, index, xcond)
mix_central_moment(model, x, index, xcond, k)
mix_moment(model, x, index, xcond, k)
mix_kurtosis(...)
mix_ekurtosis(...)
}
\arguments{
  \item{model}{a MixAR object.}
  \item{x}{a time series.}
  \item{index}{
    a vector of indices in \code{x} for which to compute the requested
    property.  If missing, the computation is done for all indices
    greater than \code{max(model@order)}.
  }
  \item{xcond}{
    a time series, the point prediction is computed for the
    first value after the end of the time series. Only the last
    \code{max(model@order)} values in \code{xcond} are used.
  }
  \item{k}{a positive integer specifying the moment to compute.}
  \item{...}{passed on to \code{mix_central_moment}.}
}
\details{
  These functions compute conditional moments and related quantities.

  \code{kurtosis} and \code{ekurtosis} compute conditional kurtosis and
  excess kurtosis, respectively. Effectively, they have the same
  parameters as \code{mix_central_moment}, since they pass \code{"..."}
  to it along with \code{k = 4}. It is an error to supply argument
  \code{k} to the kurtosis functions.
  
}
\value{
  when called with one argument (\code{model}), a function with argument \code{xcond};
  otherwise if \code{xcond} is not missing, a single numeric value;
  otherwise a vector of length \code{length(index)}.
}
\references{
  \insertRef{boshnakov2009mar}{mixAR}
}
\author{Georgi N. Boshnakov}
\note{
  \strong{TODO:} verify the above description. I wrote it recently
  from reading six years old code. 
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{mix_pdf}} for the predictive distributions
}
\examples{
## data(ibmclose, package = "fma") # `ibmclose'
ibmclose <- as.numeric(fma::ibmclose)
length(ibmclose) # 369
max(exampleModels$WL_ibm@order) # 2

## compute point predictions for t = 3,...,369
pred <- mix_location(exampleModels$WL_ibm, ibmclose)
plot(pred)
## compute one-step point predictions for t = 360,...369
mix_location(exampleModels$WL_ibm, ibmclose, index = 369 - 9:0 )

f <- mix_location(exampleModels$WL_ibm) # a function
## predict the value after the last
f(ibmclose)

## a different way to compute one-step point predictions for t = 360,...369
sapply(369 - 10:1, function(k) f(ibmclose[1:k]))

## the results are the same, but notice that xcond gives past values
## while index above specifies the times for which to compute the predictions.
identical(sapply(369 - 10:1, function(k) f(ibmclose[1:k])),
          mix_location(exampleModels$WL_ibm, ibmclose, index = 369 - 9:0 ))


## conditional variance
f <- mix_variance(exampleModels$WL_ibm) # a function
## predict the value after the last
f(ibmclose)

## a different way to compute one-step point predictions for t = 360,...369
sapply(369 - 10:1, function(k) f(ibmclose[1:k]))

## the results are the same, but notice that xcond gives past values
## while index above specifies the times for which to compute the predictions.
identical(sapply(369 - 10:1, function(k) f(ibmclose[1:k])),
          mix_variance(exampleModels$WL_ibm, ibmclose, index = 369 - 9:0 ))


# interesting example
# bimodal distribution, low kurtosis, 4th moment not much larger than 2nd
moWL <- exampleModels$WL_ibm

mix_location(moWL,xcond = c(500,450))
mix_kurtosis(moWL,xcond = c(500,450))

f1pdf <- mix_pdf(moWL,xcond = c(500,450))
f1cdf <- mix_cdf(moWL,xcond = c(500,450))
gbutils::plotpdf(f1pdf,cdf=f1cdf)
gbutils::plotpdf(f1cdf,cdf=f1cdf)
f1cdf(c(400,480))

mix_variance(moWL,xcond = c(500,450))
mix_central_moment(moWL,xcond = c(500,450), k=2)

sqrt(mix_variance(moWL,xcond = c(500,450)))
sqrt(mix_central_moment(moWL,xcond = c(500,450), k=2))
}
\keyword{MixAR}
\keyword{prediction}

