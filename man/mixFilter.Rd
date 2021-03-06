\name{mixFilter}
\docType{methods}
\alias{mixFilter}
\alias{mixFilter-methods}
\alias{mixFilter,ANY,ANY,ANY-method}
\alias{mixFilter,numeric,raggedCoef,numeric-method}
\title{
  Filter time series with MixAR filters
}
\description{
  Filter time series with MixAR filters, a generic function with no
  default method (currently).
}
\usage{
mixFilter(x, coef, index, shift = 0, residual = FALSE, scale = 1)
}
\arguments{
  \item{x}{time series}
  \item{coef}{the filter coefficients}
  \item{index}{indices for which to calculate the filtered values.}
  \item{shift}{optional shifts (intercept) terms.}
  \item{residual}{
    If FALSE (default) calculate ``predictions'', if TRUE calculate
    ``residuals''.
  }
  \item{scale}{
    optional scale factor(s), makes sense only when
    \code{residual=TRUE}, corresponds to \code{scale} in the
    specification of a MixAR model.
  }
}
\value{
  a \code{MixComp} object
}
\author{Georgi N. Boshnakov}
\section{Methods}{
\describe{

\item{\code{signature(x = "ANY", coef = "ANY", index = "ANY")}}{
  This  method simply prints an error message and stops.
}

\item{\code{signature(x = "numeric", coef = "raggedCoef", index = "numeric")}}{
  %Filter \code{x} and return a \code{MixComp} object.
}
}}
\seealso{
  \code{\link{raghat1}}
  \code{\link{mix_ek}}
  \code{\link{mix_hatk}}
}
\keyword{methods}
\keyword{MixAR}
\keyword{ts}
