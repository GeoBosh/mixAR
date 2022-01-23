\name{mix_qf-methods}
\docType{methods}

\alias{mix_qf}
\alias{mix_qf-methods}
\alias{mix_qf,MixARGaussian,missing,missing,missing,numeric-method}
\alias{mix_qf,MixARGaussian,numeric,missing,missing,numeric-method}
\alias{mix_qf,MixARGaussian,numeric,numeric,numeric,missing-method}

\title{Conditional quantile functions of MixAR models}
\description{
  Gives conditional quantile functions of mixture autoregressive models.
}

\usage{
mix_qf(model, p, x, index, xcond)
}
\arguments{
  \item{model}{mixAR model.}
  \item{p}{vector of probabilities.}
  \item{x}{time series.}
  \item{index}{vector of positive integers.}
  \item{xcond}{
    the past values needed for the conditional distribution, a numeric
    vector of length at least the maximal AR order of the components.
  }
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  depending on the arguments, a function for computing quantiles or
  a numeric vector representing quantiles, see sections 'Details' and
  'Methods
}  
%\references{
%}
\author{Georgi N. Boshnakov}

\section{Methods}{
\describe{

\item{\code{signature(model = "MixARGaussian", p = "missing", x = "missing", index = "missing", xcond = "numeric")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(model = "MixARGaussian", p = "numeric", x = "missing", index = "missing", xcond = "numeric")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(model = "MixARGaussian", p = "numeric", x = "numeric", index = "numeric", xcond = "missing")}}{
%%  ~~describe this method here~~
}
}}
\seealso{
  \code{\link{mix_pdf}},
  \code{\link{mix_cdf}};

  \code{\link{mix_moment}} for examples
}
%\examples{
%}
\keyword{methods}
\keyword{distribution}
\keyword{ts}

\concept{MixAR}
\concept{prediction}
