\name{mix_hatk}
\docType{methods}
\alias{mix_hatk}
\alias{mix_hatk-methods}

\alias{mix_hatk,MixAR,numeric,numeric,missing-method}
\title{Compute component predictions for MixAR models}
\description{

  Function and methods to compute component predictions for MixAR models
  % ~~ Methods for function \code{mix_hatk} in Package `MixAR' ~~
}
\usage{
mix_hatk(model, x, index, xcond)
}
\arguments{
  \item{model}{a model.}
  \item{x}{time series.}
  \item{index}{
    a vector of positive integer specifying the indices for which to
    compute the residuals, has a natural default.
  }
  \item{xcond}{

    the past values needed for the conditional distribution, a numeric
    vector of length at least the maximal AR order of the components.

  }
}
%\details{
%  
%}
\author{Georgi N. Boshnakov}
\section{Methods}{
\describe{

  \item{}{\code{signature(model = "MixAR", x = "numeric", index = "numeric",
 xcond = "missing")}%}{
    %: ~~describe this method here 
  }
}}
\seealso{
  class \code{"\linkS4class{MixAR}"}
}
\keyword{methods}
\keyword{MixAR}
