\name{mix_hatk}
\docType{methods}
\alias{mix_hatk}
\alias{mix_hatk-methods}

\alias{mix_hatk,mixAR,numeric,numeric,missing-method}
\title{Compute component predictions for mixAR models}
\description{

  Function and methods to compute component predictions for mixAR models
  % ~~ Methods for function \code{mix_hatk} in Package `mixAR' ~~
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
  \item{xcond}{~~ TODO: describe this argument. ~~}
}
%\details{
%  
%}
\author{Georgi N. Boshnakov}
\section{Methods}{
\describe{

  \item{}{\code{signature(model = "mixAR", x = "numeric", index = "numeric",
 xcond = "missing")}%}{
    : ~~describe this method here 
  }
}}
\seealso{
  \code{\linkS4class{mixAR}}
}
\keyword{methods}
\keyword{mixAR}
