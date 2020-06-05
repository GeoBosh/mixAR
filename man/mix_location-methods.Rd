\name{mix_location-methods}
\docType{methods}
%\alias{mix_location}
\alias{mix_location-methods}
\alias{mix_location,MixAR,missing,missing,missing-method}
\alias{mix_location,MixAR,missing,missing,numeric-method}
\alias{mix_location,MixAR,numeric,missing,missing-method}
\alias{mix_location,MixAR,numeric,numeric,missing-method}
\title{Conditional mean of MixAR models}
\description{Conditional mean of MixAR models.}
\section{Methods}{
\describe{

\item{\code{signature(model = "MixAR", x = "missing", index = "missing", xcond = "missing")}}{
  This method returns a function with argument \code{xcond}, suitable
  for calls with many values of \code{xcond}.   
}

\item{\code{signature(model = "MixAR", x = "missing", index = "missing", xcond = "numeric")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(model = "MixAR", x = "numeric", index = "missing", xcond = "missing")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(model = "MixAR", x = "numeric", index = "numeric", xcond = "missing")}}{
%%  ~~describe this method here~~
}
}}
\seealso{
  \code{\link{mix_location}} for examples and related functions
}
\keyword{methods}
\keyword{MixAR}
\keyword{internal}
