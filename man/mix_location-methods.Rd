\name{mix_location-methods}
\docType{methods}
%\alias{mix_location}
\alias{mix_location-methods}
\alias{mix_location,mixAR,missing,missing,missing-method}
\alias{mix_location,mixAR,missing,missing,numeric-method}
\alias{mix_location,mixAR,numeric,missing,missing-method}
\alias{mix_location,mixAR,numeric,numeric,missing-method}
\title{Conditional mean of mixAR models}
\description{Conditional mean of mixAR models.}
\section{Methods}{
\describe{

\item{\code{signature(model = "mixAR", x = "missing", index = "missing", xcond = "missing")}}{
  This method returns a function with argument \code{xcond}, suitable
  for calls with many values of \code{xcond}.   
}

\item{\code{signature(model = "mixAR", x = "missing", index = "missing", xcond = "numeric")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(model = "mixAR", x = "numeric", index = "missing", xcond = "missing")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(model = "mixAR", x = "numeric", index = "numeric", xcond = "missing")}}{
%%  ~~describe this method here~~
}
}}
\seealso{
  \code{\link{mix_location}} for examples and related functions
}
\keyword{methods}
\keyword{mixAR}
\keyword{internal}
